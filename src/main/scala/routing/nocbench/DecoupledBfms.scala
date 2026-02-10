package routing.nocbench

import chisel3._
import chisel3.util._
import liftoff._

trait DecoupledTransfer[T <: Data] {
  import DecoupledTransfer._
  def data: T
  def cycleInitiated: Int
  def timeInitiated: Time
  def waitedCycles: Int
  def waitedTime: Time
  def isOk: Boolean = this match { case _: Ok[T] => true; case _ => false}
  def asOk: Ok[T] = this match {
    case ok: Ok[T] => ok
    case _ => throw new Exception("DecoupledTransfer is not Ok")
  }
  def timedOut: Boolean = this match { case _: Timeout[T] => true; case _ => false}

  override def toString(): String = this match {
    case Ok(data, cycleInitiated, cycleCompleted, timeInitiated, timeCompleted) =>
      s"Ok($data, start=$cycleInitiated, end=$cycleCompleted)"
    case Timeout(data, cycleInitiated, cycleTimedOut, timeInitiated, timeTimedOut) =>
      s"Timeout($data, start=$cycleInitiated, end=$cycleTimedOut)"
  }
}
object DecoupledTransfer {
  case class Ok[T <: Data](
    data: T, 
    cycleInitiated: Int,
    cycleCompleted: Int,
    timeInitiated: Time,
    timeCompleted: Time
  ) extends DecoupledTransfer[T] {
    def waitedCycles: Int = cycleCompleted - cycleInitiated
    def waitedTime: Time = timeCompleted - timeInitiated
  }
  case class Timeout[T <: Data](
    data: T,
    cycleInitiated: Int,
    cycleTimedOut: Int,
    timeInitiated: Time,
    timeTimedOut: Time,
  ) extends DecoupledTransfer[T] {
    def waitedCycles: Int = cycleTimedOut - cycleInitiated
    def waitedTime: Time = timeTimedOut - timeInitiated
  }
}


class DecoupledProducer[T <: Data](port: DecoupledIO[T], clock: Clock) {
  def send(data: T, timeout: Int = -1): DecoupledTransfer[T] = {
    val oldValue = port.bits.peek()
    port.bits.poke(data)
    port.valid.poke(1.B)
    val startCycle = clock.cycle
    val startTime = Sim.time

    Task.withRegion(Region.Monitor) {
      clock.stepUntil(port.ready, 1.B, maxCycles = timeout) match {

        case StepUntilResult.Success(_) =>
          val acceptedCycle = clock.cycle
          val acceptedTime = Sim.time
          clock.step() // commit handshake
          port.valid.poke(0.B)
          port.bits.poke(oldValue)
          DecoupledTransfer.Ok(data, startCycle, acceptedCycle, startTime, acceptedTime)
        
        case StepUntilResult.Timeout(_) =>
          val timedOutCycle = clock.cycle
          val timedOutTime = Sim.time
          port.valid.poke(0.B)
          port.bits.poke(oldValue)
          DecoupledTransfer.Timeout(data, startCycle, timedOutCycle, startTime, timedOutTime)
      }
    }.join()
    
  }
  def sendAll(data: Seq[T], timeout: Int = -1): Seq[DecoupledTransfer[T]] = {
    data.map(send(_, timeout))
  }
}

class DecoupledConsumer[T <: Data](port: DecoupledIO[T], clock: Clock) {
  def receive(timeout: Int = -1): DecoupledTransfer[T] = {
    port.ready.poke(1.B)

    Task.withRegion(Region.Monitor) {
      clock.stepUntil(port.valid, 1.B, maxCycles = timeout) match {

        case StepUntilResult.Success(_) =>
          val receivedCycle = clock.cycle
          val receivedTime = Sim.time
          val received = port.bits.peek()
          clock.step() // commit handshake
          port.ready.poke(0.B)
          DecoupledTransfer.Ok(received, receivedCycle - 1, receivedCycle, receivedTime, receivedTime)
        
        case StepUntilResult.Timeout(_) =>
          val timedOutCycle = clock.cycle
          val timedOutTime = Sim.time
          port.ready.poke(0.B)
          DecoupledTransfer.Timeout(port.bits.peek(), timedOutCycle, timedOutCycle, timedOutTime, timedOutTime)
      }
    }.join()
  }
  def receiveAll(n: Int, timeout: Int = -1): Seq[DecoupledTransfer[T]] = {
    (0 until n).map(_ => receive(timeout))
  }
  def expect(expected: T, timeout: Int = -1): DecoupledTransfer[T] = {
    port.ready.poke(1.B)

    Task.withRegion(Region.Monitor) {
      clock.stepUntil(port.valid, 1.B, maxCycles = timeout) match {

        case StepUntilResult.Success(_) =>
          val receivedCycle = clock.cycle
          val receivedTime = Sim.time
          port.bits.expect(expected)
          clock.step() // commit handshake
          port.ready.poke(0.B)
          DecoupledTransfer.Ok(port.bits.peek(), receivedCycle - 1, receivedCycle, receivedTime, receivedTime)

        case StepUntilResult.Timeout(_) =>
          val timedOutCycle = clock.cycle
          val timedOutTime = Sim.time
          port.ready.poke(0.B)
          DecoupledTransfer.Timeout(port.bits.peek(), timedOutCycle, timedOutCycle, timedOutTime, timedOutTime)
      }
      
    }.join()
  }

  def expectAll(expected: Seq[T], timeout: Int = -1): Unit = {
    expected.foreach(expect(_, timeout))
  }
}

class DecoupledMonitor[T <: Data](port: DecoupledIO[T], clock: Clock) {
  def observe(): DecoupledTransfer.Ok[T] = {
    clock.stepUntil(port.valid, 1.B)
    val startCycle = clock.cycle
    val startTime = Sim.time
    clock.stepUntil(port.ready, 1.B)
    val acceptedCycle = clock.cycle
    val acceptedTime = Sim.time
    val data = port.bits.peek()
    clock.step() // commit handshake
    DecoupledTransfer.Ok(data, startCycle, acceptedCycle, startTime, acceptedTime)
  }
}
