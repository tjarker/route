package routing.simple

import chisel3._
import chisel3.util._
import liftoff._
import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.matchers.should.Matchers
import routing._
import liftoff.misc.WorkingDirectory

class DecoupledProducer[T <: Data](port: DecoupledIO[T], clock: Clock) {
  def send(data: T): Unit = {
    val oldValue = port.bits.peek()
    port.bits.poke(data)
    port.valid.poke(1.B)
    clock.stepUntil(port.ready, 1.B, maxCycles = 100)
    clock.step() // commit handshake
    port.valid.poke(0.B)
    port.bits.poke(oldValue)
  }
  def send(data: Seq[T]): Unit = {
    data.foreach(send)
  }
}

class DecoupledConsumer[T <: Data](port: DecoupledIO[T], clock: Clock) {
  def expect(expected: T): Unit = {
    port.ready.poke(1.B)
    clock.stepUntil(port.valid, 1.B, maxCycles = 100)
    port.bits.expect(expected)
    clock.step() // commit handshake
    port.ready.poke(0.B)
  }
  def expect(expected: Seq[T]): Unit = {
    expected.foreach(expect)
  }
}

class RouterTests extends AnyWordSpec with Matchers {

  implicit val config: SimpleNocParams[UInt] = SimpleNocParams[UInt](
    nx = 4,
    ny = 4,
    payloadGen = () => UInt(32.W),
    bufferFactory = SimpleBuffer,
    arbiterFactory = ChiselArbiter,
    routingPolicy = XYRouting
  )

  Reporting.setOutput(Reporting.NullStream)
  val routerModel = ChiselModel(
    new SimpleRouter(Coord(2, 2))(config),
    "build/router-model".toDir
  )

  def routerTest(
      sendPackets: Map[Direction, Seq[SimplePacket[UInt]]],
      expectedPackets: Map[Direction, Seq[SimplePacket[UInt]]],
      runDir: WorkingDirectory
  ) = {
    val logFile = runDir.addLoggingFile("test.log")
    Reporting.setOutput(logFile, colored = false)
    routerModel.simulate(runDir) { dut =>
      dut.reset.poke(1.B)
      dut.clock.step()
      dut.reset.poke(0.B)
      Task.scope {
        for ((dir, packets) <- sendPackets) yield Task {
          val producer =
            new DecoupledProducer(dut.ports(dir).ingress, dut.clock)
          producer.send(packets)
        }
        for ((dir, packets) <- expectedPackets) yield Task {
          val consumer = new DecoupledConsumer(dut.ports(dir).egress, dut.clock)
          consumer.expect(packets)
        }
      }
    }
  }

  "SimpleRouter" should {

    "route packets from north to south or local" in {
      val southBound = SimplePacket.Lit[UInt](
        2,
        3,
        southbound = true,
        eastbound = false,
        payload = 0x1.U
      )
      val localBound = SimplePacket.Lit[UInt](
        2,
        2,
        southbound = false,
        eastbound = false,
        payload = 0x2.U
      )
      routerTest(
        Map(
          North -> Seq(southBound, localBound)
        ),
        Map(
          South -> Seq(southBound),
          Local -> Seq(localBound)
        ),
        "simulations/router-test/north".toDir
      )
    }

    "route packets from south to north or local" in {
      val northBound = SimplePacket.Lit[UInt](
        2,
        1,
        southbound = false,
        eastbound = false,
        payload = 0x3.U
      )
      val localBound = SimplePacket.Lit[UInt](
        2,
        2,
        southbound = false,
        eastbound = false,
        payload = 0x4.U
      )

      routerTest(
        Map(
          South -> Seq(northBound, localBound)
        ),
        Map(
          North -> Seq(northBound),
          Local -> Seq(localBound)
        ),
        "simulations/router-test/south".toDir
      )
    }

    "route packets from east to north, south, west or local" in {
      val westBound = SimplePacket.Lit[UInt](
        1,
        2,
        southbound = false,
        eastbound = false,
        payload = 0x5.U
      )
      val southBound = SimplePacket.Lit[UInt](
        2,
        3,
        southbound = true,
        eastbound = false,
        payload = 0x7.U
      )
      val northBound = SimplePacket.Lit[UInt](
        2,
        1,
        southbound = false,
        eastbound = false,
        payload = 0x8.U
      )
      val localBound = SimplePacket.Lit[UInt](
        2,
        2,
        southbound = false,
        eastbound = false,
        payload = 0x6.U
      )
      routerTest(
        Map(
          East -> Seq(westBound, southBound, northBound, localBound)
        ),
        Map(
          West -> Seq(westBound),
          South -> Seq(southBound),
          North -> Seq(northBound),
          Local -> Seq(localBound)
        ),
        "simulations/router-test/east".toDir
      )
    }

    "route packets from west to north, south, east or local" in {
      val eastBound = SimplePacket.Lit[UInt](
        3,
        2,
        southbound = false,
        eastbound = true,
        payload = 0x9.U
      )
      val southBound = SimplePacket.Lit[UInt](
        2,
        3,
        southbound = true,
        eastbound = false,
        payload = 0xB.U
      )
      val northBound = SimplePacket.Lit[UInt](
        2,
        1,
        southbound = false,
        eastbound = false,
        payload = 0xC.U
      )
      val localBound = SimplePacket.Lit[UInt](
        2,
        2,
        southbound = false,
        eastbound = false,
        payload = 0xA.U
      )
      routerTest(
        Map(
          West -> Seq(eastBound, southBound, northBound, localBound)
        ),
        Map(
          East -> Seq(eastBound),
          South -> Seq(southBound),
          North -> Seq(northBound),
          Local -> Seq(localBound)
        ),
        "simulations/router-test/west".toDir
      )
    }

    "route packets from local to north, south, east or west" in {
      val eastBound = SimplePacket.Lit[UInt](
        3,
        2,
        southbound = false,
        eastbound = true,
        payload = 0x1.U
      )
      val southBound = SimplePacket.Lit[UInt](
        2,
        3,
        southbound = true,
        eastbound = false,
        payload = 0x2.U
      )
      val northBound = SimplePacket.Lit[UInt](
        2,
        1,
        southbound = false,
        eastbound = false,
        payload = 0x3.U
      )
      val westBound = SimplePacket.Lit[UInt](
        1,
        2,
        southbound = false,
        eastbound = false,
        payload = 0x4.U
      )
      routerTest(
        Map(
          Local -> Seq(eastBound, southBound, northBound, westBound)
        ),
        Map(
          East -> Seq(eastBound),
          South -> Seq(southBound),
          North -> Seq(northBound),
          West -> Seq(westBound)
        ),
        "simulations/router-test/local".toDir
      )
    }
  }
}