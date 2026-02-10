package routing.simple

import chisel3._
import chisel3.util._
import chisel3.reflect.DataMirror

abstract class Buffer[T <: Data](d: PacketPort[T])(implicit p: SimpleNocParams[T]) extends Module {
  val enq = IO(Flipped(new PacketPort(d.dir)))
  val deq = IO(new PacketPort(d.dir))
}
abstract class BufferFactory {
  def apply[T <: Data](d: PacketPort[T], depth: Int)(implicit p: SimpleNocParams[T]): Buffer[T]
}

class ChiselQueueBuffer[T <: Data](d: PacketPort[T], depth: Int)(implicit p: SimpleNocParams[T]) extends Buffer[T](d) {
  val queue = Module(new Queue(new SimplePacket[T], depth))
  enq <> queue.io.enq
  deq <> queue.io.deq
}

object ChiselQueueBuffer extends BufferFactory {
  override def apply[T <: Data](d: PacketPort[T], depth: Int)(implicit p: SimpleNocParams[T]): Buffer[T] = {
    Module(new ChiselQueueBuffer[T](d, depth))
  }
}

class SingleRegBuffer[T <: Data](d: PacketPort[T])(implicit p: SimpleNocParams[T]) extends Buffer[T](d) {
  
  val bufferReg = Reg(chiselTypeOf(enq.bits))
  val validReg = RegInit(0.B)

  bufferReg := Mux(validReg, bufferReg, enq.bits)
  validReg := Mux(validReg, !deq.ready, enq.valid)

  enq.ready := !validReg
  deq.valid := validReg
  deq.bits := bufferReg

}

object SingleRegBuffer extends BufferFactory {
  override def apply[T <: Data](d: PacketPort[T], depth: Int)(implicit p: SimpleNocParams[T]): Buffer[T] = {
    require(depth == 1, s"SingleRegBuffer only supports depth 1, got $depth")
    Module(new SingleRegBuffer[T](d))
  }
}

class DoubleRegBuffer[T <: Data](d: PacketPort[T])(implicit p: SimpleNocParams[T]) extends Buffer[T](d) {
  
  object State extends ChiselEnum {
    val Empty, HoldsOne, HoldsTwo = Value
  }

  val stateReg = RegInit(State.Empty)
  val bufferReg = Reg(chiselTypeOf(enq.bits))
  val shadowReg = Reg(chiselTypeOf(enq.bits))

  switch(stateReg) {
    is(State.Empty) {
      when(enq.valid) {
        bufferReg := enq.bits
        stateReg := State.HoldsOne
      }
    }
    is(State.HoldsOne) {
      when(deq.ready && !enq.valid) {
        stateReg := State.Empty
      }

      when(deq.ready && enq.valid) {
        bufferReg := enq.bits
      }

      when(!deq.ready && enq.valid) {
        shadowReg := enq.bits
        stateReg := State.HoldsTwo
      }
    }
    is(State.HoldsTwo) {
      when(deq.ready) {
        bufferReg := shadowReg
        stateReg := State.HoldsOne
      }
    }
  }

  enq.ready := (stateReg === State.Empty) || (stateReg === State.HoldsOne)
  deq.valid := (stateReg === State.HoldsOne) || (stateReg === State.HoldsTwo)
  deq.bits := bufferReg

}

object DoubleRegBuffer extends BufferFactory {
  override def apply[T <: Data](d: PacketPort[T], depth: Int)(implicit p: SimpleNocParams[T]): Buffer[T] = {
    require(depth == 2, s"DoubleRegBuffer only supports depth 2, got $depth")
    Module(new DoubleRegBuffer[T](d))
  }
}

/**
  * when should the buffer reg HOLD its value?
  *   - when it holds valid data and that data is not being dequeued
  * what should happen when the buffer updates its value?
  *  - when the shadow register contains valid data (`two`), load from shadow register
  *  - otherwise, load from input
  * when should the shadow reg HOLD its value?
  *  when it holds valid data and 
  */

class OptimizedDoubleRegBuffer[T <: Data](d: PacketPort[T])(implicit p: SimpleNocParams[T]) extends Buffer[T](d) {
  
  val bufferReg = Reg(chiselTypeOf(enq.bits))
  val shadowReg = Reg(chiselTypeOf(enq.bits))
  val cntReg = RegInit(0.U(2.W))

  val two = cntReg(1)
  val nonEmpty = cntReg.orR

  bufferReg := Mux(!deq.ready && nonEmpty, bufferReg, Mux(two, shadowReg, enq.bits))
  shadowReg := Mux(!deq.ready && two, shadowReg, enq.bits)

  deq.valid := nonEmpty
  deq.bits := bufferReg
  enq.ready := !two

  val accepting = enq.fire
  val exiting = deq.fire

  val offset = Mux(exiting, Mux(accepting, 0.S, -1.S), Mux(accepting, 1.S, 0.S))

  cntReg := (cntReg.asSInt + offset).asUInt

}

object OptimizedDoubleRegBuffer extends BufferFactory {
  override def apply[T <: Data](d: PacketPort[T], depth: Int)(implicit p: SimpleNocParams[T]): Buffer[T] = {
    require(depth == 2, s"DoubleBufferReg2 only supports depth 2, got $depth")
    Module(new OptimizedDoubleRegBuffer[T](d))
  }
}

object DelayAreaBufferEval extends App {

  import liftoff.pathToFileOps
  import net.liftweb.json._
  import routing.librelane
  import routing.Local

  implicit val p: SimpleNocParams[UInt] = SimpleNocParams[UInt](
    nx = 4,
    ny = 4,
    payloadGen = () => UInt(32.W),
    bufferFactory = ChiselQueueBuffer,
    arbiterFactory = ChiselArbiter,
    routingPolicy = XYRouting
    )

  val bufs = Seq(
    () => new ChiselQueueBuffer(new PacketPort[UInt](Local), 4),
    () => new DoubleRegBuffer(new PacketPort[UInt](Local)),
    () => new OptimizedDoubleRegBuffer(new PacketPort[UInt](Local)),
    () => new SingleRegBuffer(new PacketPort[UInt](Local))
  )

  val dir = "build/libre-harden-buffer-eval".toDir

  val results = bufs.map { bufGen =>
    val buf = liftoff.chisel.ChiselBridge.elaborate(bufGen())
    val resultFuture = librelane.hardenChisel(bufGen(), dir.addSubDir(dir / buf.name), JObject(
      JField("CLOCK_PERIOD", JDouble(5.0)),
      JField("FP_CORE_UTIL", JInt(50))
    ))

    buf.name -> resultFuture
  }

  val table = results.map { case (name, fut) =>
    val res = fut.await()
    Seq(name, res.coreArea, res.criticalPath1v80)
  }

  val tableString = liftoff.Reporting.table(
    Seq("Buffer", "Area (um^2)", "Critical Path (ns)") +: table
  )

  println(tableString)

  dir.addFile("buffer-eval-table.txt", tableString)


}