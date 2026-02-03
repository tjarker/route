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

class SimpleBuffer[T <: Data](d: PacketPort[T], depth: Int)(implicit p: SimpleNocParams[T]) extends Buffer[T](d) {
  val queue = Module(new Queue(new SimplePacket[T], depth))
  enq <> queue.io.enq
  deq <> queue.io.deq
}

object SimpleBuffer extends BufferFactory {
  override def apply[T <: Data](d: PacketPort[T], depth: Int)(implicit p: SimpleNocParams[T]): Buffer[T] = {
    Module(new SimpleBuffer[T](d, depth))
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