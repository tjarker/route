package routing.simple

import chisel3._
import chisel3.util._
import chisel3.reflect.DataMirror

abstract class Buffer[T <: Data](d: PacketPort[T])(implicit p: SimpleParam[T]) extends Module {
  val enq = IO(Flipped(new PacketPort(d.dir)))
  val deq = IO(new PacketPort(d.dir))
}
abstract class BufferFactory {
  def apply[T <: Data](d: PacketPort[T], depth: Int)(implicit p: SimpleParam[T]): Buffer[T]
}

class SimpleBuffer[T <: Data](d: PacketPort[T], depth: Int)(implicit p: SimpleParam[T]) extends Buffer[T](d) {
  val queue = Module(new Queue(new SimplePacket[T], depth))
  enq <> queue.io.enq
  deq <> queue.io.deq
}

object SimpleBuffer extends BufferFactory {
  override def apply[T <: Data](d: PacketPort[T], depth: Int)(implicit p: SimpleParam[T]): Buffer[T] = {
    Module(new SimpleBuffer[T](d, depth))
  }
}