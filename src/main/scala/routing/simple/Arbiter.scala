package routing.simple

import chisel3._
import chisel3.util._

import routing.Direction

abstract class Arbiter[T <: Data](egressDir: Direction, n: Int)(implicit p: SimpleParam[T]) extends Module {
  val in = IO(Flipped(Vec(n, new PacketPort(egressDir))))
  val out = IO(new PacketPort[T](egressDir))

}
abstract class ArbiterFactory {
  def apply[T <: Data](egressDir: Direction, n: Int)(implicit p: SimpleParam[T]): Arbiter[T]
}

class ChiselArbiter[T <: Data](egressDir: Direction, n: Int)(implicit p: SimpleParam[T]) extends Arbiter[T](egressDir, n) {
  val rr = Module(new RRArbiter(new SimplePacket[T], n))
  for (i <- 0 until n) {
    in(i) <> rr.io.in(i)
  }
  out <> rr.io.out
}
object ChiselArbiter extends ArbiterFactory {
  override def apply[T <: Data](egressDir: Direction, n: Int)(implicit p: SimpleParam[T]): Arbiter[T] = {
    Module(new ChiselArbiter[T](egressDir, n))
  }
}
