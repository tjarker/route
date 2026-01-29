package routing.simple

import chisel3._
import chisel3.util._

import routing.Direction

abstract class Arbiter[T <: Data](egressDir: Direction, ingresses: Seq[Direction])(implicit p: SimpleNocParams[T]) extends Module {
  val from: DirectionBundle[T] 
  val out: PacketPort[T]
}
abstract class ArbiterFactory {
  def apply[T <: Data](egressDir: Direction, ingresses: Seq[Direction])(implicit p: SimpleNocParams[T]): Arbiter[T]
}

class ChiselArbiter[T <: Data](egressDir: Direction, ingresses: Seq[Direction])(implicit p: SimpleNocParams[T]) extends Arbiter[T](egressDir, ingresses) {
  val from = IO(Flipped(new DirectionBundle(ingresses)))
  val out = IO(new PacketPort[T](egressDir))
  
  val rr = Module(new RRArbiter(new SimplePacket[T], ingresses.length))

  from.all.zipWithIndex.foreach { case (ingress, i) =>
    val arb = rr.io.in(i)
    arb.valid := ingress.valid
    arb.bits := ingress.bits
    ingress.ready := arb.ready
  }
  out <> rr.io.out
}
object ChiselArbiter extends ArbiterFactory {
  override def apply[T <: Data](egressDir: Direction, ingresses: Seq[Direction])(implicit p: SimpleNocParams[T]): Arbiter[T] = {
    Module(new ChiselArbiter[T](egressDir, ingresses))
  }
}
