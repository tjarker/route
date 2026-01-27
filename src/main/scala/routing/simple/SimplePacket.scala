package routing.simple

import chisel3._
import chisel3.util._

import routing.Direction

class Destination(implicit p: SimpleParam[_]) extends Bundle {
  val x = p.xCoordType
  val y = p.yCoordType
  /// dest.y > src.y
  val southbound = Bool()
  /// dest.x < src.x
  val eastbound = Bool()
}

class SimplePacket[T <: Data](implicit p: SimpleParam[T]) extends Bundle {
  val dest = new Destination
  val payload = p.payloadType
}

class PacketPort[T <: Data](val dir: Direction)(implicit p: SimpleParam[T]) extends chisel3.util.DecoupledIO(new SimplePacket[T]) {

}
