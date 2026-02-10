package routing.simple

import chisel3._
import chisel3.util._
import chisel3.experimental.BundleLiterals._

import routing.Direction
import routing.PacketPortLike
import routing.Coord

class Destination(implicit p: SimpleNocParams[_]) extends Bundle {
  val x = p.xCoordType
  val y = p.yCoordType
  /// dest.y > src.y
  val southbound = Bool()
  /// dest.x < src.x
  val eastbound = Bool()
  def coord: Coord = Coord(x.litValue.toInt, y.litValue.toInt)
}

class SimplePacket[T <: Data](implicit p: SimpleNocParams[T]) extends Bundle {
  val dest = new Destination
  val payload = p.payloadType

  override def toString(): String = {
    s"Packet((${dest.x.litValue}, ${dest.y.litValue}), ${payload})" 
  }
}

object SimplePacket {
  def Lit[T <: Data](destX: Int, destY: Int, southbound: Boolean, eastbound: Boolean, payload: T)(implicit p: SimpleNocParams[T]): SimplePacket[T] = {
    (new SimplePacket).Lit(
      _.dest.x -> destX.U,
      _.dest.y -> destY.U,
      _.dest.southbound -> southbound.B,
      _.dest.eastbound -> eastbound.B,
      _.payload -> payload
    )
  }

  def Lit[T <: Data](fromto: (Coord, Coord), payload: T)(implicit p: SimpleNocParams[T]): SimplePacket[T] = {
    val (from, to) = fromto
    val southbound = to.y > from.y
    val eastbound = to.x < from.x
    Lit(to.x, to.y, southbound, eastbound, payload)
  }
}

class PacketPort[T <: Data](val dir: Direction)(implicit p: SimpleNocParams[T]) extends DecoupledIO(new SimplePacket[T]) with PacketPortLike {
  type P = SimplePacket[T]
  def chisel: DecoupledIO[P] = this
}
