package routing

import chisel3._
import chisel3.util.DecoupledIO

trait PacketPortLike {
  type P <: Data // packet type
  def dir: Direction
  def chisel: DecoupledIO[P]
}

trait RouterPortLike {
  type P <: Data
  def dir: Direction
  def coord: Coord
  def ingressPort: PacketPortLike
  def egressPort: PacketPortLike
  def chisel: Bundle {
    val ingress: DecoupledIO[P]
    val egress: DecoupledIO[P]
  }
}

trait RouterLike {
  def coord: Coord
  def port(dir: Direction): RouterPortLike
  def allPorts: Seq[RouterPortLike] = {
    Seq(North, South, East, West, Local).map(port)
  }
}

abstract class RouterFactory {
  def createRouter(coord: Coord): RouterLike
}
