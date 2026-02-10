package routing.simple

import chisel3._
import routing.Direction
import routing.{North, South, East, West, Local}
import chisel3.util.MixedVec
import routing.Coord
import scala.collection.immutable.SeqMap



class SimpleRoutingBlock[T <: Data](ingressDir: Direction, coord: Coord)(
    implicit p: SimpleNocParams[T]
) extends Module {
  val in = IO(Flipped(new PacketPort[T](ingressDir)))
  val routes = p.routingPolicy
    .getRule(ingressDir)
    .generateLogic(in.bits.dest, coord)

  val to = IO(new DirectionBundle[T](routes.keys.toSeq))

  val shouldRoute = dontTouch(VecInit(routes.values.toSeq))

  val readies = routes.map { case (dir, shouldRoute) =>
    to(dir).valid := shouldRoute && in.valid
    to(dir).bits := in.bits
    shouldRoute && to(dir).ready
  }
  in.ready := readies.reduce(_ || _)
  to.all.foreach(_.bits := in.bits)
}

class DirectionBundle[T <: Data](val directions: Seq[Direction])(implicit p: SimpleNocParams[T]) extends Record {

  val map = SeqMap(directions.map(dir => dir -> new PacketPort[T](dir)):_*)
  override def elements: SeqMap[String,Data] = SeqMap(map.toSeq.map { case (dir, signal) => dir.toString -> signal }:_*)
  def apply(dir: Direction): PacketPort[T] = this.map(dir)
  def all: Seq[PacketPort[T]] = directions.map(map)

  def <>(that: DirectionBundle[T]): Unit = {
    require(this.directions.toSet == that.directions.toSet, "DirectionBundles must have the same directions to connect")
    for (dir <- directions) {
      this(dir) <> that(dir)
    }
  }
}