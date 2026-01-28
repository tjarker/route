package routing.simple

import chisel3._
import routing.Direction
import routing.{North, South, East, West, Local}
import chisel3.util.MixedVec
import routing.Coord

class SimpleRoutingBlock[T <: Data](ingressDir: Direction, coord: Coord)(
    implicit p: SimpleNocParams[T]
) extends Module {
  val in = IO(Flipped(new PacketPort[T](ingressDir)))
  val routes = p.routingPolicy
    .getRule(ingressDir)
    .generateLogic(in.bits.dest, coord)
  val out = IO(MixedVec(routes.keys.map(dir => new PacketPort(dir)).toSeq))

  val readies = out.zip(routes.toSeq).map { case (outPort, (dir, shouldRoute)) =>
    outPort.valid := shouldRoute && in.valid
    outPort.bits := in.bits
    shouldRoute && outPort.ready
  }
  in.ready := readies.reduce(_ || _)
  out.foreach(_.bits := in.bits)
}