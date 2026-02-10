package routing.simple

import chisel3._

import routing.Direction
import routing.{North, South, East, West, Local}
import chisel3.util.MixedVec
import routing.Coord
import routing.RouterPortLike
import routing.PacketPortLike
import routing.RouterLike

class SimpleRouter[T <: Data](val coord: Coord)(implicit p: SimpleNocParams[T])
    extends Module with RouterLike {
  
  type P = SimplePacket[T]

  val ports = IO(SimpleRouterIO(coord))

  // create buffers for all 5 ingress ports
  val buffers = ports.ingresses.map(port =>
    p.bufferFactory(port, depth = 1).suggestName(s"buffer_${port.dir}")
  )

  // connect ingress ports to buffers
  val buffedIngresses = ports.ingresses.zip(buffers).map { case (port, buf) =>
    port <> buf.enq
    buf.deq
  }

  val routingBlocks = ports.ingresses.map { port =>
    Module(new SimpleRoutingBlock[T](port.dir, coord)).suggestName(s"routingBlock_${port.dir}")
  }

  // create routing blocks for all buffered ingress ports and collect their fanout
  val ingressFanout = buffedIngresses.zip(routingBlocks).flatMap { case (bufOut, routingBlock) =>
    routingBlock.in <> bufOut
    routingBlock.to.all.map(port => PacketInTransit(port, bufOut.dir, port.dir))
  }

  // group requests by egress direction
  val egressRequests = ingressFanout.groupBy(_.to)

  // create arbiters for each egress direction and connect them to the egress ports
  egressRequests.foreach { case (dir, reqPorts) =>
    val arbiter = p // create arbiter
      .arbiterFactory(dir, reqPorts.map(_.from))
      .suggestName(s"arbiter_${dir}")
    reqPorts // connect requests to arbiter inputs
      .foreach { case PacketInTransit(req, origin, _) =>
        req <> arbiter.from(origin)
      }
    // connect arbiter output to egress port
    ports(dir).egress <> arbiter.out
  }

  def port(dir: Direction): SimpleRouterPort[T] = ports(dir)


}

case class PacketInTransit[T <: Data](packetPort: PacketPort[T], from: Direction, to: Direction)

class SimpleRouterPort[T <: Data](val coord: Coord, val dir: Direction)(implicit
    p: SimpleNocParams[T]
) extends Bundle with RouterPortLike {
  type P = SimplePacket[T]
  val ingress = Flipped(new PacketPort[T](dir)).suggestName(s"ingress_${dir}")
  val egress = new PacketPort[T](dir).suggestName(s"egress_${dir}")

  def ingressPort = ingress
  def egressPort = egress

  def chisel = this
}

class SimpleRouterIO[T <: Data](val coord: Coord)(implicit p: SimpleNocParams[T]) extends Bundle {
  val north = new SimpleRouterPort[T](coord, North)
  val south = new SimpleRouterPort[T](coord, South)
  val east = new SimpleRouterPort[T](coord, East)
  val west = new SimpleRouterPort[T](coord, West)
  val local = new SimpleRouterPort[T](coord, Local)

  def all: Seq[SimpleRouterPort[T]] = Seq(north, east, south, west, local)
  def apply(dir: Direction): SimpleRouterPort[T] = dir match {
    case North => north
    case East  => east
    case South => south
    case West  => west
    case Local => local
  }
  def ingresses: Seq[PacketPort[T]] = all.map(_.ingress)
  def egresses: Seq[PacketPort[T]] = all.map(_.egress)
}

object SimpleRouterIO {
  def apply[T <: Data](coord: Coord)(implicit p: SimpleNocParams[T]): SimpleRouterIO[T] = {
    new SimpleRouterIO[T](coord)
  }
}
