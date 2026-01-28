package routing.simple

import chisel3._

import routing.Direction
import routing.{North, South, East, West, Local}
import chisel3.util.MixedVec
import routing.Coord

class SimpleRouter[T <: Data](coord: Coord)(implicit p: SimpleNocParams[T])
    extends Module {

  val ports = IO(SimpleRouterIO())

  // create buffers for all 5 ingress ports
  val buffers = ports.ingresses.map(port =>
    p.bufferFactory(port, depth = 4).suggestName(s"buffer_${port.dir}")
  )

  // connect ingress ports to buffers
  val buffedIngresses = ports.ingresses.zip(buffers).map { case (port, buf) =>
    port <> buf.enq
    buf.deq
  }

  // create routing blocks for all buffered ingress ports and collect their fanout
  val ingressFanout = buffedIngresses.flatMap { bufOut =>
    val routingBlock = Module(new SimpleRoutingBlock[T](bufOut.dir, coord))
    routingBlock.in <> bufOut
    routingBlock.out
  }

  // group requests by egress direction
  val egressRequests = ingressFanout.groupBy(_.dir)

  // create arbiters for each egress direction and connect them to the egress ports
  egressRequests.foreach { case (dir, reqPorts) =>
    val arbiter = p // create arbiter
      .arbiterFactory(dir, reqPorts.length)
      .suggestName(s"arbiter_${dir}")
    reqPorts // connect requests to arbiter inputs
      .zip(arbiter.in)
      .foreach { case (reqPort, arbIn) =>
        arbIn <> reqPort
      }
    // connect arbiter output to egress port
    ports(dir).egress <> arbiter.out
  }

}

class SimpleRouterPort[T <: Data](val dir: Direction)(implicit
    p: SimpleNocParams[T]
) extends Bundle {
  val ingress = Flipped(new PacketPort[T](dir)).suggestName(s"ingress_${dir}")
  val egress = new PacketPort[T](dir).suggestName(s"egress_${dir}")
}

class SimpleRouterIO[T <: Data](implicit p: SimpleNocParams[T]) extends Bundle {
  val north = new SimpleRouterPort[T](North)
  val south = new SimpleRouterPort[T](South)
  val east = new SimpleRouterPort[T](East)
  val west = new SimpleRouterPort[T](West)
  val local = new SimpleRouterPort[T](Local)

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
  def apply[T <: Data]()(implicit p: SimpleNocParams[T]): SimpleRouterIO[T] = {
    new SimpleRouterIO[T]
  }
}
