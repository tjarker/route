package routing.simple

import chisel3._

import routing.Direction
import routing.{North, South, East, West, Local}
import chisel3.util.MixedVec
import routing.Coord

class SimpleRoutingBlock[T <: Data](ingressDir: Direction, coord: Coord)(
    implicit p: SimpleParam[T]
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

class SimpleRouter[T <: Data](coord: Coord)(implicit p: SimpleParam[T])
    extends Module {

  val ports = IO(RouterIO())

  ports.ingresses.foreach { inPort =>
    inPort.ready := 1.B
  }

  ports.egresses.foreach { outPort =>
    outPort.valid := 0.B
    outPort.bits := 0.U.asTypeOf(outPort.bits)
  }

  val buffers = ports.ingresses.map( port =>
    p.bufferFactory(port, depth = 4).suggestName(s"buffer_${port.dir}")
  )

  val buffedIngresses = ports.ingresses.zip(buffers).map { case (port, buf) =>
    port <> buf.enq
    buf.deq
  }

  val ingressFanout = buffedIngresses.flatMap { bufOut =>
    val routingBlock = Module(new SimpleRoutingBlock[T](bufOut.dir, coord))
    routingBlock.in <> bufOut
    routingBlock.out
  }

  val egressRequests = ingressFanout.groupBy(_.dir)

  egressRequests.foreach { case (dir, reqPorts) =>
    val arbiter = p.arbiterFactory(dir, reqPorts.length).suggestName(s"arbiter_${dir}")
    reqPorts.zip(arbiter.in).foreach { case (reqPort, arbIn) =>
      arbIn <> reqPort
    }
    ports(dir).egress <> arbiter.out
  }

}

class RouterPort[T <: Data](val dir: Direction)(implicit p: SimpleParam[T])
    extends Bundle {
  val ingress = Flipped(new PacketPort[T](dir)).suggestName(s"ingress_${dir}")
  val egress = new PacketPort[T](dir).suggestName(s"egress_${dir}")
}
class RouterIO[T <: Data](implicit p: SimpleParam[T]) extends Bundle {
  val north = new RouterPort[T](North)
  val south = new RouterPort[T](South)
  val east = new RouterPort[T](East)
  val west = new RouterPort[T](West)
  val local = new RouterPort[T](Local)

  def all: Seq[RouterPort[T]] = Seq(north, east, south, west, local)
  def apply(dir: Direction): RouterPort[T] = dir match {
    case North => north
    case East  => east
    case South => south
    case West  => west
    case Local => local
  }
  def ingresses: Seq[PacketPort[T]] = all.map(_.ingress)
  def egresses: Seq[PacketPort[T]] = all.map(_.egress)
}
object RouterIO {
  def apply[T <: Data]()(implicit p: SimpleParam[T]): RouterIO[T] = {
    new RouterIO[T]
  }
}
