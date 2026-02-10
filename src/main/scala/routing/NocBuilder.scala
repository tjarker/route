package routing

import chisel3._
import chisel3.util.DecoupledIO

object NocBuilder {

  def build[P <: RouterPortLike](
      portType: P,
      topologyBuilder: TopologyBuilder,
      routerFactory: RouterFactory
  ): Noc[P] = {
    new Noc(portType, topologyBuilder, routerFactory)
  }

}

trait NocLike {
  def localPorts: Seq[RouterPortLike]
}

class Noc[P <: RouterPortLike](val portType: P, topologyBuilder: TopologyBuilder, routerFactory: RouterFactory) extends Module with NocLike {

  val abstractPorts: Seq[RouterPortLike] = topologyBuilder.create(routerFactory)

  val ports = IO(Vec(abstractPorts.length, portType.chisel))

  def localPorts: Seq[RouterPortLike] = abstractPorts.zip(ports).map { case (abstractPort, ioPort) =>
    new RouterPortLike {
      type P = portType.P
      def dir: Direction = abstractPort.dir
      def coord: Coord = abstractPort.coord
      def ingressPort: PacketPortLike = new PacketPortLike {
        type P = portType.P
        def dir: Direction = abstractPort.dir
        def chisel: DecoupledIO[P] = ioPort.ingress
      }
      def egressPort: PacketPortLike = new PacketPortLike {
        type P = portType.P
        def dir: Direction = abstractPort.dir
        def chisel: DecoupledIO[P] = ioPort.egress
      }
      def chisel: Bundle {
        val ingress: DecoupledIO[P]
        val egress: DecoupledIO[P]
       } = ioPort
    }
  }

  ports.zip(abstractPorts).foreach { case (ioPort, abstractPort) =>
    ioPort.ingress.ready := abstractPort.chisel.ingress.ready
    abstractPort.chisel.ingress.valid := ioPort.ingress.valid
    abstractPort.chisel.ingress.bits := ioPort.ingress.bits

    abstractPort.chisel.egress.ready := ioPort.egress.ready
    ioPort.egress.valid := abstractPort.chisel.egress.valid
    ioPort.egress.bits := abstractPort.chisel.egress.bits
  }

}
