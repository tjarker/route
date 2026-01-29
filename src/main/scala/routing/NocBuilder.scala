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

class Noc[P <: RouterPortLike](val portType: P, topologyBuilder: TopologyBuilder, routerFactory: RouterFactory) extends Module {

  val abstractPorts: Seq[RouterPortLike] = topologyBuilder.create(routerFactory)

  val ports = IO(Vec(abstractPorts.length, portType.chisel))

  ports.zip(abstractPorts).foreach { case (ioPort, abstractPort) =>
    ioPort.ingress.ready := abstractPort.chisel.ingress.ready
    abstractPort.chisel.ingress.valid := ioPort.ingress.valid
    abstractPort.chisel.ingress.bits := ioPort.ingress.bits

    abstractPort.chisel.egress.ready := ioPort.egress.ready
    ioPort.egress.valid := abstractPort.chisel.egress.valid
    ioPort.egress.bits := abstractPort.chisel.egress.bits
  }

}
