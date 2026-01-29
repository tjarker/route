package routing

import chisel3._

abstract class TopologyBuilder {

  def create(routerFactory: RouterFactory): Seq[RouterPortLike]

}


class MeshBuilder(nx: Int, ny: Int) extends TopologyBuilder {

  def create(routerFactory: RouterFactory): Seq[RouterPortLike] = {

    val routers = Seq.tabulate(nx, ny) { (x, y) =>
      routerFactory.createRouter(Coord(x, y))
    }

    for (x <- 0 until nx; y <- 0 until ny) {
      val router = routers(x)(y)
      if (x > 0) {
        val westRouter = routers(x - 1)(y)
        router.port(West).ingressPort.chisel <> westRouter.port(East).egressPort.chisel
        westRouter.port(East).ingressPort.chisel <> router.port(West).egressPort.chisel
      } else {
        router.port(West).ingressPort.chisel.valid := 0.B
        router.port(West).ingressPort.chisel.bits := DontCare
        router.port(West).egressPort.chisel.ready := 1.B
      }
      if (x < nx - 1) {
        val eastRouter = routers(x + 1)(y)
        router.port(East).ingressPort.chisel <> eastRouter.port(West).egressPort.chisel
        eastRouter.port(West).ingressPort.chisel <> router.port(East).egressPort.chisel
      } else {
        router.port(East).ingressPort.chisel.valid := 0.B
        router.port(East).ingressPort.chisel.bits := DontCare
        router.port(East).egressPort.chisel.ready := 1.B
      }
      if (y < ny - 1) {
        val southRouter = routers(x)(y + 1)
        router.port(South).ingressPort.chisel <> southRouter.port(North).egressPort.chisel
        southRouter.port(North).ingressPort.chisel <> router.port(South).egressPort.chisel
      } else {
        router.port(South).ingressPort.chisel.valid := 0.B
        router.port(South).ingressPort.chisel.bits := DontCare
        router.port(South).egressPort.chisel.ready := 1.B
      }
      if (y > 0) {
        val northRouter = routers(x)(y - 1)
        router.port(North).ingressPort.chisel <> northRouter.port(South).egressPort.chisel
        northRouter.port(South).ingressPort.chisel <> router.port(North).egressPort.chisel
      } else {
        router.port(North).ingressPort.chisel.valid := 0.B
        router.port(North).ingressPort.chisel.bits := DontCare
        router.port(North).egressPort.chisel.ready := 1.B
      }
    }
    // seq should be index by (y * nx + x)
    for (i <- 0 until (nx * ny)) yield routers(i % nx)(i / nx).port(Local)
  }

}

class TorusBuilder(nx: Int, ny: Int) extends TopologyBuilder {

  def create(routerFactory: RouterFactory): Seq[RouterPortLike] = {

    val routers = Seq.tabulate(nx, ny) { (x, y) =>
      routerFactory.createRouter(Coord(x, y))
    }

    for (x <- 0 until nx; y <- 0 until ny) {
      val router = routers(x)(y)
      val westRouter = routers((x - 1 + nx) % nx)(y)
      router.port(West).ingressPort.chisel <> westRouter.port(East).egressPort.chisel
      westRouter.port(East).ingressPort.chisel <> router.port(West).egressPort.chisel

      val eastRouter = routers((x + 1) % nx)(y)
      router.port(East).ingressPort.chisel <> eastRouter.port(West).egressPort.chisel
      eastRouter.port(West).ingressPort.chisel <> router.port(East).egressPort.chisel

      val southRouter = routers(x)((y - 1 + ny) % ny)
      router.port(South).ingressPort.chisel <> southRouter.port(North).egressPort.chisel
      southRouter.port(North).ingressPort.chisel <> router.port(South).egressPort.chisel

      val northRouter = routers(x)((y + 1) % ny)
      router.port(North).ingressPort.chisel <> northRouter.port(South).egressPort.chisel
      northRouter.port(South).ingressPort.chisel <> router.port(North).egressPort.chisel
    }

    // seq should be index by (y * nx + x)
    for (i <- 0 until (nx * ny)) yield routers(i % nx)(i / nx).port(Local)
  }

}