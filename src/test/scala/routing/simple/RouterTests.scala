package routing.simple

import chisel3._
import liftoff._
import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.matchers.should.Matchers
import routing.Coord


class RouterTests extends AnyWordSpec with Matchers {

  val config = SimpleParam[UInt](
    nx = 4,
    ny = 4,
    payloadGen = () => UInt(32.W),
    bufferFactory = SimpleBuffer,
    arbiterFactory = ChiselArbiter,
    routingPolicy = XYRouting
  )

  val routerModel = ChiselModel(new SimpleRouter(Coord(2,2))(config), "build/router-model".toDir)


  "SimpleRouter" should {
    "route one packet from west to north" in routerModel.simulate("simulations/router-west-to-north".toDir) { dut =>
      
      dut.ports.west.ingress.bits.dest.x.poke(2.U)
      dut.ports.west.ingress.bits.dest.y.poke(1.U)
      dut.ports.west.ingress.bits.dest.southbound.poke(0.B)
      dut.ports.west.ingress.bits.payload.poke(0xdeadbeefL.U)
      dut.ports.west.ingress.valid.poke(1.B)
      dut.clock.stepUntil(dut.ports.west.ingress.ready, 1.B)
      dut.clock.step()
      dut.ports.west.ingress.valid.poke(0.B)

      dut.clock.stepUntil(dut.ports.north.egress.valid, 1.B)
      dut.ports.north.egress.bits.payload.expect(0xdeadbeefL.U)
      dut.clock.step()
      dut.ports.north.egress.ready.poke(1.B)
      dut.clock.step()
      dut.ports.north.egress.ready.poke(0.B)
      dut.clock.step(10)





    }
  }


}
