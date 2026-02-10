package routing.simple

import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.matchers.should.Matchers

import chisel3._
import liftoff._

import routing._

class SimpleNocTest extends AnyWordSpec with Matchers {

  "SimpleNoC" should {

    implicit val params: SimpleNocParams[UInt] = SimpleNocParams[UInt](
      nx = 2,
      ny = 2,
      payloadGen = () => UInt(32.W),
      bufferFactory = ChiselQueueBuffer,
      arbiterFactory = ChiselArbiter,
      routingPolicy = XYRouting
    )

    Reporting.setOutput(Reporting.NullStream)
    val model = ChiselModel(
      NocBuilder.build(new SimpleRouterPort[UInt](Coord(1,1), Local)(params), new MeshBuilder(params.nx, params.ny), new SimpleRouterFactory[UInt](params)),
      "build/noc-model".toDir
    )

    "route packets correctly" in {
      val runDir = "simulations/simple-noc/routing-test".toDir
      val logFile = runDir.addLoggingFile("test.log")
      Reporting.setOutput(logFile, colored = false)
      model.simulate(runDir) { dut =>

        dut.reset.poke(1.B)
        dut.clock.step()
        dut.reset.poke(0.B)

        val producers = Seq.tabulate(4)(i => new nocbench.DecoupledProducer(dut.ports(i).ingress, dut.clock))
        val consumers = Seq.tabulate(4)(i => new nocbench.DecoupledConsumer(dut.ports(i).egress, dut.clock))

        val packetTo_1_0 = SimplePacket.Lit(1, 0, false, true, 0x1.U)
        val packetTo_0_1 = SimplePacket.Lit(0, 1, true, false, 0x2.U)
        val packetTo_1_1 = SimplePacket.Lit(1, 1, true, true, 0x3.U)

        Task {
          producers(0).sendAll(Seq(packetTo_1_0, packetTo_0_1, packetTo_1_1))
        }
        

        for ((x,y) <- Seq((0,1), (1,0), (1,1))) yield Task {
          consumers(y * params.nx + x).expect(if (x == 1 && y == 0) packetTo_1_0
            else if (x == 0 && y == 1) packetTo_0_1
            else packetTo_1_1)
        }

      }
    }


  }

}
