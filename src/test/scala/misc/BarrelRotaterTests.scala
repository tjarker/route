package misc

import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.matchers.should.Matchers

import chisel3._
import liftoff._

class BarrelRotaterTests extends AnyWordSpec with Matchers {

  class Rotater extends Module {
    val in = IO(Input(Vec(4, UInt(8.W))))
    val shamt = IO(Input(UInt(2.W)))
    val outLeft = IO(Output(Vec(4, UInt(8.W))))
    val outRight = IO(Output(Vec(4, UInt(8.W))))

    outLeft := BarrelRotater.left(in, shamt)
    outRight := BarrelRotater.right(in, shamt)
  }


  "BarrelRotater" should {
    "rotate left" in {
      simulateChisel(new Rotater, "build/rotater-test/left".toDir) { dut =>
        for (i <- 0 until 4) {
          dut.in(i).poke((i + 1).U)
        }
        for (shamt <- 0 until 4) {
          dut.shamt.poke(shamt.U)
          dut.clock.step()
          val expected = BarrelRotater.rotatedLeft(Seq(1,2,3,4), shamt)
          dut.outLeft.zip(expected).foreach { case (out, exp) =>
            out.expect(exp.U)
          }
        }
      }
    }

    "rotate right" in {
      simulateChisel(new Rotater, "build/rotater-test/right".toDir) { dut =>
        for (i <- 0 until 4) {
          dut.in(i).poke((i + 1).U)
        }
        for (shamt <- 0 until 4) {
          dut.shamt.poke(shamt.U)
          dut.clock.step()
          val expected = BarrelRotater.rotatedRight(Seq(1,2,3,4), shamt)
          dut.outRight.zip(expected).foreach { case (out, exp) =>
            out.expect(exp.U)
          }
        }
      }
    }
  }

}
