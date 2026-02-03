package routing.nocbench


import chisel3._
import chisel3.util._
import routing._
import liftoff._

object NocBench {
  object Clock extends Config[Clock]
}

class NocBench(noc: Noc[_]) extends Test with ResetPhase {



  def reset(): Unit = {
    noc.reset.poke(1.B)
    noc.clock.step()
    noc.reset.poke(0.B)
  }
    
  def test(): Unit = ???

}
