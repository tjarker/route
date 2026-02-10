package routing.simple

import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.matchers.should.Matchers

import chisel3._
import liftoff._
import routing._

class ArbiterTests extends AnyWordSpec with Matchers {

  implicit val p: SimpleNocParams[UInt] = SimpleNocParams.default.copy(
    payloadGen = () => UInt(4.W),
  )

  Reporting.setOutput(Reporting.NullStream)

  def test(a: Arbiter[UInt]) = {

    a.out.bits.payload.dependsCombinationallyOn(a.from.all.flatMap { case p => 
      Seq(p.bits.payload)
    })
    a.out.valid.dependsCombinationallyOn(a.from.all.map(_.valid))

    a.from.all.foreach { ingress =>
      ingress.ready.dependsCombinationallyOn(Seq(a.out.ready))
    }

    a.reset.poke(1.B)
    a.clock.step()
    a.reset.poke(0.B)

    a.from.all.zipWithIndex.foreach { case (ingress, i) =>
      Task {
        val bfm = new nocbench.DecoupledProducer(ingress, a.clock)
        Seq.fill(10)(SimplePacket.Lit(Coord(0,0) -> Coord(1,1), i.U)).foreach { pkt =>
          val receipt = bfm.send(pkt, timeout = 4)
          assert(receipt.isOk, s"ingress $ingress was starved")
        }
      }
    }


    val consumerTask = Task {

      val consumer = new nocbench.DecoupledConsumer(a.out, a.clock)

      val received = consumer.receiveAll(a.from.all.length * 10, 1)

      assert(received.forall(_.isOk), s"Receiver did not receive packets every cycle")

      val counts = (0 until a.from.all.length).map(i => received.count(_.data.payload.litValue == i))
      assert(counts.forall(_ == 10), s"Expected 10 packets from each ingress, but got counts: $counts")

    }.join()
    
  }

  def testArbiter(a: => Arbiter[UInt], dir: WorkingDirectory) = {
    simulateChisel(a, dir)(test)
  }


  "ChiselArbiter should arbitrate correctly" in {
    testArbiter(new ChiselArbiter[UInt](Local, Seq(North, South, East, West)), "build/arbiter_tests/chisel".toDir)
  }

  "BlobArbiter should arbitrate correctly" in {
    testArbiter(new BlobArbiter[UInt](Local, Seq(North, South, East, West)), "build/arbiter_tests/blob".toDir)
  }

  "RotateUnrotateArbiter should arbitrate correctly" in {
    testArbiter(new RotateUnrotateArbiter[UInt](Local, Seq(North, South, East, West)), "build/arbiter_tests/rotate_unrotate".toDir)
  }

  "MaskedPriorityArbiter should arbitrate correctly" in {
    testArbiter(new MaskedPriorityArbiter[UInt](Local, Seq(North, South, East, West)), "build/arbiter_tests/masked_priority".toDir)
  }

  "TdmArbiter should arbitrate correctly" in {
    testArbiter(new TdmArbiter[UInt](Local, Seq(North, South, East, West)), "build/arbiter_tests/tdm".toDir)
  }

  "TdmOnehotArbiter should arbitrate correctly" in {
    testArbiter(new TdmOnehotArbiter[UInt](Local, Seq(North, South, East, West)), "build/arbiter_tests/tdm_one_hot".toDir)
  }

}
