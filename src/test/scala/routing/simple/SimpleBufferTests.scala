package routing.simple

import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.matchers.should.Matchers
import routing.nocbench.DecoupledProducer
import routing.nocbench.DecoupledConsumer

import scala.util.Random

import chisel3._
import liftoff._
import routing.Coord
import routing.Local

case class Tx(data: SimplePacket[UInt], delay: Int)

class SimpleBufferTests extends AnyWordSpec with Matchers {

  implicit val p: SimpleNocParams[UInt] = SimpleNocParams.default

  Reporting.setOutput(Reporting.NullStream)


  def test(b: Buffer[UInt]) = {

    b.reset.poke(1.B)
    b.clock.step()
    b.reset.poke(0.B)

    val in = new DecoupledProducer(b.enq, b.clock)
    val out = new DecoupledConsumer(b.deq, b.clock)

    Random.setSeed(1532567890L)

    val pckts = Seq.tabulate(200) { i =>
      SimplePacket.Lit(Coord(0,0) -> Coord(1,1), i.U)
    }

    val producer = pckts.map( pkt => Tx(pkt, Random.nextInt(3)))
    val consumer = pckts.map( pkt => Tx(pkt, Random.nextInt(3)))

    Task.scope {
      Task {
        producer.foreach { case Tx(pkt, delay) =>
          b.clock.step(delay)
          in.send(pkt, timeout = 10)
        }
      }
      Task.withRegion(Region.Monitor) {
        consumer.foreach { case Tx(pkt, delay) =>
          b.clock.step(delay)
          out.expect(pkt, timeout = 10)
        }
      }
    }

  }

  def runTest(b: => Buffer[UInt], dir: WorkingDirectory) = {
    simulateChisel(b, dir)(test)
  }

  "ChiselBuffer should buffer correctly" in runTest(new ChiselQueueBuffer(new PacketPort(Local), 2), "build/buffertests/chiselbuf".toDir)

  "DoubleRegBuffer should buffer correctly" in runTest(new DoubleRegBuffer(new PacketPort(Local)), "build/buffertests/doubleregbuf".toDir)

  "Optimized DoubleRegBuffer should buffer correctly" in runTest(new OptimizedDoubleRegBuffer(new PacketPort(Local)), "build/buffertests/doubleregbuf2".toDir)

  "SingleRegBuffer should buffer correctly" in runTest(new SingleRegBuffer(new PacketPort(Local)), "build/buffertests/singleregbuf".toDir)

}