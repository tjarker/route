package routing.nocbench

import chisel3._
import chisel3.util._
import routing._
import liftoff._

import scala.util.Random

trait TrafficPattern {
  def start(
    ingress: Coord, 
    size: Coord, 
    ingressPort: RoundTripReceiverPort[NocTx, NocSendReceipt],
    egressPort: RoundTripReceiverPort[Int, NocRecvReceipt]
  ): Receipt[Unit]
}

class AllToAll(repetitions: Int) extends TrafficPattern {

  def start(
    ingress: Coord, 
    size: Coord, 
    ingressPort: RoundTripReceiverPort[NocTx, NocSendReceipt],
    egressPort: RoundTripReceiverPort[Int, NocRecvReceipt]
  ): Receipt[Unit] = {

    val ingressComplete = new Receipt[Unit]
    
    Task {
      for (r <- 0 until repetitions) {
        for (x <- 0 until size.x; y <- 0 until size.y) {
          val egress = Coord(x, y)
          if (ingress != egress) {
            val tx = NocTx(
              ingress = ingress,
              egress = egress,
              payload = ((ingress.x + ingress.y * size.x) << 16) | (egress.x + egress.y * size.x), // Payload can be set to any value as needed
              waitCycles = 0//Random.nextInt(10)
            )
            val receipt = ingressPort.send(tx)
            println(s"Ingress $ingress sent to Egress $egress with payload ${tx.payload} at cycle ${receipt.cycle}")
          }
        }
      }
      ingressComplete.complete(())
    }

    val egressComplete = new Receipt[Unit]
    val expectedReceivedTxs = repetitions * (size.x * size.y - 1)

    Task {
      for (i <- 0 until expectedReceivedTxs) {
        val waitCycles = 0 // Random.nextInt(10)
        val receipt = egressPort.send(waitCycles) // Wait cycles can be set to 0 for receive
        println(s"Egress $ingress received from Ingress ${receipt.ingress} with payload ${receipt.payload} at cycle ${receipt.cycle}")
      }
      egressComplete.complete(())
    }

    ingressComplete.combine(egressComplete).map(_ => ())
  }

}
