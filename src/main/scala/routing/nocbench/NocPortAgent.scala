package routing.nocbench

import chisel3._
import chisel3.util._
import routing._
import liftoff._


class NocPortAgent(nocPort: NocPortBfm) extends Component {

  val ingressDriver = Component.create[NocPortIngressDriver](nocPort)
  val egressDriver = Component.create[NocPortEgressDriver](nocPort)
  val ingressMonitor = Component.create[NocPortMonitor](nocPort, (p: NocPortBfm) => p.observeIngress())
  val egressMonitor = Component.create[NocPortMonitor](nocPort, (p: NocPortBfm) => p.observeEgress())

  val ingressPort = ingressDriver.port
  val egressPort = egressDriver.port
  val monitorPort = Port.sender[NocTxReceipt]
  monitorPort.forwardTo(ingressMonitor.port)
  monitorPort.forwardTo(egressMonitor.port)

  def coord: Coord = nocPort.coord

}

class NocPortIngressDriver(nocPort: NocPortBfm) extends Component with SimPhase {

  val port = Port.roundTripReceiver[NocTx, NocSendReceipt]
  val clock = Config.get(NocBench.Clock)

  def sim(): Unit = port.foreach { tx =>
    clock.step(tx.waitCycles)
    val receipt = nocPort.sendIngress(tx)
    receipt
  }

}

class NocPortEgressDriver(nocPort: NocPortBfm) extends Component with SimPhase {

  val port = Port.roundTripReceiver[Int, NocRecvReceipt]
  
  val clock = Config.get(NocBench.Clock)

  def sim(): Unit = port.foreach { waitCycles =>
    clock.step(waitCycles)
    val receipt = nocPort.receiveEgress()
    receipt
  }

}

class NocPortMonitor(nocPort: NocPortBfm, observeFun: NocPortBfm => NocTxReceipt) extends Component with SimPhase {

  val port = Port.sender[NocTxReceipt]

  val clock = Config.get(NocBench.Clock)

  def sim(): Unit = Task.withRegion(Region.Monitor) {
    forever {
      val receipt = observeFun(nocPort)
      port.send(receipt)
    }
  }

}