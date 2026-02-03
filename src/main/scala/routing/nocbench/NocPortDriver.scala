package routing.nocbench

import chisel3._
import chisel3.util._
import routing._
import liftoff._


class NocPortDriver(nocPort: NocPortBfm) extends Component {

  val ingressDriver = new NocPortIngressDriver(nocPort)
  val egressDriver = new NocPortEgressDriver(nocPort)
  val ingressMonitor = new NocPortMonitor(nocPort, _.observeIngress())
  val egressMonitor = new NocPortMonitor(nocPort, _.observeEgress())

  val ingressPort = ingressDriver.port
  val egressPort = egressDriver.port
  val monitorPort = Port.sender[NocTxReceipt]
  monitorPort.forward(ingressMonitor.port)
  monitorPort.forward(egressMonitor.port)

}

class NocPortIngressDriver(nocPort: NocPortBfm) extends Component with SimPhase {

  val port = Port.receiver[(NocTx, Option[Channel[NocSendReceipt]])]
  
  def drive(tx: NocTx): NocSendReceipt = {
    val responseChannel = Channel[NocSendReceipt]()
    port.channel.send((tx, Some(responseChannel)))
    responseChannel.receive()
  }

  val clock = Config.get(NocBench.Clock)

  def sim(): Unit = port.foreach { case (tx, receiptChannel) =>
    clock.step(tx.waitCycles)
    val receipt = nocPort.sendIngress(tx)
    receiptChannel.map(_.send(receipt))
  }

}

class NocPortEgressDriver(nocPort: NocPortBfm) extends Component with SimPhase {

  val port = Port.receiver[(Int, Channel[NocRecvReceipt])]

  def drive(waitCycles: Int): NocRecvReceipt = {
    val responseChannel = Channel[NocRecvReceipt]()
    port.channel.send((waitCycles, responseChannel))
    responseChannel.receive()
  }
  
  val clock = Config.get(NocBench.Clock)

  def sim(): Unit = port.foreach { case (waitCycles, receiptChannel) =>
    clock.step(waitCycles)
    val receipt = nocPort.receiveEgress()
    receiptChannel.send(receipt)
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