package routing.nocbench


import chisel3._
import chisel3.util._
import routing._
import liftoff._
import routing.simple.SimplePacket

object NocBench {
  object Clock extends Config[Clock]
}

class NocBench(
  val noc: Noc[_], 
  bfmFactory: NocPortBfmFactory,
  trafficPattern: TrafficPattern
  ) extends Test with ResetPhase {

  Config.set(NocBench.Clock, noc.clock)

  val portBfms = noc.localPorts.map(p => bfmFactory.createBfm(p.coord, p.chisel))

  val agents = Component.createSeq[NocPortAgent](portBfms.length) { i =>
    Seq(portBfms(i))
  }

  def reset(): Unit = {
    noc.reset.poke(1.B)
    noc.clock.step()
    noc.reset.poke(0.B)
  }
    
  def test(): Unit = {
    val receipts = agents.zipWithIndex.map { case (agent, i) =>
      trafficPattern.start(
        ingress = agent.coord,
        size = Coord(2,2), // fix this, maybe ids are better
        ingressPort = agent.ingressPort,
        egressPort = agent.egressPort
      )
    }
    //receipts.foreach(_.await())

    noc.clock.step(100)
  }

}











import routing.simple.SimpleNocParams

object SimpleNocPortBfm {
  object NocParams extends Config[SimpleNocParams[UInt]]
}

object SimpleNocPortBfmFactory extends NocPortBfmFactory {
  def createBfm[P <: Data](here: Coord, p: Bundle { val ingress: DecoupledIO[P]; val egress: DecoupledIO[P] }): NocPortBfm = {
    new SimpleNocPortBfm(here, p.asInstanceOf[Bundle { val ingress: DecoupledIO[SimplePacket[UInt]]; val egress: DecoupledIO[SimplePacket[UInt]] }])
  }
}

class SimpleNocPortBfm(val coord: Coord, p: Bundle { val ingress: DecoupledIO[SimplePacket[UInt]]; val egress: DecoupledIO[SimplePacket[UInt]] }) extends NocPortBfm {
  val clock = Config.get(NocBench.Clock)
  val params = Config.get(SimpleNocPortBfm.NocParams)
  val ingressProducer = new DecoupledProducer(p.ingress, clock)
  val egressConsumer = new DecoupledConsumer(p.egress, clock)
  val ingressMonitor = new DecoupledMonitor(p.ingress, clock)
  val egressMonitor = new DecoupledMonitor(p.egress, clock)

  def sendIngress(tx: NocTx): NocSendReceipt = {
    require(tx.ingress == coord, s"Tx ingress ${tx.ingress} does not match BFM location $coord")
    val packet = SimplePacket.Lit(coord -> tx.egress, tx.payload.U)(params)
    val receipt = ingressProducer.send(packet)
    NocSendReceipt(tx.ingress, tx.egress, tx.payload, receipt.asOk.cycleCompleted)
  }

  def observeIngress(): NocSendReceipt = {
    val receipt = ingressMonitor.observe()
    NocSendReceipt(receipt.data.dest.coord, coord, receipt.data.payload.litValue, receipt.asOk.cycleCompleted)
  }

  def receiveEgress(): NocRecvReceipt = {
    val receipt = egressConsumer.receive()
    NocRecvReceipt(receipt.data.dest.coord, coord, receipt.data.payload.litValue, receipt.asOk.cycleCompleted)
  }

  def observeEgress(): NocRecvReceipt = {
    val receipt = egressMonitor.observe()
    NocRecvReceipt(receipt.data.dest.coord, coord, receipt.data.payload.litValue, receipt.asOk.cycleCompleted)
  }
}

object NocBenchSimpleNoc extends App {
  import routing.simple._

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
    NocBuilder.build(new SimpleRouterPort[UInt](Coord(0,0), Local)(params), new MeshBuilder(params.nx, params.ny), new SimpleRouterFactory[UInt](params)),
    "build/noc-bench-simple-noc".toDir
  )

  model.simulate("build/noc-bench-simple-noc/sim".toDir) { noc =>
    Config.set(SimpleNocPortBfm.NocParams, params)
    Test.run(new NocBench(
      noc = noc,
      bfmFactory = SimpleNocPortBfmFactory,
      trafficPattern = new AllToAll(1)
    ))
  }

}