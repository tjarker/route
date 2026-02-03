package routing

package object nocbench {

  import chisel3._

  case class NocTx(
    ingressId: Int,
    egressId: Int,
    payload: BigInt,
    waitCycles: Int
  )

  trait NocTxReceipt {
    def ingressId: Int
    def egressId: Int
    def payload: BigInt
    def cycle: Int
  }

  case class NocSendReceipt(
    ingressId: Int,
    egressId: Int,
    payload: BigInt,
    cycle: Int
  ) extends NocTxReceipt

  case class NocRecvReceipt(
    ingressId: Int,
    egressId: Int,
    payload: BigInt,
    cycle: Int
  ) extends NocTxReceipt

  

  trait NocPortBfm {
    def sendIngress(tx: NocTx): NocSendReceipt
    def observeIngress(): NocSendReceipt
    def receiveEgress(): NocRecvReceipt
    def observeEgress(): NocRecvReceipt
  }
}
