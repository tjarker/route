package routing

package object nocbench {

  import chisel3._
  import chisel3.util._

  case class NocTx(
    ingress: Coord,
    egress: Coord,
    payload: BigInt,
    waitCycles: Int
  )

  trait NocTxReceipt {
    def ingress: Coord
    def egress: Coord
    def payload: BigInt
    def cycle: Int
  }

  case class NocSendReceipt(
    ingress: Coord,
    egress: Coord,
    payload: BigInt,
    cycle: Int
  ) extends NocTxReceipt

  case class NocRecvReceipt(
    ingress: Coord,
    egress: Coord,
    payload: BigInt,
    cycle: Int
  ) extends NocTxReceipt


  trait NocPortBfmFactory {
    def createBfm[P <: Data](here: Coord, p: Bundle {
    val ingress: DecoupledIO[P]
    val egress: DecoupledIO[P]
  }): NocPortBfm
  }
  

  trait NocPortBfm {
    def coord: Coord
    def sendIngress(tx: NocTx): NocSendReceipt
    def observeIngress(): NocSendReceipt
    def receiveEgress(): NocRecvReceipt
    def observeEgress(): NocRecvReceipt
  }
}
