package routing.simple

import chisel3._
import chisel3.util._

import routing.Direction._
import routing._

case class SimpleNocParams[T <: Data](
  nx: Int, 
  ny: Int, 
  payloadGen: () => T,
  bufferFactory: BufferFactory,
  arbiterFactory: ArbiterFactory,
  routingPolicy: RoutingPolicy
) {
  val dimensions: (Int, Int) = (nx, ny)
  def payloadType: T = payloadGen()
  def xCoordType: UInt = UInt(log2Ceil(nx).W)
  def yCoordType: UInt = UInt(log2Ceil(ny).W)
}

object SimpleNoC extends App {

  val p = SimpleNocParams[UInt](
    nx = 4,
    ny = 4,
    payloadGen = () => UInt(8.W),
    bufferFactory = SimpleBuffer,
    arbiterFactory = ChiselArbiter,
    routingPolicy = XYRouting
  )
  emitVerilog(new SimpleRouter[UInt](Coord(2,2))(p), Array("--target-dir", "generated"))
}



abstract class FlowModule[I <: Data, DI <: DecoupledIO[I], O <: Data, DO <: DecoupledIO[O]](decI: DI, decO: DO) extends Module {
  val flow = IO(new Bundle {
    val in = Flipped(decI)
    val out = decO
  })

  def <>: (that: FlowModule[I, DI, _, _]): Unit = {
    this.flow.out <> that.flow.in
  }
}