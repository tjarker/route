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

class SimpleRouterFactory[T <: Data](p: SimpleNocParams[T]) extends RouterFactory {
  override def createRouter(coord: Coord): RouterLike = {
    Module(new SimpleRouter[T](coord)(p)).suggestName(s"router_${coord.x}_${coord.y}")
  }
}

object SimpleNoc extends App {

  val p = SimpleNocParams[UInt](
    nx = 4,
    ny = 4,
    payloadGen = () => UInt(32.W),
    bufferFactory = SimpleBuffer,
    arbiterFactory = ChiselArbiter,
    routingPolicy = XYRouting
  )
  //emitVerilog(new SimpleRouter[UInt](Coord(2,2))(p), Array("--target-dir", "generated"))

  emitVerilog(
    NocBuilder.build(new SimpleRouterPort[UInt](Local)(p), new TorusBuilder(2,2), new SimpleRouterFactory[UInt](p)),
    Array("--target-dir", "generated")
  )
}


