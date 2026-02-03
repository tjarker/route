package routing.simple

import chisel3._
import chisel3.util._

import routing.Direction._
import routing._

import net.liftweb.json._
import liftoff._

import liftoff.pathToFileOps

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

  def evalRouter[T <: Data](noc: => SimpleRouter[T], dir: WorkingDirectory): librelane.AwaitableResult = {

    val sdcFile = "layout/simple_router/ports_constraints.sdc".toFile
    val pinFile = "layout/simple_router/pin_order.cfg".toFile

    val res = librelane.hardenChisel(noc, dir, JObject(
      JField("CLOCK_PERIOD", JDouble(10.0)),
      JField("FP_CORE_UTIL", JInt(50)),
      JField("PNR_SDC_FILE", JString(sdcFile.getAbsolutePath)),
      JField("SIGNOFF_SDC_FILE", JString(sdcFile.getAbsolutePath)),
      JField("FP_PIN_ORDER_CFG", JString(pinFile.getAbsolutePath)),
      JField("FP_PDN_MULTILAYER", JBool(false))
    ))
    
    res

  }

  val p = SimpleNocParams[UInt](
    nx = 4,
    ny = 4,
    payloadGen = () => UInt(32.W),
    bufferFactory = SimpleBuffer,
    arbiterFactory = ChiselArbiter,
    routingPolicy = XYRouting
  )

  val dir = "build/libre-harden-simple-noc".toDir

  val chiselDir = dir.addSubDir(dir / "chisel")

  val resChiselRun = evalRouter(new SimpleRouter(Coord(2,2))(p), chiselDir)

  val customBufferDir = dir.addSubDir(dir / "custom_buffer")

  val customBufferRun = evalRouter(new SimpleRouter(Coord(2,2))(p.copy(bufferFactory = DoubleRegBuffer)), customBufferDir)

  val resChiselBuf = resChiselRun.await()
  val customBufferRes = customBufferRun.await()

  dir.addFile(s"chiselbuf-result.json", prettyRender(resChiselBuf.metrics))

  dir.addFile(s"custombuf-result.json", prettyRender(customBufferRes.metrics))

  
}


