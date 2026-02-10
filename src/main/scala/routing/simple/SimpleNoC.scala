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

object SimpleNocParams {
  def default: SimpleNocParams[UInt] = SimpleNocParams[UInt](
    nx = 4,
    ny = 4,
    payloadGen = () => UInt(32.W),
    bufferFactory = ChiselQueueBuffer,
    arbiterFactory = ChiselArbiter,
    routingPolicy = XYRouting
  )
}

class SimpleRouterFactory[T <: Data](p: SimpleNocParams[T]) extends RouterFactory {
  override def createRouter(coord: Coord): RouterLike = {
    Module(new SimpleRouter[T](coord)(p)).suggestName(s"router_${coord.x}_${coord.y}")
  }
}

object SimpleNoc extends App {

  def evalRouter[T <: Data](noc: => SimpleRouter[T], dir: WorkingDirectory): librelane.AwaitableResult = {

    //val sdcFile = "layout/simple_router/ports_constraints.sdc".toFile
    val sdcFile = "dummy_constraints.sdc".toFile
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

  

  val bufferFactory = SingleRegBuffer
  val arbiterFactory = MaskedPriorityArbiter

  val p = SimpleNocParams.default.copy(
    bufferFactory = bufferFactory,
    arbiterFactory = arbiterFactory
  )

  val routerFactory = () => new SimpleRouter(Coord(2,2))(p)

  val dir = "build/libre-harden-router".toDir

  val runner = evalRouter(routerFactory(), dir)

  val res = runner.await()

  dir.addFile(s"result.json", prettyRender(res.metrics))

  println(res.areaDelayTable())

  
}


object BufferCompare extends App {

  val p = SimpleNocParams[UInt](
    nx = 4,
    ny = 4,
    payloadGen = () => UInt(32.W),
    bufferFactory = ChiselQueueBuffer,
    arbiterFactory = ChiselArbiter,
    routingPolicy = XYRouting
  )

  val buf1 = librelane.hardenChisel(new DoubleRegBuffer[UInt](new PacketPort[UInt](Local)(p))(p), "build/libre-harden-double-reg-buffer".toDir, JObject(
    JField("CLOCK_PERIOD", JDouble(10.0))
  ))
  val buf2 = librelane.hardenChisel(new OptimizedDoubleRegBuffer[UInt](new PacketPort[UInt](Local)(p))(p), "build/libre-harden-double-buffer-reg2".toDir, JObject(
    JField("CLOCK_PERIOD", JDouble(10.0))
  ))


  buf1.await()
  buf2.await()
}