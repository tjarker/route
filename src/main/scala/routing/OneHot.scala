package routing

import chisel3._
import chisel3.util.PopCount

final class OneHot private[routing] (val bits: Int) extends Bundle {
  private val v: UInt = UInt(bits.W)
  def toUInt: UInt = v
  def apply(i: Int): Bool = {
    require(i >= 0 && i < bits, s"Index $i out of bounds for OneHot of size $bits")
    v(i)
  }
}

object OneHot {
  def apply(value: UInt): OneHot = {
    val bits = value.getWidth
    val oh = new OneHot(bits)
    oh.v := value
    assert(
      PopCount(value) === 1.U,
      cf"OneHot.apply: input value $value is not one-hot encoded"
    )
    oh
  }
}