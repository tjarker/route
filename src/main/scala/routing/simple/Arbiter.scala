package routing.simple

import chisel3._
import chisel3.util._

import routing.Direction
import misc.BarrelRotater

abstract class Arbiter[T <: Data](egressDir: Direction, ingresses: Seq[Direction])(implicit p: SimpleNocParams[T]) extends Module {
  val from: DirectionBundle[T] 
  val out: PacketPort[T]
}
abstract class ArbiterFactory {
  def apply[T <: Data](egressDir: Direction, ingresses: Seq[Direction])(implicit p: SimpleNocParams[T]): Arbiter[T]
}

class ChiselArbiter[T <: Data](egressDir: Direction, ingresses: Seq[Direction])(implicit p: SimpleNocParams[T]) extends Arbiter[T](egressDir, ingresses) {
  val from = IO(Flipped(new DirectionBundle(ingresses)))
  val out = IO(new PacketPort[T](egressDir))
  
  val rr = Module(new RRArbiter(new SimplePacket[T], ingresses.length))

  from.all.zipWithIndex.foreach { case (ingress, i) =>
    val arb = rr.io.in(i)
    arb.valid := ingress.valid
    arb.bits := ingress.bits
    ingress.ready := arb.ready
  }
  out <> rr.io.out
}
object ChiselArbiter extends ArbiterFactory {
  override def apply[T <: Data](egressDir: Direction, ingresses: Seq[Direction])(implicit p: SimpleNocParams[T]): Arbiter[T] = {
    Module(new ChiselArbiter[T](egressDir, ingresses))
  }
}

class BlobArbiter[T <: Data](egressDir: Direction, ingresses: Seq[Direction])(implicit p: SimpleNocParams[T]) extends Arbiter[T](egressDir, ingresses) {
  val from = IO(Flipped(new DirectionBundle(ingresses)))
  val out = IO(new PacketPort[T](egressDir))

  require(isPow2(ingresses.length), s"BlobArbiter requires number of ingresses to be a power of 2, got ${ingresses.length}")

  val counterReg = RegInit(0.U(log2Ceil(ingresses.length).W))

  def rotate[T](xs: Seq[T], n: Int): Seq[T] = n match {
    case 0 => xs
    case _ => rotate(xs.tail :+ xs.head, n - 1)
  }

  val grantVec = (0 until ingresses.length).map { i =>
    val rotated = rotate(from.all.zipWithIndex, i)
    MuxCase(0.U, rotated.map { case (ingress, j) =>
      (ingress.valid, (1 << j).U)
    })
  }
  val grant = Wire(UInt(ingresses.length.W))
    
    
  grant := VecInit(grantVec)(counterReg)

  from.all.zip(grant.asBools).foreach { case (ingress, g) =>
    ingress.ready := g && out.ready
  }

  out.valid := VecInit(from.all.map(_.valid)).reduceTree(_ || _)
  out.bits := VecInit(from.all.zip(grant.asBools).map { case (ingress, g) =>
    ingress.bits.asUInt & Fill(out.bits.getWidth, g)
  }).reduceTree(_ | _).asTypeOf(out.bits)

  when(out.fire) {
    counterReg := counterReg + 1.U
  }

}

class RotateUnrotateArbiter[T <: Data](egressDir: Direction, ingresses: Seq[Direction])(implicit p: SimpleNocParams[T]) extends Arbiter[T](egressDir, ingresses) {
  val from = IO(Flipped(new DirectionBundle(ingresses)))
  val out = IO(new PacketPort[T](egressDir))

  require(isPow2(ingresses.length), s"RotateUnrotateArbiter requires number of ingresses to be a power of 2, got ${ingresses.length}")

  val counterReg = RegInit(0.U(log2Ceil(ingresses.length).W))

  val rotated = BarrelRotater.left(from.all.map(_.valid),counterReg)

  val grantVec = rotated.foldLeft(Seq.empty[Bool] -> 0.B) { case ((grants, alreadyGranted), valid) =>
    val grant = valid && !alreadyGranted
    (grants :+ grant) -> (alreadyGranted || valid)
  }._1
  val rotatedGrants = dontTouch(WireDefault(VecInit(grantVec)))
  assert(grantVec.length == ingresses.length, s"Expected grantVec length to be equal to number of ingresses (${ingresses.length}), but got ${grantVec.length}")
  val alignedGrantVec = BarrelRotater.right(grantVec, counterReg)

  alignedGrantVec.zip(from.all).foreach { case (g, ingress) =>
    ingress.ready := g && out.ready
  }

  val masked = from.all.zip(alignedGrantVec).map { case (req, g) =>
    req.bits.asUInt & Fill(out.bits.getWidth, g)
  }

  out.valid := VecInit(from.all.map(_.valid)).reduceTree(_ || _)
  out.bits := VecInit(masked).reduceTree(_ | _).asTypeOf(out.bits)

  when(out.fire) {
    counterReg := counterReg + 1.U
  }

}

object RotateUnrotateArbiter extends ArbiterFactory {
  override def apply[T <: Data](egressDir: Direction, ingresses: Seq[Direction])(implicit p: SimpleNocParams[T]): Arbiter[T] = {
    Module(new RotateUnrotateArbiter[T](egressDir, ingresses))
  }
}

class MaskedPriorityArbiter[T <: Data](egressDir: Direction, ingresses: Seq[Direction])(implicit p: SimpleNocParams[T]) extends Arbiter[T](egressDir, ingresses) {
  val from = IO(Flipped(new DirectionBundle(ingresses)))
  val out = IO(new PacketPort[T](egressDir))

  require(isPow2(ingresses.length), s"MaskedPriorityArbiter requires number of ingresses to be a power of 2, got ${ingresses.length}")

  val counterReg = RegInit(0.U(log2Ceil(ingresses.length).W))

  when(out.fire) {
    counterReg := counterReg + 1.U
  }

  val maskVec = VecInit((0 until ingresses.length).map { i =>
    val base = (1 << ingresses.length) - 1
    val zeroMask = (1 << i) - 1
    val mask = base - zeroMask
    mask.U
  })
  val mask = maskVec(counterReg)

  val noMask = !VecInit(from.all.map(_.valid)).reduceTree(_ || _)

  val unmaskedGrant = from.all.map(_.valid).foldLeft(Seq.empty[Bool] -> 0.B) { case ((grants, alreadyGranted), valid) =>
    val grant = valid && !alreadyGranted
    (grants :+ grant) -> (alreadyGranted || valid)
  }._1

  val maskedReq = from.all.zip(mask.asBools).map { case (req, m) =>
    req.valid && m
  }

  val maskedGrant = maskedReq.foldLeft(Seq.empty[Bool] -> 0.B) { case ((grants, alreadyGranted), valid) =>
    val grant = valid && !alreadyGranted
    (grants :+ grant) -> (alreadyGranted || valid)
  }._1

  val grantVec = VecInit(maskedGrant).zip(unmaskedGrant).map { case (mg, ug) =>
    mg || (ug && noMask)
  }

  grantVec.zip(from.all).foreach { case (g, ingress) =>
    ingress.ready := g && out.ready
  }

  val maskedData = from.all.zip(grantVec).map { case (req, g) =>
    req.bits.asUInt & Fill(out.bits.getWidth, g)
  }

  out.valid := VecInit(from.all.map(_.valid)).reduceTree(_ || _)
  out.bits := VecInit(maskedData).reduceTree(_ | _).asTypeOf(out.bits)


}

object MaskedPriorityArbiter extends ArbiterFactory {
  override def apply[T <: Data](egressDir: Direction, ingresses: Seq[Direction])(implicit p: SimpleNocParams[T]): Arbiter[T] = {
    Module(new MaskedPriorityArbiter[T](egressDir, ingresses))
  }
}

class TdmArbiter[T <: Data](egressDir: Direction, ingresses: Seq[Direction])(implicit p: SimpleNocParams[T]) extends Arbiter[T](egressDir, ingresses) {
  val from = IO(Flipped(new DirectionBundle(ingresses)))
  val out = IO(new PacketPort[T](egressDir))

  require(isPow2(ingresses.length), s"TdmArbiter requires number of ingresses to be a power of 2, got ${ingresses.length}")

  val counterReg = RegInit(0.U(log2Ceil(ingresses.length).W))

  when(out.fire) {
    counterReg := counterReg + 1.U
  }

  val grantVec = VecInit((0 until ingresses.length).map(i => counterReg === i.U))

  grantVec.zip(from.all).foreach { case (g, ingress) =>
    ingress.ready := g && out.ready
  }

  val masked = from.all.zip(grantVec).map { case (req, g) =>
    req.bits.asUInt & Fill(out.bits.getWidth, g)
  }

  out.valid := VecInit(from.all.map(_.valid)).reduceTree(_ || _)
  out.bits := VecInit(masked).reduceTree(_ | _).asTypeOf(out.bits)


}

object TdmArbiter extends ArbiterFactory {
  override def apply[T <: Data](egressDir: Direction, ingresses: Seq[Direction])(implicit p: SimpleNocParams[T]): Arbiter[T] = {
    Module(new TdmArbiter[T](egressDir, ingresses))
  }
}

class TdmOnehotArbiter[T <: Data](egressDir: Direction, ingresses: Seq[Direction])(implicit p: SimpleNocParams[T]) extends Arbiter[T](egressDir, ingresses) {
  val from = IO(Flipped(new DirectionBundle(ingresses)))
  val out = IO(new PacketPort[T](egressDir))

  require(isPow2(ingresses.length), s"TdmOnehotArbiter requires number of ingresses to be a power of 2, got ${ingresses.length}")

  val counterReg = RegInit(1.U(ingresses.length.W))

  when(out.fire) {
    counterReg := counterReg(ingresses.length - 1, 0) ## counterReg(ingresses.length - 1)
  }

  val grantVec = counterReg.asBools

  grantVec.zip(from.all).foreach { case (g, ingress) =>
    ingress.ready := g && out.ready
  }

  val masked = from.all.zip(grantVec).map { case (req, g) =>
    req.bits.asUInt & Fill(out.bits.getWidth, g)
  }

  out.valid := VecInit(from.all.map(_.valid)).reduceTree(_ || _)
  out.bits := VecInit(masked).reduceTree(_ | _).asTypeOf(out.bits)
  
}

object TdmOnehotArbiter extends ArbiterFactory {
  override def apply[T <: Data](egressDir: Direction, ingresses: Seq[Direction])(implicit p: SimpleNocParams[T]): Arbiter[T] = {
    Module(new TdmOnehotArbiter[T](egressDir, ingresses))
  }
}

object ArbiterComparison extends App {
  import routing._
  import liftoff.pathToFileOps  
  import net.liftweb.json._

  implicit val p: SimpleNocParams[UInt] = SimpleNocParams.default.copy(
    payloadGen = () => UInt(8.W)
  )

  val dir = "build/arbiter_comparison".toDir

  val res = librelane.compareChisel(
    gens = Seq(
      () => new ChiselArbiter(Local, Seq(North, South, East, West)),
      () => new BlobArbiter(Local, Seq(North, South, East, West)),
      () => new RotateUnrotateArbiter(Local, Seq(North, South, East, West)),
      () => new MaskedPriorityArbiter(Local, Seq(North, South, East, West)),
      () => new TdmArbiter(Local, Seq(North, South, East, West)),
      () => new MartinArbiterTree[SimplePacket[UInt]](4, new SimplePacket[UInt]),
      () => new MartinArbiterSimpleTree[SimplePacket[UInt]](4, new SimplePacket[UInt]),
      () => new TdmOnehotArbiter(Local, Seq(North, South, East, West))
    ),
    dir = dir, 
    params = JObject(
      JField("CLOCK_PERIOD", JDouble(10.0)),
      JField("FP_CORE_UTIL", JInt(15)),
      JField("PNR_SDC_FILE", JString("dummy_constraints.sdc".toFile.getAbsolutePath)),
      JField("SIGNOFF_SDC_FILE", JString("dummy_constraints.sdc".toFile.getAbsolutePath))
    )
  )

  val results = res.map { case (name, res) =>
    val r = res.await()
    val area = r.coreArea
    val delay = r.criticalPath1v80
    Seq(name, area, delay)
  }.toSeq.sortBy(_(1).asInstanceOf[Double])

  val table = liftoff.misc.Reporting.table(Seq(Seq("Arbiter Type", "Area", "Critical Path Delay")) ++ results)
  
  println(table)

  dir.addFile("arbiter_comparison_table.txt", table)

}





//- start fun_arbiter_head
class MartinArbiter[T <: Data: Manifest](n: Int, private val gen: T) extends Module {
  val io = IO(new Bundle {
    val in = Flipped(Vec(n, new DecoupledIO(gen)))
    val out = new DecoupledIO(gen)
  })
  //- end
}

// Only one will be ready, as we cannot take two values
// This would need a shadow register, a reasonable optimisation
// Without optimisation one channel can only take one data every 2 clock cycles
class MartinArbiterTree[T <: Data: Manifest](n: Int, private val gen: T) extends MartinArbiter(n, gen) {

  //- start fun_arbiter
  def arbitrateFair(a: DecoupledIO[T], b: DecoupledIO[T]) = {
    object State extends ChiselEnum {
      val idleA, idleB, hasA, hasB = Value
    }
    import State._
    val regData = Reg(gen)
    val regState = RegInit(idleA)
    val out = Wire(new DecoupledIO(gen))
    a.ready := regState === idleA
    b.ready := regState === idleB
    out.valid := (regState === hasA || regState === hasB)
    switch(regState) {
      is (idleA) {
        when (a.valid) {
          regData := a.bits
          regState := hasA
        } otherwise {
          regState := idleB
        }
      }
      is (idleB) {
        when (b.valid) {
          regData := b.bits
          regState := hasB
        } otherwise {
          regState := idleA
        }
      }
      is (hasA) {
        when (out.ready) {
          regState := idleB
        }
      }
      is (hasB) {
        when (out.ready) {
          regState := idleA
        }
      }
    }
    out.bits := regData
    out
  }
  //- end
  io.out <> io.in.reduceTree((a, b) => arbitrateFair(a, b))
}


class MartinArbiterSimpleTree[T <: Data: Manifest](n: Int, private val gen: T) extends MartinArbiter(n, gen) {

  //- start fun_arbiter_simple
  def arbitrateSimp(a: DecoupledIO[T], b: DecoupledIO[T]) = {

    val regData = Reg(gen)
    val regEmpty = RegInit(true.B)
    val regReadyA = RegInit(false.B)
    val regReadyB = RegInit(false.B)

    val out = Wire(new DecoupledIO(gen))

    when (a.valid & regEmpty & !regReadyB) {
      regReadyA := true.B
    } .elsewhen (b.valid & regEmpty & !regReadyA) {
      regReadyB := true.B
    }
    a.ready := regReadyA
    b.ready := regReadyB

    when (regReadyA) {
      regData := a.bits
      regEmpty := false.B
      regReadyA := false.B
    }
    when (regReadyB) {
      regData := b.bits
      regEmpty := false.B
      regReadyB := false.B
    }

    out.valid := !regEmpty
    when (out.ready) {
      regEmpty := true.B
    }

    out.bits := regData
    out
  }
  //- end

  //- start fun_arbiter_end
  io.out <> io.in.reduceTree((a, b) => arbitrateSimp(a, b))
}