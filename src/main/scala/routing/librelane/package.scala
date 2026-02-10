package routing

import liftoff.Time
import liftoff.WorkingDirectory
import liftoff.pathToFileOps
import java.io.File
import net.liftweb.json._
import chisel3.emitVerilog

package object librelane {

  trait ConfigValue
  case class Conf(value: Any) extends ConfigValue
  case class ConfList(values: Seq[Any]) extends ConfigValue

  def exploreChisel[M <: chisel3.Module](gen: => M, dir: WorkingDirectory, params: JObject): Seq[AwaitableResult] = {
    emitVerilog(gen, Array("--target-dir", dir.dir.getAbsolutePath()))
    val m = liftoff.chisel.ChiselBridge.elaborate(gen)
    val verilogFile = dir / s"${m.name}.v"
    val updatedParams = params merge JObject(
      JField("VERILOG_FILES", JArray(List(JString(verilogFile.getAbsolutePath())))),
      JField("DESIGN_NAME", JString(m.name)),
      JField("CLOCK_PORT", JString("clock")),
    )
    explore(dir, updatedParams)
  }

  def hardenChisel[M <: chisel3.Module](gen: => M, dir: WorkingDirectory, params: JObject): AwaitableResult = {
    val results = exploreChisel(gen, dir, params)
    if (results.size != 1) {
      throw new Exception(s"HardenChisel expected a single configuration, but got ${results.size}")
    }
    results.head
  }

  def compareChisel(gens: Seq[() => chisel3.Module], dir: WorkingDirectory, params: JObject): Map[String, AwaitableResult] = {
    val results = gens.map { gen =>
      val m = liftoff.chisel.ChiselBridge.elaborate(gen())
      val designDir = dir.addSubDir(dir / m.name)
      val verilogFile = designDir / s"${m.name}.v"
      emitVerilog(gen(), Array("--target-dir", designDir.dir.getAbsolutePath()))
      val updatedParams = params merge JObject(
        JField("VERILOG_FILES", JArray(List(JString(verilogFile.getAbsolutePath())))),
        JField("DESIGN_NAME", JString(m.name)),
        JField("CLOCK_PORT", JString("clock")),
      )
      val res = explore(designDir, updatedParams)
      if (res.size != 1) {
        throw new Exception(s"CompareChisel expected a single configuration for ${m.name}, but got ${res.size}")
      }
      (m.name, res.head)
    }.toMap
    results
  }

  def explore(dir: WorkingDirectory, params: JObject): Seq[AwaitableResult] = {



    val paramList = params.obj.map {
      case JField(key, JArray(List())) => Seq(JField(key, JArray(List())))
      case JField(key, JArray(versions)) => versions.map(v => JField(key, v))
      case JField(key, v)                => Seq(JField(key, v))
    }

    // create cross product of all parameters
    def cartesianProduct[T](lists: Seq[Seq[T]]): Seq[Seq[T]] = lists match {
      case Nil => Seq(Seq())
      case head :: tail =>
        for {
          h <- head
          t <- cartesianProduct(tail)
        } yield h +: t
    }

    val combinations = cartesianProduct(paramList.toSeq).map(seq => JObject(seq.toList))

    val results = combinations.zipWithIndex.map { case (params, index) =>
      val runDir = dir.addSubDir(dir / f"run_$index%03d")
      run(runDir, params)
    }

    results
  }

  trait RunResult {
    def corners: Seq[TimingCorner]
    def criticalPath1v80: Double = {
      val targetClockPeriod = metrics \ "CLOCK_PERIOD" match {
        case JDouble(num) => num
        case JInt(num)    => num.toDouble
        case JsonAST.JNothing => throw new Exception("CLOCK_PERIOD not found in metrics")
        case _            => throw new Exception("Invalid CLOCK_PERIOD in metrics")
      }
      val worstSetupSlack = corners.filter(_.name.contains("1v80")).map(_.setupWorstSlack).min
      targetClockPeriod - worstSetupSlack
    }
    def coreArea = metrics \ "design__core__area" match {
      case JDouble(num) => num
      case JInt(num)    => num.toDouble
      case JsonAST.JNothing => throw new Exception("design__core__area not found in metrics")
      case _            => throw new Exception("Invalid design__core__area in metrics")
    }
    def metrics: JValue
    def warnings(): Seq[FailureReason] = this match {
      case s: SuccessfulRun       => Seq()
      case f: FailedRun     => f.reasons
    }
    def areaDelayTable(): String = {
      liftoff.Reporting.table(
        Seq(Seq("Area (um^2)", "Critical Path (ns)"), Seq(coreArea, criticalPath1v80))
      )
    }
  }
  case class SuccessfulRun(corners: Seq[TimingCorner], metrics: JValue) extends RunResult
  case class FailedRun(corners: Seq[TimingCorner], metrics: JValue, reasons: Seq[FailureReason]) extends RunResult
  trait FailureReason
  case class SetupViolation(corners: Seq[TimingCorner]) extends FailureReason
  case class HoldViolation(corners: Seq[TimingCorner]) extends FailureReason
  case object CantFitIOPads extends FailureReason

  class AwaitableResult(process: Process, dirFile: File, params: JObject) {
    def await(): RunResult = {
      process.waitFor()

      val dir = new WorkingDirectory(dirFile)


      val resultJson = dir / "final/metrics.json"
      val warningFile = dir / "warning.log"
      val errorFile = dir / "error.log"

      // find sta reports
      // find dir with pattern "runs/runner/\d+-openroad-stapostpnr"
      val staDirPattern = """runs/runner/(\d+)-openroad-stapostpnr""".r
      val runDirs = dir.dir.listFiles().filter(_.isDirectory).sortBy(_.getName)
      val staDirs = runDirs.filter { d =>
        staDirPattern.findFirstIn(d.getAbsolutePath()).isDefined
      }
      // get the one with the highest number
      val staDir = staDirs.maxBy { d =>
        val numStr = staDirPattern.findFirstMatchIn(d.getAbsolutePath()).get.group(1)
        numStr.toInt
      }

      val summaryFile = new WorkingDirectory(staDir) / "summary.rpt"

      // print sta summary
      val corners = if (summaryFile.exists()) {
        extractTimingSummary(summaryFile)
      } else {
        Seq()
      }

      val holdTimeViolations = corners.filter(_.holdVioCount > 0)
      val setupTimeViolations = corners.filter(_.setupVioCount > 0)
      
      val targetClockPeriod = params \ "CLOCK_PERIOD" match {
        case JDouble(num) => num
        case JInt(num)    => num.toDouble
        case JsonAST.JNothing => throw new Exception("CLOCK_PERIOD not found in metrics")
        case _            => throw new Exception("Invalid CLOCK_PERIOD in metrics")
      }
      val worstSetupSlack = corners.filter(_.name.contains("1v80")).map(_.setupWorstSlack).min
      val criticalPath1v80 = targetClockPeriod - worstSetupSlack


      if (resultJson.exists()) {
        
        val jsonStr = scala.io.Source.fromFile(resultJson).mkString
        val sanitizedStr = jsonStr.replaceAll("""Infinity""", "\"Infinity\"")
        implicit val formats: DefaultFormats.type = DefaultFormats
        val json = parse(sanitizedStr)
        val merged = json merge params merge JObject(
          JField("critical_path_1v80", JDouble(criticalPath1v80)),
          JField("core_area", json \ "design__core__area")
        )
        SuccessfulRun(corners, merged)
      } else {
        val warnings = if (warningFile.exists()) {
          scala.io.Source.fromFile(warningFile).getLines().toSeq
        } else Seq()
        val errors = if (errorFile.exists()) {
          scala.io.Source.fromFile(errorFile).getLines().toSeq
        } else Seq()
        val reasons = (warnings ++ errors).flatMap { line =>
          if (line.contains("Setup violation")) {
            Some(SetupViolation(corners))
          } else if (line.contains("Hold violation")) {
            Some(HoldViolation(corners))
          } else if (line.contains("Number of IO pins")) {
            Some(CantFitIOPads)
          } else {
            None
          }
        }
        FailedRun(corners, params, reasons)
      }
    }
  }

  case class TimingCorner(
    name: String,
    holdWorstSlack: Double,
    holdWorstRegToReg: Double,
    holdTNS: Double,
    holdVioCount: Int,
    holdVioCountRegToReg: Int,
    setupWorstSlack: Double,
    setupWorstRegToReg: Double,
    setupTNS: Double,
    setupVioCount: Int,
    setupVioCountRegToReg: Int,
    maxCapVios: Int,
    maxSlewVios: Int
  ) {
    def toStringDetailed(): String = {
      s"""Corner: $name
         |  Hold Worst Slack: $holdWorstSlack
         |  Hold Worst Reg to Reg: $holdWorstRegToReg
         |  Hold TNS: $holdTNS
         |  Hold Violation Count: $holdVioCount
         |  Hold Violation Count Reg to Reg: $holdVioCountRegToReg
         |  Setup Worst Slack: $setupWorstSlack
         |  Setup Worst Reg to Reg: $setupWorstRegToReg
         |  Setup TNS: $setupTNS
         |  Setup Violation Count: $setupVioCount
         |  Setup Violation Count Reg to Reg: $setupVioCountRegToReg
         |  Max Capacitance Violations: $maxCapVios
         |  Max Slew Violations: $maxSlewVios
       """.stripMargin
    }
  }

  def extractTimingSummary(summaryFile: File): Seq[TimingCorner] = {
    val lines = scala.io.Source.fromFile(summaryFile).getLines().drop(6).toSeq.dropRight(1)

    val corners = lines.map { line =>
      val cols = line.split("│").map(_.trim.replace("…", "")).filter(_.nonEmpty)
      TimingCorner(
        name = cols(0),
        holdWorstSlack = cols(1).toDouble,
        holdWorstRegToReg = cols(2).toDoubleOption.getOrElse(0.0),
        holdTNS = cols(3).toDouble,
        holdVioCount = cols(4).toInt,
        holdVioCountRegToReg = cols(5).toInt,
        setupWorstSlack = cols(6).toDouble,
        setupWorstRegToReg = cols(7).toDoubleOption.getOrElse(0.0),
        setupTNS = cols(8).toDouble,
        setupVioCount = cols(9).toInt,
        setupVioCountRegToReg = cols(10).toInt,
        maxCapVios = cols(11).toInt,
        maxSlewVios = cols(12).toInt
      )
    }
    corners
  }


  def run(dir: WorkingDirectory, params: JObject): AwaitableResult = {

    val updatedConf = params merge JObject(JField("TIMING_VIOLATION_CORNERS", JArray(List())))

    val conf = writeConfig(dir, updatedConf)

    // create process to run libre
    val processBuilder = new ProcessBuilder("librelane", "--run-tag", "runner", conf.getAbsolutePath())

    processBuilder.directory(dir.dir)

    val process = processBuilder.inheritIO().start()


    new AwaitableResult(process, dir / "runs/runner", updatedConf)
  }

  def writeConfig(dir: WorkingDirectory, params: JObject): File = {
    val confFile = dir.addFile("config.json", prettyRender(params))
    confFile
  }

  
}


import chisel3._
class ChiselAlu extends Module {
  val w = 8
  val io = IO(new Bundle {
    val a = Input(UInt(w.W))
    val b = Input(UInt(w.W))
    val sum = Output(UInt(w.W))
  })
  val acc = RegInit(0.U(128.W))
  acc := acc + io.a + io.b
  io.sum := acc(127, 128 - w)
}

object LibreChisel extends App {

  val dir = "build/libre-explore-chisel".toDir

  val params = JObject(
    JField("CLOCK_PERIOD", JArray(List.range(1, 10).map(d => JDouble(d.toDouble)))),
    JField("FP_CORE_UTIL", JInt(60)),
  )

  val res = librelane.exploreChisel(new ChiselAlu, dir, params).map(_.await())


  res.foreach { r =>
    val JDouble(cp) = r.metrics \ "CLOCK_PERIOD"
    val JInt(fp) = r.metrics \ "FP_CORE_UTIL"
    println(s"CP Target: ${cp}")
    println(s"FP Util: ${fp}")
    println(s"Core Area: ${r.coreArea}")
    println(s"Critical Path 1v80: ${r.criticalPath1v80}")
    r.warnings().foreach { w =>
      println(s"Warning: $w")
    }
    dir.addFile(s"result-${cp.toInt}.json", prettyRender(r.metrics))
  }




}

object LibreTest extends App {

  import librelane._

  val name = "adder"

  val verilog = s"""
module $name#(W = 32) (input logic [W-1:0] a,
              input logic [W-1:0] b,
              input logic clock,
              output logic [W-1:0] sum);
  reg [W-1:0] temp;
  always_ff @(posedge clock) begin
    temp <= a + b;
  end
  assign sum = temp;
endmodule
  """

  val dir = "build/libre-explore".toDir

  dir.createIfNotExists()
  dir.clean()

  val vFile = dir.addFile(s"$name.v", verilog)


  val params = JObject(
    JField("CLOCK_PERIOD", JDouble(1.0)),
    JField("FP_CORE_UTIL", JDouble(30.0)),
    JField("DESIGN_NAME", JString(name)),
    JField("VERILOG_FILES", JArray(List(JString(vFile.getAbsolutePath())))),
    JField("CLOCK_PORT", JString("clock"))
  )

  librelane.explore(dir, params)

}
