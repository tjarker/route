package routing.simple

import routing.Coord
import routing._

import chisel3._

trait RoutingRule {
  def generateLogic(dest: Destination, here: Coord): Map[Direction, Bool]
}
abstract class RoutingPolicy {
  def getRule(ingressDir: Direction): RoutingRule
}

object XYRouting extends RoutingPolicy {
  def getRule(ingressDir: Direction): RoutingRule = ingressDir match {
    case East | West   => HorizontalIngressRule(ingressDir)
    case North | South => VerticalIngressRule(ingressDir)
    case Local         => LocalIngressRule
  }

  /** Direction-order routing: first along x then y
    *
    *   1. full match -> local
    *   2. no x match -> continue in ingress direction
    *   3. no y match -> turn left or right based on southbound
    *
    * @param ingressDir
    */
  case class HorizontalIngressRule(ingressDir: Direction) extends RoutingRule {
    def generateLogic(dest: Destination, here: Coord): Map[Direction, Bool] = {
      val xMatch = dest.x === here.x.U
      val yMatch = dest.y === here.y.U
      val turnLeft = ingressDir match {
        case East => dest.southbound
        case West => !dest.southbound
        case _    =>
          throw new Exception(
            "Invalid ingress direction for HorizontalIngressRule"
          )
      }
      Map(
        Local -> (xMatch && yMatch),
        ingressDir.opposite -> (!xMatch && yMatch),
        ingressDir.rotateNegative -> (!yMatch && turnLeft),
        ingressDir.rotatePositive -> (!yMatch && !turnLeft)
      )
    }
  }

  /** Direction-order routing: first along x then y
    *
    *   1. y match -> local
    *   2. no y match -> continue in direction opposite to ingress
    *
    * @param ingressDir
    */
  case class VerticalIngressRule(ingressDir: Direction) extends RoutingRule {
    def generateLogic(dest: Destination, here: Coord): Map[Direction, Bool] = {
      val yMatch = dest.y === here.y.U
      Map(
        Local -> yMatch,
        ingressDir.opposite -> !yMatch
      )
    }
  }

  /** Direction-order routing: first along x then y
    *
    *   1. no x match -> go west or east based on eastbound
    *   2. x match -> go north or south based on southbound
    */
  case object LocalIngressRule extends RoutingRule {
    def generateLogic(dest: Destination, here: Coord): Map[Direction, Bool] = {
      val xMatch = dest.x === here.x.U
      Map(
        East -> (!xMatch && dest.eastbound),
        West -> (!xMatch && !dest.eastbound),
        South -> (xMatch && dest.southbound),
        North -> (xMatch && !dest.southbound)
      )
    }
  }
}
