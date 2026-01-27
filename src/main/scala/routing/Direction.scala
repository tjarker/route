package routing

sealed trait Direction {
  def toInt: Int = this match {
    case North => 0
    case East  => 1
    case South => 2
    case West  => 3
    case Local => 4
  }
  override def toString: String = this match {
    case North => "N"
    case East  => "E"
    case South => "S"
    case West  => "W"
    case Local => "L"
  }
  def opposite: Direction = this match {
    case North => South
    case East  => West
    case South => North
    case West  => East
    case Local => Local
  }
  def rotateNegative: Direction = this match {
    case North => East
    case East  => South
    case South => West
    case West  => North
    case Local => Local
  }
  def rotatePositive: Direction = this match {
    case North => West
    case East  => North
    case South => East
    case West  => South
    case Local => Local
  }
}

case object North extends Direction
case object East extends Direction
case object South extends Direction
case object West extends Direction
case object Local extends Direction

object Direction {
  val all: Seq[Direction] = Seq(North, East, South, West, Local)
  val count: Int = all.length
  def fromInt(i: Int): Direction = i match {
    case 0 => North
    case 1 => East
    case 2 => South
    case 3 => West
    case 4 => Local
    case _ => throw new Exception(s"Invalid integer for Direction: $i")
  }
}