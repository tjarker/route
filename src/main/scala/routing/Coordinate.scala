package routing

case class Coord(x: Int, y: Int) {
  def +(that: Coord): Coord = Coord(this.x + that.x, this.y + that.y)
  def -(that: Coord): Coord = Coord(this.x - that.x, this.y - that.y)
}
