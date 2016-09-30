import scala.util.Random

sealed trait Entity {
  val name: String
  val board: Board
}

case class Player(name: String) extends Entity {
  override val board: Board = Board.empty
}

case class AI(name: String) extends Entity {
  override val board: Board = Board.empty
  def randomBetween(start: Int, end: Int) = start + Random.nextInt(end - start)

  def guessPoint: Point = {
    Point(randomBetween(0, Board.BOARD_X), randomBetween(0, Board.BOARD_Y))
  }

  def guessPosition: Position = {
    val x1 = randomBetween(0, Board.BOARD_X)
    val y1 = randomBetween(0, Board.BOARD_Y)
    val p1 = Point(x1, y1)

    if (Random.nextBoolean()) Position(p1, Point(x1, randomBetween(0, Board.BOARD_Y)))
    else Position(p1, Point(randomBetween(0, Board.BOARD_X), y1))
  }

}
