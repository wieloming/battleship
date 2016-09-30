import Board.BoardSymbol

case class Board(private val array: Array[Array[BoardSymbol]]) {

  def placeShip(p: Position): Board = {
    if (p.a.x == p.b.x) {
      val x = p.a.x
      val y1 = p.a.y
      val y2 = p.b.y
      for (y <- y1 to y2 by (if (y1 > y2) -1 else 1)) {
        array(x)(y) = Board.BOARD_SHIP
      }
    } else {
      val y = p.a.y
      val x1 = p.a.x
      val x2 = p.b.x
      for (x <- x1 to x2 by (if (x1 > x2) -1 else 1)) {
        array(x)(y) = Board.BOARD_SHIP
      }
    }
    Board(array)
  }

  def hit(p: Point): BoardSymbol = array(p.x)(p.y) match {
    case Board.BOARD_EMPTY =>
      array(p.x)(p.y) = Board.SHOT_MISS
      Board.SHOT_MISS
    case Board.BOARD_SHIP =>
      array(p.x)(p.y) = Board.SHOT_HIT
      Board.SHOT_HIT
    case Board.SHOT_HIT | Board.SHOT_MISS =>
      Board.ALREADY_GUESSED
  }

  def shipsLeft = array.flatten.count(_ == Board.BOARD_SHIP)

  def print() = array.foreach(l => println(l.mkString("")))
}

object Board {
  val BOARD_X = 10
  val BOARD_Y = 10

  sealed case class BoardSymbol(s: String) {
    override def toString = s
  }
  val BOARD_EMPTY = BoardSymbol(".")
  val BOARD_SHIP = BoardSymbol("X")
  val SHOT_MISS = BoardSymbol("~")
  val SHOT_HIT = BoardSymbol("H")
  val ALREADY_GUESSED = BoardSymbol("G")

  def contains(ps: Point*) =
    ps.forall(p => ((0 until BOARD_X) contains p.x) && ((0 until BOARD_Y) contains p.y))
  def empty =
    Board(Array.fill[BoardSymbol](BOARD_X, BOARD_Y)(BOARD_EMPTY))
}
