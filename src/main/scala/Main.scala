import scala.annotation.tailrec
import scala.util.{Success, Try}

object Main {

  def readShipPosition(entity: Entity): Position = {
    entity match {
      case player: Player => playerCoordinates(player)
      case ai: AI => ai.guessPosition
    }
  }

  def takeGuess(entity: Entity): Point = {
    entity match {
      case player: Player => playerGuess(player)
      case ai: AI => ai.guessPoint
    }
  }

  def main(args: Array[String]): Unit = {
    val player1 = Player("player 1")
    val player2 = AI("player 2")

    println("Board: ")
    player1.board.print()
    println("Let's Get Ready To Rumble!!!")

    val ship1Position = readShipPosition(player1)
    val ship2Position = readShipPosition(player2)

    player1.board.placeShip(ship1Position)
    player1.board.print()
    player2.board.placeShip(ship2Position)

    Game.gameLoop(player1, player2, takeGuess(_))
  }

  @tailrec
  private def playerCoordinates(player: Player): Position = {
    def pointsInOneLine(x1: Int, y1: Int, x2: Int, y2: Int) = x1 == x2 || y1 == y2
    println(player.name + ": Enter ship position (format: x1 y1 x2 y2)")
    readLine().split(" ").map(l => Try(l.toInt)) match {
      case Array(Success(x1), Success(y1), Success(x2), Success(y2))
        if Board.contains(Point(x1, y1), Point(x2, y2)) && pointsInOneLine(x1, y1, x2, y2) =>
        Position(Point(x1, y1), Point(x2, y2))
      case Array(Success(x1), Success(y1), Success(x2), Success(y2))
        if Board.contains(Point(x1, y1), Point(x2, y2)) =>
        println("Points must create strait line")
        playerCoordinates(player)
      case Array(Success(x1), Success(y1), Success(x2), Success(y2)) =>
        println("Points position out of range")
        playerCoordinates(player)
      case _ =>
        println("Undefined input, try again")
        playerCoordinates(player)
    }
  }

  @tailrec
  def playerGuess(player: Player): Point = {
    println(player.name + ": Take a guess (format: x y).")
    readLine().split(" ").map(l => Try(l.toInt)) match {
      case Array(Success(x), Success(y)) if Board.contains(Point(x, y)) => Point(x, y)
      case Array(Success(x), Success(y)) =>
        println("Point position out of range")
        playerGuess(player)
      case _ =>
        println("Wrong input, try again")
        playerGuess(player)
    }
  }
}
