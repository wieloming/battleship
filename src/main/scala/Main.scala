import scala.annotation.tailrec
import scala.util.{Success, Try}

object Main {

  @tailrec
  def readCoordinates(player: Player): Position = {
    def pointsInOneLine(x1: Int, y1: Int, x2: Int, y2: Int) = x1 == x2 || y1 == y2
    println(player.name + ": Enter x1 y1 x2 y2")
    readLine().split(" ").map(l => Try(l.toInt)) match {
      case Array(Success(x1), Success(y1), Success(x2), Success(y2))
        if Board.contains(Point(x1, y1), Point(x2, y2)) && pointsInOneLine(x1, y1, x2, y2) =>
        Position(Point(x1, y1), Point(x2, y2))
      case Array(Success(x1), Success(y1), Success(x2), Success(y2))
        if Board.contains(Point(x1, y1), Point(x2, y2)) =>
        println("Points must create strait line")
        readCoordinates(player)
      case Array(Success(x1), Success(y1), Success(x2), Success(y2)) =>
        println("Points position out of range")
        readCoordinates(player)
    }
  }

  @tailrec
  def takeGuess(player: Player): Point = {
    println(player.name + ": Take a guess.")
    readLine().split(" ").map(l => Try(l.toInt)) match {
      case Array(Success(x), Success(y)) if Board.contains(Point(x, y)) => Point(x, y)
      case Array(Success(x), Success(y)) =>
        println("Point position out of range")
        takeGuess(player)
      case _ =>
        println("Wrong input, try again")
        takeGuess(player)
    }
  }


  def main(args: Array[String]): Unit = {
    val player1 = Player("player 1")
    val player2 = Player("player 2")

    println("Board: ")
    player1.board.print()
    println("Let's Get Ready To Rumble!!!")

    val ship1Position = readCoordinates(player1)
    val ship2Position = readCoordinates(player2)

    player1.board.placeShip(ship1Position)
    player2.board.placeShip(ship2Position)

//    //TODO: remove
//    println("Player 1: ")
//    player1.board.print()
//    println("Player 2: ")
//    player2.board.print()
    Game.gameLoop(player1, player2, takeGuess(_))
  }

}
