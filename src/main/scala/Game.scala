import scala.annotation.tailrec

object Game {

  @tailrec
  def gameLoop(currentPlayer: Player, otherPlayer: Player, userGuess: Player => Point): Unit = {
    val guess = userGuess(currentPlayer)

    otherPlayer.board.hit(guess) match {
      case Board.SHOT_HIT => println("Hit!")
      case Board.SHOT_MISS => println("Miss.")
      case Board.ALREADY_GUESSED => println("You already tried it.")
    }
//    TODO: remove
//    println(otherPlayer.name + ": ")
    otherPlayer.board.print()
    if (otherPlayer.board.shipsLeft <= 0) println("You win!")
    else gameLoop(otherPlayer, currentPlayer, userGuess)
  }
}
