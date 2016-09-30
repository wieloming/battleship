import scala.annotation.tailrec

object Game {

  @tailrec
  def gameLoop(currentPlayer: Entity, otherPlayer: Entity, entityGuess: Entity => Point): Unit = {
    val guess = entityGuess(currentPlayer)

    otherPlayer.board.hit(guess) match {
      case Board.SHOT_HIT => println(currentPlayer.name + " hits!")
      case Board.SHOT_MISS => println(currentPlayer.name + " misses.")
      case Board.ALREADY_GUESSED => println("You already tried it.")
    }
    currentPlayer match {
      case ai: AI => otherPlayer.board.print()
      case _ => ()
    }

    if (otherPlayer.board.shipsLeft <= 0) println(currentPlayer.name + " wins!")
    else gameLoop(otherPlayer, currentPlayer, entityGuess)
  }
}
