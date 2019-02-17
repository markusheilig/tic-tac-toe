import scala.annotation.tailrec
import scala.io.StdIn.{readChar, readInt}
import scala.util.{Failure, Success, Try}

object TicTacToeApp {

  val userName: String = System.getProperty("user.name")
  val robotEmoji: String = "\uD83E\uDD16"
  val partyPopperEmoji: String = "\uD83C\uDF89"
  val thumsUpEmoji: String = "\uD83D\uDC4D"
  val goodbyeEmoji: String = "\uD83D\uDC4B"


  def main(args: Array[String]): Unit = {
    println("Welcome to Tic-Tac-Toe!")
    start()
  }

  @tailrec
  def readDimension(): Int = {
    val minDimension = 3
    val defaultDimension = 3
    print(s"Please insert dimension (default $defaultDimension): ")

    Try(readInt()) match {
      case Success(dimension) if dimension >= minDimension =>
        dimension
      case Failure(t: NumberFormatException) if t.getMessage == "For input string: \"\"" =>
        // in this case the user just hit the 'return' key  --> use the default dimension
        defaultDimension
      case _ =>
        printInvalidChoice()
        readDimension()
    }
  }

  @tailrec
  def readColumn(dimension: Int): Int = {
    val columnRange = 1 to dimension
    print(s"Please insert column number (${columnRange.mkString(", ")}): ")

    Try(readInt()) match {
      case Success(column) if columnRange.contains(column) =>
        column
      case _ =>
        printInvalidChoice()
        readColumn(dimension)
    }
  }

  @tailrec
  def readRow(dimension: Int): Int = {
    val rowRange = 1 to dimension
    print(s"Please insert row number (${rowRange.mkString(", ")}): ")

    Try(readInt()) match {
      case Success(column) if rowRange.contains(column) =>
        column
      case _ =>
        printInvalidChoice()
        readRow(dimension)
    }
  }

  @tailrec
  def readUserMark(): Mark = {
    print(s"Hey $userName, please choose your mark, 'x' or 'o': ")

    Try(readChar().toLower) match {
      case Success('x') =>
        X
      case Success('o') =>
        O
      case _ =>
        printInvalidChoice()
        readUserMark()
    }
  }

  @tailrec
  def readUserStart(): Boolean = {
    print(s"Do you want to make the opening move, (y)es or (n)o? ")

    Try(readChar().toLower) match {
      case Success('y') =>
        true
      case Success('n') =>
        false
      case _ =>
        printInvalidChoice()
        readUserStart()
    }
  }

  @tailrec
  def readRestartGame(): Boolean = {
    print(s"Do you want to play another round, (y)es or (n)o? ")

    Try(readChar().toLower) match {
      case Success('y') =>
        true
      case Success('n') =>
        false
      case _ =>
        printInvalidChoice()
        readRestartGame()
    }
  }

  def printInvalidChoice(): Unit = {
    println("That's an invalid choice!")
  }

  @tailrec
  def readAI(): AI = {
    print(s"Please choose difficulty, (e)asy or (h)ard? ")

    Try(readChar().toLower) match {
      case Success('e') =>
        RandomAI
      case Success('h') =>
        NegaMaxAI
      case _ =>
        printInvalidChoice()
        readAI()
    }
  }


  @tailrec
  def start(): Unit = {
    val user = readUserMark()
    val userStart = readUserStart()
    val ai = readAI()
    val dim = readDimension()
    val computer = user.opponent

    var currentPlayer = if (userStart) user else computer
    var grid = Grid(dim)

    while (GameStatus(grid) == Active) {
      println(grid + System.lineSeparator)

      val (row, column) = (currentPlayer: @unchecked) match {
        case `user` =>
          println(s"Hey $userName, its your turn!")
          val row = readRow(grid.dimension)
          val column = readColumn(grid.dimension)
          println(s"You chose the move row = $row, column = $column")
          (row, column)
        case `computer` =>
          println(s"The computer$robotEmoji makes a decision...")
          val (row, column) = ai.selectPosition(grid, computer)
          println(s"The computer$robotEmoji chose the move row = $row, column = $column")
          (row, column)
      }

      grid.updated(row, column, currentPlayer) match {
        case Right(updatedGrid) =>
          grid = updatedGrid
          GameStatus(grid) match {
            case Win(`user`) =>
              println(grid)
              println(s"Congratulations $userName, you have won!$partyPopperEmoji")
            case Win(`computer`) =>
              println(grid)
              println(s"The computer$robotEmoji has won!")
              println(s"Keep trying $userName, next time it will work!")
            case Draw =>
              println(grid)
              println(s"That's a draw, that's good!$thumsUpEmoji")
            case Active =>
              currentPlayer = currentPlayer.opponent
          }
        case Left(error) =>
          println(error)
      }
    }

    if (readRestartGame()) {
      start()
    } else {
      println(s"Thank you for playing! See you next time, $userName $goodbyeEmoji")
    }
  }

}
