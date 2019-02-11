import scala.annotation.tailrec
import scala.io.StdIn.{readChar, readInt}
import scala.util.{Success, Try}

object TicTacToeApp extends App {

  val userName = System.getProperty("user.name")

  @tailrec
  def readColumn(dimension: Int): Int = {
    val columnRange = 1 to dimension
    print(s"Please insert column number (${columnRange.mkString(", ")}): ")

    Try(readInt()) match {
      case Success(column) if columnRange.contains(column) =>
        column
      case _ =>
        println(s"That's an invalid choice!")
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
        println(s"That's an invalid choice!")
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
        println(s"That's an invalid choice!")
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
        println(s"That's an invalid choice!")
        readUserStart()
    }
  }

  @tailrec
  def readRestartGame(): Boolean = {
    println(s"Do you want to play another round, (y)es or (n)o? ")

    Try(readChar().toLower) match {
      case Success('y') =>
        true
      case Success('n') =>
        false
      case _ =>
        println(s"That's an invalid choice!")
        readRestartGame()
    }
  }


  @tailrec
  def start(): Unit = {
    val user = readUserMark()
    val userStart = readUserStart()

    val computer = user.opponent
    var currentPlayer = if (userStart) user else computer
    var grid = Grid()

    while (GameStatus(grid) == Active) {
      println(grid)

      val (row, column) = currentPlayer match {
        case `user` =>
          println(s"Hey $userName, its your turn!")
          val row = readRow(grid.dimension)
          val column = readColumn(grid.dimension)
          (row, column)
        case `computer` =>
          println(s"The computer\uD83E\uDD16 makes a decision...")
          // read computer input from cmd line for test purposes
          val row = readRow(grid.dimension)
          val column = readColumn(grid.dimension)
          (row, column)
      }

      grid.updated(row, column, currentPlayer) match {
        case Right(updatedGrid) =>
          grid = updatedGrid
          println(grid)
          GameStatus(grid) match {
            case Win(`user`) =>
              println(s"Congratulations $userName, you have won! \uD83C\uDF89")
            case Win(`computer`) =>
              println("The computer\uD83E\uDD16 has won!")
              println(s"Keep trying $userName, next time it will work!")
            case Draw =>
              println("That's a draw, that's good! \uD83D\uDC4D")
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
      println(s"Thank you for playing! See you next time, $userName!")
    }
  }


  start()
}
