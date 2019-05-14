import TicTacToe.{Column, Row}

import scala.annotation.tailrec
import scala.io.StdIn.{readChar, readInt}

object Main {

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
  private def start(): Unit = {
    val gameState = initializeGameState()

    TicTacToe.play(gameState, getHumanMove, getComputerMove, EventPrinter.printEvent)

    if (readRestartGame()) {
      start()
    } else {
      println(s"Thank you for playing $userName, goodbye!")
    }
  }

  private def getHumanMove(gameState: GameState): (Row, Column) = {
    print("Please enter row: ")
    val row = InputReader.readChoice(readInt, (1 to gameState.grid.dimension).toSet, onInputError = printInvalidChoice)
    print("Please enter column: ")
    val column = InputReader.readChoice(readInt, (1 to gameState.grid.dimension).toSet, onInputError = printInvalidChoice)
    (row, column)
  }

  private def getComputerMove(gameState: GameState): (Row, Column) = {
    println("The computer makes a selection...")
    gameState.ai.selectPosition(gameState.grid, gameState.aiPlayer)
  }

  private def readDimension(): Int = {
    print(s"Please insert dimension (e.g. 3): ")
    InputReader.read[Int](readInt, _ > 2, printInvalidChoice)
  }

  private def readUserMark(): Mark = {
    print(s"Hey $userName, please choose your mark, 'x' or 'o': ")
    InputReader.readChoice(readChar, Set('x', 'X', 'o', 'O'), printInvalidChoice) match {
      case 'X' =>
        X
      case 'x' =>
        X
      case _ =>
        O
    }
  }

  private def readUserStart(): Boolean = {
    print(s"Do you want to make the opening move, (y)es or (n)o? ")
    InputReader.readBoolean(readChar, printInvalidChoice)
  }

  private def printInvalidChoice(err: Any): Unit = {
    println("That's an invalid choice!")
  }

  private def readAI(): AI = {
    print(s"Please choose difficulty, (e)asy or (h)ard? ")
    InputReader.readChoice(readChar, Set('e', 'E', 'h', 'H'), printInvalidChoice) match {
      case 'H' =>
        NegaMaxAI
      case 'h' =>
        NegaMaxAI
      case _ =>
        RandomAI
    }
  }

  private def readRestartGame() = {
    println("Do you want to play another round? (y)es or (n)o?")
    InputReader.readBoolean(readChar, printInvalidChoice)
  }

  private def initializeGameState() = {
    val userMark = readUserMark()
    val startMark =
      if (readUserStart())
        userMark
      else
        userMark.opponent
    val computerAI = readAI()
    val dimension = readDimension()
    val board = Board(dimension)

    GameState(board, startMark, userMark, computerAI)
  }

}
