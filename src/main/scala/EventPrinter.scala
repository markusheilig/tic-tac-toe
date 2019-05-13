object EventPrinter {


  def printEvent(action: GameEvent): Unit = {

    println()

    action match {
      case _: GameInitialized =>
        println("Let's start...")

      case _: GameUpdated =>
        println("Game continues...")

      case win: GameWon if win.humanWon =>
        println("Congratulations, you've won!")

      case win: GameWon if win.computerWon =>
        println("Sorry, computer has won :-/")

      case _: GameDraw =>
        println("Draw, nice!")

      case _: CellAlreadyTaken =>
        println("The cell is already taken")

      case _: InvalidRowColumn =>
        println("This is an invalid choice!")
    }

    println(action.gameState.grid)
  }

}
