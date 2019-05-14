import scala.annotation.tailrec

object TicTacToe {

  type Row = Int
  type Column = Int
  type MoveProvider = GameState => (Row, Column)
  type Observer = GameEvent => Unit

  def play(gameState: GameState, humanMoveProvider: MoveProvider, computerMoveProvider: MoveProvider, observer: Observer): List[GameEvent] = {

   @tailrec
    def go(gameState: GameState, results: List[GameEvent]): List[GameEvent] =
      BoardStatus(gameState.grid) match {
        case Active =>
          val (row, column) =
            if (gameState.currentPlayer == gameState.humanPlayer)
              humanMoveProvider(gameState)
            else
              computerMoveProvider(gameState)
          val result = makeMove(row, column, gameState)
          observer(result)
          go(result.gameState, results :+ result)
        case _ =>
          results
      }

    val initGameEvent = GameInitialized(gameState)
    observer(initGameEvent)
    go(gameState, initGameEvent :: Nil)
  }


  def makeMove(row: Int, column: Int, gameState: GameState): GameEvent = {
    if (BoardStatus(gameState.grid) == Active) {
      doSetMark(row, column, gameState)
    } else {
      GameDraw(gameState)
    }
  }

  private def doSetMark(row: Int, column: Int, gameState: GameState) = {
    gameState.grid(row, column) match {
      case Some(Empty) =>
        val updatedGrid = gameState.grid.updated(row, column, gameState.currentPlayer)
        val nextPlayer = gameState.currentPlayer.opponent
        val updatedGameState = gameState.copy(currentPlayer = nextPlayer, grid = updatedGrid)
        BoardStatus(updatedGrid) match {
          case Win(_) => GameWon(updatedGameState)
          case Draw => GameDraw(updatedGameState)
          case Active => GameUpdated(updatedGameState)
        }
      case Some(_) =>
        CellAlreadyTaken(gameState)
      case None =>
        InvalidRowColumn(gameState)
    }
  }
}