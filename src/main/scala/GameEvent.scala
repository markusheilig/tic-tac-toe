
sealed trait GameEvent {
  def gameState: GameState
}

case class GameInitialized(gameState: GameState) extends GameEvent

case class GameUpdated(gameState: GameState) extends GameEvent

case class GameWon(gameState: GameState) extends GameEvent {
  val computerWon: Boolean = gameState.currentPlayer == gameState.humanPlayer
  val humanWon: Boolean = !computerWon
}

case class GameDraw(gameState: GameState) extends GameEvent

case class CellAlreadyTaken(gameState: GameState) extends GameEvent

case class InvalidRowColumn(gameState: GameState) extends GameEvent


