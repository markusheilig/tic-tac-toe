case class GameState private(
                              grid: Board = Board(),
                              currentPlayer: Mark = X,
                              humanPlayer: Mark = X,
                              ai: AI = NegaMaxAI
                            ) {
  val otherPlayer: Mark = currentPlayer.opponent

  val aiPlayer: Mark = humanPlayer.opponent
}
