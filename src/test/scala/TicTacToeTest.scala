import org.scalatest.{FlatSpec, Matchers}

class TicTacToeTest extends FlatSpec with Matchers {

  "A Game" should "update a board when the chosen cell is empty" in {
    val gameState = GameState(
      currentPlayer = X,
      grid = new Board(Vector(
        Empty, Empty, Empty,
        Empty, Empty, Empty,
        Empty, Empty, Empty
      ))
    )

    val actionResult = TicTacToe.makeMove(row = 1, column = 1, gameState)
    actionResult shouldBe a[GameUpdated]
    val updatedGameState = actionResult.gameState
    updatedGameState.grid(1, 1) shouldBe Some(X)
    updatedGameState.currentPlayer shouldBe O
  }

  it should "not update a board when a cell is already taken" in {
    val gameState = GameState(
      currentPlayer = X,
      grid = new Board(Vector(
        Empty, Empty, Empty,
        Empty, O, Empty,
        Empty, Empty, Empty
      ))
    )

    val actionResult = TicTacToe.makeMove(row = 2, column = 2, gameState)
    actionResult shouldBe a[CellAlreadyTaken]
    val updatedGameState = actionResult.gameState
    updatedGameState.grid(2, 2) shouldBe Some(O)
    updatedGameState.currentPlayer shouldBe X
  }

  it should "not update a board given an invalid cell index" in {
    val gameState = GameState(
      currentPlayer = X,
      grid = new Board(Vector(
        Empty, Empty, Empty,
        Empty, Empty, Empty,
        Empty, Empty, Empty
      ))
    )

    val actionResult = TicTacToe.makeMove(row = 5, column = 42, gameState)
    actionResult shouldBe a[InvalidRowColumn]
    actionResult.gameState.currentPlayer shouldBe X
  }

  it should "not update a board when a game is already won" in {
    val gameState = GameState(
      currentPlayer = X,
      grid = new Board(Vector(
        X, X, X,
        Empty, O, O,
        O, O, Empty
      ))
    )

    val actionResult = TicTacToe.makeMove(row = 3, column = 3, gameState)
    actionResult shouldBe a[GameDraw]
  }

  it should "not update the board if a game already ended in a draw" in {
    val gameState = GameState(
      currentPlayer = X,
      grid = new Board(Vector(
        X, O, X,
        O, O, X,
        O, X, O
      ))
    )

    val actionResult = TicTacToe.makeMove(row = 3, column = 3, gameState)
    actionResult shouldBe a[GameDraw]
  }

  it should "determine if a game is won" in {
    val gameState = GameState(
      currentPlayer = X,
      grid = new Board(Vector(
        X, X, Empty,
        O, O, X,
        O, O, X
      ))
    )

    val actionResult = TicTacToe.makeMove(row = 1, column = 3, gameState)
    actionResult shouldBe a[GameWon]
  }

  it should "play until game is finished by a won or a draw" in {
    val gameState = GameState(Board(), X)
    val moves = collection.mutable.ListBuffer[(Int, Int)](
      (1, 1), // X
      (1, 1), // O (already taken)
      (5, 5), // O (invalid row/column)
      (3, 3), // O
      (1, 2), // X
      (3, 2), // O
      (1, 3)  // X win
    )

    val nextMoveProvider: GameState => (Int, Int) = _ => {
      val move = moves.head
      moves.remove(0)
      move
    }
    val results = TicTacToe.play(gameState, nextMoveProvider, nextMoveProvider, _ => ())
    results.length shouldBe 8
    results(0) shouldBe a[GameInitialized]
    results(1) shouldBe a[GameUpdated]
    results(2) shouldBe a[CellAlreadyTaken]
    results(3) shouldBe a[InvalidRowColumn]
    results(4) shouldBe a[GameUpdated]
    results(5) shouldBe a[GameUpdated]
    results(6) shouldBe a[GameUpdated]
    results(7) shouldBe a[GameWon]
  }

  it should "inform an observer about the game actions" in {
    val gameState = GameState(Board(), X)
    val moves = collection.mutable.ListBuffer[(Int, Int)](
      (1, 1), // X
      (3, 3), // O
      (1, 2), // X
      (3, 2), // O
      (1, 3)  // X win
    )
    val totalMoves = moves.size

    val nextMoveProvider: GameState => (Int, Int) = _ => {
      val move = moves.head
      moves.remove(0)
      move
    }

    var eventCounter = 0
    TicTacToe.play(gameState, nextMoveProvider, nextMoveProvider, _ => eventCounter += 1)

    eventCounter shouldBe totalMoves + 1 // + 1 because of initial game event
  }

}
