import org.scalatest.{FlatSpec, Matchers}

class NegaMaxAITest extends FlatSpec with Matchers {

  "A NegaMaxAI" should "draw if it plays against itself" in {
    var player: Mark = O
    var grid = Board()
    while (BoardStatus(grid) == Active) {
      val (row, column) = NegaMaxAI.selectPosition(grid, player)
      val updatedGrid = grid.updated(row, column, player)
      grid = updatedGrid
      player = player.opponent
    }
    BoardStatus(grid) shouldBe Draw
  }

  it should "select a winning position if win is possible" in {
    val grid = new Board(Vector(
      O, Empty, O,
      X, Empty, X,
      Empty, Empty, Empty
    ))
    NegaMaxAI.selectPosition(grid, X) shouldBe(2, 2)
    NegaMaxAI.selectPosition(grid, O) shouldBe(1, 2)
  }

  it should "keep the opponent from winning if it cannot win itself" in {
    val grid = new Board(Vector(
      X, Empty, Empty,
      O, O, Empty,
      X, Empty, O
    ))
    NegaMaxAI.selectPosition(grid, X) shouldBe(2, 3)
  }

  it should "return 'null' on a filled grid" in {
    val grid = new Board(Vector(
      X, O, X,
      O, X, O,
      O, X, O
    ))
    NegaMaxAI.selectPosition(grid, X) shouldBe null
    NegaMaxAI.selectPosition(grid, O) shouldBe null
  }

}
