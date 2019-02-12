import org.scalatest.{FlatSpec, Matchers}

class NegaMaxAITest extends FlatSpec with Matchers {

  "A NegaMaxAI" should "draw if it plays against itself" in {
    var player: Mark = O
    var grid = Grid()
    while (GameStatus(grid) == Active) {
      val (row, column) = NegaMaxAI.selectPosition(grid, player)
      val Right(updatedGrid) = grid.updated(row, column, player)
      grid = updatedGrid
      player = player.opponent
    }
    GameStatus(grid) shouldBe Draw
  }

  it should "select a winning position if win is possible" in {
    val grid = new Grid(Vector(
      O, Empty, O,
      X, Empty, X,
      Empty, Empty, Empty
    ))
    NegaMaxAI.selectPosition(grid, X) shouldBe(2, 2)
    NegaMaxAI.selectPosition(grid, O) shouldBe(1, 2)
  }

  it should "keep the opponent from winning if it cannot win itself" in {
    val grid = new Grid(Vector(
      X, Empty, Empty,
      O, O, Empty,
      X, Empty, O
    ))
    NegaMaxAI.selectPosition(grid, X) shouldBe(2, 3)
  }

}