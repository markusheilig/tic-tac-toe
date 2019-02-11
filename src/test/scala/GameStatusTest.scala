import org.scalatest.{FlatSpec, Matchers}

class GameStatusTest extends FlatSpec with Matchers {

  "A GameStatus" should "win by row" in {
    val grid = new Grid(Vector(
      X, X, X,
      O, X, O,
      O, Empty, Empty
    ))
    GameStatus(grid) shouldBe Win(X)
  }

  it should "win by column" in {
    val grid = new Grid(Vector(
      X, O, Empty,
      X, O, X,
      O, O, Empty
    ))
    GameStatus(grid) shouldBe Win(O)
  }

  it should "win by first diagonal" in {
    val grid = new Grid(Vector(
      X, O, Empty,
      X, X, O,
      O, O, X
    ))
    GameStatus(grid) shouldBe Win(X)
  }

  it should "win by second diagonal" in {
    val grid = new Grid(Vector(
      X, X, O,
      X, O, O,
      O, X, Empty
    ))
    GameStatus(grid) shouldBe Win(O)
  }

  it should "draw if there is no win and the grid is full" in {
    val grid = new Grid(Vector(
      X, X, O,
      O, O, X,
      X, O, X
    ))
    GameStatus(grid) shouldBe Draw
  }

  it should "be active if there is no win and there are empty places" in {
    val grid = new Grid(Vector(
      X, X, Empty,
      X, O, O,
      O, X, X
    ))
    GameStatus(grid) shouldBe Active
  }
}
