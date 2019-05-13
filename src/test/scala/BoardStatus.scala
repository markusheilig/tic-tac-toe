import org.scalatest.{FlatSpec, Matchers}

class BoardStatusTest extends FlatSpec with Matchers {

  "A GameStatus" should "win by row" in {
    val grid = new Board(Vector(
      X, X, X,
      O, X, O,
      O, Empty, Empty
    ))
    BoardStatus(grid) shouldBe Win(X)
  }

  it should "win by column" in {
    val grid = new Board(Vector(
      X, O, Empty,
      X, O, X,
      O, O, Empty
    ))
    BoardStatus(grid) shouldBe Win(O)
  }

  it should "win by first diagonal" in {
    val grid = new Board(Vector(
      X, O, Empty,
      X, X, O,
      O, O, X
    ))
    BoardStatus(grid) shouldBe Win(X)
  }

  it should "win by second diagonal" in {
    val grid = new Board(Vector(
      X, X, O,
      X, O, O,
      O, X, Empty
    ))
    BoardStatus(grid) shouldBe Win(O)
  }

  it should "draw if there is no win and the grid is full" in {
    val grid = new Board(Vector(
      X, X, O,
      O, O, X,
      X, O, X
    ))
    BoardStatus(grid) shouldBe Draw
  }

  it should "be active if there is no win and there are empty places" in {
    val grid = new Board(Vector(
      X, X, Empty,
      X, O, O,
      O, X, X
    ))
    BoardStatus(grid) shouldBe Active
  }
}
