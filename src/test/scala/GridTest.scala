import org.scalatest.{FlatSpec, Matchers}

class GridTest extends FlatSpec with Matchers {

  "A Grid" should "have a dimension" in {
    val grid = Grid()
    grid.dimension shouldBe 3
  }

  it should "set a token if the position is empty" in {
    val grid = Grid()
    grid(1, 1) shouldBe Empty
    val Right(updatedGrid) = grid.updated(1, 1, X)
    updatedGrid(1, 1) shouldBe X
  }

  it should "not set a token if the position is outside the grid" in {
    val grid = Grid()
    grid.updated(42, 21, X) shouldBe Left("Invalid row/column 42/21!")
  }

  it should "not set a token if the position is already taken" in {
    val Right(grid) = Grid().updated(1, 2, X)
    grid.updated(1, 2, X) shouldBe Left("Position 1/2 is already taken!")
    grid.updated(1, 2, O) shouldBe Left("Position 1/2 is already taken!")
  }


  it should "have rows" in {
    val grid = new Grid(Vector(
      O, Empty, X,
      X, X, Empty,
      Empty, O, Empty
    ))
    val rows = grid.rows
    rows(0) shouldBe Seq(O, Empty, X)
    rows(1) shouldBe Seq(X, X, Empty)
    rows(2) shouldBe Seq(Empty, O, Empty)
  }

  it should "have columns" in {
    val grid = new Grid(Vector(
      O, Empty, X,
      X, X, Empty,
      Empty, O, Empty
    ))
    val columns = grid.columns
    columns(0) shouldBe Seq(O, X, Empty)
    columns(1) shouldBe Seq(Empty, X, O)
    columns(2) shouldBe Seq(X, Empty, Empty)
  }

  it should "have diagonals" in {
    val grid = new Grid(Vector(
      O, Empty, X,
      X, X, Empty,
      Empty, O, Empty
    ))
    val diagonals = grid.diagonals
    diagonals(0) shouldBe Seq(O, X, Empty)
    diagonals(1) shouldBe Seq(X, X, Empty)
  }

  it should "be filled if there is no token 'Empty'" in {
    Grid().isFilled shouldBe false

    new Grid(Vector(
      O, X, O,
      X, O, X,
      O, X, Empty
    )).isFilled shouldBe false

    new Grid(Vector(
      O, X, O,
      X, O, X,
      O, X, O
    )).isFilled shouldBe true
  }

  it should "have a 'toString' representation" in {
    val grid = new Grid(Vector(
      X, O, X,
      Empty, O, O,
      X, Empty, X
    ))
    grid.toString shouldBe
      " x | o | x \n" +
        "-----------\n" +
        "   | o | o \n" +
        "-----------\n" +
        " x |   | x "
  }

}
