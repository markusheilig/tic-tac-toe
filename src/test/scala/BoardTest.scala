import org.scalatest.{FlatSpec, Matchers}

class BoardTest extends FlatSpec with Matchers {

  "A Grid" should "have a dimension" in {
    val grid = Board()
    grid.dimension shouldBe 3
  }

  it should "set a mark if the position is empty" in {
    val grid = Board()
    grid(1, 1) shouldBe Some(Empty)
    val updatedGrid = grid.updated(1, 1, X)
    updatedGrid(1, 1) shouldBe Some(X)
  }

  it should "have rows" in {
    val grid = new Board(Vector(
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
    val grid = new Board(Vector(
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
    val grid = new Board(Vector(
      O, Empty, X,
      X, X, Empty,
      Empty, O, Empty
    ))
    val diagonals = grid.diagonals
    diagonals(0) shouldBe Seq(O, X, Empty)
    diagonals(1) shouldBe Seq(X, X, Empty)
  }

  it should "be filled if there is no mark 'Empty'" in {
    Board().isFilled shouldBe false

    new Board(Vector(
      O, X, O,
      X, O, X,
      O, X, Empty
    )).isFilled shouldBe false

    new Board(Vector(
      O, X, O,
      X, O, X,
      O, X, O
    )).isFilled shouldBe true
  }

  it should "have a 'toString' representation" in {
    val grid = new Board(Vector(
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

  it should "have a 'toString' representation for arbitrary dimension (here 4x4)" in {
    val grid = new Board(Vector(
      X, O, X, Empty,
      Empty, O, O, X,
      X, Empty, X, Empty,
      O, X, O, X
    ))
    grid.toString shouldBe
        " x | o | x |   \n" +
        "---------------\n" +
        "   | o | o | x \n" +
        "---------------\n" +
        " x |   | x |   \n" +
        "---------------\n" +
        " o | x | o | x "
  }

  it should "return available positions" in {
    Board().availablePositions shouldBe IndexedSeq(
      (1, 1), (1, 2), (1, 3), (2, 1), (2, 2),
      (2, 3), (3, 1), (3, 2), (3, 3)
    )

    new Board(Vector(
      X, O, X,
      O, X, O,
      O, X, O
    )).availablePositions shouldBe IndexedSeq()

    new Board(Vector(
      X, O, Empty,
      Empty, O, O,
      X, Empty, Empty
    )).availablePositions shouldBe IndexedSeq(
      (1, 3), (2, 1), (3, 2), (3, 3)
    )
  }

  it should "throw an assertion error if the grid is not of dimension NxN" in {
    assertThrows[AssertionError](
      new Board(Vector(
        X, O, Empty,
        X, Empty, Empty
      ))
    )
  }

}
