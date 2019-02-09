
class Grid(grid: Vector[Token]) {

  val dimension: Int = {
    val dim = Math.sqrt(grid.length)
    assert(dim.toInt % dim == 0, "grid must have size NxN")
    dim.toInt
  }

  def updated(row: Int, column: Int, token: Token): Either[String, Grid] = {
    val index = buildIndex(row, column)
    grid.lift(index) match {
      case Some(Empty) =>
        val updatedGrid = grid.updated(index, token)
        new Right(new Grid(updatedGrid))
      case Some(_) =>
        Left(s"Position $row/$column is already taken!")
      case None =>
        Left(s"Invalid row/column $row/$column!")
    }
  }

  def apply(row: Int, column: Int): Token = {
    val index = buildIndex(row, column)
    grid(index)
  }

  private def buildIndex(row: Int, column: Int) = {
    val rowIndex = row - 1
    val columnIndex = column - 1
    val index = rowIndex * dimension + columnIndex
    index
  }

  def rows: Seq[Seq[Token]] = for {
    row <- 1 to dimension
  } yield for {
    column <- 1 to dimension
    index = buildIndex(row, column)
  } yield grid(index)

  def columns: Seq[Seq[Token]] = for {
    column <- 1 to dimension
  } yield for {
    row <- 1 to dimension
    index = buildIndex(row, column)
  } yield grid(index)

  def diagonals: Seq[Seq[Token]] = {

    val topLeftToBottomRight = for {
      row <- 1 to dimension
      column = row
      index = buildIndex(row, column)
    } yield grid(index)

    val topRightToBottomLeft = for {
      column <- dimension to 1 by -1
      row = (dimension - column) + 1
      index = buildIndex(row, column)
    } yield grid(index)

    Seq(topLeftToBottomRight, topRightToBottomLeft)
  }

  def isFilled: Boolean = !grid.contains(Empty)

  override def toString: String = {
    rows.map(row => row.map(cell => s" $cell "))
      .map(row => row.mkString("|"))
      .mkString(s"\n-----------\n")
  }

}

object Grid {

  def apply(): Grid = {
    val dimension = 3
    val grid = Vector.fill[Token](dimension * dimension)(Empty)
    new Grid(grid)
  }

}
