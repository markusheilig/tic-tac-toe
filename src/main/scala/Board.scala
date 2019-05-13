
class Board(grid: Vector[Mark]) {

  val dimension: Int = {
    val dim = Math.sqrt(grid.length)
    assert(dim.toInt % dim == 0, "grid must have size NxN")
    dim.toInt
  }

  val status = BoardStatus(this)

  def updated(row: Int, column: Int, mark: Mark): Board = {
    val index = buildIndex(row, column)
    val updatedGrid = grid.updated(index, mark)
    new Board(updatedGrid)
  }

  def apply(row: Int, column: Int): Option[Mark] = {
    val index = buildIndex(row, column)
    grid.lift(index)
  }

  private def buildIndex(row: Int, column: Int) = {
    val rowIndex = row - 1
    val columnIndex = column - 1
    val index = rowIndex * dimension + columnIndex
    index
  }

  def buildPosition(index: Int): (Int, Int) = {
    val row = (index / dimension) + 1
    val column = (index % dimension) + 1
    (row, column)
  }

  def rows: Seq[Seq[Mark]] = for {
    row <- 1 to dimension
  } yield for {
    column <- 1 to dimension
    index = buildIndex(row, column)
  } yield grid(index)

  def columns: Seq[Seq[Mark]] = for {
    column <- 1 to dimension
  } yield for {
    row <- 1 to dimension
    index = buildIndex(row, column)
  } yield grid(index)

  def diagonals: Seq[Seq[Mark]] = {

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

  def availablePositions: IndexedSeq[(Int, Int)] = for {
    index <- grid.indices if grid(index) == Empty
    position = buildPosition(index)
  } yield position

  override def toString: String = {
    val dashes = "-" * (dimension * 3 + dimension - 1)
    rows.map(row => row.map(cell => s" $cell "))
      .map(row => row.mkString("|"))
      .mkString(s"\n$dashes\n")
  }

}

object Board {

  def apply(dimension: Int = 3): Board = {
    val grid = Vector.fill[Mark](dimension * dimension)(Empty)
    new Board(grid)
  }

}
    