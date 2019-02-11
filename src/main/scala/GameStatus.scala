trait GameStatus

case class Win(mark: Mark) extends GameStatus

case object Active extends GameStatus

case object Draw extends GameStatus

object GameStatus {

  def apply(grid: Grid): GameStatus = {
    checkWin(grid, X).orElse(checkWin(grid, O)) match {
      case Some(mark) =>
        Win(mark)
      case None if grid.isFilled =>
        Draw
      case None =>
        Active
    }
  }

  private def checkWin(grid: Grid, mark: Mark): Option[Mark] = {
    def isLineFilled(line: Seq[Mark]) = line.forall(_ == mark)

    if (grid.rows.exists(isLineFilled) ||
      grid.columns.exists(isLineFilled) ||
      grid.diagonals.exists(isLineFilled)) {
      Some(mark)
    } else {
      None
    }
  }

}
