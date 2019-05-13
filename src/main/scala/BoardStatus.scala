trait BoardStatus

case class Win(mark: Mark) extends BoardStatus

case object Active extends BoardStatus

case object Draw extends BoardStatus

object BoardStatus {

  def apply(board: Board): BoardStatus = {
    checkWin(board, X).orElse(checkWin(board, O)) match {
      case Some(mark) =>
        Win(mark)
      case None if board.isFilled =>
        Draw
      case None =>
        Active
    }
  }

  private def checkWin(grid: Board, mark: Mark): Option[Mark] = {
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
