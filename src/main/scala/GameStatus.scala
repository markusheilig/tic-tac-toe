trait GameStatus

case class Win(token: Token) extends GameStatus

case object Unfinished extends GameStatus

case object Draw extends GameStatus

object GameStatus {

  def apply(grid: Grid): GameStatus = {
    checkWin(grid, X).orElse(checkWin(grid, O)) match {
      case Some(token) =>
        Win(token)
      case None if grid.isFilled =>
        Draw
      case None =>
        Unfinished
    }
  }

  private def checkWin(grid: Grid, token: Token): Option[Token] = {
    def isLineFilled(line: Seq[Token]) = line.forall(_ == token)

    if (grid.rows.exists(isLineFilled) ||
      grid.columns.exists(isLineFilled) ||
      grid.diagonals.exists(isLineFilled)) {
      Some(token)
    } else {
      None
    }
  }

}
