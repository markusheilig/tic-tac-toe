import scala.util.Random

trait AI {

  type Position = (Int, Int)

  def selectPosition(grid: Board, mark: Mark): Position

}


object RandomAI extends AI {

  override def selectPosition(grid: Board, mark: Mark): Position = {
    Random.shuffle(grid.availablePositions).head
  }

}


object NegaMaxAI extends AI {

  private type Ranking = Int

  override def selectPosition(grid: Board, ai: Mark): Position = {
    val opponent = ai.opponent

    def negamax(g: Board, mark: Mark, level: Int, depth: Int): (Ranking, Position) = {
      BoardStatus(g) match {
        case Win(`ai`) =>
          (1, null)
        case Win(`opponent`) =>
          (-1, null)
        case Draw =>
          (0, null)
        case Active =>
          var bestRanking: Ranking = -level
          var bestPos: Position = null
          // pruning: check if we already found the best ranking
          for (position@(row, column) <- g.availablePositions
               if bestRanking != level && depth >= 0) {
            val updatedGrid = g.updated(row, column, mark)
            val (ranking, _) = negamax(updatedGrid, mark.opponent, -level, depth - 1)
            // negamax: multiply ranking by level and check for max value
            // -> in this case we do not have to distinguish between min and max level
            if (ranking * level >= bestRanking || depth == 0) {
              bestRanking = ranking
              bestPos = position
            }
          }
          (bestRanking, bestPos)
      }
    }

    // check best position for ai by maximizing game result
    val max = 1
    val (_, bestPosition) = negamax(grid, mark = ai, level = max, depth = 10)
    bestPosition
  }

}
