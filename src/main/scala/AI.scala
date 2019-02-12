import scala.util.Random

trait AI {

  type Position = (Int, Int)

  def selectPosition(grid: Grid, mark: Mark): Position

}


object RandomAI extends AI {

  override def selectPosition(grid: Grid, mark: Mark): Position = {
    Random.shuffle(grid.availablePositions).head
  }

}
