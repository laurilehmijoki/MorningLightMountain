abstract class MovementStrategy {
  def nextMove(input: View): Option[String]

  private def someMovement(xy: XY) = 
    Some("Move(dx=%d,dy=%d)".format(xy.x, xy.y))
}

object MovementStrategy {
  def meanderer: MovementStrategy = new MovementStrategyMeanderer
  def gatherer: MovementStrategy = new MovementStrategyGatherer
  def hunter: MovementStrategy = new MovementStrategyHunter

  private abstract class AttractedTo extends MovementStrategy {
    def attraction: Char

    override def nextMove(view: View): Option[String] = {
        view.offsetToNearest(attraction) match {
        case Some(offset) =>
          val offsetSignum = offset.signum
          if (view.cellAtRelPos(offsetSignum) == 'W') {
            someMovement(view.randomSafeDirection) // Don't rush against wall
          }
          else someMovement(offsetSignum)
        case None =>
          None 
      }
    }
  }

  private trait FluppetHunter extends AttractedTo {
    override def attraction = 'B'
  }

  private trait ZugarGatherer extends AttractedTo {
    override def attraction = 'P'
  }

  private class MovementStrategyGatherer extends AttractedTo with ZugarGatherer

  private class MovementStrategyHunter extends AttractedTo with FluppetHunter 

  private class MovementStrategyMeanderer extends MovementStrategy {
    var direction: XY = XY.Right

    def nextMove(view: View): Option[String] = {
      val newOffset = nextDirection(view)
      someMovement(newOffset)
    }

    private def nextDirection(view: View) = {
      if (view.cellAtRelPos(direction) == 'W')
        direction = view.randomSafeDirection // Start meandering to a new direction

      direction
    }
  }
}
