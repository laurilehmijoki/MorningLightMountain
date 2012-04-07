import util.Random

class ControlFunctionFactory {
  def create = new Bot().respond _
}

class Bot {
  val movementStrategies = List(
    MovementStrategy.gatherer,
    MovementStrategy.meanderer
  )

  def respond(input: String) = {
    val (opcode, params) = CommandParser(input)

    opcode match {
      case "React" =>
        val view = View(params("view"))
        consultMovementStrategies(view = view) 
      case _ =>
        ""
    }
  }

  private def consultMovementStrategies(strategyIndex: Int = 0, view: View): String = {
    val response = movementStrategies(strategyIndex).respond(view)
    if (response.isDefined) response.get else consultMovementStrategies(strategyIndex + 1, view)
  }
}

abstract class MovementStrategy {
  def respond(input: View): Option[String]
}

object MovementStrategy {
  def meanderer: MovementStrategy = new MovementStrategyMeanderer
  def gatherer: MovementStrategy = new MovementStrategyGatherer

  private def someMovement(xy: XY) = 
    Some("Move(dx=%d,dy=%d)".format(xy.x, xy.y))

  private class MovementStrategyGatherer extends MovementStrategy {
    def respond(input: View): Option[String] = {
        input.offsetToNearest('P') match {
        case Some(offset) =>
          val offsetSignum = offset.signum
          someMovement(offsetSignum)
        case None =>
          None 
      }
    }
  }

  private class MovementStrategyMeanderer extends MovementStrategy {
    var direction: XY = XY.Right

    def respond(input: View): Option[String] = {
      val newOffset = nextDirection(input)
      someMovement(newOffset)
    }

    private def nextDirection(input: View) = {
      if (input.cellAtRelPos(direction) == 'W')
        direction = randomSafeDirection(input)

      direction
    }

    private def randomSafeDirection(view: View): XY = {
      val safeCells = view.emptyCellsAround
      safeCells(new Random().nextInt(safeCells.length))
    }
  }
}
