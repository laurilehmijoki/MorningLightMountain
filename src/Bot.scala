import util.Random

class ControlFunctionFactory {
  def create = new Bot().respond _
}

class Bot {
  def respond(input: String) = {
    val (opcode, params) = CommandParser(input)

    opcode match {
      case "React" =>
        val view = View(params("view"))
        MovementStrategyGatherer.respondOpt(view) match {
          case Some(response) => response
          case None => MovementStrategyMeander.respond(view)
        }
      case _ =>
        ""
    }
  }
}

object MovementStrategyGatherer {
  def respondOpt(input: View): Option[String] = {
      input.offsetToNearest('P') match {
      case Some(offset) =>
        val offsetSignum = offset.signum
        Some("Move(dx=%d,dy=%d)".format(offsetSignum.x, offsetSignum.y))
      case None =>
        None 
    }
  }
}

object MovementStrategyMeander {
  
  var direction: XY = XY.Right

  def respond(input: View): String = {
    val newOffset = nextDirection(input)
    "Move(dx=%d,dy=%d)".format(newOffset.x, newOffset.y)
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
