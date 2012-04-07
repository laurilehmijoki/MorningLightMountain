class ControlFunctionFactory {
  def create = new Bot().respond _
}

class Bot {
  val movementStrategies = List(
    MovementStrategy.hunter,
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
    val response = movementStrategies(strategyIndex).nextMove(view)
    if (response.isDefined) response.get else consultMovementStrategies(strategyIndex + 1, view)
  }
}
