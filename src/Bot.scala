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
    val (movementStrategy, response) = opcode match {
      case "React" =>
        val view = View(params("view"))
        consultMovementStrategies(view = view) 
      case _ =>
        (None, "")
    }
    "Status(text=%s)|%s".format(movementStrategy.getClass.getSimpleName, response)
  }

  private def consultMovementStrategies(strategyIndex: Int = 0, view: View): Tuple2[MovementStrategy, String] = {
    val movementStrategy = movementStrategies(strategyIndex)
    val response = movementStrategy.nextMove(view)
    if (response.isDefined) (movementStrategy, response.get) else consultMovementStrategies(strategyIndex + 1, view)
  }
}
