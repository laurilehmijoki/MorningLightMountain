abstract class MovementStrategy {
  def nextMove(input: View): Option[String]

  private def someMovement(xy: XY) = 
    Some("Move(dx=%d,dy=%d)".format(xy.x, xy.y))
}

object MovementStrategy {
  def meanderer: MovementStrategy = new MovementStrategyMeanderer
  def hunterGatherer: MovementStrategy = new MovementStrategyHunterGatherer

  private class Priority(nval: Double) {
    val numval = nval
  }

  private object Priority {
    val High = new Priority(1.3)
    val Low = new Priority(1)
  }

  private class Attraction(targ: Char, prior: Priority = Priority.High) {
    val target = targ
    val priority = prior
  }

  private abstract class AttractedTo extends MovementStrategy {
    val attractions: List[Attraction]

    override def nextMove(view: View): Option[String] = {
      val attractionsAndOffsets = searchAttractions(view)
      attractionsAndOffsets.nonEmpty match {
        case true => // An attraction is within sight
          toMovement(attractionsAndOffsets, view)
        case false => None
      }
    }

    private def toMovement(attractionsAndOffsets: List[Tuple2[Attraction, XY]], view: View): Option[String] = {
      val heviestOffset = weight(attractionsAndOffsets, view).maxBy(_._1)._2

      val offsetSignum = heviestOffset.signum
      view.cellAtRelPos(offsetSignum) match {
        case 'W' => someMovement(view.randomSafeDirection) // Don't rush against wall
        case _ => someMovement(offsetSignum)
      }
    }

    private def weight(attractionsAndOffsets: List[Tuple2[Attraction, XY]], view: View): List[Tuple2[Double, XY]] = {
      attractionsAndOffsets.map((x) => {
          val distanceToAttraction = x._2.distanceTo(view.center)
          val weight = x._1.priority.numval / distanceToAttraction
          (weight, x._2)
        }
      )
    }

    private def searchAttractions(view: View): List[Tuple2[Attraction, XY]] = {
      attractions.map((attraction) => 
        (attraction, view.offsetToNearest(attraction.target).getOrElse(XY.Zero))
      ).filter(_._2 != XY.Zero)
    }
  }

  private trait FluppetHunterZugarGatherer extends AttractedTo {
    override val attractions = List(new Attraction('B', Priority.High), new Attraction('P', Priority.Low))
  }

  private class MovementStrategyHunterGatherer extends AttractedTo with FluppetHunterZugarGatherer 

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
