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

  private case class Attraction(targ: Char, prior: Priority = Priority.High) {
    val target = targ
    val priority = prior
  }

  private abstract class AttractedTo extends MovementStrategy {
    val attractions: List[Attraction]
    private var curAttractionState = (Attraction('M'), 0) // A placeholder value; will be immediately overwritten

    override def nextMove(view: View): Option[String] = {
      val attractionsAndOffsets = searchAttractions(view)
      attractionsAndOffsets.nonEmpty match {
        case true => // An attraction is within sight
          avoidWallAndResolveNext(attractionsAndOffsets, view)
        case false => None
      }
    }

    private def searchAttractions(view: View): List[Tuple2[Attraction, XY]] = {
      attractions.map((attraction) => 
        (attraction, view.offsetToNearest(attraction.target).getOrElse(XY.Zero))
      ).filter(_._2 != XY.Zero)
    }

    private def avoidWallAndResolveNext(attractionsAndOffsets: List[Tuple2[Attraction, XY]], view: View): Option[String] = {
      val offsetSignum = resolveNextTarget(attractionsAndOffsets, view).signum
      view.cellAtRelPos(offsetSignum) match {
        case 'W' => someMovement(view.randomSafeDirection) // Don't rush against wall
        case _ => someMovement(offsetSignum)
      }
    }

    /*
     * Resolve which attraction to pursue.
     */
    private def resolveNextTarget(attractionsAndOffsets: List[Tuple2[Attraction, XY]], view: View): XY = {
      val weightedOffsets = weight(attractionsAndOffsets, view)
      val heaviestOffset = weightedOffsets(0)

      val aboutToAttractToNewTarget = curAttractionState._1 != Attraction(view.cellAtRelPos(heaviestOffset))
      val minStepsToFollow = 3
      val botIsHeedless = curAttractionState._2 <= minStepsToFollow 
      val moreThanOnePossibleAttractions = weightedOffsets.length > 1 
      val possibleAttractionsContainsOldAttraction = 
        weightedOffsets.find(x => view.cellAtRelPos(x) == curAttractionState._1.target).isDefined

      val targetOffset = if (
        aboutToAttractToNewTarget && 
        botIsHeedless && 
        moreThanOnePossibleAttractions && 
        possibleAttractionsContainsOldAttraction  
      ) {
        // Reduce bot heedlessness by going after same attraction for at least {minStepsToFollow} steps
        view.offsetToNearest(curAttractionState._1.target).get
      } else heaviestOffset // Otherwise go after the heaviest target

      val newAttr = Attraction(view.cellAtRelPos(targetOffset))
      curAttractionState = (newAttr, if (newAttr != curAttractionState._1) 1 else curAttractionState._2 + 1) // Update current attraction state
      targetOffset
    }

    /*
     * Weight potential movement targets. Criteria: distance and priority.
     */
    private def weight(attractionsAndOffsets: List[Tuple2[Attraction, XY]], view: View): List[XY] = {
      attractionsAndOffsets.map((x) => {
          val distanceToAttraction = x._2.distanceTo(view.center)
          val weight = x._1.priority.numval / distanceToAttraction
          (weight, x._2)
        }
      ).sortBy(x => x._1).reverse.map(x => x._2) // Heaviest XY first
    }
  }

  private trait FluppetHunterZugarGatherer extends AttractedTo {
    override val attractions = List(Attraction('B', Priority.High), Attraction('P', Priority.Low))
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
