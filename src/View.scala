import util.Random

case class View(cells: String) {
  val size = math.sqrt(cells.length).toInt
  val center = XY(size/2, size/2)

  def indexFromAbsPos(absPos: XY) = absPos.x + absPos.y * size
  def absPosFromIndex(index: Int) = XY(index % size, index / size) 
  def absPosFromRelPos(relPos: XY) = relPos + center
  def cellAtAbsPos(absPos: XY) = cells.charAt(indexFromAbsPos(absPos))
  def indexFromRelPos(relPos: XY) = indexFromAbsPos(absPosFromRelPos(relPos))
  def relPosFromAbsPos(absPos: XY) = absPos - center
  def relPosFromIndex(index: Int) = relPosFromAbsPos(absPosFromIndex(index))
  def cellAtRelPos(relPos: XY) = cells.charAt(indexFromRelPos(relPos))

  def safeCellsAround(): List[XY] = {
    XY.AllDirections.filter(direction => cellAtRelPos(direction) == '_')   
  }

  def randomSafeDirection(): XY = {
    val safeCells = safeCellsAround
    safeCells(new Random().nextInt(safeCells.length))
  }

  def offsetToNearest(c: Char) = {
    val relativePositions = cells.view.zipWithIndex
      .filter(_._1 == c)
      .map(p => relPosFromIndex(p._2))
    if (relativePositions.isEmpty) None
    else Some(relativePositions.minBy(_.length))
  }

  override def toString = {
    val arr = new Array[String](size)
    for (x <- 0 until size) {
      arr(x) = cells.slice(x * size, x * size + size)
    }

    arr.mkString("\n")
  }
}
