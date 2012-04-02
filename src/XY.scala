import util.Random

object XY {
  private val Rnd = new Random()

  def random() = new XY(Rnd.nextInt(3)-1, Rnd.nextInt(3)-1)
  def fromEntityName(name: String) = {
    val xy = name.split('_').map(_.toInt)
    XY(xy.head, xy.last)
  }
  val Zero = XY(0,0) 
  val One = XY(1,1)
  val Right = XY(1,0)
  val RightUp = XY(1,1)
  val Up = XY(0,1)
  val UpLeft = XY(-1,1)
  val Left = XY(-1,0)
  val LeftDown = XY(-1,-1)
  val Down = XY( 0,-1)
  val DownRight = XY( 1,-1)

  val AllDirections = List(Right, Up, Left, Down) // All the directions to which the bot can move
}

case class XY(val x: Int, val y: Int) {
  def isNonZero = x!=0 || y!=0
  def isZero = x==0 && y==0
  def isNonNegative = x>=0 && y>=0
  def updateX(newX: Int) = XY(newX, y) 
  def updateY(newY: Int) = XY(x, newY)
  def addToX(dx: Int) = XY(x+dx, y) 
  def addToY(dy: Int) = XY(x, y+dy)
  def +(pos: XY) = XY(x+pos.x, y+pos.y)
  def -(pos: XY) = XY(x-pos.x, y-pos.y)
  def *(factor: Double) = XY((x*factor).intValue, (y*factor).intValue)
  def distanceTo(pos: XY) : Double = (this-pos).length 
  def length : Double = math.sqrt(x*x + y*y)
  def signum = XY(x.signum, y.signum)
  def negate = XY(-x, -y) 
  def negateX = XY(-x, y) 
  def negateY = XY(x, -y)
  def toEntityName = "%s_%s".format(x,y)
}
