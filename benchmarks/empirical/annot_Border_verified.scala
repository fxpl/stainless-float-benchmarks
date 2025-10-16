package annot
import stainless.lang.*

// https://github.com/outr/youi/blob/6c81b1262569cdaba4e81681362d909022fe3a8e/core/shared/src/main/scala/io/youi/Direction.scala

sealed trait Direction

sealed trait Compass extends Direction {
  def horizontal: Horizontal
  def vertical: Vertical
}

object Compass {
  case object Center extends Compass {
    override def horizontal: Horizontal = Horizontal.Center
    override def vertical: Vertical = Vertical.Middle
  }
  case object North extends Compass {
    override def horizontal: Horizontal = Horizontal.Center
    override def vertical: Vertical = Vertical.Top
  }
  case object South extends Compass {
    override def horizontal: Horizontal = Horizontal.Center
    override def vertical: Vertical = Vertical.Bottom
  }
  case object East extends Compass {
    override def horizontal: Horizontal = Horizontal.Right
    override def vertical: Vertical = Vertical.Middle
  }
  case object West extends Compass {
    override def horizontal: Horizontal = Horizontal.Left
    override def vertical: Vertical = Vertical.Middle
  }
  case object NorthWest extends Compass {
    override def horizontal: Horizontal = Horizontal.Left
    override def vertical: Vertical = Vertical.Top
  }
  case object NorthEast extends Compass {
    override def horizontal: Horizontal = Horizontal.Right
    override def vertical: Vertical = Vertical.Top
  }
  case object SouthWest extends Compass {
    override def horizontal: Horizontal = Horizontal.Left
    override def vertical: Vertical = Vertical.Bottom
  }
  case object SouthEast extends Compass {
    override def horizontal: Horizontal = Horizontal.Right
    override def vertical: Vertical = Vertical.Bottom
  }
}

sealed class Horizontal(val compass: Compass) extends Direction

object Horizontal {
  val Left = Horizontal(Compass.West)
  val Center = Horizontal(Compass.Center)
  val Right = Horizontal(Compass.East)
}

sealed class Vertical(val compass: Compass) extends Direction

object Vertical {
  val Top = Vertical(Compass.North)
  val Middle = Vertical(Compass.Center)
  val Bottom = Vertical(Compass.South)
}


// https://github.com/outr/youi/blob/6c81b1262569cdaba4e81681362d909022fe3a8e/gui/src/main/scala/io/youi/paint/Stroke.scala

case class Stroke(lineWidth: Double = 1.0) {
    require(lineWidth >= 0.0)
}

object Stroke {
  lazy val none: Stroke = Stroke(0.0)
}

// https://github.com/outr/youi/blob/6c81b1262569cdaba4e81681362d909022fe3a8e/gui/src/main/scala/io/youi/paint/Border.scala

sealed trait Border {
  def size(compass: Compass): Double

  // TO SPECIFY: 277
  def width: Double = {
    size(Compass.West) + size(Compass.East)
  }.ensuring(res => res >= 0.0)


  // TO SPECIFY: 278
  def height: Double = {
    size(Compass.North) + size(Compass.South)
  }.ensuring(res => res >= 0.0)
}

object Border {
  lazy val empty: Border = apply(Stroke.none)

  def apply(stroke: Stroke, radius: Double = 0.0): RectangleBorder = RectangleBorder(stroke, radius)
}

case class RectangleBorder(stroke: Stroke, radius: Double) extends Border {

  override def size(compass: Compass): Double = stroke.lineWidth


}