package tsp

import scala.math.{pow, sqrt}

abstract class ACity {
  val X: Double
  val Y: Double
  val isEmpty: Boolean = false

  def getX(): Double = X
  def getY(): Double = Y
  
  

  def DistanceTo(that: ACity): Double = {
    val xd = pow(this.getX() - that.getX(), 2)
    val yd = pow(this.getY() - that.getY(), 2)
    val dist = sqrt(xd+yd)
    return dist
  }

  override def toString: String = {
    return "(" + getX() + "," + getY() + ")"
  }
}

class RCity extends ACity{
  val X = Math.random()*200
  val Y = Math.random()*200
}

class DCity(x: Double, y: Double) extends ACity{
  val X = x
  val Y = y

}

class ECity() extends ACity {
  val X = 0.0
  val Y = 0.0
  override val isEmpty = true
  
  override def DistanceTo(that: ACity): Double = {
    throw new RuntimeException("empty city")
    return 0
  }
  
  override def toString: String = {
    return "( Empty )"
  }
}