package week5

/**
 * Created with IntelliJ IDEA.
 * User: coderh
 * Date: 11/2/14
 * Time: 11:42 PM
 */

import math._

trait Point2D {
  val x: Double
  val y: Double

  def distanceTo(that: Point2D) = {
    pow(this.x - that.x, 2) + pow(this.y - that.y, 2)
  }
}