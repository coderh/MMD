package week5

import breeze.linalg.DenseVector

/**
 * Created with IntelliJ IDEA.
 * User: coderh
 * Date: 11/2/14
 * Time: 10:03 PM
 */

object Advanced extends App {
  def question2() = {

    case class Advertiser(name: String, bid: Double, ctr1: Double, ctr2: Double, ctr3: Double, var budget: Double) {
      val exp1 = bid * ctr1
      val exp2 = bid * ctr2
      val exp3 = bid * ctr3
    }
    //                      bid   ctr1   ctr2   ctr3   budget
    val A = Advertiser("A", 0.10, 0.015, 0.010, 0.005, 1.0)
    val B = Advertiser("B", 0.09, 0.016, 0.012, 0.006, 2.0)
    val C = Advertiser("C", 0.08, 0.017, 0.014, 0.007, 3.0)
    val D = Advertiser("D", 0.07, 0.018, 0.015, 0.008, 4.0)
    val E = Advertiser("E", 0.06, 0.019, 0.016, 0.010, 5.0)

    val ads = List(A, B, C, D, E)

    println(ads.sortBy(-_.exp1).map(_.name))
    println(ads.sortBy(-_.exp2).map(_.name))
    println(ads.sortBy(-_.exp3).map(_.name))

  }

  def question3() = {
    case class LabeledPoint2D(label: String, x: Double, y: Double) extends Point2D

    val points = List(
      LabeledPoint2D("x", 0d, 0d),
      LabeledPoint2D("y", 10d, 10d),
      LabeledPoint2D("a", 1d, 6d),
      LabeledPoint2D("b", 3d, 7d),
      LabeledPoint2D("c", 4d, 3d),
      LabeledPoint2D("d", 7d, 7d),
      LabeledPoint2D("e", 8d, 2d),
      LabeledPoint2D("f", 9d, 5d))

    val dist = for {
      x <- points
      y <- points if y != x
    } yield (x, y, x distanceTo y)

    val (furthest1, furthest2, distSquare) = dist.maxBy(_._3)
    val repSetInit = List(furthest1, furthest2)
    val others = points diff repSetInit

    def representativeSet(pointSet: List[LabeledPoint2D], acc: List[LabeledPoint2D], n: Int): List[LabeledPoint2D] = {
      if (n != 0) {
        val dist = pointSet map {
          case pt: LabeledPoint2D => (pt, acc.map(rept => rept distanceTo pt).min)
        }
        val chosen = dist.maxBy(_._2)._1
        println(chosen)
        representativeSet(pointSet diff List(chosen), acc :+ chosen, n - 1)
      } else {
        acc
      }


    }

    representativeSet(others, repSetInit, 5)
  }

  question2
  //  question3

}