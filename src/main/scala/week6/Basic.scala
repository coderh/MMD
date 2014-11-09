package week6

import breeze.linalg.{DenseVector, DenseMatrix}

/**
 * Created with IntelliJ IDEA.
 * User: coderh
 * Date: 11/8/14
 * Time: 10:56 PM
 */

object Basic extends App {

  case class LabeledPoint(x: Int, y: Int, label: Int)

  def question1() = {
    val A1 = DenseMatrix(
      (8d, 3d, 1d),
      (7d, 2d, 1d),
      (3d, 3d, 1d))
    val b1 = DenseVector(1d, -1d, -1d)

    val w1b = A1 \ b1

    val A2 = DenseMatrix(
      (5d, 4d, 1d),
      (8d, 3d, 1d),
      (7d, 2d, 1d))
    val b2 = DenseVector(1d, 1d, -1d)

    val w2b = A2 \ b2

    println(Array(w1b, w2b).minBy(_.slice(0, 2).norm()))

  }


  def question2() = {


    val ds = Array(
      LabeledPoint(1, 2, -1),
      LabeledPoint(1, 4, -1),
      LabeledPoint(3, 2, -1),
      LabeledPoint(5, 4, -1),
      LabeledPoint(5, 6, -1),
      LabeledPoint(5, 8, -1),
      LabeledPoint(7, 4, -1),
      LabeledPoint(7, 6, -1),
      LabeledPoint(1, 6, 1),
      LabeledPoint(1, 8, 1),
      LabeledPoint(3, 4, 1),
      LabeledPoint(3, 6, 1),
      LabeledPoint(3, 8, 1),
      LabeledPoint(5, 10, 1),
      LabeledPoint(7, 8, 1),
      LabeledPoint(7, 10, 1))

    val res = ds.map {
      lpt =>
        val slack = if (lpt.label >= 0) -lpt.x + lpt.y - 3
        else -lpt.x + lpt.y - 1
        (lpt, slack)
    }

    res.sortBy(_._1.x) foreach println
  }


  def question3() = {
    val ds = Array(
      LabeledPoint(28, 145, 1),
      LabeledPoint(38, 115, 1),
      LabeledPoint(43, 83, 1),
      LabeledPoint(50, 130, 1),
      LabeledPoint(50, 90, 1),
      LabeledPoint(50, 60, 1),
      LabeledPoint(50, 30, 1),
      LabeledPoint(55, 118, 1),
      LabeledPoint(63, 88, 1),
      LabeledPoint(65, 140, 1),
      LabeledPoint(23, 40, -1),
      LabeledPoint(25, 125, -1),
      LabeledPoint(29, 97, -1),
      LabeledPoint(33, 22, -1),
      LabeledPoint(35, 63, -1),
      LabeledPoint(42, 57, -1),
      LabeledPoint(44, 105, -1),
      LabeledPoint(55, 63, -1),
      LabeledPoint(55, 20, -1),
      LabeledPoint(64, 37, -1))

    def decisionTree(pt: LabeledPoint) = {
      if (pt.x < 45) {
        if (pt.y < 110) -1 else 1
      } else {
        if (pt.y < 75) -1 else 1
      }
    }

    ds.filter(pt => pt.label != decisionTree(pt)) foreach println
  }

  // question1()
  // question2()
  question3()
}
