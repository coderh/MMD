package finalExam

import breeze.linalg.{DenseVector, DenseMatrix}
import pagerank.solver._

/**
 * Created with IntelliJ IDEA.
 * User: invkrh
 * Date: 11/30/14
 * Time: 8:39 PM
 */

import scala.math._

object Advanced extends App {

  val m = DenseMatrix(
    (-0.57, -0.11, -0.57, -0.11, -0.57),
    (-0.09, 0.7, -0.09, 0.7, -0.09)
  )

  val v1 = DenseVector(5d, 0d, 0d, 0d, 0d)
  val v2 = DenseVector(0d, 5d, 0d, 0d, 0d)
  val v3 = DenseVector(0d, 0d, 0d, 0d, 4d)

  println(m * v1)
  println(m * v2)
  println(m * v3)

}
