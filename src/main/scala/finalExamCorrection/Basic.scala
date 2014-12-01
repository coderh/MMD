package finalExamCorrection

import breeze.linalg.{DenseVector, DenseMatrix}
import scala.math._

/**
 * Created with IntelliJ IDEA.
 * User: coderh
 * Date: 12/1/14
 * Time: 11:50 AM
 */
object Basic extends App {
  // q12
  q19

  def q12() = {
    val m = DenseMatrix(
      (-0.57, -0.11, -0.57, -0.11, -0.57),
      (-0.09, 0.7, -0.09, 0.7, -0.09)
    )

    val v1 = DenseVector(5d, 0d, 0d, 0d, 0d)
    val v2 = DenseVector(0d, 2d, 0d, 0d, 4d)

    val f1 = m * v1
    val f2 = m * v2
    val cosSim = (f1 :* f2).sum / f1.norm() / f2.norm()

    // cosine similarity is not the angle of the cosine, no acos is needed here.
    println(cosSim)
  }

  def q19() = {
    val N = 1000
    val SUM = List(-323d, 1066d, 1776d)
    val SUMSQ = List(412d, 1500d, 3500d)

    val cs = SUM.map(_ / 1000)
    val sds = SUMSQ zip SUM map {
      //standard deviation is the square root of variance
      case (sq, s) => sqrt((sq / N) - pow(s / N, 2))
    }

    val md = sqrt((cs zip sds map { case (c, sd) => pow(-c / sd, 2)}).sum)

    println(md)
  }

}
