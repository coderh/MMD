package week4

import breeze.linalg.DenseVector

/**
 * Created with IntelliJ IDEA.
 * User: coderh
 * Date: 10/27/14
 * Time: 10:12 PM
 */

object Advanced extends App {
  def question_1() = {
    val choices = Array(
      DenseVector(0.312, 0.156, -0.937),
      DenseVector(0.608, -0.459, -0.119),
      DenseVector(0.702, -0.702, 0.117),
      DenseVector(0.975, 0.700, -0.675)
    )

    choices.map(_.dot(DenseVector(2d / 7, 3d / 7, 6d / 7))) foreach println
  }


  def question_2() = {

  }
}
