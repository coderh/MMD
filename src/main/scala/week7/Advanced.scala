package week7

import breeze.linalg.{max, DenseVector, DenseMatrix}

/**
 * Created with IntelliJ IDEA.
 * User: coderh
 * Date: 11/15/14
 * Time: 2:09 AM
 */

object Advanced extends App {
  def question3() = {


    import pagerank.solver._
    val L = DenseMatrix(
      (0d, 1d, 1d, 0d),
      (1d, 0d, 0d, 0d),
      (0d, 0d, 0d, 1d),
      (0d, 0d, 1d, 0d)
    )

    val res = solveHIT(L, v_h = Some(DenseVector(1d, 1d, 1d, 1d)), iteration = 2)
    println(res)

  }

  def question4() = {

    val k = 3d
    val x = 0.75

    val res = 4 * (21 + k + 3 * (x + (1 - x) * k))
    println(res)
  }

  // question3()

  question4()
}
