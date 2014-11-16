package week7

import breeze.linalg.DenseMatrix
import pagerank.solver._

/**
 * Created with IntelliJ IDEA.
 * User: coderh
 * Date: 11/14/14
 * Time: 11:03 PM
 */

object Basic extends App {

  // Topic-specific page-rank
  def question1() = {

    val M = DenseMatrix(
      (0d, 1d, 0d, 0d),
      (0.5, 0d, 0d, 0d),
      (0.5, 0d, 0d, 1d),
      (0d, 0d, 1d, 0d)
    )

    val res = solveTSPR(M, Array(0, 1), Array(2, 1), 0.7)

    println(res)
  }


  def question2(): Unit = {
    val x = 0.85
    val a = 1 / (1 - x * x * x)
    val b = (1 - x) * x * a
    val c = (1 - x) * x * x * a
    println("a = " + a)
    println("b = " + b)
    println("c = " + c)
  }

  //  question1()
  question2()

}
