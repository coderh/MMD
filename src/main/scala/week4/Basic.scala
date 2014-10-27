package week4

import breeze.linalg.DenseVector

/**
 * Created with IntelliJ IDEA.
 * User: coderh
 * Date: 10/27/14
 * Time: 9:48 PM
 */

object Basic extends App {

  // Quesetion 2

  val A = (x: Double) => DenseVector(1, 0, 1, 0, 1, 2 * x)
  val B = (x: Double) => DenseVector(1, 1, 0, 0, 1, 6 * x)
  val C = (x: Double) => DenseVector(0, 1, 0, 1, 0, 2 * x)

  val disAB = (x: Double) => (A(x) dot B(x)).asInstanceOf[Int].toDouble / A(x).norm() / B(x).norm()
  val disAC = (x: Double) => (A(x) dot C(x)).asInstanceOf[Int].toDouble / A(x).norm() / C(x).norm()
  val disBC = (x: Double) => (B(x) dot C(x)).asInstanceOf[Int].toDouble / B(x).norm() / C(x).norm()

  def showRes(x: Double) = {
    println(
      s"""
        |Notice: The more approch to 1, the more similar
        |
        |When x = $x
        |distance between A B = ${disAB(x)}
        |distance between A C = ${disAC(x)}
        |distance between B C = ${disBC(x)}
      """.stripMargin)
  }

  Array(0.0, 0.5, 1, 2) foreach showRes

}
