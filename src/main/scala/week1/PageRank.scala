package week1

import breeze.linalg._
import breeze.numerics.abs

/**
 * Created with IntelliJ IDEA.
 * User: coderh
 * Date: 10/5/14
 * Time: 9:42 PM
 */

object PageRank extends App {

  def solve(m: DenseMatrix[Double], beta: Double = 1d, magnitude: Int = 1, eps: Double = 0.0001, iter: Int = 1000): DenseVector[Double] = {
    val N = m.rows
    var r = DenseVector.tabulate(N)(i => magnitude.toDouble / N)
    var r_old = r
    var cnt = 0

    do {
      r_old = r
      r = (m * r).mapValues(_ * beta) + DenseVector.tabulate(N)(i => magnitude * (1 - beta) / N)
      cnt = cnt + 1
      println("iteration " + cnt + " => " + r)
    } while (sum(abs(r_old - r)) > eps && cnt < iter)
    r
  }

  // Question 1:

  def solution1(): Unit = {
    val M1 = DenseMatrix(
      (0d, 0d, 0d),
      (0.5, 0d, 0d),
      (0.5, 1d, 1d)
    )

    val res = solve(M1, 0.7, 3)
    val a = res(0)
    val b = res(1)
    val c = res(2)

    println("a + c = " + (a + c))
    println("b + c = " + (b + c))
  }

  // Question 2:

  def solution2(): Unit = {
    val M2 = DenseMatrix(
      (0d, 0d, 1d),
      (0.5, 0d, 0d),
      (0.5, 1d, 0d)
    )

    val res = solve(M2, 0.85)
    val a = res(0)
    val b = res(1)
    val c = res(2)

    println((0.85 * b) + " == " + (0.575 * a + 0.15 * c))
    println((0.95 * c) + " == " + (0.9 * b + 0.475 * a))
    println(c + " == " + (b + 0.575 * a))
    println(a + " == " + (c + 0.15 * b))
  }

  // Question 3:

  def solution3(): Unit = {
    val M3 = DenseMatrix(
      (0d, 0d, 1d),
      (0.5, 0d, 0d),
      (0.5, 1d, 0d)
    )
    solve(M3, magnitude = 3)
  }

  //  solution1()
  //  solution2()
  solution3()
}
