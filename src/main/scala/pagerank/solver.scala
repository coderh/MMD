package pagerank

import breeze.linalg.{max, sum, DenseVector, DenseMatrix}
import breeze.numerics._

/**
 * Created with IntelliJ IDEA.
 * User: coderh
 * Date: 11/15/14
 * Time: 12:39 AM
 */

object solver {
  def solve(m: DenseMatrix[Double], beta: Double = 1d, magnitude: Int = 1, eps: Double = 0.0001, iter: Int = 1000): DenseVector[Double] = {
    val N = m.rows
    var r = DenseVector.tabulate(N)(i => magnitude.toDouble / N)
    val tlptVector = DenseVector.tabulate(N)(i => magnitude * (1 - beta) / N)
    var r_old = r
    var cnt = 0

    do {
      r_old = r
      r = (m * r).mapValues(_ * beta) + tlptVector
      cnt = cnt + 1
      println("iteration " + cnt + " => " + r)
    } while (sum(abs(r_old - r)) > eps && cnt < iter)
    r
  }

  /**
   * Topic-specific pank rank
   *
   * @param m
   * @param teleportSet
   * @param weights
   * @param beta
   * @param magnitude
   * @param eps
   * @param iter
   * @return
   */

  def solveTSPR(m: DenseMatrix[Double], teleportSet: Array[Int], weights: Array[Int], beta: Double = 1d, magnitude: Int = 1, eps: Double = 0.0001, iter: Int = 1000): DenseVector[Double] = {
    require(teleportSet.size == weights.size)
    val N = m.rows
    var r = DenseVector.tabulate(N)(i => magnitude.toDouble / N)
    val teleportVector = DenseVector.tabulate(N) {
      i =>
        if (teleportSet.contains(i))
          magnitude * (1 - beta) * weights(i) / weights.sum
        else
          0
    }
    var r_old = r
    var cnt = 0

    do {
      r_old = r
      r = (m * r).mapValues(_ * beta) + teleportVector
      cnt = cnt + 1
      println("iteration " + cnt + " => " + r)
    } while (sum(abs(r_old - r)) > eps && cnt < iter)
    r
  }

  /**
   * Typertext-Induced Topic Selection
   *
   * @param L
   * @param v_h
   * @param v_a
   * @param iteration
   * @return
   */
  def solveHIT(L: DenseMatrix[Double], v_h: Option[DenseVector[Double]] = None, v_a: Option[DenseVector[Double]] = None, iteration: Int = 1000) = {

    if (v_h.isDefined && v_a.isDefined)
      throw new IllegalArgumentException
    else if (!v_h.isDefined && !v_a.isDefined)
      throw new IllegalArgumentException

    if (v_h.isDefined) {
      var a, h = v_h.get
      var cnt = 0
      do {
        println("Iter = " + cnt)
        a = L.t * h
        a /= max(a)
        h = L * a
        h /= max(h)
        println("a = " + a)
        println("h = " + h)
        println
        cnt += 1

      } while (cnt < iteration)
      (a, h)
    } else {
      var a, h = v_a.get
      var cnt = 0
      do {
        h = L * a
        h /= max(h)
        a = L.t * h
        a /= max(a)
        cnt += 1
      } while (cnt < iteration)
      (a, h)
    }
  }
}
