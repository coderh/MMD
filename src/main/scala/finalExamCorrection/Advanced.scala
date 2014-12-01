package finalExamCorrection

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

  // q7()

  def q7() = {
    // HIT use adjacency matrix Aij = 1 if i -> j, it is not the one use in page rank
    val m = DenseMatrix(
      (0d, 1d, 0d, 0d),
      (0d, 0d, 1d, 0d),
      (0d, 0d, 0d, 1d),
      (0d, 0d, 0d, 0d)
    )
    val (authority, hubbiness) = solveHIT(m, v_h = Some(DenseVector(1d, 1d, 1d, 1d)), iteration = 5)

    println("authority = " + authority)
    println("hubbiness = " + hubbiness)
  }

  q12()


  def q12() = {
    val y = true
    val n = false
    val chocolate = List(y, n, n, y, y, n, y, n, y, y)
    val vanilla = List(n, y, n, y, y, n, y, y, n, n)
    val strawberry = List(y, y, n, y, n, n, y, n, y, y)
    val peanut = List(y, n, n, y, y, n, y, n, n, y)

    def xlogx(x: Double) = {
      if (x == 0) 0d else x * log(x) / log(2)
    }

    def averagedConditionalEntropy(y: List[Boolean], x: List[Boolean]) = {
      val px_pos = x.count(identity) / x.size.toDouble
      val px_neg = 1 - px_pos
      val pair = x zip y

      val hy_xpos = {
        val xpos = pair.filter(_._1)
        val proba_ypos_xpos = xpos.count(_._2) / xpos.size.toDouble
        val proba_yneg_xpos = 1 - proba_ypos_xpos
        -xlogx(proba_ypos_xpos) - xlogx(proba_yneg_xpos)
      }

      val hy_xneg = {
        val xneg = pair.filter(!_._1)
        val proba_ypos_xneg = xneg.count(_._2) / xneg.size.toDouble
        val proba_yneg_xneg = 1 - proba_ypos_xneg
        -xlogx(proba_ypos_xneg) - xlogx(proba_yneg_xneg)
      }

      println("--------------------")
      println("hy_xpos = " + hy_xpos)
      println("hy_xneg = " + hy_xneg)
      println("--------------------")

      px_pos * hy_xpos + px_neg * hy_xneg
    }

    val hp = {
      val proba_hp_pos = peanut.count(identity) / peanut.size.toDouble
      val proba_hp_neg = 1 - proba_hp_pos
      -xlogx(proba_hp_pos) - xlogx(proba_hp_neg)
    }

    val hpc = averagedConditionalEntropy(peanut, chocolate)
    val hpv = averagedConditionalEntropy(peanut, vanilla)
    val hps = averagedConditionalEntropy(peanut, strawberry)

    val igpc = hp - hpc
    val igpv = hp - hpv
    val igps = hp - hps

    println
    println(igpc)
    println(igpv)
    println(igps)

    val v = List(n, y, y, y, n, n)
    val s = List(y, y, n, y, y, y)
    val p = List(y, y, y, y, n, y)

    val hp_bis = {
      val proba_hp_pos = p.count(identity) / p.size.toDouble
      val proba_hp_neg = 1 - proba_hp_pos
      -xlogx(proba_hp_pos) - xlogx(proba_hp_neg)
    }

    val hpv_bis = averagedConditionalEntropy(p, v)
    val hps_bis = averagedConditionalEntropy(p, s)

    println
    println(hp_bis - hpv_bis)
    println(hp_bis - hps_bis)

    println(hp)

  }

}
