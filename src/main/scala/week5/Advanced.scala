package week5

import breeze.linalg.DenseVector

/**
 * Created with IntelliJ IDEA.
 * User: coderh
 * Date: 11/2/14
 * Time: 10:03 PM
 */

object Advanced extends App {
  def question2() = {
    /**
     * The publisher uses the following strategy to allocate the three ad slots:
     *
     * Any advertiser whose budget is spent is ignored in what follows.
     * The first slot goes to the advertiser whose expected yield for the first slot (product of the bid and the CTR for the first slot) is the greatest. This advertiser is ignored in what follows.
     * The second slot goes to the advertiser whose expected yield for the second slot (product of the bid and the CTR for the second slot) is the greatest. This advertiser is ignored in what follows.
     * The third slot goes to the advertiser whose expected yield for the third slot (product of the bid and the CTR for the third slot) is the greatest.
     *
     * The same three advertisers get the three ad positions until one of two things happens:
     * An advertiser runs out of budget, or
     * All 101 click-throughs have been obtained.
     *
     * Either of these events ends one phase of the allocation.
     * If a phase ends because an advertiser ran out of budget, then they are assumed to get all the clicks their budget buys.
     * During the same phase, we calculate the number of click-throughs received by the other two advertisers by assuming that all three received click-throughs in proportion to their respective CTR's for their positions (round to the nearest integer).
     * If click-throughs remain, the publisher reallocates all three slots and starts a new phase.
     *
     * If the phase ends because all click-throughs have been allocated,
     * assume that the three advertisers received click-throughs in proportion to their respective CTR's (again, rounding if necessary).
     */

    case class Advertiser(name: String, bid: Double, ctr1: Double, ctr2: Double, ctr3: Double, var budget: Double, var click: Int = 0) {
      val exp1 = bid * ctr1
      val exp2 = bid * ctr2
      val exp3 = bid * ctr3

      val ctrMap = Map(1 -> ctr1, 2 -> ctr2, 3 -> ctr3)
    }
    //                      bid    ctr1    ctr2    ctr3    budget
    val A = Advertiser("A", 0.10, 0.015, 0.010, 0.005, 1.0)
    val B = Advertiser("B", 0.09, 0.016, 0.012, 0.006, 2.0)
    val C = Advertiser("C", 0.08, 0.017, 0.014, 0.007, 3.0)
    val D = Advertiser("D", 0.07, 0.018, 0.015, 0.008, 4.0)
    val E = Advertiser("E", 0.06, 0.019, 0.016, 0.010, 5.0)


    def nearestInteger(nb: Double): Int = {
      val number = f"$nb%.3f".toDouble
      if (number - number.toInt > 0.5) number.toInt + 1
      else number.toInt
    }

    def strategy(availableAds: List[Advertiser], clicksLeft: Int): Unit = {

      if (clicksLeft > 0) {
        var clickToGo = clicksLeft

        // select winner for each auction
        val auction1 = availableAds.maxBy(_.exp1)
        val auction2 = (availableAds diff List(auction1)).maxBy(_.exp2)
        val auction3 = (availableAds diff List(auction1, auction2)).maxBy(_.exp3)

        val auctionMap = Map(
          auction1 -> 1,
          auction2 -> 2,
          auction3 -> 3)

        val clickInLastPhase = Map(
          auction1 -> auction1.click,
          auction2 -> auction2.click,
          auction3 -> auction3.click)

        val budgetInLasePhase = Map(
          auction1 -> auction1.budget,
          auction2 -> auction2.budget,
          auction3 -> auction3.budget)

        val players = auctionMap.unzip._1
        val adsOutOfBudget = players.minBy(ad => ad.budget / ad.bid)
        // base line for computing clicks proportion
        val refCTR = adsOutOfBudget.ctrMap(auctionMap(adsOutOfBudget))

        val others = auctionMap.unzip._1.toList diff List(adsOutOfBudget)

        var localClick = 0
        while (players.forall(p => p.budget >= p.bid) && clickToGo > 0) {
          adsOutOfBudget.budget =
            BigDecimal(adsOutOfBudget.budget - adsOutOfBudget.bid).setScale(2, BigDecimal.RoundingMode.HALF_UP).toDouble
          adsOutOfBudget.click += 1

          localClick += 1

          others foreach {
            ad =>
              val localCTR = ad.ctrMap(auctionMap(ad))
              val clickInCurrentPhase = nearestInteger(localClick / refCTR * localCTR)
              ad.click = clickInLastPhase(ad) + clickInCurrentPhase
              ad.budget = budgetInLasePhase(ad) - ad.bid * clickInCurrentPhase
          }

          clickToGo = clicksLeft - players.map(ad => ad.click - clickInLastPhase(ad)).sum
        }

        availableAds foreach println
        println("clicksLeft = " + clickToGo)
        println
        strategy(availableAds diff List(adsOutOfBudget), clickToGo)
      } else {
        println("Done")
      }
    }

    strategy(List(A, B, C, D, E), 101)

  }

  def question3() = {
    case class LabeledPoint2D(label: String, x: Double, y: Double) extends Point2D

    val points = List(
      LabeledPoint2D("x", 0d, 0d),
      LabeledPoint2D("y", 10d, 10d),
      LabeledPoint2D("a", 1d, 6d),
      LabeledPoint2D("b", 3d, 7d),
      LabeledPoint2D("c", 4d, 3d),
      LabeledPoint2D("d", 7d, 7d),
      LabeledPoint2D("e", 8d, 2d),
      LabeledPoint2D("f", 9d, 5d))

    val dist = for {
      x <- points
      y <- points if y != x
    } yield (x, y, x distanceTo y)

    val (furthest1, furthest2, distSquare) = dist.maxBy(_._3)
    val repSetInit = List(furthest1, furthest2)
    val others = points diff repSetInit

    def representativeSet(pointSet: List[LabeledPoint2D], acc: List[LabeledPoint2D], n: Int): List[LabeledPoint2D] = {
      if (n != 0) {
        val dist = pointSet map {
          case pt: LabeledPoint2D => (pt, acc.map(rept => rept distanceTo pt).min)
        }
        val chosen = dist.maxBy(_._2)._1
        println(chosen)
        representativeSet(pointSet diff List(chosen), acc :+ chosen, n - 1)
      } else {
        acc
      }


    }

    representativeSet(others, repSetInit, 5)
  }

  question2
  //  question3

}