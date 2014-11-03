package week5

import scala.collection.mutable


/**
 * Created with IntelliJ IDEA.
 * User: coderh
 * Date: 10/31/14
 * Time: 11:47 PM
 */

object Basic extends App {

  case class Point(x: Double, y: Double) extends Point2D

  def question1() = {

    case class Cluster(pointSet: Set[Point2D]) {
      val centroid = Point(pointSet.map(_.x).sum / pointSet.size, pointSet.map(_.y).sum / pointSet.size)
    }

    val clusters = mutable.Map(
      1 -> Cluster(Set(Point(23, 40))),
      2 -> Cluster(Set(Point(25, 125))),
      3 -> Cluster(Set(Point(29, 97))),
      4 -> Cluster(Set(Point(33, 22))),
      5 -> Cluster(Set(Point(35, 63))),
      6 -> Cluster(Set(Point(42, 57))),
      7 -> Cluster(Set(Point(44, 105))),
      8 -> Cluster(Set(Point(55, 20))),
      9 -> Cluster(Set(Point(55, 63))),
      10 -> Cluster(Set(Point(64, 37))))

    val pts = Set(
      Point(28, 145),
      Point(38, 115),
      Point(43, 83),
      Point(50, 30),
      Point(50, 60),
      Point(50, 90),
      Point(50, 130),
      Point(55, 118),
      Point(63, 88),
      Point(65, 140))

    def KMeans(idToCluster: mutable.Map[Int, Cluster], ds: Set[Point]) = {
      // re-assgin
      val res = ds map {
        pt =>
          val closest = idToCluster.map {
            case (id, cluster) => (id, pt distanceTo cluster.centroid)
          }.minBy(_._2)._1
          pt -> closest
      }

      // recompute
      res foreach {
        case (pt, cid) => idToCluster.update(cid, Cluster(idToCluster(cid).pointSet + pt))
      }
    }
    //    clusters foreach println
    clusters.mapValues(_.centroid) foreach println
    println
    KMeans(clusters, pts)
    //    clusters foreach println
    clusters.mapValues(_.centroid) foreach println
    println
  }

  def question2() = {

    def boundary = (pt: Point2D) => 3 * pt.x - pt.y - 30

    val points = List(
      (Point(13, 7), Point(14, 10)),
      (Point(11, 4), Point(14, 10)),
      (Point(13, 7), Point(16, 16)),
      (Point(13, 7), Point(16, 19)))

    val res = points.filter {
      case (yellow, blue) => boundary(yellow) < 0 && boundary(blue) > 0
    }

    println(res)
  }

  def question3() = {
    case class Advertiser(var budget: Int, bid: Map[String, Int])



    def balance(queries: List[String], advers: List[Advertiser]): Int = {
      queries match {
        case q :: qs =>
          advers.filter(ader => ader.budget > 0 && ader.bid.contains(q)) match {
            case x :: xs =>
              val chosen = (x :: xs).maxBy(_.budget)
              val bid = chosen.bid(q)
              chosen.budget = chosen.budget - bid
              bid + balance(qs, advers)
            case Nil => 0
          }
        case Nil => 0
      }
    }

    val queryA = List("x", "x", "x", "z")
    val queryB = List("y", "y", "x", "x")
    val queryC = List("x", "x", "x", "y")
    val queryD = List("x", "z", "z", "z")

    def advertisers = {
      val A = Advertiser(2, Map("x" -> 1, "y" -> 1))
      val B = Advertiser(2, Map("x" -> 1, "z" -> 1))
      List(A, B)
    }

    println(balance(queryA, advertisers))
    println(balance(queryB, advertisers))
    println(balance(queryC, advertisers))
    println(balance(queryD, advertisers))

  }

  def question4() = {
    // set cover problem

    def distinctCharSet(src: List[String]): List[Char] = {
      src.map(_.toCharArray).flatten.distinct
    }

    val setList = List("AB", "BC", "CD", "DE", "EF", "FG", "GH", "AH", "ADG", "ADF")
    val all = distinctCharSet(setList)

    def dumb(src: List[String], acc: List[String] = Nil): List[String] = {
      src match {
        case x :: xs =>
          if ((all diff distinctCharSet(acc :+ x)).size != 0)
            dumb(xs, acc :+ x)
          else acc :+ x
        case _ => acc
      }
    }

    def simple(src: List[String], acc: List[String] = Nil): List[String] = {
      src match {
        case x :: xs =>
          if (x.toCharArray.exists(ch => !distinctCharSet(acc).contains(ch))) {
            if ((all diff distinctCharSet(acc :+ x)).size != 0)
              simple(xs, acc :+ x)
            else
              acc :+ x
          } else {
            simple(xs, acc)
          }
        case Nil => acc
      }
    }

    def largestFirst(src: List[String], acc: List[String] = Nil): List[String] = {
      src.sortBy(-_.size) match {
        case x :: xs =>
          if (x.toCharArray.exists(ch => !distinctCharSet(acc).contains(ch))) {
            if ((all diff distinctCharSet(acc :+ x)).size != 0)
              simple(xs, acc :+ x)
            else
              acc :+ x
          } else {
            simple(xs, acc)
          }
        case Nil => acc
      }
    }

    def mostHelp(src: List[String], acc: List[String] = Nil): List[String] = {
      val sorted = src sortBy (set => -set.count(ch => !distinctCharSet(acc).contains(ch)))
      sorted match {
        case x :: xs =>
          if ((all diff distinctCharSet(acc :+ x)).size != 0)
            mostHelp(xs, acc :+ x)
          else
            acc :+ x
        case Nil => acc
      }
    }

    def showRes(res: List[String], name: String) = {
      println("Method: " + name)
      println(s"$res\nsize = ${res.size}\n")
    }

    showRes(dumb(setList), "dumb")
    showRes(simple(setList), "simple")
    showRes(largestFirst(setList), "largestFirst")
    showRes(mostHelp(setList), "mostHelp")
  }


  // question1
  question2
  // question3
  //  question4
}

