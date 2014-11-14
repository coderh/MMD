package project

import org.apache.spark.rdd.RDD
import org.apache.spark.storage.StorageLevel

import scala.util.hashing._

/**
 * Created with IntelliJ IDEA.
 * User: coderh
 * Date: 11/10/14
 * Time: 4:14 PM
 */

object SimilarSentence extends App {

  /**
   * 9,397,023 sentences
   * 1,350,205 distinct words
   * 10,392,158 distinct (word, index) pair
   * longest sentence: 12452 words
   * 1092 groups by size
   * 117 byte per sentence
   * 30 Integers in size per sentence
   */

  import SparkSettings._

  def isSubstitutionValid(p: Array[(String, String)], acc: Int = 0): Boolean = {
    p match {
      case Array(x, xs@_*) =>
        if (x._1 equals x._2)
          isSubstitutionValid(xs.toArray)
        else {
          if (acc + 1 == 2) false
          else isSubstitutionValid(xs.toArray, 1)
        }
      case Array() => true
    }
  }

  def isAdditionDeletionValid(short: Array[String], long: Array[String], acc: Int = 0): Boolean = {
    (long, short) match {
      case (l, s) if s.size == 0 => true
      case (l, s) if l.head equals s.head => isAdditionDeletionValid(l.tail, s.tail)
      case (l, s) if !l.head.equals(s.head) => l.tail zip s forall (p => p._1 equals p._2)
    }
  }

  //  val sentences1 = sc.textFile("data/sentences.txt", 20).map {
  //    str => str.split(" ") match {
  //      case Array(id, s@_*) => (id.toInt, s.toArray)
  //    }
  //  } //.filter(_._1 < 100000)
  //
  //  val sentences2 = sc.parallelize(Array(
  //    (1, Array("A", "B", "C", "D")),
  //    (2, Array("A", "B", "X", "D")),
  //    (3, Array("A", "B", "C")),
  //    (4, Array("A", "B", "X", "C"))
  //  ))

  // Test validation function

  //  val s1 = Array("A", "B", "C", "D")
  //  val s2 = Array("A", "B", "X", "D")
  //  val s3 = Array("A", "B", "C")
  //  val s4 = Array("A", "B", "X", "C")
  //
  //  println(isValidate(s1, s2))
  //  println(isValidate(s1, s3))
  //  println(isValidate(s1, s4))
  //  println(isValidate(s2, s3))
  //  println(isValidate(s2, s4))
  //  println(isValidate(s3, s4))


  // data choice
  //  val sentences = sentences2

  import org.apache.spark.SparkContext._


}
