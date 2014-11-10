package project

import breeze.linalg.DenseVector


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
   * longest sentence: 12452 words
   */

  import SparkSettings._

  //  val sentences = sc.textFile("data/sentences.txt").map {
  //    str => str.split(" ") match {
  //      case Array(id, s@_*) => (id.toLong, s.toArray)
  //    }
  //  }

  val sentences = sc.parallelize(Array(
    (1, Array("A", "B", "C", "D")),
    (2, Array("A", "B", "X", "FD")),
    (3, Array("A", "B", "C")),
    (4, Array("A", "B", "X", "C"))
  ))

  val wordSet = sc.broadcast(sentences.map(_._2).flatMap(x => x).distinct.collect.sorted.zipWithIndex.toMap)
  val booleanMatrix = sentences.map {
    case (sid, words) =>
      val universalWords = wordSet.value
      val mp = (words.zipWithIndex map {
        case (wd, index) => (universalWords(wd), index)
      }).toMap
      (sid, DenseVector.tabulate(universalWords.size)(i => mp.getOrElse(i, -1)))
  }

  booleanMatrix.take(20) foreach println
  println(wordSet.value)
}
