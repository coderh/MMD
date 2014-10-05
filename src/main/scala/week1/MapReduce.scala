package week1

/**
 * Created with IntelliJ IDEA.
 * User: coderh
 * Date: 10/5/14
 * Time: 11:38 PM
 */

object MapReduce extends App {

  def mapFunc(num: Int): List[(Int, Int)] = {
    val pd = for (i <- 2 to num - 1
                  if (2 to scala.math.sqrt(i).toInt).forall(num % _ != 0) && num % i == 0)
    yield i

    pd.map(v => (v, num)).toList
  }

  def reducer(mapOut: List[(Int, Int)]) = {
    mapOut.sortBy(_._1).groupBy(_._1).mapValues(_.map(_._2))
  }

  def reduceFunc(mapVal: (Int, List[Int])) = {
    (mapVal._1, mapVal._2.sum)
  }

  val res = reducer(List(15, 21, 24, 30, 49).map(mapFunc).flatten).map(reduceFunc)

  println(res)
}