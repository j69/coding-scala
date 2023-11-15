package coding.hackerrank

import scala.io.StdIn

/** https://www.hackerrank.com/challenges/functions-or-not/problem */
object FP_04_FuncOrNot {

  def main(args: Array[String]): Unit = {
    val n = StdIn.readInt()
    (1 to n).foreach { _ =>
      val k = StdIn.readInt()
      val list = (1 to k).map(x => StdIn.readLine().split("\\s+")(0)).toList
      if (list.distinct.size == list.size) println("YES")
      else println("NO")
    }
  }

}
