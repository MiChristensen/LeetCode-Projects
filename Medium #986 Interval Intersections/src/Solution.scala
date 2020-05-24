import scala.collection.mutable
import scala.collection.mutable.TreeSet

/*
Given two lists of closed intervals, each list of intervals is pairwise disjoint and in sorted order.

Return the intersection of these two interval lists.

(Formally, a closed interval [a, b] (with a <= b) denotes the set of real numbers x with a <= x <= b.
The intersection of two closed intervals is a set of real numbers that is either empty,
or can be represented as a closed interval.  For example, the intersection of [1, 3] and [2, 4] is [2, 3].)

Example 1:
Input: A = [[0,2],[5,10],[13,23],[24,25]], B = [[1,5],[8,12],[15,24],[25,26]]
Output: [[1,2],[5,5],[8,10],[15,23],[24,24],[25,25]]

Reminder: The inputs and the desired output are lists of Interval objects, and not arrays or lists.

Note:
0 <= A.length < 1000
0 <= B.length < 1000
0 <= A[i].start, A[i].end, B[i].start, B[i].end < 10^9
NOTE: input types have been changed on April 15, 2019. Please reset to default code definition to get new method signature.
*/

/* The idea behind this solution is to find the interval 'curInterval' with the least start value. We save this
   as a tuple (Array[Int],Char). The char indicates which array the interval is from ('A' or 'B').
   We now find intervals from the opposite array that have a start value that is less than the end val
   of curInterval (curInterval._1(1)) We update the result array every time we find an interscetion.
 */

object Solution {

  def intervalIntersection(A: Array[Array[Int]], B: Array[Array[Int]]): Array[Array[Int]] = {
    if(A.length <= 0 || B.length <= 0) return Array()
    var result: Array[Array[Int]] = Array()
    var i: Int = 0
    var j: Int = 0
    while (i <= A.length - 1 && j <= B.length - 1) {
      var curInterval: (Array[Int], Char) = (Array(), ' ')
      if (A(i)(0) < B(j)(0) || (A(i)(0) == B(j)(0) && A(i)(1) >= B(j)(1))) {
        curInterval = (A(i), 'A');
      }
      else {
        curInterval = (B(j), 'B');
      }
      if (curInterval._2 == 'A') {
        if (B(j)(0) <= A(i)(1)) {
          if (B(j)(1) <= A(i)(1)) {
            result = result :+ B(j)
            j += 1
          }
          else {
            result = result :+ Array(B(j)(0), curInterval._1(1))
            i += 1
          }
        }
        else i += 1
      }
      else if (curInterval._2 == 'B') {
        if (A(i)(0) <= B(j)(1)) {
          if (A(i)(1) <= B(j)(1)) {
            result = result :+ A(i)
            i += 1
          }
          else {
            result = result :+ Array(A(i)(0), curInterval._1(1))
            j += 1
          }
        }
        else j += 1
      }
    }
      result
  }

  def main(args: Array[String]): Unit = {
    val a: Array[Array[Int]] = Array(Array(0,2),Array(5,10),Array(13,23),Array(24,25))
    val b: Array[Array[Int]] = Array(Array(1,5),Array(8,12),Array(15,24),Array(25,26))
    val c: Array[Array[Int]] = Array()
    printArray(intervalIntersection(a,b))
    printArray(intervalIntersection(a,c))
  }

  def printArray(arr: Array[Array[Int]]): Unit = {
    print("(")
    for (c <- arr) {
      print("(")
      for(int <- c) {
        print(s"$int,")
      }
      print(")")
    }
    println(")")
  }

  def printArray(arr: Array[Int], start: Int, end: Int): Unit = {
    print(s"Array($start, $end)")
    for(c <- start to end) {
      print(s" ${arr(c)}")
    }
    println("")
  }

  def printArray(arr: Array[Int]): Unit = {
    print("Array(")
    for (c <- arr) print(s"$c ")
    print(")")
    println("")
  }

}
