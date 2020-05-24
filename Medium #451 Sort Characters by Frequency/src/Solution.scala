import scala.collection.immutable.HashMap
import scala.collection.mutable

/* Given a string, sort it in decreasing order based on the frequency of characters.

Example 1:

Input:
"tree"

Output:
"eert"

Explanation:
'e' appears twice while 'r' and 't' both appear once.
So 'e' must appear before both 'r' and 't'. Therefore "eetr" is also a valid answer.

Example 2:

Input:
"cccaaa"

Output:
"cccaaa"

Explanation:
Both 'c' and 'a' appear three times, so "aaaccc" is also a valid answer.
Note that "cacaca" is incorrect, as the same characters must be together.

Example 3:

Input:
"Aabb"

Output:
"bbAa"

Explanation:
"bbaA" is also a valid answer, but "Aabb" is incorrect.
Note that 'A' and 'a' are treated as two different characters.
 */

/* The idea behind this solution is that first we make a hashmap 'map' and hash every char in s to the
    amount of times it is contained in s. We then make a new PriorityQueue[Char] 'heap' and we order these
    chars by the hashed Int from 'map' such that the char with the highest priority is the char with the
    highest frequency in the input string s. We now use a StringBuilder to create the resulting string.
    We keep dequeuing the Char 'c' with the highest priority from 'heap' and we append 'c' x times where
    x is the Int it is hashed to in 'map'
 */
object Solution {
  def frequencySort(s: String): String = {
    var map: HashMap[Char, Int] = new HashMap()
    for (c <- s) {
      map = map + (c -> (map.getOrElse(c, 0) + 1))
    }
    val heap: mutable.PriorityQueue[Char] = new mutable.PriorityQueue()(Ordering.by[Char, Int]((a) => map.getOrElse(a, -1)))
    heap.addAll(map.keySet)

    var res: StringBuilder = new StringBuilder("")
    while (!heap.isEmpty) {
      val c: Char = heap.dequeue
      for (i <- 0 to map.getOrElse(c, -1) - 1) {
        res = res.append(c)
      }
    }
    res.toString()
  }

  def main(args: Array[String]): Unit = {
    val s1: String = "abbccccCPPP"
    val s2: String = ""
    assert(frequencySort(s1)=="ccccPPPbbaC")
    assert(frequencySort(s2)=="")
  }
}

