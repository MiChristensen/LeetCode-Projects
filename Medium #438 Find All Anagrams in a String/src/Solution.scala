/* Find All Anagrams in a String
Given a string s and a non-empty string p, find all the start indices of p's anagrams in s.
Strings consists of lowercase English letters only and the length of both strings s and
p will not be larger than 20,100.

The order of output does not matter.

Example 1:
Input:
s: "cbaebabacd" p: "abc"

Output:
[0, 6]

Explanation:
The substring with start index = 0 is "cba", which is an anagram of "abc".
The substring with start index = 6 is "bac", which is an anagram of "abc".

Example 2:
Input:
s: "abab" p: "ab"

Output:
[0, 1, 2]

Explanation:
The substring with start index = 0 is "ab", which is an anagram of "ab".
The substring with start index = 1 is "ba", which is an anagram of "ab".
The substring with start index = 2 is "ab", which is an anagram of "ab".
*/

/* The idea of the solution is that we create two arrays of Ints, arrP and arrS. arrP will contain
   the # of every letter in string p. index 0 = a, index 1 = b etc. arrP(0) = 2 means that string p
   contains 2 a's.
   Similarly we fill arrS in the same way, but only a portion of length p.length at a time starting
   (0, p.length-1). We then continuously update this array as we slide the 'window' we have created.
   Whenever the two arrays are equal, the window of string s is an anagram of string p, and therefore
   we add the index of the start of the current window to the returned List[Int] list
 */

object Solution {
  def findAnagrams(s: String, p: String): List[Int] = {
    val arrP: Array[Int] = new Array(26) //Array corresponding to string p
    val arrS: Array[Int] = new Array(26) //Array corresponding to string s
    var list: List[Int] = List()

    //s can't contain anagrams of p if p is longer than s
    if (s.length < p.length) {
      return list
    }

    //Fill arrP
    for (c <- p) {
      arrP(c - 'a') += 1
    }

    /*Fill arrS with the (p.length-1) first chars of string s setting
      the window at the start position (0, p.length-1)
     */
    for (c <- 0 to p.length - 1) {
      arrS(s.charAt(c) - 'a') += 1
    }
    var windowStart: Int = 0
    var windowEnd: Int = p.length - 1

    //Every iteration slides the window one index up
    while (windowEnd < s.length) {
      //if the to arrays are equal, we add the index of the start of the window to list
      if (equal(arrS, arrP)) list = list :+ windowStart
      if (windowEnd + 1 < s.length) {
        //Update arrS for next window
        arrS(s.charAt(windowStart) - 'a') -= 1
        arrS(s.charAt(windowEnd + 1) - 'a') += 1
      }
      //slide the window
      windowStart += 1
      windowEnd += 1
    }
    list
  }

  def equal(s1: Seq[Int], s2: Seq[Int]): Boolean = s1 == s2

  def main(args: Array[String]): Unit = {
    println(findAnagrams("cbaebabacd", "abc"))
    println(findAnagrams("abab","ab"))
    println(findAnagrams("abab",""))
    println(findAnagrams("","ab"))
    println(findAnagrams("",""))
  }

  def printArray(arr: Array[Int]): Unit = {
    for (c <- arr) print(c)
    println("")
  }
}