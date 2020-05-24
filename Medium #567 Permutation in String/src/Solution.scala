/*
Given two strings s1 and s2, write a function to return true if s2 contains
the permutation of s1. In other words, one of the first string's permutations is
the substring of the second string.

Example 1:
Input: s1 = "ab" s2 = "eidbaooo"
Output: True

Explanation: s2 contains one permutation of s1 ("ba").

Example 2:
Input:s1= "ab" s2 = "eidboaoo"
Output: False

Note:

The input strings only contain lower case letters.
The length of both given strings is in range [1, 10,000].
 */

object Solution {

  /*We create 2 arrays arr1 and arr2 both of length 26 (amount of letters in english alphabet).
    Index 0 of arr1 corresponds to the amount of a's in string s1. Index 2 of arr2 = amount of c's in
    string s2 etc..
    We first fill arr1 and then we fill arr2 with the first (s1.length) letters of s2. We then check run
    through s2 with a window of length s1.length, continuously updating arr2 every time we slide the window.
   */

  def checkInclusion(s1: String, s2: String): Boolean = {
    val arr1: Array[Int] = new Array(26)
    val arr2: Array[Int] = new Array(26)
    if(s1.length>s2.length) {return false}
    for(c <- s1) {
       arr1(c - 'a') += 1
    }
    for(c <- 0 to s1.length-1) {
      arr2(s2.charAt(c) - 'a') += 1
    }
    var windowStart: Int = 0
    var windowEnd: Int = s1.length-1
    while(windowEnd<s2.length) {
      if(equal(arr1,arr2)) return true
      else if(windowEnd+1<s2.length) {
        arr2(s2.charAt(windowStart) - 'a') -= 1
        arr2(s2.charAt(windowEnd+1) - 'a') += 1
      }
      windowStart +=1
      windowEnd +=1
    }
    false
  }

    def equal(s1: Seq[Int], s2: Seq[Int]): Boolean = s1==s2

  def main(args: Array[String]): Unit = {
    println(checkInclusion("a","ab"))
  }

  def printArray(arr: Array[Int]): Unit = {
    for(c <- arr) print(c)
    println("")
  }
}