/*
Given an arbitrary ransom note string and another string containing letters
from all the magazines, write a function that will return true if the ransom
note can be constructed from the magazines ; otherwise, it will return false.
Each letter in the magazine string can only be used once in your ransom note.

Example 1
Input: ransomNote = "a", magazine = "b"
Output: false

Example 2:
Input: ransomNote = "aa", magazine = "ab"
Output: false

Example 3:
Input: ransomNote = "aa", magazine = "aab"
Output: true

Constraints:
You may assume that both strings contain only lowercase letters.
 */

import java.lang.Math.max
import java.lang.Math.min

object RansomNote {
  def main(args: Array[String]): Unit = {
    val mag: String = "aabbccddee"
    val ran1: String = "abedec"
    val ran2: String = "d"
    val ran3: String = ""
    val ran0: String = "aaabdce"
    println(s"ran1: ${canConstruct(ran1,mag)}")
    println(s"ran2: ${canConstruct(ran2,mag)}")
    println(s"ran3: ${canConstruct(ran3,mag)}")
    println(s"ran0: ${canConstruct(ran0,mag)}")
  }
  var magChar: Array[Char] = Array()

  /*The idea of this solution is that we have we convert string mag to a charArray magChar.
    For every letter in String ran we then remove this letter in magChar if we can. If we
    manage to remove every letter of String ran from charArray magChar, we know that every
    letter must have been contained in this charArray, and return true.
   */

  def canConstruct(ran: String, mag: String): Boolean = {
    magChar = mag.toCharArray
    for(c <- ran) {
      val temp: Boolean = remove(c)
      println(s"Magazine after removing first $c: ${magChar.mkString("")}")
      if(temp == false) return temp
    }
    true
  }

  /*This remove method doesn't remove the chars from the array, but just overwrites them with
    a '*', which is okay since we know the strings will only contain lowercase letters */
  def remove(c: Char): Boolean = {
    val index: Int = magChar.indexOf(c)
    if (index > -1) {
        magChar(index) = '*'
      true}
    else false
    }
}

