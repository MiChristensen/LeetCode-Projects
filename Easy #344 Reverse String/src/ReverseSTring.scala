
/*
Write a function that reverses a string. The input string is given as an array of characters char[].

Do not allocate extra space for another array, you must do this by modifying the input array
in-place with O(1) extra memory.

You may assume all the characters consist of printable ascii characters.

Example 1:
Input: ["h","e","l","l","o"]
Output: ["o","l","l","e","h"]

Example 2:
Input: ["H","a","n","n","a","h"]
Output: ["h","a","n","n","a","H"]
 */


object ReverseString {
  def main(args: Array[String]): Unit = {
    val array: Array[Char] = Array('h','e','l','l','o')
    for(c <- array) print(c)
    println("")
    reverseString(array)
    for(c <- array) print(c)
  }
  def reverseString(s: Array[Char]): Unit = {
    val lastIndex: Int = math.max(s.length - 1,0)
    println(s"lastIndex: $lastIndex")
    def reverse(l: Int, h:Int): Unit = {
      println(s"Reversing($l, $h) = (${s(l)},${s(h)})")
      val temp: Char = s(l)
      s(l)=s(h)
      s(h) = temp
      if(l<lastIndex/2) reverse(l+1,h-1)
    }
    reverse(0,lastIndex)
  }
}
