/*
Given an array nums, we call (i, j) an important reverse pair if i < j and nums[i] > 2*nums[j].

You need to return the number of important reverse pairs in the given array.

Example1:
Input: [1,3,2,3,1]
Output: 2

Example2:
Input: [2,4,3,5,1]
Output: 3

Note:
The length of the given array will not exceed 50,000.
All the numbers in the input array are in the range of 32-bit integer.
 */

object Solution {
  def reversePairs(nums: Array[Int]): Int = {

    /*Counts the number of important reverse pairs and mergesorts the array after
      This way, the subarrays [start...mid] & [mid+1...end] are always sorted and so
      it's easy to count important reverse pairs since:
      0 <= i <= mid & mid+1 <= j <= end then if arr(i) > arr(j)*2  then we know that
      arr(i+1..mid) > arr(j)*2 since arr(i...mid) is sorted.
    */
    def mergeCount(arr: Array[Int], start: Int, end: Int): Int = {
      if (start >= end) return 0
      val mid: Int = start + (end - start) / 2
      var count: Int = mergeCount(arr, start, mid) + mergeCount(arr, mid + 1, end)
      var i: Int = start
      var j: Int = mid + 1
      while (j <= end && i <= mid) {
        val aj: Long = arr(j)
        if (arr(i) > aj*2) {
          count += (mid + 1) - i
          j += 1
        }
        else i += 1
      }
      mergeSort(arr, start, mid, end)
      count
    }

    def mergeSort(arr: Array[Int], start: Int, mid: Int, end: Int) = {
      var i: Int = start
      var j: Int = mid + 1
      var index: Int = 0
      val buffer: Array[Int] = new Array((end-start)+1)
      while (index < buffer.length) {
        if (j > end) {
          buffer(index) = arr(i)
          i += 1
        }
        else if (i > mid) {
          buffer(index) = arr(j)
          j += 1
        }
        else if (arr(i) <= arr(j)) {
          buffer(index) = arr(i)
          i += 1
        }
        else {
          buffer(index) = arr(j)
          j += 1
        }
        index += 1
      }
      for (x <- start to end) {
        arr(x) = buffer(x-start)
      }
      arr
    }

    mergeCount(nums, 0, nums.length - 1)
  }

  def main(args: Array[String]): Unit = {
    val arr: Array[Int] = Array(10, 4, 1, 5, 1)
    assert(reversePairs(arr)==6)
    assert(reversePairs(Array(20,3,7,1,0,Int.MaxValue,Int.MinValue))==15)
    assert(reversePairs(Array())==0)
  }

  def printArray(arr: Array[Int]): Unit = {
    print("Array(")
    for (c <- arr) print(s"$c ")
    print(")")
    println("")
  }

  def printArray(arr: Array[Int], start: Int, end: Int): Unit = {
    print(s"Array($start, $end)")
    for(c <- start to end) {
      print(s" ${arr(c)}")
    }
    println("")
  }
}