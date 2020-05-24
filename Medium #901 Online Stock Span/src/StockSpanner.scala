/*
Write a class StockSpanner which collects daily price quotes for some stock,
 and returns the span of that stock's price for the current day.
The span of the stock's price today is defined as the maximum number of consecutive days
(starting from today and going backwards) for which the price of the stock was less than or
equal to today's price.

For example, if the price of a stock over the next 7 days were [100, 80, 60, 70, 60, 75, 85],
then the stock spans would be [1, 1, 1, 2, 1, 4, 6].

Example 1:
Input: ["StockSpanner","next","next","next","next","next","next","next"], [[],[100],[80],[60],[70],[60],[75],[85]]
Output: [null,1,1,1,2,1,4,6]

Explanation:
First, S = StockSpanner() is initialized.  Then:
S.next(100) is called and returns 1,
S.next(80) is called and returns 1,
S.next(60) is called and returns 1,
S.next(70) is called and returns 2,
S.next(60) is called and returns 1,
S.next(75) is called and returns 4,
S.next(85) is called and returns 6.

Note that (for example) S.next(75) returned 4, because the last 4 prices
(including today's price of 75) were less than or equal to today's price.

Note:
Calls to StockSpanner.next(int price) will have 1 <= price <= 10^5.
There will be at most 10000 calls to StockSpanner.next per test case.
There will be at most 150000 calls to StockSpanner.next across all test cases.
The total time limit for this problem has been reduced by 75% for C++, and 50% for all other languages.
 */

class StockSpanner() {
  var list: List[(Int,Int)] = List()
  def next(price: Int): Int = {
    var res: Int = 1
    var i: Int = list.length-1
    var dropCount: Int = 0
    while(i>=0 && list(i)._1<=price) {
      res += list(i)._2
      dropCount += 1
      i -= 1
    }
    list = list.dropRight(dropCount)
    list = list :+ (price,res)
    println(s"list after: $list")
    res
  }
}

object Solution {
  def main(args: Array[String]): Unit = {
    val s = new StockSpanner()
    println(s.next(100))
    println(s.next(80))
    println(s.next(60))
    println(s.next(70))
    println(s.next(60))
    println(s.next(75))
    println(s.next(85))
    println(s.next(-10))
    println(s.next(Int.MaxValue))
    println(s.next(Int.MinValue))
  }
}


