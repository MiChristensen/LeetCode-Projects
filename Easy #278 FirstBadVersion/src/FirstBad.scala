object FirstBad {

  def main(args: Array[String]): Unit = {
    println(firstBadVersion(2126753390))
  }

  def firstBadVersion(n: Int): Int = {
    def find(l: Int, h: Int): Int = {
      if (l == h) return l
      val mid: Int = l + (h - l) / 2
      if (isBadVersion(mid)) find(l, mid)
      else find(mid+1, h)
    }
    find(1, n)
  }

  def isBadVersion(n: Int): Boolean = {
    if (n >= 12301230) true else false
  }

}
