import scala.annotation.tailrec

/*Given a linked list, swap every two adjacent nodes and return its head.
You may not modify the values in the list's nodes, only nodes itself may be changed.

Example:
Given 1->2->3->4, you should return the list as 2->1->4->3.*/

object SwapNodes {

  class ListNode(_x: Int = 0, _next: ListNode = null) {
    var next: ListNode = _next
    var x: Int = _x

    override def toString: String = s"($x,$next)"
  }

    def swapPairs(head: ListNode): ListNode = {
      @tailrec def swapper(l: ListNode, r: ListNode): Unit = {
        val after: ListNode = r.next
        r.next = l
        if (after == null || (after != null && after.next == null)) l.next = after
        else l.next = after.next
        if (after != null && after.next != null) {
          swapper(after, after.next)
        }
      }

      if (head != null && head.next != null) {
        val temp = head.next
        swapper(head, head.next)
        temp}
      else head
    }

  def main(args: Array[String]): Unit = {
    val listnode1: ListNode = new ListNode(3, new ListNode(6, new ListNode(8, new ListNode(1, null))))
    val listnode2: ListNode = new ListNode(0,null)
    val listnode3 = null
    println(swapPairs(listnode1))
    println(swapPairs(listnode2))
    println(swapPairs(listnode3))
  }
  }

