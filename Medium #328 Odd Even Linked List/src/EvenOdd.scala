/*
Given a singly linked list, group all odd nodes together followed by the even nodes.
Please note here we are talking about the node number and not the value in the nodes.

You should try to do it in place. The program should run in O(1) space complexity and O(nodes) time complexity.

Example 1:
Input: 1->2->3->4->5->NULL
Output: 1->3->5->2->4->NULL

Example 2:
Input: 2->1->3->5->6->4->7->NULL
Output: 2->3->6->7->1->5->4->NULL

Note:
The relative order inside both the even and odd groups should remain as it was in the input.
The first node is considered odd, the second node even and so on ...
 */






object Solution {

  class ListNode(_x: Int = 0, _next: ListNode = null) {
    var next: ListNode = _next
    var x: Int = _x
    override def toString: String = s"${_x}, ${next}"
  }

  def main(args: Array[String]): Unit = {
    val list: ListNode = new ListNode(1, new ListNode(2, new ListNode(3,new ListNode(4,null))))
    println(list)
    println(oddEvenList(list))
  }

  def oddEvenList(head: ListNode): ListNode = {
    var isOdd: Boolean = true
    val firstEven: ListNode = head.next
    var current: ListNode = head
    var lastOdd: ListNode = null
    while(current.next!=null) {
      println(s"current: $current")
      val nextt: ListNode = current.next
      println(s"next before: ${current.next}")
      current.next = current.next.next
      println(s"next after: ${current.next}")
      if(nextt.next == null && !isOdd) lastOdd = nextt
      if(isOdd) {lastOdd = current; isOdd = false} else isOdd = true
      current = nextt
    }
    println(s"lastOdd: $lastOdd")
    println(s"firstEven: $firstEven")
    lastOdd.next = firstEven
    head
  }
}
