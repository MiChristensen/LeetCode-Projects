import scala.collection.mutable

/*
Given a binary search tree, write a function kthSmallest to find the kth smallest element in it.

Example 1:
Input: root = [3,1,4,null,2], k = 1
   3
  / \
 1   4
  \
   2
Output: 1

Example 2:
Input: root = [5,3,6,2,4,null,null,1], k = 3
       5
      / \
     3   6
    / \
   2   4
  /
 1
Output: 3

Follow up:
What if the BST is modified (insert/delete operations) often and you need to find
the kth smallest frequently? How would you optimize the kthSmallest routine?

Constraints:

The number of elements of the BST is between 1 to 10^4.
You may assume k is always valid, 1 ≤ k ≤ BST's total elements.
*/

/* This solution is really simple. First we add all values of the TreeNodes in the BST to a min heap
   by the recursive method AddAllToQueue. We then dequeue k-1 elements from the min heap and return the
   head which is then the kth smallest value.
 */

object Solution {
  class TreeNode(_value: Int = 0, _left: TreeNode = null, _right: TreeNode = null) {
    var value: Int = _value
    var left: TreeNode = _left
    var right: TreeNode = _right
  }

  def kthSmallest(root: TreeNode, k: Int): Int = {
    val queue: mutable.PriorityQueue[Int] = new mutable.PriorityQueue[Int]()(Ordering.by[Int,Int]((x) => -x))
    addAllToQueue(root,queue)
    var counter: Int = 1
    while(counter < k) {
      queue.dequeue
      counter += 1
    }
    queue.head
  }

  //Adds the value of 'node' and all values of the children of 'node' to the min heap 'queue'
  def addAllToQueue(node: TreeNode, queue: mutable.PriorityQueue[Int]): Unit = {
    queue.addOne(node.value)
    if(node.left != null) addAllToQueue(node.left,queue)
    if(node.right != null) addAllToQueue(node.right,queue)
  }

  def main(args: Array[String]): Unit = {
    val tree1: TreeNode = new TreeNode(5, new TreeNode(3, new TreeNode(2, new TreeNode(1,
      null,null),null), new TreeNode(4,null,null)),
      new TreeNode(6,null,null))
    assert(kthSmallest(tree1,3)==3)
  }
}
