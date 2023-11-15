package coding.leetcode.structures.heap

import java.util.PriorityQueue
import scala.collection.immutable.HashMap
import scala.collection.mutable
import scala.math.abs

object HeapSolutions {
  // Kth Largest Element in an Array
  object Solution1 {
    def findKthLargest(nums: Array[Int], k: Int): Int = {
      val heap = mutable.PriorityQueue.from(nums)(Ordering.Int)
      (1 until k).foldLeft(heap) { (h, ndx) => h.dequeue(); h }.head
    }
  }

  object Solution2 {
    def topKFrequent(nums: Array[Int], k: Int): Array[Int] = {
      val count = nums.foldLeft(HashMap[Int, Int]()) { (agg, v) =>
        agg + (v -> (agg.getOrElse(v, 0) + 1))
      }

      object FreqOrdering extends Ordering[Int] {
        def compare(n1: Int, n2: Int): Int = count(n1) - count(n2)
      }
      val heap = mutable.PriorityQueue.empty[Int](FreqOrdering)
      count.keySet.foreach(k => heap.addOne(k))
      heap.dequeueAll.toArray.slice(0, k)
    }
  }

  object Solution3 {
    class KthLargest(k: Int, nums: Array[Int]) {

      private val heap: PriorityQueue[Integer] = new PriorityQueue()
      for (n <- nums) {
        heap.offer(new Integer(n))
        if (heap.size() > k) heap.poll()
      }

      def add(value: Int): Int = {
        heap.offer(new Integer(value))
        if (heap.size() > k) heap.poll()

        heap.peek()
      }

    }
  }

  object Solution4 {
    def lastStoneWeight(stones: Array[Int]): Int = {
      val heap = mutable.PriorityQueue.from(stones)(Ordering.Int)
      while (heap.size >= 2) {
        val x = heap.dequeue()
        val y = heap.dequeue()
        if (x != y) heap.addOne(abs(x - y))
      }
      if (heap.isEmpty) 0 else heap.dequeue()
    }
  }

  object Solution5 {
    def kWeakestRows(mat: Array[Array[Int]], k: Int): Array[Int] = {
      object NdxSoldiers extends Ordering[(Int, Int)] {
        def compare(n1: (Int, Int), n2: (Int, Int)): Int =
          if (n1._2 == n2._2) n2._1 - n1._1
          else
            n2._2 - n1._2
      }

      val ndx_soldiers_tuples = (0 until mat.length).zip(mat.map(arr => arr.sum))
      val heap = mutable.PriorityQueue.from(ndx_soldiers_tuples)(NdxSoldiers)
      heap.dequeueAll.slice(0, k).map(_._1).toArray
    }

  }
}
