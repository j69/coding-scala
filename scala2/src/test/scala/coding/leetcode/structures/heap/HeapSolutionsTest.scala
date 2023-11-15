package coding.leetcode.structures.heap

import coding.leetcode.structures.heap.HeapSolutions.Solution3
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.must.Matchers.be
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper

class HeapSolutionsTest extends AnyFlatSpec {

  behavior of "Heap Solutions"

  it should "find Kth Largest Element in an Array" in {
    HeapSolutions.Solution1.findKthLargest(Array(3, 2, 1, 5, 6, 4), 2) should be(5)

    HeapSolutions.Solution1.findKthLargest(Array(3, 2, 3, 1, 2, 4, 5, 5, 6), 4) should be(4)
  }

  it should "find Top K Frequent Elements" in {
    HeapSolutions.Solution2.topKFrequent(Array(1, 1, 1, 2, 2, 3), 2) should be(Array(1, 2))
    HeapSolutions.Solution2.topKFrequent(Array(1), 1) should be(Array(1))
    HeapSolutions.Solution2.topKFrequent(Array(), 0) should be(Array())
  }

  it should "find Kth Largest Element in a Stream" in {
    val obj = new Solution3.KthLargest(3, Array(4, 5, 8, 2))
    obj.add(3) should be(4)
    obj.add(5) should be(5)
    obj.add(10) should be(5)
    obj.add(9) should be(8)
    obj.add(4) should be(8)
  }

  it should "find Last Stone Weight" in {
    HeapSolutions.Solution4.lastStoneWeight(Array(2, 7, 4, 1, 8, 1)) should be(1)
  }

  it should "find The K Weakest Rows in a Matrix" in {
    val input = Array(
      Array(1, 1, 0, 0, 0),
      Array(1, 1, 1, 1, 0),
      Array(1, 0, 0, 0, 0),
      Array(1, 1, 0, 0, 0),
      Array(1, 1, 1, 1, 1)
    )
    HeapSolutions.Solution5.kWeakestRows(input, 3) should be(Array(2, 0, 3))
  }
}
