package coding.hackerrank

import org.scalatest.flatspec.AnyFlatSpec

class FP_01_UpdateListTest extends AnyFlatSpec {

  behavior of "FP_01_UpdateList"

  it should "updateList" in {
    val in = List.apply(2, -4, 3, -1, 23, -4, -54)
    val out = FP_01_UpdateList.f(in)

    assert(out.equals(List.apply(2, 4, 3, 1, 23, 4, 54)))

  }
}
