package coding.hackerrank

import org.scalatest.flatspec.AnyFlatSpec

class FP_02_CalcETest extends AnyFlatSpec {

  behavior of "FP_02_CalcE"

  ignore should "calcE" in {
    val in = List.apply(20.0000, 5.0000, 0.5000, -0.5000)

    assertResult(List.apply(2423600.1887, 143.6895, 1.6487, 0.6065))(in.map(FP_02_CalcE.f(_)))
  }

  it should "calcE in 1 step" in {
    assertResult(11.0)(FP_02_CalcE.calcE(10, 0, 1))
  }

  it should "calcE in 2 step" in {
    assertResult(61.0)(FP_02_CalcE.calcE(10, 11, 2))
  }
}
