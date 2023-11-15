package coding.hackerrank

object FP_02_CalcE {
  def f(in: Double): Double =
    (2 to 9).foldLeft(calcE(in, 0, 1))((ePrev, step) => calcE(in, ePrev, step))

  // expect from step 2 only
  def calcE(num: Double, ePrev: Double, step: Int): Double = {
    //		println(f"num: $num%f, ePrev: $ePrev%f, step: $step%f")
    return step match {
      case 1 => 1 + num
      case _ => ePrev + Math.pow(num, step) / (1 to step).product
    }
  }
}
