package coding.hackerrank

import scala.math.BigDecimal.double2bigDecimal

/** https://www.hackerrank.com/challenges/area-under-curves-and-volume-of-revolving-a-curv/problem */
object FP_03_CalcAreaAndVolume {

  /**
   * This function will be used while invoking "Summation" to compute
   * The area under the curve.
   *
   * @param coefficients
   * @param powers
   * @param x
   * @return
   */
  def f(coefficients: List[Int], powers: List[Int], x: Double): Double =
    coefficients.zip(powers).foldLeft(0.0) { case (acc, (c, p)) => acc + c * math.pow(x, p) }
  // (0 until coefficients.size).map(ndx => coefficients(ndx) * Math.pow(x, powers(ndx))).sum

  /**
   * This function will be used while invoking "Summation" to compute
   * The Volume of revolution of the curve around the X-Axis
   * The 'Area' referred to here is the area of the circle obtained
   * By rotating the point on the curve (x,f(x)) around the X-Axis
   */
  def area(coefficients: List[Int], powers: List[Int], x: Double): Double =
    math.Pi * math.pow(f(coefficients, powers, x), 2)

  /**
   * This is the part where the series is summed up
   * This function is invoked once with func = f to compute the area
   * under the curve
   * Then it is invoked again with func = area to compute the volume
   * of revolution of the curve
   */
  def summation(
    func: (List[Int], List[Int], Double) => Double,
    upperLimit: Int,
    lowerLimit: Int,
    coefficients: List[Int],
    powers: List[Int]
  ): Double = {
    val step = 0.001
    val subIntervals = lowerLimit.toDouble to upperLimit.toDouble by step.toDouble
    subIntervals
      .foldLeft(0.0) { case (acc, x) => acc + func(coefficients, powers, x.toDouble) * step }

  }
}
