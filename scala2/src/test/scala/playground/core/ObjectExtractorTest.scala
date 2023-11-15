package playground.core

import org.scalatest.flatspec.AnyFlatSpec

class ObjectExtractorTest extends AnyFlatSpec {
  it should "check Patter Extractor  behaviour" in {
    class Order(val no: Int, val description: String)

    object Order {
      def unapply(order: Order): Option[(Int, String)] =
        return Some((order.no, order.description + " (extracted)"))
    }

    val o = new Order(12, "test desc")

    val descriptionValue: String = o match {
      case Order(_, d) => d
    }

    require(descriptionValue === "test desc (extracted)")
  }

}
