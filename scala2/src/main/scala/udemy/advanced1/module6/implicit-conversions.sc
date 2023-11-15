trait Food {
  def name: String
}

case class Fruit(name: String) extends Food

case class Cereal(name: String) extends Food

case class Meat(name: String) extends Food

trait Eater {
  def name: String
}

case class Vegan(name: String) extends Eater

case class Vegetarian(name: String) extends Eater

case class Paleo(name: String) extends Eater

import scala.annotation.implicitNotFound

@implicitNotFound(msg = "Illegal Feeding: No Eats rule from ${EATER} to ${FOOD}")
trait Eats[EATER <: Eater, FOOD <: Food] {
  def feed(food: FOOD, eater: EATER) = s"${eater.name} eats ${food.name}"
}


def feedTo[FOOD <: Food, EATER <: Eater](food: FOOD, eater: EATER)
                                        (implicit ev: Eats[EATER, FOOD]) = {
  ev.feed(food, eater)
}

val apple = Fruit("Apple")
val alpen = Cereal("Alpen")
val beef = Meat("Beef")
val alice = Vegan("Alice")
val bob = Vegetarian("Bob")
val charlie = Paleo("Charlie")

object VeganRules {
  implicit object veganEatsFruit extends Eats[Vegan, Fruit]
}

import VeganRules._

feedTo(apple, alice)


abstract class X[From, To] extends (From => To) {

}


class XImpl extends X[Int, String] {
  override def apply(v1: Int) = s"v1"
}

val v = new XImpl
v.apply(123)