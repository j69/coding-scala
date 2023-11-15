trait Food {
  val name: String
}

trait Fruit extends Food

trait Cereal extends Food

case class Apple(name: String) extends Fruit

case class Orange(name: String) extends Fruit

case class Muesli(name: String) extends Cereal

abstract class FoodBowl2 {
  type FOOD <: Food
  val food: FOOD

  def eat: String = s"Yummy ${food.name}"
}

case class AppleBowl(val food: Apple) extends FoodBowl2 {
  type FOOD = Apple
}

val appleBowl = new AppleBowl(Apple("Fiji"))

val apple1 = appleBowl.food

val apple2: Apple = appleBowl.food

val apple3: appleBowl.FOOD = appleBowl.food

val apple4: AppleBowl#FOOD = appleBowl.food

val input: appleBowl.FOOD = new Apple(null)


def consumeAppleBowl(s: appleBowl.type) = print(s)
def consumeApple(s: appleBowl.FOOD) = print(s)
def consumeSpecificApple(s: appleBowl.food.type) = print(s)

consumeApple(Apple("Fiji"))

consumeAppleBowl(appleBowl)

consumeSpecificApple(appleBowl.food)