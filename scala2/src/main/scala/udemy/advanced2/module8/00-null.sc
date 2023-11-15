def tryit(thing: Null): Unit = {
  println("That worked!");
}

val refNull = null

tryit(null)
tryit(refNull)

Nil
Nil.length
List(Nil)


val listOfAny : List[Any] = List[Nothing]()
val listOfInt : List[Int] = List[Nothing]()

val u : Unit = null

def safeD (d : Int) = if (d == 0) None else 12 /d