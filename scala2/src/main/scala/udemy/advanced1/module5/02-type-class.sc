import scala.annotation.implicitNotFound

@implicitNotFound("You need to define a CompareT for ${T}")
abstract class CompareT[T] {
  def isSmaller(i1: T, i2: T): Boolean
  def isLarger(i1: T, i2: T): Boolean
}

def genInsert[T: Ordering](item: T, rest: List[T]): List[T] = {
  val cmp = implicitly[Ordering[T]]
  rest match {
    case Nil => List(item)
    case head :: _ if cmp.lt(item, head) => item :: rest
    case head :: tail => head :: genInsert(item, tail)
  }
}

def genSort[T: Ordering](xs: List[T]): List[T] = xs match {
  case Nil => Nil
  case head :: tail => genInsert(head, genSort(tail))
}

val nums = List(1, 4, 3, 2, 6, 5)

genSort(nums)


def genInsert2[T](item: T, rest: List[T])(implicit cmp: CompareT[T]): List[T] = {
  rest match {
    case Nil => List(item)
    case head :: _ if cmp.isSmaller(item, head) => item :: rest
    case head :: tail => head :: genInsert2(item, tail)
  }
}

def genSort2[T: CompareT](xs: List[T]): List[T] = xs match {
  case Nil => Nil
  case head :: tail => genInsert2(head, genSort2(tail))
}

implicit object CompareInt extends CompareT[Int] {
  override def isSmaller(i1: Int, i2: Int) = i1 < i2

  override def isLarger(i1: Int, i2: Int) = i1 > i2
}

nums
genSort2(nums)


case class Person(first: String, age: Int)

object Person {
  implicit object PersonOrdering extends Ordering[Person] {
    override def compare(x: Person, y: Person): Int = x.age - y.age
  }
}

implicit object POrdering2 extends Ordering[Person] {
  override def compare(x: Person, y: Person) = 0
}

val people = List(
  Person("Fred", 25),
  Person("Sally", 22),
  Person("George", 53)
)

genSort(people)

