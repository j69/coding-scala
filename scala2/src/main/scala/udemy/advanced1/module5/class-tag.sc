import scala.reflect._


val s = "hi"

def getClassTag[T: ClassTag](x: T): ClassTag[T] = classTag[T]
val sCT = getClassTag(s)
sCT.runtimeClass // res0: Class[_$1] = int


val intCT = getClassTag(10)
intCT.runtimeClass // res0: Class[_$1] = int


def isA[T: ClassTag](x: Any): Boolean = x match {
  case _: T => true
  case _ => false
}

isA[Int](7) // true
isA("Hello") // true??

isA[Map[String, Int]](Map("hello" -> 2)) // true
isA[Map[String, Int]](Map("hello" -> "foo")) // true :-(

import scala.reflect.runtime.universe._

case class Tagged[A](value: A)(implicit val tag: TypeTag[A])

val taggedMap1 = Tagged(Map(1 -> "one", 2 -> "two"))
val taggedMap2 = Tagged(Map(1 -> 1, 2 -> 2))
def taggedIsA[A, B](x: Tagged[Map[A, B]]): Boolean = x.tag.tpe match {
  case t if t =:= typeOf[Map[Int, String]] => true
  case _ => false
}
taggedIsA(taggedMap1) // true
taggedIsA(taggedMap2) // false