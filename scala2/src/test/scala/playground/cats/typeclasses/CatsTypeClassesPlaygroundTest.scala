package playground.cats.typeclasses

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.must.Matchers.be
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper

import java.util.Date
import scala.language.higherKinds

class CatsTypeClassesPlaygroundTest extends AnyFlatSpec {
  it should "check Alternative behaviour" in {

    import cats.Alternative
    import cats.implicits._

    val concatenated = 7.pure[Vector] <+> 8.pure[Vector]
    concatenated should be(Vector(7, 8))
    val double: Int => Int = _ * 2
    val addFive: Int => Int = _ + 5
    (double.pure[Vector] <+> addFive.pure[Vector]) ap concatenated should be(Vector(14, 16, 12, 13))

    Alternative[Vector].ap(Vector(double, addFive, double))(concatenated) should be(
      Vector(14, 16, 12, 13, 14, 16)
    )

  }
  it should "check Applicative behaviour" in {

    import cats.Applicative
    // ap
    Applicative[Option].ap[Int, String](Some(_ + ""))(Some(100)) should be(Some("100"))

    // pure
    Applicative[Option].pure((1, "a")) should be(Some((1, "a")))

    // as
    Applicative[List].as(List(1, 2, 3), 4) should be(List(4, 4, 4))

    // compose
    val appl = Applicative[List].compose[Option]
    appl.as(List(Option(1), Option(2), Option(3)), 4) should be(List(Some(4), Some(4), Some(4)))

    // mapN
    Applicative[Option].map3[String, Int, Date, String](
      Option("Name"),
      Option(123),
      Option(new Date())
    )((x, y, z) => s"$x + $y") should be(Some("Name + 123"))

    import cats.implicits._
    List(1, 23).traverse(i => Option(i)) should be(Some(List(1, 23)))
  }

  it should "check Applicative Error behaviour" in {
    import cats.ApplicativeError
    import cats.instances.either._

    def attemptDivideApplicativeError[F[_]](x: Int, y: Int)(implicit
      ae: ApplicativeError[F, String]
    ): F[Int] = {
      if (y == 0) ae.raiseError("divisor is error")
      else {
        ae.pure(x / y)
      }
    }

    attemptDivideApplicativeError(30, 10) should be(Right(3))
    attemptDivideApplicativeError(30, 0) should be(Left("divisor is error"))
  }

  it should "check Bifunctor behaviour" in {
    import cats.implicits._

    val x: (List[String], Int) = (List("foo", "bar"), 3)
    x.bimap(_.headOption, _.toLong + 1) should be((Some("foo"), 4))

    // Tuple2 is also effect with 2 types = F[_,_]
    // list of balances and want divide them by the number of months in the lifetime of the account holder
    val records: List[(Int, Int)] =
      List((450000, 3), (770000, 4), (990000, 2), (2100, 4), (43300, 3))

    // we want an average contribution per month
    def calculateContributionPerMonth(balance: Int, lifetime: Int) = balance / lifetime

    val result: List[Int] =
      records
        .map(record =>
          record.bimap(
            cents => cents / 100, // The balances are given in cents.
            years => 12 * years
          )
        )
        .map((calculateContributionPerMonth _).tupled)
    // result: List[Int] = List(125, 160, 412, 0, 12)

  }

  it should "check Eq behaviour" in {

    import cats.implicits._
    import cats.kernel.Eq

    // expression "1 === 1" cannot be used in ScalaTest
    Eq[Int].eqv(1, 1)
    // res2: Boolean = true
    "Hello" =!= "World"
    // res3: Boolean = true

    case class Foo(a: Int, b: String)
    implicit val eqFoo: Eq[Foo] = Eq.fromUniversalEquals
    // eqFoo: Eq[Foo] = cats.kernel.Eq$$anon$6@3e53e327

    // expression "Foo(10, "") === Foo(10, "")" cannot be used in ScalaTest
    Eq[Foo].eqv(Foo(10, ""), Foo(10, "")) should be(true)

    val aEq = Eq.instance[Foo]((v1, v2) => v1.a == v2.a)
    val bEq = Eq.instance[Foo]((v1, v2) => v1.b == v2.b)
    val aOrBEq = Eq.or(aEq, bEq)

    aOrBEq.eqv(Foo(10, "X"), Foo(10, "Y")) should be(true)
    aOrBEq.eqv(Foo(10, "X"), Foo(9, "X")) should be(true)
    aOrBEq.eqv(Foo(10, "X"), Foo(9, "Y")) should be(false)

  }
  it should "check Foldable behaviour" in {
    import cats._
    import cats.implicits._

    val keys = List(1, 2, 4, 5)
    Foldable[List].collectFirst(keys)({ case 1 => 11 }) should be(Some(11))
    Foldable[List].collectFirst(keys)({
      case 0 => 11
      case 6 => 66
      case 7 => 77
    }) should be(None)
    Foldable[List].combineAllOption(List("a", "b", "c")) should be(Some("abc"))
    Foldable[List].fold(List("a", "b", "c")) should be("abc")
    Foldable[List].foldA(
      List(Either.right[String, Int](1), Either.right[String, Int](2))
    ) should be(Right(3))
    Foldable[List].foldMap(List(1, 2, 4))(_.toString) should be("124")
    Foldable[List].foldK(List(List(1, 2, 3), List(2, 3, 4))) should be(List(1, 2, 3, 2, 3, 4))
    Foldable[List].reduceLeftToOption(List[Int]())(_.toString)((s, i) => s + i) should be(None)
    Foldable[List].reduceLeftToOption(List(1, 2, 3, 4))(_.toString)((s, i) => s + i) should be(
      Some("1234")
    )
    Foldable[List]
      .reduceRightToOption(List(1, 2, 3, 4))(_.toString)((i, s) => Later(s.value + i))
      .value should be(Some("4321"))
    Foldable[List]
      .reduceRightToOption(List[Int]())(_.toString)((i, s) => Later(s.value + i))
      .value should be(None)
    Foldable[List].find(List(1, 2, 3))(_ > 2) should be(Some(3))
    Foldable[List].exists(List(1, 2, 3))(_ > 2) should be(true)
    Foldable[List].forall(List(1, 2, 3))(_ > 2) should be(false)
    Foldable[List].forall(List(1, 2, 3))(_ < 4) should be(true)
    Foldable[Vector].filter_(Vector(1, 2, 3))(_ < 3) should be(List(1, 2))
    Foldable[List].isEmpty(List(1, 2)) should be(false)
    Foldable[Option].isEmpty(None) should be(true)
    Foldable[List].nonEmpty(List(1, 2)) should be(true)
    Foldable[Option].toList(Option(1)) should be(List(1))
    Foldable[Option].toList(None) should be(List())

    val list = List(1, 2, 3, 4)
    Foldable[List].partitionBifold(list)(a =>
      ("value " + a.toString, if (a % 2 == 0) -a else a)
    ) should
      be((List("value 1", "value 2", "value 3", "value 4"), List(1, -2, 3, -4)))

    /// traverse + sequence
    def parseInt(s: String): Option[Int] = scala.util.Try(Integer.parseInt(s)).toOption

    Foldable[List].traverse_(List("1", "2"))(parseInt) should be(Some(()))
    Foldable[List].traverse_(List("1", "A"))(parseInt) should be(None)
    Foldable[List].sequence_(List(Option(1), Option(2))) should be(Some(()))
    Foldable[List].sequence_(List(Option(1), None)) should be(None)

    //sliding
    Foldable[List].sliding4((1 to 8).toList) should be(
      List((1, 2, 3, 4), (2, 3, 4, 5), (3, 4, 5, 6), (4, 5, 6, 7), (5, 6, 7, 8))
    )

    import cats.instances.list._
    Foldable[List].forallM[Option, Int](List(1, 2, 3))(i =>
      if (i < 2) Some(i % 2 == 0) else None
    ) should be(Some(false))
    // res21: Option[Boolean] = Some(false)
    Foldable[List].existsM[Option, Int](List(1, 2, 3))(i =>
      if (i < 2) Some(i % 2 == 0) else None
    ) should be(None)
    // res22: Option[Boolean] = None
    Foldable[List].existsM[Option, Int](List(1, 2, 3))(i =>
      if (i < 3) Some(i % 2 == 0) else None
    ) should be(Some(true))
    // res23: Option[Boolean] = Some(true)

    Foldable[List].dropWhile_(List[Int](2, 4, 5, 6, 7))(_ % 2 == 0) should be(List(5, 6, 7))
    Foldable[List].dropWhile_(List[Int](1, 2, 4, 5, 6, 7))(_ % 2 == 0) should be(
      List(1, 2, 4, 5, 6, 7)
    )

    import cats.data.Nested
    val listOption0 = Nested(List(Option(1), Option(2), Option(3)))
    // listOption0: Nested[List, Option, Int] = Nested(
    //   List(Some(1), Some(2), Some(3))
    // )
    val listOption1 = Nested(List(Option(1), Option(2), None))
    // listOption1: Nested[List, Option, Int] = Nested(
    //   List(Some(1), Some(2), None)
    // )

    type ListOptionInt[A] = Nested[List, Option, A]
    Foldable[ListOptionInt].fold(listOption0) should be(6)
    // res27: Int = 6
    Foldable[ListOptionInt].fold(listOption1) should be(3)
    // res28: Int = 3

  }
  it should "check Functor behaviour" in {
    import cats.Functor
    import cats.implicits._

    // compose
    val listOption = List(Some(1), None, Some(2))
    val func: List[Option[Int]] => List[Option[Int]] =
      x => Functor[List].compose[Option].map(x)(_ + 1)
    func(listOption) should be(List(Some(2), None, Some(3)))
    // fproduct
    Functor[Option].fproduct(Some(100))(_ + 100) should be(Some((100, 200)))
    // ifF
    Functor[Option].ifF(Option(true))(100, None) should be(Some(100))
    Functor[Option].ifF(Option(false))(100, 0) should be(Some(0))

    // lift
    val fun = Functor[Option].lift[Int, Int](x => x + 11)
    fun(Some(100)) should be(Some(111))
    fun(None) should be(None)

    // tuple
    Functor[List].tupleLeft(List("1", "2"), "hello") should be(List(("hello", "1"), ("hello", "2")))

    // unzip
    // unzip List of tuples [A,B](a,b)
    Functor[List].unzip(List(("hello", 1))) should be((List("hello"), List(1)))
  }
  it should "check Monad behaviour" in {
    import cats.Monad
    import cats.implicits._

    Monad[List].ifM(List(true, false, true))(ifTrue = List(1, 2), ifFalse = List(3, 4)) should be(
      List(1, 2, 3, 4, 1, 2)
    )

    //    Monad[List].iterateWhile(List(Some(1), Some(2), Some(3), None, Some(4)))(el => {
    //      println(el)
    //      el.isDefined
    //    }) should be(List(1, 2, 3, 4, 1, 2))
    // Monad[List].iterateUntil(List(Some(1),Some(2),Some(3),None,Some(4)))(_.isDefined) should be(List(1, 2, 3, 4, 1, 2))

  }
  it should "check MonoidK behaviour" in {
    // Let’s compare the usage of Monoid[A] and MonoidK[F].
    import cats.implicits._
    import cats.{Monoid, MonoidK}

    Monoid[List[String]].empty should be(List())
    MonoidK[List].empty[String] should be(List())
    MonoidK[List].empty[Int] should be(List())

    Monoid[List[String]].combine(List("hello", "world"), List("goodbye", "moon")) should be(
      List("hello", "world", "goodbye", "moon")
    )
    MonoidK[List].combineK[String](List("hello", "world"), List("goodbye", "moon")) should be(
      List("hello", "world", "goodbye", "moon")
    )
    MonoidK[List].combineK[Int](List(1, 2), List(3, 4)) should be(List(1, 2, 3, 4))
  }
  it should "check Parallel behaviour" in {
    /*
    When browsing the various Monads included in Cats, you may have noticed that some of them have data types that are
     actually of the same structure, but instead have instances of Applicative. E.g. Either and Validated.
    This is because defining a Monad instance for data types like Validated would be inconsistent with its
    error-accumulating behaviour.


    In short, Monads describe dependent computations and Applicatives describe independent computations.
     */

    import cats.data._
    import cats.implicits._

    case class Name(value: String)
    case class Age(value: Int)
    case class Person(name: Name, age: Age)

    def parse(s: String): Either[NonEmptyList[String], Int] =
      if (s.matches("-?[0-9]+")) Right(s.toInt)
      else Left(NonEmptyList.one(s"$s is not a valid integer."))

    def validateAge(a: Int): Either[NonEmptyList[String], Age] =
      if (a > 18) Right(Age(a))
      else Left(NonEmptyList.one(s"$a is not old enough"))

    def validateName(n: String): Either[NonEmptyList[String], Name] =
      if (n.length >= 8) Right(Name(n))
      else Left(NonEmptyList.one(s"$n Does not have enough characters"))

    def parsePerson(ageString: String, nameString: String) =
      for {
        age <- parse(ageString)
        person <- (validateName(nameString), validateAge(age)).parMapN(Person)
      } yield person

    parsePerson("12", "BilliBob") should be(Left(NonEmptyList("12 is not old enough", List())))
    parsePerson("120", "BilliBob") should be(Right(Person(Name("BilliBob"), Age(120))))

    val parSeqLeft = List(
      Either.right(42),
      Either.left(NonEmptyList.one("Error 1")),
      Either.left(NonEmptyList.one("Error 2"))
    ).parSequence
    parSeqLeft should be(Left(NonEmptyList("Error 1", List("Error 2"))))
    parSeqLeft.left.get.toList should be(List("Error 1", "Error 2"))

    val seqLeft = List(
      Either.right(42),
      Either.left(NonEmptyList.one("Error 1")),
      Either.left(NonEmptyList.one("Error 2"))
    ).sequence
    seqLeft should be(Left(NonEmptyList("Error 1", List())))

  }
  it should "check Semigroup behaviour" in {
    import cats.Semigroup
    import cats.implicits._

    def optionCombine[A: Semigroup](a: A, opt: Option[A]): A =
      opt.map(a |+| _).getOrElse(a)

    def mergeMap[K, V: Semigroup](lhs: Map[K, V], rhs: Map[K, V]): Map[K, V] =
      lhs.foldLeft(rhs) { case (acc, (k, v)) =>
        acc.updated(k, optionCombine(v, acc.get(k)))
      }

    val ym1 = Map(1 -> List("hello"))
    val ym2 = Map(2 -> List("cats"), 1 -> List("world"))

    val y = mergeMap(ym1, ym2) should be(Map(2 -> List("cats"), 1 -> List("hello", "world")))

    Semigroup.combineN(List("hello"), 5) should be(
      List("hello", "hello", "hello", "hello", "hello")
    )

  }
  it should "check Traverse behaviour" in {
    import cats.Traverse

    Traverse[List].traverse(List(1, 2, 3))(e => Option(e + 100)) should be(
      Some(List(101, 102, 103))
    )
    Traverse[List].sequence(List(Option(1), Option(2), Option(3))) should be(Some(List(1, 2, 3)))

    val funIdentity = identity()
  }

}
