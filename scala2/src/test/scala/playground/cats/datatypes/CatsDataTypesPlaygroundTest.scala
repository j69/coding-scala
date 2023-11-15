package playground.cats.datatypes

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.must.Matchers.be
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper

import scala.util.Try

class CatsDataTypesPlaygroundTest extends AnyFlatSpec {

  it should "check EitherT  behaviour" in {

    def parseDouble(s: String): Either[String, Double] =
      Try(s.toDouble).map(Right(_)).getOrElse(Left(s"$s is not a number"))

    def divide(a: Double, b: Double): Either[String, Double] =
      Either.cond(b != 0, a / b, "Cannot divide by zero")

    def parseDoubleOption(s: String): Option[Either[String, Double]] =
      Option(parseDouble(s))

    def divideOption(a: Double, b: Double): Option[Either[String, Double]] =
      Option(divide(a, b))

    def divisionProgramOption(inputA: String, inputB: String): Option[Either[String, Double]] =
      parseDoubleOption(inputA) flatMap { eitherA =>
        parseDoubleOption(inputB) flatMap { eitherB =>
          (eitherA, eitherB) match {
            case (Right(a), Right(b)) => divideOption(a, b)
            case (Left(err), _)       => Option(Left(err))
            case (_, Left(err))       => Option(Left(err))
          }
        }
      }

    divisionProgramOption("4", "2").get should be(Right(2.0))
    divisionProgramOption("a", "b").get should be(Left("a is not a number"))

    import cats.data.EitherT

    def divisionProgramOption2(inputA: String, inputB: String): EitherT[Option, String, Double] =
      for {
        a <- EitherT(parseDoubleOption(inputA))
        b <- EitherT(parseDoubleOption(inputB))
        result <- EitherT(divideOption(a, b))
      } yield result

    divisionProgramOption2("4", "2").value.get should be(Right(2.0))
    divisionProgramOption2("a", "b").value.get should be(Left("a is not a number"))

  }
  it should "check Eval  behaviour" in {

    import cats.Eval

    // Now: evaluated immediately
    val eager = Eval.now {
      println("Running expensive calculation...")
      1 + 2 * 3
    }
    eager.value should be(7)

    // Later: evaluated once when value is needed
    val lazyEval = Eval.later {
      println("Running expensive calculation...")
      1 + 2 * 3
    }
    lazyEval.value should be(7)
    lazyEval.value should be(7)

    // Always: evaluated every time value is needed
    val alwaysEval = Eval.always {
      println("Running expensive calculation...")
      1 + 2 * 3
    }
    alwaysEval.value should be(7)
    alwaysEval.value should be(7)

  }
  it should "check FunctionK  behaviour" in {
    val first: List[Int] => Option[Int] = l => l.headOption
    // which is
    val first2: Function1[List[Int], Option[Int]] = l => l.headOption
    // which is
    val first3: Function1[List[Int], Option[Int]] = new Function1[List[Int], Option[Int]] {
      def apply(l: List[Int]): Option[Int] = l.headOption
    }

    import cats.arrow.FunctionK
    val first4: FunctionK[List, Option] = new FunctionK[List, Option] {
      def apply[A](l: List[A]): Option[A] = l.headOption
    }
    first4(List(1)) should be(Some(1))
    first4(List(1, 2)) should be(Some(1))

    // it also works with more-than-one high-order-types
    type ErrorOr[A] = Either[String, A]
    val errorOrFirst: FunctionK[List, ErrorOr] = new FunctionK[List, ErrorOr] {
      override def apply[A](fa: List[A]): ErrorOr[A] =
        fa.headOption.toRight("ERROR: the list was empty!")
    }
    errorOrFirst(List(1, 2, 3)) should be(Right(1))
    errorOrFirst(List()) should be(Left("ERROR: the list was empty!"))
  }
  it should "check NonEmptyList  behaviour" in {
    import cats.data.NonEmptyList

    NonEmptyList.one(42) should be(NonEmptyList(42, List()))
    NonEmptyList.of(1) should be(NonEmptyList(1, List()))
    NonEmptyList.of(1, 2) should be(NonEmptyList(1, List(2)))
    NonEmptyList.of(1, 2, 3, 4) should be(NonEmptyList(1, List(2, 3, 4)))

    NonEmptyList.ofInitLast(List(), 4) should be(NonEmptyList(4, List()))
    NonEmptyList.ofInitLast(List(1, 2, 3), 4) should be(NonEmptyList(1, List(2, 3, 4)))
    NonEmptyList.fromList(List()) should be(None)
    NonEmptyList.fromList(List(1, 2, 3)) should be(Some(NonEmptyList(1, List(2, 3))))

    import cats.implicits._
    NonEmptyList.fromFoldable(List()) should be(None)
    NonEmptyList.fromFoldable(List(1, 2, 3)) should be(Some(NonEmptyList(1, List(2, 3))))
    NonEmptyList.fromFoldable(Vector(42)) should be(Some(NonEmptyList(42, List())))
    NonEmptyList.fromFoldable(Vector(42)) should be(Some(NonEmptyList(42, List())))

    // Everything that has a Foldable instance!
    NonEmptyList.fromFoldable(Either.left[String, Int]("Error")) should be(None)
    NonEmptyList.fromFoldable(Either.right[String, Int](42)) should be(
      Some(NonEmptyList(42, List()))
    )
    import cats.data.NonEmptyVector
    NonEmptyList.fromReducible(NonEmptyVector.of(1, 2, 3)) should be(NonEmptyList(1, List(2, 3)))

  }
  it should "check Nested  behaviour" in {
    val x: Option[Option[Int]] = Some(Some(123))

    x.map(_.map(_.toString)) should be(Some(Some("123")))

    import cats.data.Nested
    import cats.implicits._
    val nested: Nested[Option, Option, Int] = Nested(Some(Some(123)))

    nested.map(_.toString).value should be(Some(Some("123")))

  }
}
