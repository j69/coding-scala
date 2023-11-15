package playground.monix.eval

import monix.eval.{Coeval, Task}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.must.Matchers.{an, be}
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper

import scala.concurrent.Await
import scala.concurrent.duration.Duration

class MonixEvalPlaygroundTest extends AnyFlatSpec {
  // We need a scheduler whenever asynchronous
  // execution happens, substituting your ExecutionContext

  import monix.execution.Scheduler.Implicits.global

  it should "check Task, the Lazy Future" in {
    import monix.eval._
    import monix.execution.CancelableFuture
    val task = Task {
      1 + 1
    }

    // Tasks get evaluated only on runToFuture!
    // Callback style:
    val cancelable = task.runAsync {
      case Right(value) =>
        println(value)
      case Left(ex) =>
        System.out.println(s"ERROR: ${ex.getMessage}")
    }
    //=> 2

    // Or you can convert it into a Future
    val future: CancelableFuture[Int] =
      task.runToFuture

    // Printing the result asynchronously
    future.foreach(println)
  }

  it should "check Error Handling" in {
    import scala.util.{Failure, Success, Try}

    val source = Task.raiseError[Int](new IllegalStateException)
    // Now we can flatMap over both success and failure:
    val recoveredPre = source.flatMap(i => Task.now(i * 2))
    an[IllegalStateException] should be thrownBy Await
      .result(recoveredPre.runToFuture, Duration.Inf)

    // val source = Task.raiseError[Int](new IllegalStateException)

    val materialized: Task[Try[Int]] =
      source.materialize

    // Now we can flatMap over both success and failure:
    val recovered = materialized.flatMap {
      case Success(value) => Task.now(value)
      case Failure(_)     => Task.now(0)
    }
    Await.result(recovered.runToFuture, Duration.Inf) should be(0)
  }

  /**
   * In summary the Monix Coeval:
   * <ul>
   * <li>resembles Task, but works only for immediate, synchronous evaluation</li>
   * <li>can be a replacement for lazy val and by-name parameters</li>
   * <li>doesn’t trigger the execution, or any effects until value or run</li>
   * <li>allows for controlling of side-effects</li>
   * <li>handles errors</li>
   * </ul>
   */
  it should "check Coeval" in {
    import monix.eval.Coeval

    val coeval = Coeval {
      println("Effect!")
      "Hello!"
    }

    // Coeval has lazy behavior, so nothing
    // happens until being evaluated:
    coeval.value
    //=> Effect!
    // res1: String = Hello!

    // And we can handle errors explicitly:
    import scala.util.{Failure, Success}

    coeval.runTry match {
      case Success(value) =>
        println(value)
      case Failure(ex) =>
        System.err.println(ex)
    }
  }

  it should "check Eager, the replacement for scala.util.Try" in {
    import monix.eval.Coeval
    import monix.eval.Coeval.{Error, Now}
    val coeval1 = Coeval(1 + 1)
    coeval1.run should be(Now(2))
    val exc = new RuntimeException("Hello!")
    val coeval2 = Coeval.raiseError[Int](exc)
    coeval2.run should be(Error(exc))
  }

  it should "check Convert any Coeval into a Task" in {
    import monix.eval.Task
    val coeval = Coeval.eval(1 + 1)
    val task = coeval.to[Task]
  }

  it should "check Coeval.now" in {
    import monix.eval.Coeval

    val coeval = Coeval.now {
      println("Effect"); "Hello!"
    }
  }

  it should "check Coeval.eval" in {
    val coeval = Coeval.eval {
      println("Effect"); "Hello!"
    }
    coeval.value
    // gets triggered every time
    coeval.value
  }

  it should "check Coeval.evalOnce" in {
    val coeval = Coeval.evalOnce {
      println("Effect"); "Hello!"
    }
    coeval.value
    // Result was memoized on the first run!
    coeval.value
  }

  it should "check Coeval.defer" in {
    val coeval = Coeval.defer {
      Coeval.now {
        println("Effect"); "Hello!"
      }
    }
    coeval.value
    coeval.value
  }

  it should "check FlatMap and Tail-Recursive Loops" in {
    import scala.annotation.tailrec
    // usual
    @tailrec
    def fib(cycles: Int, a: BigInt, b: BigInt): BigInt = {
      if (cycles > 0)
        fib(cycles - 1, b, a + b)
      else
        b
    }

    fib(5, 0, 1) should be(8)

    // naive
    def fibCoeval(cycles: Int, a: BigInt, b: BigInt): Coeval[BigInt] = {
      if (cycles > 0)
        Coeval.defer(fibCoeval(cycles - 1, b, a + b))
      else
        Coeval.now(b)
    }

    fibCoeval(5, 0, 1).value() should be(8)

    // monadic
    def fibCoevalMonadic(cycles: Int, a: BigInt, b: BigInt): Coeval[BigInt] =
      Coeval.eval(cycles > 0).flatMap {
        case true =>
          fibCoevalMonadic(cycles - 1, b, a + b)
        case false =>
          Coeval.now(b)
      }

    fibCoevalMonadic(5, 0, 1).value() should be(8)

  }

  it should "check The Applicative: zip2, zip3, … zip6" in {
    val locationCoeval: Coeval[String] = Coeval.eval(???)
    val phoneCoeval: Coeval[String] = Coeval.eval(???)
    val addressCoeval: Coeval[String] = Coeval.eval(???)

    val aggregate =
      Coeval.zip3(locationCoeval, phoneCoeval, addressCoeval).map {
        case (location, phone, address) => "Gotcha!"
      }
  }

  {
    val source = Coeval.raiseError[String](new IllegalStateException)

    it should "check Recovering from Error #1" in {

      val recovered = source.onErrorHandleWith {
        case _: IllegalStateException =>
          // Oh, we know about illegal states, recover it
          Coeval.now("Recovered!")
        case other =>
          // We have no idea what happened, raise error!
          Coeval.raiseError(other)
      }

      recovered.runTry
    }

    it should "check Recovering from Error #2" in {
      val recovered = source.onErrorRecoverWith { case _: IllegalStateException =>
        // Oh, we know about illegal states, recover it
        Coeval.now("Recovered!")
      }

      recovered.runTry
    }

    it should "check Recovering from Error #3" in {
      val recovered = source.onErrorHandle {
        case _: IllegalStateException =>
          // Oh, we know about illegal states, recover it
          "Recovered!"
        case other =>
          throw other // Rethrowing
      }
    }
  }

  it should "check Restart On Error #1" in {
    import scala.util.Random

    val source = Coeval(Random.nextInt).flatMap {
      case even if even % 2 == 0 =>
        Coeval.now(even)
      case other =>
        Coeval.raiseError(new IllegalStateException(other.toString))
    }

    // Will retry 4 times for a random even number,
    // or fail if the maxRetries is reached!
    val randomEven = source.onErrorRestart(maxRetries = 4)
  }

  it should "check Restart On Error #2" in {
    import scala.util.Random

    val source = Coeval(Random.nextInt).flatMap {
      case even if even % 2 == 0 =>
        Coeval.now(even)
      case other =>
        Coeval.raiseError(new IllegalStateException(other.toString))
    }

    // Will keep retrying for as long as the source fails
    // with an IllegalStateException
    val randomEven = source.onErrorRestartIf {
      case _: IllegalStateException => true
      case _                        => false
    }
  }

  it should "check Expose Errors (materialize/dematerialize)" in {
    import scala.util.{Failure, Success, Try}

    val source: Coeval[Int] = Coeval.raiseError[Int](new IllegalStateException)
    val materialized: Coeval[Try[Int]] = source.materialize

    // Now we can flatMap over both success and failure:
    val recovered: Coeval[Int] = materialized.flatMap {
      case Success(value) => Coeval.now(value)
      case Failure(_)     => Coeval.now(0)
    }

    // Exposing errors
    val materialized2: Coeval[Try[Int]] = source.materialize

    // Hiding errors again
    val dematerialized: Coeval[Int] = materialized2.dematerialize
  }
}
