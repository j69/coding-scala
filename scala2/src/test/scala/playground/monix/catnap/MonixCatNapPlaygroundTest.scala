package playground.monix.catnap

import org.scalatest.ParallelTestExecution
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.must.Matchers.be
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper

import scala.concurrent.Await
import scala.language.postfixOps

class MonixCatNapPlaygroundTest extends AnyFlatSpec with ParallelTestExecution {

  it should "use Circuit Breaker" in {
    import monix.catnap.CircuitBreaker
    import monix.eval._
    import monix.execution.Scheduler.Implicits.global

    import scala.concurrent.duration._

    val circuitBreaker: Task[CircuitBreaker[Task]] =
      CircuitBreaker[Task].of(
        maxFailures = 5,
        resetTimeout = 10.seconds
      )

    val problematicTask = Task {
      val nr = util.Random.nextInt()
      if (nr % 2 == 0) nr
      else
        throw new RuntimeException("dummy")
    }

    val problematicTaskWithCB = for {
      ci <- circuitBreaker
      r <- ci.protect(problematicTask)
    } yield r

    val materializedTask = problematicTaskWithCB.materialize
    val results = for {
      _ <- 0 to 10
      fut = problematicTaskWithCB.materialize.runToFuture
      res = Await.result(fut, Duration.Inf)
    } yield res

    // results.foreach(println)
    results.count(_.isSuccess) should be > 0
    results.count(_.isFailure) should be > 0
  }

}
