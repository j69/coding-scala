package playground.monix.reactive

import monix.eval.Task
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.must.Matchers.{be, have}
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper

class MonixReactivePlaygroundTest extends AnyFlatSpec {

  import monix.execution.Scheduler.Implicits.global
  import monix.reactive._

  import scala.concurrent.duration._

  it should "check Observable" in {
    val tick = Observable
      .interval(5.millis)
      // common filtering and mapping
      .filter(_ % 2 == 0)
      .map(_ * 2)
      // any respectable Scala type has flatMap, w00t!
      .flatMap(x => Observable.fromIterable(Seq(x, x)))
      // only take the first 5 elements, then stop
      .take(5)

    val consumer = Consumer.foldLeft[List[Long], Long](List[Long]())(_ :+ _)
    val cancelable = tick.consumeWith(consumer)
    cancelable.runSyncUnsafe(Duration.Inf) should be(List(0, 0, 4, 4, 8))
  }

  it should "check load balancing COnsumer" in {
    val sumConsumer =
      Consumer.foldLeft[Long, Long](0L)(_ + _)
    val parallelConsumer =
      Consumer.loadBalance(parallelism = 10, sumConsumer).map(_.sum)

    val resultTask = Observable
      .range(0, 10000)
      .consumeWith(parallelConsumer)
    resultTask.runSyncUnsafe(Duration.Inf) should be(49995000)
  }

  it should "Observable builders" in {

    def generateSetOfSeveralInvocations(task: Task[Long]): Set[Long] = {
      val list = for {
        _ <- 0 until 5
        v = task.runSyncUnsafe(Duration.Inf)
      } yield v
      list.toSet
    }

    val pure = Observable.pure(System.nanoTime()).firstL
    generateSetOfSeveralInvocations(pure) should have size 1

    // delay / eval
    val delay = Observable.delay(System.nanoTime()).firstL
    generateSetOfSeveralInvocations(delay) should have size 5

    val evalOnce = Observable.evalOnce(System.nanoTime()).firstL
    generateSetOfSeveralInvocations(evalOnce) should have size 1

    val fromIterable = Observable.fromIterable(List(1L, 2L, 3L)).firstL
    generateSetOfSeveralInvocations(fromIterable) should have size 1
  }

  it should "Akka Streams interoperation" in {
    import akka.actor.ActorSystem
    import akka.stream.scaladsl._
    import monix.execution.Scheduler.Implicits.global
    import monix.reactive.Observable

    implicit val system = ActorSystem("akka-streams")
    val source = Source(1 to 3)
      .map(_ * 10)
      .map(_.toString)
    val publisher = source.runWith(Sink.asPublisher[String](fanout = false))
    val observable = Observable.from(publisher)
    val task = observable.toListL
    task.runSyncUnsafe(Duration.Inf) should be(List("10", "20", "30"))
  }

}
