package playground.monix.execution

import org.scalatest.ParallelTestExecution
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.must.Matchers.be
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper

import java.util.concurrent.TimeUnit
import scala.concurrent.Future.never
import scala.concurrent.duration._
import scala.concurrent.{Future, Promise}

class MonixExecutionPlaygroundTest extends AnyFlatSpec with ParallelTestExecution {

  import monix.execution.Scheduler.{global => scheduler}

  it should "execute Runnable" in {
    import monix.execution.Scheduler.{global => scheduler}

    scheduler.execute(new Runnable {
      def run(): Unit =
        println("Hello, world!")
    })
  }

  it should "schedule with delay" in {
    import java.util.concurrent.TimeUnit

    val cancelable = scheduler.scheduleOnce(
      5,
      TimeUnit.SECONDS,
      new Runnable {
        def run(): Unit =
          println("Hello, world!")
      }
    )

    // In case we change our mind, before time's up
    cancelable.cancel()
  }

  it should "schedule with delay (scala-way)" in {
    import scala.concurrent.duration._

    val c = scheduler.scheduleOnce(5.seconds) {
      println("Hello, world!")
    }
  }

  it should "schedule periodically" in {
    val c = scheduler.scheduleWithFixedDelay(
      3,
      5,
      TimeUnit.SECONDS,
      new Runnable {
        def run(): Unit =
          println("Fixed delay task")
      }
    )

    // If we change our mind and want to cancel
    c.cancel()
  }

  it should "check canceled status" in {
    import monix.execution.cancelables._

    // Building an instance that's already canceled
    val c = BooleanCancelable.alreadyCanceled

    // Doesn't do anything
    c.cancel()

    // Always returns true
    c.isCanceled
  }

  it should "cancel in batches" in {
    import monix.execution.Cancelable
    import monix.execution.cancelables.CompositeCancelable

    val c = CompositeCancelable()

    c += Cancelable(() => println("Canceled #1"))
    c += Cancelable(() => println("Canceled #2"))
    c += Cancelable(() => println("Canceled #3"))

    // Cancelling will trigger all 3 of them
    c.cancel()

    // Appending a new cancelable to it after cancel
    // will trigger its cancelation immediately
    c += Cancelable(() => println("Canceled #4"))
    // => Canceled #4
  }

  it should "cancel in order" in {
    import monix.execution._
    import monix.execution.cancelables.OrderedCancelable

    import concurrent.duration._
    def delayedExecution(cb: () => Cancelable)(implicit s: Scheduler): Cancelable = {
      val ref = OrderedCancelable()
      val delay = s.scheduleOnce(5.seconds) {
        ref.orderedUpdate(cb(), 2)
      }

      // This should be the first update, but
      // if not, then it is ignored!
      ref.orderedUpdate(delay, 1)
      ref
    }
  }
  // Now we'll need a Scheduler for delaying stuff
  import monix.execution.Scheduler.Implicits.global

  // We could use the functions defined on the object
  import monix.execution.FutureUtils

  // Or the extension methods exposed
  import monix.execution.FutureUtils.extensions._

  it should "timeout slow Futures " in {

    // Creating a never ending Future
    val p = Promise[Unit]()
    val never = p.future

    // Creates a new Future that has a race condition
    // with an error signaling a `TimeoutException`
    // if the source doesn't complete in time
    never.timeout(3.seconds)

    // Or as a simple function call
    FutureUtils.timeout(never, 3.seconds)
  }

  it should "timeout and fallback to another Futures" in {
    import scala.concurrent.TimeoutException

    // After 3 seconds of inactivity, discards the
    // source and fallbacks to the backup
    never.timeoutTo(3.seconds, Future.failed(new TimeoutException))

    // Or as a simple function call
    FutureUtils.timeoutTo(never, 3.seconds, Future.failed(new TimeoutException))
  }

  it should "materialize/dematerialize" in {
    import scala.util.Try

    // materialize
    {
      val f: Future[Int] = Future(1)
      // Expose errors
      val ft: Future[Try[Int]] = f.materialize
      // Or as a simple function call
      FutureUtils.materialize(f)

    }

    // dematerialize
    {
      val ft: Future[Try[Int]] = Future(1).materialize
      // Hide exposed errors
      val f: Future[Int] = ft.dematerialize
      // Or as a simple function call
      FutureUtils.dematerialize(ft)
    }
  }

  import monix.execution.atomic._
  it should "check Atomics" in {
    val refInt1: Atomic[Int] = Atomic(0)
    val refInt2: AtomicInt = Atomic(0)

    val refLong1: Atomic[Long] = Atomic(0L)
    val refLong2: AtomicLong = Atomic(0L)

    val refString1: Atomic[String] = Atomic("hello")
    val refString2: AtomicAny[String] = Atomic("hello")
  }

  it should "check Atomics Numbers" in {
    val ref = Atomic(BigInt(1))
    // now we can increment a BigInt
    ref.incrementAndGet() should be(BigInt("2"))
    // or adding to it another value
    ref.addAndGet(BigInt("329084291234234")) should be(BigInt("329084291234236"))
  }

  it should "check Atomics Number constrains" in {
    val string = Atomic("hello")
    assertDoesNotCompile("string.incrementAndGet()")
  }

  it should "use AsyncQueue" in {
    import monix.execution.BufferCapacity.Bounded
    import monix.execution.ChannelType.MPSC
    import monix.execution.{AsyncQueue, CancelableFuture}

    val queue = AsyncQueue.withConfig[Int](
      capacity = Bounded(64),
      channelType = MPSC
    )

    def producer(n: Int): CancelableFuture[Unit] =
      queue.offer(n).flatMap { _ =>
        if (n >= 0) producer(n - 1)
        else CancelableFuture.unit
      }

    def consumer(index: Int): CancelableFuture[Unit] =
      queue.poll().map { a =>
        println(s"Worker $$index: $$a")
      }
  }

}
