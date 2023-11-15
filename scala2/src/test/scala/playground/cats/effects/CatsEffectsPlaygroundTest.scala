package playground.cats.effects

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.must.Matchers.be
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper

import java.io.{File, FileInputStream}

class CatsEffectsPlaygroundTest extends AnyFlatSpec {

  it should "check IO behaviour" in {
    import cats.effect.IO
    val v = IO(2 + 2)
    v.unsafeRunSync() should be(4)
  }

  it should "check Async/Sync" in {
    import cats.effect.{Async, IO, Resource, Sync}
    def inputStream[F[_]: Sync](f: File): Resource[F, FileInputStream] = ???

    val t = Async[IO]
  }

  {
    import cats.effect.Sync

    def calculationInIO[F[_]: Sync](a: Int, b: Int): F[Int] =
      Sync[F].delay(a * b)

    it should "check Async in Cats Effects" in {
      import cats.effect.IO

      implicit val catsEffects = Sync[IO]
      val res: IO[Int] = calculationInIO(2, 8)
      res.unsafeRunSync() should be(16)

    }

    it should "check Async in Monix" in {
      import monix.eval.Task
      import monix.eval.instances.CatsAsyncForTask
      import monix.execution.Scheduler.Implicits.global

      implicit val monixAsync: Sync[Task] = new CatsAsyncForTask()
      val res: Task[Int] = calculationInIO(2, 8)
      res.runSyncUnsafe() should be(16)
    }

  }
}
