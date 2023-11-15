package playground.zio

import org.scalatest.flatspec.AnyFlatSpec
import zio._
import zio.console.Console

class ZioDataTypesPlaygroundTest extends AnyFlatSpec {

  //<editor-fold desc="Core Data Types">
  {
    behavior of "ZIO-core-data-types: ZIO"
    it should "create from Synchronous code" in {
      import scala.io.StdIn

      val getStrLine: Task[String] = ZIO.effect(StdIn.readLine())

      def putStrLine(line: String): UIO[Unit] = ZIO.effectTotal(println(line))
      val effectTotalTask: UIO[Long] = ZIO.effectTotal(System.nanoTime())

      // consider this when use blocking code
      // it will be executed on a separate thread pool designed specifically for blocking effects.
      import zio.blocking._
      val sleeping: RIO[Blocking, Unit] = effectBlocking(Thread.sleep(Long.MaxValue))

      import zio.UIO

      import java.net.ServerSocket
      // Some blocking side-effects can only be interrupted by invoking a cancellation effect.
      def accept(l: ServerSocket) =
        effectBlockingCancelable(l.accept())(UIO.effectTotal(l.close()))
    }
    it should "do mapping" in {
      import zio.{IO, UIO}

      val mappedValue: UIO[Int] = IO.succeed(21).map(_ * 2)

      val mappedError: IO[Exception, String] =
        IO.fail("No no!").mapError(msg => new Exception(msg))

      val task: RIO[Any, Int] = ZIO.succeed("hello").mapEffect(_.toInt)
    }

    it should "do chaining" in {

      val chainedActionsValue: UIO[List[Int]] = IO.succeed(List(1, 2, 3)).flatMap { list =>
        IO.succeed(list.map(_ + 1))
      }

      val chainedActionsValue2: UIO[List[Int]] = for {
        list <- IO.succeed(List(1, 2, 3))
        res <- IO.succeed(list.map(_ + 1))
      } yield res
    }
  }
  //</editor-fold>
  //<editor-fold desc="Contextual Types">
  {
    behavior of "ZIO-contextual-types: ZIO Environment"
    it should "be provided to make runnable" in {
      import zio.console.{putStrLn, Console}
      val effect: ZIO[Console, Nothing, Unit] = putStrLn("Hello, World!").orDie
      val mainApp: ZIO[Any, Nothing, Unit] = effect.provideLayer(Console.live)
      val toMainMethod: URIO[ZEnv, ExitCode] = mainApp.exitCode
    }

    it should "be composable" in {

      type UserPrefLayer = Has[UserPref]
      type UserDataLayer = Has[UserData]
      type UserLayer = UserPrefLayer with UserDataLayer

      trait UserPref {
        def isAdmin: Task[Boolean]
      }
      trait UserData {
        def allAccounts: Task[List[String]]

        def ownAccount: Task[List[String]]
      }

      object UserServicesModule {
        val userPrefLive: ULayer[UserPrefLayer] = ZLayer.succeed(new UserPref {
          override def isAdmin: Task[Boolean] = Task.succeed(true)
        })
        val userDataLive: ULayer[UserDataLayer] = ZLayer.succeed(new UserData {
          override def allAccounts: Task[List[String]] = ???

          override def ownAccount: Task[List[String]] = ???
        })
      }

      // #4 Accessor Methods
      object UserPref {
        def isAdmin: ZIO[UserPrefLayer, Throwable, Boolean] = ZIO.serviceWith(_.isAdmin)
      }

      val app: ZIO[UserPrefLayer, Throwable, Boolean] = UserPref.isAdmin

      val allLayers: ULayer[UserLayer] =
        UserServicesModule.userPrefLive ++ UserServicesModule.userDataLive

      zio.Runtime.default.unsafeRun(
        app.provideLayer(allLayers)
      )
    }

    it should "define Module Patter #2" in {
      // #1 Service Definition
      trait Logging {
        def log(line: String): UIO[Unit]
      }
      // #2 Define Service Dependencies + Service Implementation
      import zio.clock.Clock
      import zio.console.Console
      case class LoggingLive(console: Console.Service, clock: Clock.Service) extends Logging {
        override def log(line: String): UIO[Unit] =
          for {
            current <- clock.currentDateTime.orDie
            _ <- console.putStrLn(current.toString + "--" + line).orDie
          } yield ()
      }
      // #3 Defining ZLayer
      object LoggingLive {
        val layer: URLayer[Has[Console.Service] with Has[Clock.Service], Has[Logging]] =
          (LoggingLive(_, _)).toLayer
      }

      // #4 Accessor Methods
      object Logging {
        def log(line: String): URIO[Has[Logging], Unit] = ZIO.serviceWith[Logging](_.log(line))
      }

      // Use it
      val app = Logging.log("Application Started")

      zio.Runtime.default.unsafeRun(
        app.provideLayer(LoggingLive.layer)
      )
    }

    behavior of "ZIO-contextual-types: ZLayer"
    it should "create" in {
      import java.io.{Closeable, FileInputStream}
      import scala.io.BufferedSource

      // basic
      val nameLayer: ULayer[Has[String]] = ZLayer.succeed("Adam")
      // from managed resource
      val managedFile: ZManaged[Any, Throwable, BufferedSource] = ZManaged.fromAutoCloseable(
        ZIO.effect(scala.io.Source.fromFile("file.txt"))
      )
      val fileLayer1: ZLayer[Any, Throwable, Has[BufferedSource]] = ZLayer.fromManaged(managedFile)
      val fileLayer2: ZLayer[Any, Throwable, Has[BufferedSource]] = managedFile.toLayer
      // more direct way
      def acquire = ZIO.effect(new FileInputStream("file.txt"))
      def release(resource: Closeable) = ZIO.effectTotal(resource.close())
      val inputStreamLayer = ZLayer.fromAcquireRelease(acquire)(release)

      // from effect
      val layer = ZLayer.fromEffect(ZIO.succeed("Hello, World!"))

      // from another service
      object logging {
        type LoggingService = Has[Logging.Service]

        object Logging {
          trait Service {
            def log(msg: String): UIO[Unit]
          }

          val live: ZLayer[Console, Nothing, LoggingService] = ZLayer.fromService(console =>
            new Service {
              override def log(msg: String): UIO[Unit] = console.putStrLn(msg).orDie
            }
          )
        }
      }
      val loggingLive: ZLayer[Console, Nothing, logging.LoggingService] = logging.Logging.live
    }
  }
  //</editor-fold>
}
