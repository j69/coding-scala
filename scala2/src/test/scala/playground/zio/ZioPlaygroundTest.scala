package playground.zio

import org.scalatest.flatspec.AnyFlatSpec
import zio._
import zio.clock.Clock

import java.io.IOException

class ZioPlaygroundTest extends AnyFlatSpec {
  behavior of "ZIO-basic"

  it should "Create Effects" in {
    val zio = ZIO.succeed(42)
    // U (unthrowable)
    val uio = UIO.succeed(42) // ZIO[Any, Nothing, A],
    val urio = URIO.succeed(42) // ZIO[R, Nothing, A]
    // task = feature
    val task = Task.succeed(42) // ZIO[Any, Throwable, A]

    // Cats Effects like
    val rio = RIO.succeed(42) // ZIO[R, Throwable, A]
    val io = IO.succeed(42) // ZIO[Any, E, A]
    // from Scala types
    val zoption: IO[Option[Nothing], Int] = ZIO.fromOption(Some(2))
    val zeither: IO[Nothing, String] = ZIO.fromEither(Right("Success!"))
    import scala.util.Try
    val ztry: Task[Int] = ZIO.fromTry(Try(42 / 0))
    val zfun: URIO[Int, Int] = ZIO.fromFunction((i: Int) => i * i)
  }

  it should "Create Effects from From Side-Effects with Synchronous Side-Effects" in {
    import scala.io.StdIn

    val getStrLn: Task[String] = ZIO.effect(StdIn.readLine())

    def putStrLn(line: String): UIO[Unit] = ZIO.effectTotal(println(line))
    import java.io.IOException

    // If you wish to refine the error type of an effect (by treating other errors as fatal), then you can use the ZIO#refineToOrDie method
    val getStrLn2: IO[IOException, String] = ZIO.effect(StdIn.readLine()).refineToOrDie[IOException]
  }

  it should "Create Effects from From Side-Effects with Asynchronous Side-Effects" in {
    case class User()
    case class AuthError()
    object legacy {
      def login(onSuccess: User => Unit, onFailure: AuthError => Unit): Unit = ???
    }

    val login: IO[AuthError, User] = IO.effectAsync[AuthError, User] { callback =>
      legacy.login(
        user => callback(IO.succeed(user)),
        err => callback(IO.fail(err))
      )
    }
  }

  it should "Create Effects from From Side-Effects with Blocking Synchronous Side-Effects" in {
    import zio.blocking._

    val sleeping = effectBlocking(Thread.sleep(Long.MaxValue))
    // Some blocking side-effects can only be interrupted by invoking a cancellation effect.
    // You can convert these side-effects using the effectBlockingCancelable method

    import zio.UIO

    import java.net.ServerSocket
    def accept(l: ServerSocket) = effectBlockingCancelable(l.accept())(UIO.effectTotal(l.close()))

    import scala.io.{Codec, Source}

    def download(url: String) =
      Task.effect {
        Source.fromURL(url)(Codec.UTF8).mkString
      }

    // conversion of existing ZIO to blocking
    def safeDownload(url: String) =
      blocking(download(url))
  }

  it should "combine effects" in {
    val zipped: UIO[(String, Int)] = ZIO.succeed("4").zip(ZIO.succeed(2))
  }

  behavior of "ZIO-error-handling"

  it should "handle errors" in {
    import java.io.{FileNotFoundException, IOException}

    // combine Output to Either
    val zeither: UIO[Either[String, Int]] = IO.fail("Uh oh!").either
    // reverse operation
    val prev: ZIO[Any, String, Int] = ZIO.absolve(zeither)

    def openFile(name: String): IO[IOException, Array[Byte]] = ZIO.succeed(new Array[Byte](1))

    // catch all
    val z: IO[IOException, Array[Byte]] =
      openFile("primary.json").catchAll(_ => openFile("backup.json"))
    // catch some only all
    val data: IO[IOException, Array[Byte]] = openFile("primary.data").catchSome {
      case _: FileNotFoundException =>
        openFile("backup.data")
    }
    // fallback
    val primaryOrBackupData: IO[IOException, Array[Byte]] =
      openFile("primary.data").orElse(openFile("backup.data"))

    // fold
    lazy val DefaultData: Array[Byte] = Array(0, 0)
    // #1
    val primaryOrDefaultData: UIO[Array[Byte]] =
      openFile("primary.data").fold(_ => DefaultData, data => data)
    // #2
    val primaryOrSecondaryData: IO[IOException, Array[Byte]] =
      openFile("primary.data").foldM(_ => openFile("secondary.data"), data => ZIO.succeed(data))

    // retry
    import zio.clock._
    // #1
    val retriedOpenFile: ZIO[Clock, IOException, Array[Byte]] =
      openFile("primary.data").retry(Schedule.recurs(5))
    // #2
    openFile("primary.data").retryOrElse(
      Schedule.recurs(5),
      (_, z: Long) => ZIO.succeed(DefaultData)
    )
  }
  //<editor-fold desc="resource-handling">
  {
    behavior of "ZIO-resource-handling"

    def openFile(name: String): IO[IOException, Array[Byte]] = ZIO.succeed(new Array[Byte](1))

    it should "finalize" in {
      val finalizer =
        UIO.effectTotal(println("Finalizing!"))

      val finalized: IO[String, Unit] =
        IO.fail("Failed!").ensuring(finalizer)

    }

    it should "use Bracket" in {

      def closeFile(name: String)(smt: Array[Byte]): UIO[Any] = URIO.succeed(())

      def decodeData(data: Array[Byte]): Task[Array[Byte]] = ???

      def groupData(data: Array[Byte]): Task[Array[Byte]] = ???

      val fileName = "data.json"
      val groupedFileData =
        openFile(fileName).bracket(
          closeFile(fileName),
          file =>
            for {
              data <- decodeData(file)
              grouped <- groupData(data)
            } yield grouped
        )

    }
  }
  //</editor-fold>

  //<editor-fold desc="Concurrency">
  {
    behavior of "ZIO-concurrency"
    // start it in fact
    it should "Fork Effects" in {
      def fib(n: Long): UIO[Long] = UIO {
        if (n <= 1) UIO.succeed(n)
        else fib(n - 1).zipWith(fib(n - 2))(_ + _)
      }.flatten

      val fib100Fiber: UIO[Fiber[Nothing, Long]] =
        for {
          fiber <- fib(100).fork
        } yield fiber

    }

    it should "Join" in {
      val msg: ZIO[Any, Nothing, String] = for {
        fiber <- IO.succeed("Hi!").fork
        message <- fiber.join
      } yield message

    }

    it should "Awaiting Fibers" in {
      val res: ZIO[Any, Nothing, Exit[Nothing, String]] = for {
        fiber <- IO.succeed("Hi!").fork
        exit <- fiber.await
      } yield exit

    }
    it should "Interrupting Fibers" in {
      val res = for {
        fiber <- IO.succeed("Hi!").forever.fork
        exit <- fiber.interrupt
      } yield exit

    }

    it should "Composing Fibers" in {
      val res: ZIO[Any, Nothing, (String, String)] = for {
        fiber1 <- IO.succeed("Hi!").fork
        fiber2 <- IO.succeed("Bye!").fork
        fiber = fiber1.zip(fiber2)
        tuple <- fiber.join
      } yield tuple
    }

    it should "Timeout" in {
      import zio.duration._

      val res: ZIO[Any with Clock, Nothing, Option[String]] =
        IO.succeed("Hello").timeout(10.seconds)
    }
  }
  //</editor-fold>

  //<editor-fold desc="Testing">
  {

    behavior of "ZIO-testing"
    it should "use Environments #1" in {
      final case class Config(server: String, port: Int)

      val configString: URIO[Config, String] =
        for {
          server <- ZIO.access[Config](_.server)
          port <- ZIO.access[Config](_.port)
        } yield s"Server: $server, port: $port"
    }

    it should "use Environments #2 (environmental effect)" in {
      trait DatabaseOps {
        def getTableNames: Task[List[String]]

        def getColumnNames(table: String): Task[List[String]]
      }

      val tablesAndColumns: ZIO[DatabaseOps, Throwable, (List[String], List[String])] =
        for {
          tables <- ZIO.accessM[DatabaseOps](_.getTableNames)
          columns <- ZIO.accessM[DatabaseOps](_.getColumnNames("user_table"))
        } yield (tables, columns)
    }

    it should "provide an Environment" in {
      val square: URIO[Int, Int] =
        for {
          env <- ZIO.environment[Int]
        } yield env * env

      val result: UIO[Int] = square.provide(42)
    }

    it should "test with the help of Environmental Effects" in {

      case class UserProfile()
      case class UserID(id: String)

      // #1 Define the Service (inside module Database)
      object Database {
        trait Service {
          def lookup(id: UserID): Task[UserProfile]

          def update(id: UserID, profile: UserProfile): Task[Unit]
        }
      }
      trait Database {
        def database: Database.Service
      }

      // #2 Provide Helpers
      object db {
        def lookup(id: UserID): RIO[Database, UserProfile] =
          ZIO.accessM(_.database.lookup(id))

        def update(id: UserID, profile: UserProfile): RIO[Database, Unit] =
          ZIO.accessM(_.database.update(id, profile))
      }

      // #3 Use the Service
      def lookedupProfile(userId: UserID): RIO[Database, UserProfile] =
        for {
          profile <- db.lookup(userId)
        } yield profile

      // #4 Implement Live Service
      trait DatabaseLive extends Database {
        def database: Database.Service =
          new Database.Service {
            def lookup(id: UserID): Task[UserProfile] = ???

            def update(id: UserID, profile: UserProfile): Task[Unit] = ???
          }
      }
      object DatabaseLive extends DatabaseLive

      // #5 Run the Database Effect
      def main(userID: UserID): RIO[Database, UserProfile] = lookedupProfile(userID)

      def main2(userID: UserID): Task[UserProfile] =
        main(userID).provide(DatabaseLive)

      // #6 Implement Test Service
      class TestService extends Database.Service {

        private var map: Map[UserID, UserProfile] = Map()

        def setTestData(map0: Map[UserID, UserProfile]): Task[Unit] = Task {
          map = map0
        }

        def getTestData: Task[Map[UserID, UserProfile]] = Task(map)

        override def lookup(id: UserID): Task[UserProfile] = Task(map(id))

        override def update(id: UserID, profile: UserProfile): Task[Unit] = Task.effect {
          map = map + (id -> profile)
        }
      }
      trait TestDatabase extends Database {
        val database: TestService = new TestService
      }
      object TestDatabase extends TestDatabase

      // #7 (final step) Test Database Code
      def code(userID: UserID): RIO[Database, UserProfile] = lookedupProfile(userID)

      def code2(userID: UserID): Task[UserProfile] =
        code(userID).provide(TestDatabase)

    }

  }
  //</editor-fold>

  //<editor-fold desc="Running">
  {

    behavior of "ZIO-running"

    it should "run with App" in {
      import zio.console._

      object MyApp extends zio.App {

        def run(args: List[String]) =
          myAppLogic.exitCode

        val myAppLogic =
          for {
            _ <- putStrLn("Hello! What is your name?")
            name <- getStrLn
            _ <- putStrLn(s"Hello, ${name}, welcome to ZIO!")
          } yield ()
      }
    }

    it should "run with Default Runtime" in {
      val runtime = Runtime.default
      runtime.unsafeRun(ZIO(println("Hello World!")))
    }

    it should "run with Custom Runtime" in {
      import zio.internal.Platform
      val myRuntime: Runtime[Int] = Runtime(42, Platform.default)
      val effect: ZIO[Int, Nothing, Unit] = for {
        environmentVar <- ZIO.environment[Int]
      } yield println(s"Hello, $environmentVar")

      myRuntime.unsafeRun(effect)
    }
  }
  //</editor-fold>
}
