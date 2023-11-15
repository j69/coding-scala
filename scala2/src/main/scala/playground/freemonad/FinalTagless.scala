package playground.freemonad

import cats.Monad
import cats.implicits._
import playground.freemonad.Initial.User

import java.util.UUID
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

/**
 * Free and tagless compared - how not to commit to a monad too early.
 * https://softwaremill.com/free-tagless-compared-how-not-to-commit-to-monad-too-early/
 */
object FinalTagless {

  trait UserRepositoryAlg[F[_]] {
    def findUser(id: UUID): F[Option[User]]
    def updateUser(u: User): F[Unit]
  }

  trait EmailAlg[F[_]] {
    def sendEmail(email: String, subject: String, body: String): F[Unit]
  }

  class LoyaltyPoints[F[_]: Monad](ur: UserRepositoryAlg[F], es: EmailAlg[F]) {

    def addPoints(userId: UUID, pointsToAdd: Int): F[Either[String, Unit]] = {
      ur.findUser(userId).flatMap {
        case None => implicitly[Monad[F]].pure(Left("User not found"))
        case Some(user) =>
          val updated = user.copy(loyaltyPoints = user.loyaltyPoints + pointsToAdd)
          for {
            _ <- ur.updateUser(updated)
            _ <- es.sendEmail(user.email, "Points added!", s"You now have ${updated.loyaltyPoints}")
          } yield Right(())
      }
    }
  }

  // and now interpretation....
  trait FutureInterpreter extends UserRepositoryAlg[Future] {
    override def findUser(id: UUID): Future[Option[User]] =
      Future.successful(None) /* go and talk to a database */

    override def updateUser(u: User): Future[Unit] =
      Future.successful(()) /* as above */
  }

  trait FutureEmailInterpreter extends EmailAlg[Future] {
    override def sendEmail(email: String, subject: String, body: String): Future[Unit] =
      Future.successful(()) /* use smtp */
  }

  val result: Future[Either[String, Unit]] =
    new LoyaltyPoints(new FutureInterpreter {}, new FutureEmailInterpreter {})
      .addPoints(UUID.randomUUID(), 10)

  // low level implementations
  trait KVAlg[F[_]] {
    def get(k: String): F[Option[String]]
    def put(k: String, v: String): F[Unit]
  }

  trait KvToFutureInterpreter extends KVAlg[Future] {
    override def get(k: String): Future[Option[String]] =
      Future.successful(None) /* go and talk to a database */

    override def put(k: String, v: String): Future[Unit] =
      Future.successful(()) /* as above */
  }

  class UserThroughKvInterpreter[F[_]: Monad](kv: KVAlg[F]) extends UserRepositoryAlg[F] {

    override def findUser(id: UUID): F[Option[User]] =
      kv.get(id.toString).map(_.map(User.parse))

    override def updateUser(u: User): F[Unit] = {
      val serialized = u.serialize
      for {
        _ <- kv.put(u.id.toString, serialized)
        _ <- kv.put(u.email, serialized) // we also maintain a by-email index
      } yield ()
    }
  }

  val resultKv: Future[Either[String, Unit]] =
    new LoyaltyPoints(
      new UserThroughKvInterpreter(new KvToFutureInterpreter {}),
      new FutureEmailInterpreter {}
    ).addPoints(UUID.randomUUID(), 10)
}
