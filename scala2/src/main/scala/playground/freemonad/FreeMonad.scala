package playground.freemonad

import cats.free.Free
import playground.freemonad.FreeMonad.Refactored._
import playground.freemonad.Initial.User

import cats.implicits._
import java.util.UUID
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

/**
 * Free and tagless compared - how not to commit to a monad too early.
 * https://softwaremill.com/free-tagless-compared-how-not-to-commit-to-monad-too-early/
 */
object FreeMonad {

  object Refactored {
    sealed trait UserRepositoryAlg[T]
    case class FindUser(id: UUID) extends UserRepositoryAlg[Option[User]]
    case class UpdateUser(u: User) extends UserRepositoryAlg[Unit]

    sealed trait EmailAlg[T]
    case class SendEmail(email: String, subject: String, body: String) extends EmailAlg[Unit]

    type UserRepository[T] = Free[UserRepositoryAlg, T]

    def findUser(id: UUID): UserRepository[Option[User]] = Free.liftF(FindUser(id))

    def updateUser(u: User): UserRepository[Unit] = Free.liftF(UpdateUser(u))
  }

  // and now we can...
  def addPoints(userId: UUID, pointsToAdd: Int): UserRepository[Either[String, Unit]] = {
    findUser(userId).flatMap {
      case None => Free.pure(Left("User not found"))
      case Some(user) =>
        val updated = user.copy(loyaltyPoints = user.loyaltyPoints + pointsToAdd)
        updateUser(updated).map(_ => Right(()))
    }
  }
  // and of course we need an interpreter...

  import cats.~>

  val futureInterpreter = new (UserRepositoryAlg ~> Future) {
    override def apply[A](fa: UserRepositoryAlg[A]): Future[A] = fa match {
      case FindUser(id) =>
        /* go and talk to a database */
        Future.successful(None)
      case UpdateUser(u) =>
        /* as above */
        Future.successful(())
    }
  }

  val result: Future[Either[String, Unit]] =
    addPoints(UUID.randomUUID(), 10).foldMap(futureInterpreter)

}
