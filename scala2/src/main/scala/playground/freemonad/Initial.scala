package playground.freemonad

import java.util.UUID
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

object Initial {
  case class User(id: UUID, email: String, loyaltyPoints: Int) {
    def serialize: String = id.toString + "," + loyaltyPoints + "," + email
  }

  trait UserRepository {
    def findUser(id: UUID): Future[Option[User]]
    def updateUser(u: User): Future[Unit]
  }
  trait EmailService {
    def sendEmail(email: String, subject: String, body: String): Future[Unit]
  }

  class LoyaltyPoints(ur: UserRepository, es: EmailService) {
    def addPoints(userId: UUID, pointsToAdd: Int): Future[Either[String, Unit]] = {
      ur.findUser(userId).flatMap {
        case None => Future.successful(Left("User not found"))
        case Some(user) =>
          val updated = user.copy(loyaltyPoints = user.loyaltyPoints + pointsToAdd)
          for {
            _ <- ur.updateUser(updated)
            _ <- es.sendEmail(user.email, "Points added!", s"You now have ${updated.loyaltyPoints}")
          } yield Right(())
      }
    }
  }

  // an implementation by KV store
  object User {
    def parse(s: String): User = {
      val parts = s.split(",")
      User(UUID.fromString(parts(0)), parts(2), parts(1).toInt)
    }
  }

  trait KVStore {
    def get(k: String): Future[Option[String]]
    def put(k: String, v: String): Future[Unit]
  }

  class UserRepositoryUsingKVStore(kvStore: KVStore) extends UserRepository {
    override def findUser(id: UUID): Future[Option[User]] =
      kvStore.get(id.toString).map(serialized => serialized.map(User.parse))

    override def updateUser(u: User): Future[Unit] = {
      val serialized = u.serialize
      for {
        _ <- kvStore.put(u.id.toString, serialized)
        _ <- kvStore.put(u.email, serialized) // let's say we also maintain a by-email index
      } yield ()
    }
  }
}
