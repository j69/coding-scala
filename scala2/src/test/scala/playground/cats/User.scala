package playground.cats

case class User(
  userName: String,
  firstName: String,
  lastName: String,
  email: String,
  hash: String,
  phone: String,
  id: Option[Long] = None
)
