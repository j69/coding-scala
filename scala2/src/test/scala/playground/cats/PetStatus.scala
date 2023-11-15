package playground.cats

sealed trait PetStatus

case object Available extends PetStatus

case object Pending extends PetStatus

case object Adopted extends PetStatus
