package playground.cats

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.must.Matchers.be
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper

import scala.language.higherKinds

class CatsPlaygroundTest extends AnyFlatSpec {
  //<editor-fold desc="Order">
  {

    import cats.data.EitherT
    import cats.syntax.all._
    import cats.{Applicative, Functor, Id}

    import scala.collection.concurrent.TrieMap

    trait OrderRepository[F[_]] {
      def create(order: Order): F[Order]

      def get(orderId: Long): F[Option[Order]]

      def delete(orderId: Long): F[Option[Order]]
    }

    case class Order(id: Option[Long] = None, petId: Long, userId: Option[Long])

    it should "check EitherT behaviour" in {
      class OrderService[F[_]](orderRepo: OrderRepository[F]) {
        def placeOrder(order: Order): F[Order] = orderRepo.create(order)

        def get(id: Long)(implicit F: Functor[F]): EitherT[F, OrderNotFoundError.type, Order] =
          EitherT.fromOptionF(orderRepo.get(id), OrderNotFoundError)
      }

      class OrderRepositoryInMemory[F[_]: Applicative] extends OrderRepository[F] {
        private val cache = new TrieMap[Long, Order]

        def create(order: Order): F[Order] = {
          val toSave = order.copy(id = order.id.orElse(2L.some))
          toSave.id.foreach(cache.put(_, toSave))
          toSave.pure[F]
        }

        def get(orderId: Long): F[Option[Order]] =
          cache.get(orderId).pure[F]

        def delete(orderId: Long): F[Option[Order]] =
          cache.remove(orderId).pure[F]
      }

      val orderInstance = new OrderRepositoryInMemory[Id]()
      val service = new OrderService(orderInstance)

      val goodOrder = Order(Some(2), 12, None)

      service.placeOrder(Order(None, 12L, None)) should be(goodOrder)
      val existing = service.get(2)
      val goodResponse = EitherT[Id, OrderNotFoundError.type, Order]((Either.right(goodOrder)))
      existing should be(goodResponse)
      val notExisting = service.get(0)
      val badResponse =
        EitherT[Id, OrderNotFoundError.type, Order]((Either.left(OrderNotFoundError)))
      notExisting should be(badResponse)

    }
  }
  //</editor-fold>

  //<editor-fold desc="Pet">
  {
    import cats.data.EitherT
    import cats.syntax.all._
    import cats.{Applicative, Id, Monad}

    trait PetRepositoryAlgebra[F[_]] {
      def create(pet: Pet): F[Pet]

      def update(pet: Pet): F[Option[Pet]]
    }

    trait PetValidationAlgebra[F[_]] {
      /* Fails with a PetAlreadyExistsError */
      def doesNotExist(pet: Pet): EitherT[F, PetAlreadyExistsError, Unit]

      /* Fails with a PetNotFoundError if the pet id does not exist or if it is none */
      def exists(petId: Option[Long]): EitherT[F, PetNotFoundError.type, Unit]
    }

    class PetService[F[_]](
      repository: PetRepositoryAlgebra[F],
      validation: PetValidationAlgebra[F]
    ) {
      def create(pet: Pet)(implicit M: Monad[F]): EitherT[F, PetAlreadyExistsError, Pet] =
        for {
          _ <- validation.doesNotExist(pet)
          saved <- EitherT.liftF(repository.create(pet))
        } yield saved

      /* Could argue that we could make this idempotent on put and not check if the pet exists */
      def update(pet: Pet)(implicit M: Monad[F]): EitherT[F, PetNotFoundError.type, Pet] =
        for {
          _ <- validation.exists(pet.id)
          saved <- EitherT.fromOptionF(repository.update(pet), PetNotFoundError)
        } yield saved

    }

    class PetValidationAlgebraStatic[F[_]: Applicative](doesExist: Boolean, notExists: Boolean)
        extends PetValidationAlgebra[F] {
      override def doesNotExist(pet: Pet): EitherT[F, PetAlreadyExistsError, Unit] =
        if (doesExist)
          EitherT(Either.left[PetAlreadyExistsError, Unit](PetAlreadyExistsError(pet)).pure[F])
        else EitherT(Either.right[PetAlreadyExistsError, Unit](()).pure[F])

      override def exists(petId: Option[Long]): EitherT[F, PetNotFoundError.type, Unit] =
        if (notExists) EitherT(Either.right[PetNotFoundError.type, Unit](()).pure[F])
        else EitherT(Either.left[PetNotFoundError.type, Unit](PetNotFoundError).pure[F])
    }

    class PetRepositoryAlgebraInMemory[F[_]: Applicative] extends PetRepositoryAlgebra[F] {
      override def create(pet: Pet): F[Pet] = pet.pure[F]

      override def update(pet: Pet): F[Option[Pet]] = pet.some.pure[F]
    }

    val samplePet = Pet("puppy", "domesticated pet", "was bor and lives")

    it should "check Monad behaviour [SUCCESS](create)" in {
      val validator: PetValidationAlgebraStatic[Id] =
        new PetValidationAlgebraStatic[Id](false, false)
      val repo: PetRepositoryAlgebraInMemory[Id] = new PetRepositoryAlgebraInMemory()
      val service: PetService[Id] = new PetService(repo, validator)

      // success path
      val expected = EitherT(Either.right[PetAlreadyExistsError, Pet](samplePet).pure[Id])
      service.create(samplePet) should be(expected)
    }

    it should "check Monad behaviour [FAILURE](create)" in {
      val validator: PetValidationAlgebraStatic[Id] =
        new PetValidationAlgebraStatic[Id](true, false)
      val repo: PetRepositoryAlgebraInMemory[Id] = new PetRepositoryAlgebraInMemory()
      val service: PetService[Id] = new PetService(repo, validator)

      // failure path
      val expected =
        EitherT(Either.left[PetAlreadyExistsError, Pet](PetAlreadyExistsError(samplePet)).pure[Id])
      val response = service.create(samplePet)
      response should be(response)
    }

    it should "check Monad behaviour [SUCCESS](update)" in {
      val validator: PetValidationAlgebraStatic[Id] =
        new PetValidationAlgebraStatic[Id](true, false)
      val repo: PetRepositoryAlgebraInMemory[Id] = new PetRepositoryAlgebraInMemory()
      val service: PetService[Id] = new PetService(repo, validator)

      // success path
      val expected = EitherT(Either.right[PetNotFoundError.type, Pet](samplePet).pure[Id])
      val response = service.update(samplePet)
      response should be(response)
    }
  }
  //</editor-fold>

}
