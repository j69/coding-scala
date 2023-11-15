package videos.code01_contrvariance

object IsContrvarianceHard {

  sealed class Animal

  case class Dog(name: String) extends Animal

  case class Cat(name: String) extends Animal

  def caseVariant(): Unit = {
    // if List[Dog] <: List[Animal] - they are COVARIANT
    val bobik = Dog("Bobik")
    val barsik = Cat("Barsik")

    // in fact list is a List[+A]
    val dogs: List[Animal] = List(bobik, new Dog("Laika"), new Dog("Rex"), barsik)
  }

  def caseInvariant(): Unit = {
    // NOVARIANT CASE
    class MyInvariantType[T]

    // doesn't compile even
    // val myInvDogs : MyInvariantType[Animal] = new MyInvariantType[Dog]
  }

  def caseContrvariantCase1(): Unit = {
    class MyContrvariantType[-T]
    val myContrvDogs: MyContrvariantType[Dog] = new MyContrvariantType[Animal]

    trait Vet[-T] {
      def heal(animal: T): Boolean
    }

    def callAllAnimalVet() = new Vet[Animal] {
      override def heal(animal: Animal): Boolean = true
    }

    def callDogsOnlyVet() = new Vet[Dog] {
      override def heal(animal: Dog): Boolean = true
    }

    val mySickDog = Dog("Sick Buddy")
    val mySickCat = Cat("Sick Barsik")
    val allAnimalVet = callAllAnimalVet()
    allAnimalVet.heal(mySickDog)
    allAnimalVet.heal(mySickCat)
    val onlyDogsVet = callDogsOnlyVet()
    onlyDogsVet.heal(mySickDog)

    // won't compile as expected
    // onlyDogsVet.heal(mySickCat)
  }

  // RULE #1!!!
  // CO-VARIANT - creates OR contains
  // CONTR-VARIANT  -- consumes OR processes

  def caseContrvariantCase2(): Unit = {
    trait Vet[-T] {
      def heal(animal: T): Boolean
    }

    def callAllAnimalVet() = new Vet[Animal] {
      override def heal(animal: Animal): Boolean = true
    }

    def callDogsOnlyVet() = new Vet[Dog] {
      override def heal(animal: Dog): Boolean = true
    }

    def hireToDogClinic(vet: Vet[Dog]): Unit = {
      val mySickDog = Dog("Sick Buddy")
      vet.heal(mySickDog)
    }

    hireToDogClinic(callDogsOnlyVet())
    hireToDogClinic(callAllAnimalVet())

    def callToDogHealer(healer: Dog => Dog): Unit = healer.apply(Dog("Sick dog"))

    callToDogHealer((d: Dog) => Dog("Healed Dog"))
    callToDogHealer((a: Animal) => Dog("Healed Dog"))

    // won't compile
    // callToDogHealer((c: Cat) => Dog("Healed Dog"))
  }

}
