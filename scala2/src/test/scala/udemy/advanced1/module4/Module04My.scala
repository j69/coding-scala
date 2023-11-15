package udemy.advanced1.module4

import org.scalatest.SeveredStackTraces
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.must.Matchers
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper

import scala.collection.mutable.ListBuffer
import scala.language.reflectiveCalls

class Module04My extends AnyFunSpec with Matchers with SeveredStackTraces {

  // using structural typing, define a method sameLength which takes 2 of any kind of object that defines length, returning
  // true if they are the same length, false otherwise. If you do it right, we should be able to compare a string
  // with a list and have true come out if they are the same length

  // uncomment the following to test your sameLength implementation
  describe("Function sameLength") {
    def sameLength(l1: { def length: Int }, l2: { def length: Int }): Boolean =
      l1.length == l2.length

    it("should compare 2 lists of the same length correctly") {
      sameLength(List(1, 2, 3), List("1", "2", "3")) should be(true)
    }
    it("should compare 2 lists of different length correctly") {
      sameLength(List(1, 2, 3), List("1", "2", "3", "4")) should be(false)
    }
    it("should compare a list and an array of the same length correctly") {
      sameLength(List(1, 2, 3), Array("1", "2", "3")) should be(true)
    }
    it("should compare a list and an array of different length correctly") {
      sameLength(List(1, 2, 3), Array("1", "2", "3", "4")) should be(false)
    }
    it("should compare a String and a List of the same length correctly") {
      sameLength("hello", List('H', 'E', 'L', 'L', 'O')) should be(true)
    }
    it("should compare a String and a List of the different length correctly") {
      sameLength("hello!", List('H', 'E', 'L', 'L', 'O')) should be(false)
    }

    // the following should not compile, please check that it doesn't
    it("should not allow things that do not have a length to be compared") {
      assertDoesNotCompile("""sameLength(5, "Hello")""")
    }
  }

  sealed trait Fruit

  case class Lemon(name: String, ph: Int) extends Fruit

  case class Grapefruit(name: String, ph: Int) extends Fruit

  case class Banana(name: String, potassium: Int) extends Fruit

  sealed trait Meat

  case class Beef(name: String, ph: Int) extends Meat

  describe("Using refinement types") {
    // create a method that returns the ph for a fruit but only if the fruit has a ph method
    def phForFruit(x: Fruit { def ph: Int }) = x.ph

    // uncomment the following to test it
    it("should give ph for a Lemon") {
      val lemon = Lemon("Jif", 4)
      phForFruit(lemon) should be(4)
    }

    it("should also give ph for a Grapefruit") {
      val grapefruit = Grapefruit("Pink", 3)
      phForFruit(grapefruit) should be(3)
    }

    it("should not compile for a banana") {
      val banana = Banana("Fife", 328)
      val beef = Beef("MOw", 328)
      assertDoesNotCompile("phForFruit(banana)")
      assertDoesNotCompile("phForFruit(beef)")
    }

    // now create a mutable ListBuffer to which only Fruits with a ph can be added
    // and uncomment below to test it
    describe("Using a collection of ph Fruits") {
      it("should only allow Fruits with a ph to be added") {
        val phFruits = new ListBuffer[Fruit { def ph: Int }]()

        phFruits += Lemon("Jif", 4)
        phFruits += Grapefruit("Pink", 3)

        assertDoesNotCompile("""phFruits += Banana("Fife", 328)""")

        phFruits should be(List(Lemon("Jif", 4), Grapefruit("Pink", 3)))

        val meanPh = phFruits.map(_.ph.toDouble).sum / phFruits.length
        meanPh should be(3.5 +- 1e-6)
      }
    }

  }

  // define an enumeration for DNA Nucleotides with the values A, C, G and T, and the names
  // Adenine, Cytosine, Guanine and Thymine so that the following tests pass, when uncommented
  object DNA extends Enumeration {
    val A = Value(1, "Adenine")
    val C = Value(2, "Cytosine")
    val T = Value(3, "Thymine")
    val G = Value(4, "Guanine")
  }

  describe("A string of DNA") {
    import DNA._
    it("should map to a sequence of Nucleotide enumeration values") {
      val str = "GATTACA"
      val nseq = str.map {
        case 'A' => DNA.A
        case 'C' => DNA.C
        case 'T' => DNA.T
        case 'G' => DNA.G
      }

      nseq should be(Seq(G, A, T, T, A, C, A))
    }

    it("should print out to the full names of the nucleotides") {
      val str = "GATTACA"
      val nseq = str.map {
        case 'A' => A
        case 'C' => C
        case 'T' => T
        case 'G' => G
      }

      val fullNames = nseq.mkString(" ")
      fullNames should be("Guanine Adenine Thymine Thymine Adenine Cytosine Adenine")

    }
  }

}
