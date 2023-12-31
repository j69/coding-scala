package udemy.advanced2.module10

import org.scalatest.SeveredStackTraces
import org.scalatest.matchers.must.Matchers
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper
import play.api.libs.json._
import udemy.advanced2.module10.support.KoanSuite

class Module10JsonHeapSolutions extends KoanSuite with Matchers with SeveredStackTraces {

  trait Item {
    def title: String
  }

  case class Book(title: String, author: String, year: Int, pages: Int) extends Item

  case class Journal(title: String, issue: Int, month: Int, year: Int) extends Item

  case class DVD(title: String, director: String, year: Int, length: Int) extends Item

  case class CD(title: String, artist: String, year: Int, length: Int) extends Item

  case class Library(items: Seq[Item])

  // this is more tricky than it looks at first. Create a JSON format for a library and all of its parts
  // to serialize out and bring back in a library of items. Note that since Library holds on to Item
  // you will need to make Item, not just the individual case classes, serializable to/from JSON
  // which means differentiating them. Fortunately, they have different fields in each case so you
  // can likely find a way to use that information to tell them apart, or come up with a more cunning
  // way...

  // the easy ones
  implicit val bookJsonFormat: Format[Book] = Json.format[Book]
  implicit val journalJsonFormat: Format[Journal] = Json.format[Journal]
  implicit val dvdJsonFormat: Format[DVD] = Json.format[DVD]
  implicit val cdJsonFormat: Format[CD] = Json.format[CD]

  // now, the Item definition
  implicit object ItemJsonFormat extends Format[Item] {
    override def writes(o: Item): JsValue = o match {
      case book: Book       => bookJsonFormat.writes(book)
      case journal: Journal => journalJsonFormat.writes(journal)
      case dvd: DVD         => dvdJsonFormat.writes(dvd)
      case cd: CD           => cdJsonFormat.writes(cd)
    }

    override def reads(json: JsValue): JsResult[Item] = {
      val opt =
        json.asOpt[Book].orElse(json.asOpt[Journal]).orElse(json.asOpt[DVD]).orElse(json.asOpt[CD])
      opt.map(item => JsSuccess(item)).getOrElse(JsError("Could not parse Item from JSON"))
    }
  }

  implicit val libraryJsonFormat: Format[Library] = Json.format[Library]

  val library = Library(
    Seq(
      Book("Catcher in the Rye", "J.D. Salinger", 1951, 224),
      Journal("National Geographic", 170, 12, 2005),
      DVD("This is Spinal Tap", "Rob Reiner", 1984, 82),
      CD("Live Fast, Die Fast", "Wolfsbane", 1989, 35),
      Book("Daemon", "Daniel Suarez", 2009, 448),
      Journal("American Motorcyclist", 58, 12, 2010),
      DVD("The Mummy", "Stephen Sommers", 1999, 125),
      CD("Masterplan", "Oasis", 1998, 68),
      Book("Lila", "Robert M. Pirsig", 1992, 480),
      Journal("Linux Format", 200, 11, 2010),
      DVD("Coraline", "Henry Selick", 2009, 96),
      CD("Alright, Still", "Lily Allen", 2007, 46)
    )
  )

  test(
    "It should serialize a library full of items out to a file, and serialize it back in again"
  ) {
    val jsonLibraryString = Json.prettyPrint(Json.toJson(library))

    // if you're feeling brave, uncomment this (but the strings will need to match exactly!)
    jsonLibraryString should be(compareString)

    Json.parse(jsonLibraryString).as[Library] should be(library)
  }

  val compareString =
    """{
      |  "items" : [ {
      |    "title" : "Catcher in the Rye",
      |    "author" : "J.D. Salinger",
      |    "year" : 1951,
      |    "pages" : 224
      |  }, {
      |    "title" : "National Geographic",
      |    "issue" : 170,
      |    "month" : 12,
      |    "year" : 2005
      |  }, {
      |    "title" : "This is Spinal Tap",
      |    "director" : "Rob Reiner",
      |    "year" : 1984,
      |    "length" : 82
      |  }, {
      |    "title" : "Live Fast, Die Fast",
      |    "artist" : "Wolfsbane",
      |    "year" : 1989,
      |    "length" : 35
      |  }, {
      |    "title" : "Daemon",
      |    "author" : "Daniel Suarez",
      |    "year" : 2009,
      |    "pages" : 448
      |  }, {
      |    "title" : "American Motorcyclist",
      |    "issue" : 58,
      |    "month" : 12,
      |    "year" : 2010
      |  }, {
      |    "title" : "The Mummy",
      |    "director" : "Stephen Sommers",
      |    "year" : 1999,
      |    "length" : 125
      |  }, {
      |    "title" : "Masterplan",
      |    "artist" : "Oasis",
      |    "year" : 1998,
      |    "length" : 68
      |  }, {
      |    "title" : "Lila",
      |    "author" : "Robert M. Pirsig",
      |    "year" : 1992,
      |    "pages" : 480
      |  }, {
      |    "title" : "Linux Format",
      |    "issue" : 200,
      |    "month" : 11,
      |    "year" : 2010
      |  }, {
      |    "title" : "Coraline",
      |    "director" : "Henry Selick",
      |    "year" : 2009,
      |    "length" : 96
      |  }, {
      |    "title" : "Alright, Still",
      |    "artist" : "Lily Allen",
      |    "year" : 2007,
      |    "length" : 46
      |  } ]
      |}""".stripMargin
}
