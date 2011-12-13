package edu.umass.cs.iesl.bibmogrify.model

import java.net.URL

trait CitationMention
  {
  val doctype: DocType
  val docSubtype: Option[String] = None // for journal articles: Letter; Application Note; Research Article, etc.  For grants: R01, K99, etc.
  val title: String
  val authors: Seq[AuthorInRole] = Nil
  val otherContributors: Seq[OtherContributorInRole] = Nil
  val language: Option[Language] = None
  val identifiers: Seq[Identifier] = Nil
  val locations: Seq[Location] = Nil
  val supplementaryLocations: Seq[Location] = Nil // where to find supplementary material, databases, etc.
  val containedIn: Option[ContainmentInfo] = None // journal, book, proceedings, etc.
  val publisher: Option[Institution] = None // likely blank when there is containedIn, i.e. paper -> journal -> publisher
  val dates: Seq[CitationEvent] = Nil
  val grants: Seq[GrantInfo] = Nil

  val references: Seq[CitationMention] = Nil // could include context here
  val keywords: Seq[Keyword] = Nil

  val abstractText: Option[String] = None
  val introText: Option[String] = None
  val bodyText: Option[String] = None

  val notes: Seq[String] = Nil

  val refMarker: Option[String] = None // within-document ID
  }

// don't model hierarchical keywords, just leave them slash-delimited in the string
case class Keyword(authority: KeywordAuthority, word: String)


sealed class DocType
case object JournalArticle extends DocType
case object Journal extends DocType
case object ProceedingsArticle extends DocType
case object Proceedings extends DocType
case object BookChapter extends DocType
case object Book extends DocType
case object TechnicalReport extends DocType
case object Patent extends DocType
case object Thesis extends DocType
case object Grant extends DocType

sealed class Language

sealed class Country

trait Identifier
  {
  val authority: IdentifierAuthority
  val value: String

  def qualifiedValue = authority.shortName + ":" + value
  }

trait Location
  {
  // are there other kinds of locations?  e.g., call numbers
  val url: URL
  val hashes: Seq[Hash]
  }

trait Hash
  {
  val hashType: HashType
  val hashValue: String
  }

sealed class HashType

case object CRC32 extends HashType

case object SHA1 extends HashType

case object SHA256 extends HashType

case object MD5 extends HashType


trait ContainmentInfo
  {
  val container: CitationMention
  val series: Option[String]
  val volume: Option[String]
  val number: Option[Int] // journal number, or chapter number
  val pages: Option[PageRange]
  }

trait PageRange

trait NormalPageRange
  {
  val start: Int
  val end: Option[Int]
  }


trait StringPageRange
  {
  val start: Int
  val end: Option[Int]
  }

/**
 * Enumeration of types of events that may occur in the lifecycle of a document.
 *
 * @param primaryPriority indicates the order in which entries should be consulted to determine the "primary" date of the document.  A value of zero indicates that the date cannot be primary.
 */
sealed class EventType(val primaryPriority: Int)
case object Received extends EventType(6)
case object Revised extends EventType(7)
case object Epub extends EventType(8)
case object Published extends EventType(9)
case object Retracted extends EventType(0)
case object Captured extends EventType(0)
case object Begin extends EventType(10)
// for grants, patents
case object End extends EventType(0)

// for grants, patents
trait CitationEvent
  {
  val eventType: EventType
  val date: Option[PartialDate] // possibly we know that a given event type occurred but we don't know when
  }

trait PartialDate extends Ordered[PartialDate]
  {
  // this could be a lot fancier
  val year: Option[Int]
  val month: Option[Int]
  val day: Option[Int]

  def compare(that: PartialDate): Int =
    {
    (year, that.year) match
    {
      case (None, None) => compareMonth(that)
      case (Some(x), None) => 1
      case (None, Some(y)) => -1
      case (Some(x), Some(y)) if x == y => compareMonth(that)
      case (Some(x), Some(y)) if x != y => x.compare(y)
    }
    }

  private def compareMonth(that: PartialDate): Int =
    {
    (month, that.month) match
    {
      case (None, None) => compareDay(that)
      case (Some(x), None) => 1
      case (None, Some(y)) => -1
      case (Some(x), Some(y)) if x == y => compareDay(that)
      case (Some(x), Some(y)) if x != y => x.compare(y)
    }
    }

  private def compareDay(that: PartialDate): Int =
    {
    (month, that.month) match
    {
      case (None, None) => 0
      case (Some(x), None) => 1
      case (None, Some(y)) => -1
      case (Some(x), Some(y)) => x.compare(y)
    }
    }
  }

trait GrantInfo
  {
  //val institution: Option[Institution] // no point in recording the grant without this?
  //val identifier: Option[String]
  val grant: CitationMention
  val grantees: Seq[AuthorInRole] // could also have a GrantRecipient role?
  }


/*
	altAuthorlist(61),  // maybe authors??
	altTitle(100),      // maybe title??
	cdrom(33 ),         // dblp
	crossref(35),       // bibtex
	degree(7),          // dblp??
	ee(32),             // dblp??
	layout(38),         // ?
	location(14),       // ?
	ref(39),            // ?
	sourceText(25),     // ?
	tech(20),           // ?

	*/
