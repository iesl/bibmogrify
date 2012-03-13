package edu.umass.cs.iesl.bibmogrify.model

import java.net.URL

// ** add citances with context for sentiment

trait StructuredCitation {
  val doctype: Option[DocType] = None
  val docSubtype: Option[String] = None // for journal articles: Letter; Application Note; Research Article, etc.  For grants: R01, K99, etc.
  val title: Option[String] = None  // ** need to track titles in multiple languages.  Probably Map[Language, String]
  val authors: Seq[AuthorInRole] = Nil
  val otherContributors: Seq[OtherContributorInRole] = Nil
  val sourceLanguage: Option[Language] = None // the original language, if this is a translation
  val language: Option[Language] = None // the language of this version of the document (e.g., after translation)
  val identifiers: Seq[Identifier] = Nil
  val locations: Seq[Location] = Nil
  val supplementaryLocations: Seq[Location] = Nil // where to find supplementary material, databases, etc.
  val containedIn: Option[ContainmentInfo] = None // journal, book, proceedings, etc.
  val publisher: Option[Institution] = None // likely blank when there is containedIn, i.e. paper -> journal -> publisher
  val dates: Seq[CitationEvent] = Nil
  val grants: Seq[GrantInfo] = Nil

  val references: Seq[StructuredCitation] = Nil // could include context here


  val keywords: Seq[Keyword] = Nil

  val abstractLanguages: Seq[Option[Language]] = Nil
  val abstractText: Option[String] = None // ** need to track abstracts in multiple languages.  Probably Map[Language, String]
  //val introText: Option[String] = None
  val bodyText: Seq[BodyTextSection] = Nil // can't use LinkedHashMap because keys may recur

  val notes: Seq[String] = Nil

  val refMarker: Option[String] = None // within-document ID

  def textOfType(sectionType: BodyTextSectionType): Seq[String] = bodyText.filter(_.sectionType == sectionType).map(_.text)

}

trait StructuredPatent extends StructuredCitation {

  override val doctype = Some(Patent)
  val priorityClaims: Seq[StructuredCitation] = Nil
  val mainFamily: Seq[StructuredCitation] = Nil
  val completeFamily: Seq[StructuredCitation] = Nil
  val searchReportReferences: Seq[StructuredCitation] = Nil
}

trait BodyTextSection {
  val sectionType: BodyTextSectionType
  val text: String
}

case class BasicBodyTextSection(override val sectionType: BodyTextSectionType, override val text: String) extends BodyTextSection

case class UndifferentiatedBodyTextSection(override val text: String) extends BodyTextSection {
  val sectionType = GeneralBodyText
}

sealed class BodyTextSectionType

case object GeneralBodyText extends BodyTextSectionType

case object Summary extends BodyTextSectionType

case object IntroductionAndBackground extends BodyTextSectionType

case object Results extends BodyTextSectionType

case object MaterialsAndMethods extends BodyTextSectionType

case object DiscussionAndConclusion extends BodyTextSectionType

case object UnparsedReferences extends BodyTextSectionType

case object Acknowledgements extends BodyTextSectionType

case object FigureCaptions extends BodyTextSectionType

case object Sidebar extends BodyTextSectionType

case object Table extends BodyTextSectionType

case object Appendix extends BodyTextSectionType

case object Claims extends BodyTextSectionType


object CitationUtils {

  implicit def docTypeToOption(a: DocType) = Some(a)

  implicit def languageToOption(a: Language) = Some(a)

  implicit def containmentInfoToOption(a: ContainmentInfo) = Some(a)

  implicit def institutionToOption(a: Institution) = Some(a)
}

trait Keyword {
  val authority: Option[KeywordAuthority] = None
  val word: String
}

// don't model hierarchical keywords, just leave them slash-delimited in the string
case class BasicKeyword(override val word: String, override val authority: Option[KeywordAuthority] = None) extends Keyword


sealed class DocType

case object JournalArticle extends DocType

case object Journal extends DocType

case object ProceedingsArticle extends DocType

case object Proceedings extends DocType

case object CollectionArticle extends DocType

case object CollectionOfArticles extends DocType

case object BookChapter extends DocType

case object Book extends DocType

case object TechnicalReport extends DocType

case object Patent extends DocType

case object PhdThesis extends DocType

case object MastersThesis extends DocType

case object Grant extends DocType

case object WwwArticle extends DocType


sealed class Country

trait Identifier {
  val authority: Option[IdentifierAuthority] = None
  val value: String

  def qualifiedValue = authority.map(_.shortName).getOrElse("Unknown") + ":" + value
}

case class BasicIdentifier(override val value: String, override val authority: Option[IdentifierAuthority] = None) extends Identifier

trait Location {
  // are there other kinds of locations?  e.g., call numbers
  val url: URL
  val hashes: Seq[Hash]
}

case class BasicLocation(override val url: URL, override val hashes: Seq[Hash]) extends Location

trait Hash {
  val hashType: HashType
  val hashValue: String
}

case class BasicHash(override val hashType: HashType, override val hashValue: String) extends Hash

sealed class HashType

case object CRC32 extends HashType

case object SHA1 extends HashType

case object SHA256 extends HashType

case object MD5 extends HashType


trait ContainmentInfo {
  val container: StructuredCitation
  val series: Option[String]
  val volume: Option[String]
  val number: Option[String] // journal number, or chapter number.  Not necessarily integer?
  val pages: Option[PageRange]
}

case class BasicContainmentInfo(override val container: StructuredCitation, override val series: Option[String], override val volume: Option[String], override val number: Option[String],
                                override val pages: Option[PageRange]) extends ContainmentInfo

trait PageRange

trait NormalPageRange extends PageRange {
  val start: Int
  val end: Option[Int] = None
}

case class BasicNormalPageRange(override val start: Int, override val end: Option[Int]) extends NormalPageRange

trait StringPageRange extends PageRange {
  val start: String
  val end: Option[String] = None
}

case class BasicStringPageRange(override val start: String, override val end: Option[String]) extends StringPageRange

/**
 * Enumeration of types of events that may occur in the lifecycle of a document.
 *
 * @param primaryPriority indicates the order in which entries should be consulted to determine the "primary" date of the document.  A value of zero indicates that the date cannot be primary.
 */
 sealed class EventType(val primaryPriority: Int,val shortName : String)

case object Received extends EventType(6,"rec")

case object Revised extends EventType(7,"rev")

case object Epub extends EventType(8,"epub")

case object Published extends EventType(9,"pub")

case object Retracted extends EventType(0,"retr")

case object Captured extends EventType(0,"capt")

case object Begin extends EventType(10,"beg")

// for grants, patents
case object End extends EventType(0,"end")

trait CitationEvent {
  val eventType: EventType
  val date: Option[PartialDate] // possibly we know that a given event type occurred but we don't know when
}

case class BasicCitationEvent(override val date: Option[PartialDate], override val eventType: EventType) extends CitationEvent

trait PartialDate extends Ordered[PartialDate] {
  // this could be a lot fancier
  val year: Option[Int]
  val month: Option[Int]
  val day: Option[Int]

  def compare(that: PartialDate): Int = {
    (year, that.year) match {
      case (None, None) => compareMonth(that)
      case (Some(x), None) => 1
      case (None, Some(y)) => -1
      case (Some(x), Some(y)) if x == y => compareMonth(that)
      case (Some(x), Some(y)) if x != y => x.compare(y)
    }
  }

  private def compareMonth(that: PartialDate): Int = {
    (month, that.month) match {
      case (None, None) => compareDay(that)
      case (Some(x), None) => 1
      case (None, Some(y)) => -1
      case (Some(x), Some(y)) if x == y => compareDay(that)
      case (Some(x), Some(y)) if x != y => x.compare(y)
    }
  }

  private def compareDay(that: PartialDate): Int = {
    (month, that.month) match {
      case (None, None) => 0
      case (Some(x), None) => 1
      case (None, Some(y)) => -1
      case (Some(x), Some(y)) => x.compare(y)
    }
  }
}

case class BasicPartialDate(override val year: Option[Int], override val month: Option[Int], override val day: Option[Int]) extends PartialDate

trait GrantInfo {
  //val institution: Option[Institution] // no point in recording the grant without this?
  //val identifier: Option[String]
  val grant: StructuredCitation
  val grantees: Seq[AuthorInRole] // could also have a GrantRecipient role?
}

case class BasicGrantInfo(override val grant: StructuredCitation, override val grantees: Seq[AuthorInRole]) extends GrantInfo

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
