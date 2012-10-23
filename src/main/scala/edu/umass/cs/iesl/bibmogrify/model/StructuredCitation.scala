package edu.umass.cs.iesl.bibmogrify.model

import java.net.URL
import com.weiglewilczek.slf4s.Logging
import com.cybozu.labs.langdetect.{LangDetectException, Detector, DetectorFactory}
import edu.umass.cs.iesl.scalacommons.{StringUtils, NonemptyString}
import tools.nsc.io.Directory
import StringUtils._
import annotation.tailrec

// ** add citances with context for sentiment
trait StructuredCitation {
	val doctype               : Option[DocType]             = None
	val docSubtype            : Option[NonemptyString]      = None
	// for journal articles: Letter; Application Note; Research Article, etc.  For grants: R01, K99, etc.
	val title                 : Option[NonemptyString]      = None
	// ** need to track titles in multiple languages.  Probably Map[Language, String]
	val authors               : Seq[AuthorInRole]           = Nil
	val otherContributors     : Seq[OtherContributorInRole] = Nil
	val sourceLanguage        : Option[Language]            = None
	// the original language, if this is a translation
	val language              : Option[Language]            = None
	// the language of this version of the document (e.g., after translation)
	val identifiers           : Iterable[Identifier]        = Nil
	val locations             : Iterable[Location]          = Nil
	val supplementaryLocations: Iterable[Location]          = Nil
	// where to find supplementary material, databases, etc.
	val containedIn           : Option[ContainmentInfo]     = None
	// journal, book, proceedings, etc.
	val publisher             : Option[Institution]         = None
	// likely blank when there is containedIn, i.e. paper -> journal -> publisher
	val dates                 : Iterable[CitationEvent]     = Nil
	val grants                : Iterable[GrantInfo]         = Nil

	val unstructuredString: Option[NonemptyString] = None

	//val referenceStrings: Seq[String] = Nil
	val references: Seq[StructuredCitation] = Nil
	// could include context here
	val keywords  : Set[Keyword]            = Set.empty

	//val abstractLanguages: Seq[Option[Language]] = Nil
	val abstractText: Iterable[TextWithLanguage] = Nil
	//val introText: Option[String] = None
	val bodyText    : Seq[BodyTextSection]       = Nil
	// can't use LinkedHashMap because keys may recur
	val notes       : Iterable[NonemptyString]   = Nil

	val refMarker: Option[NonemptyString] = None

	// within-document ID
	def textOfType(sectionType: BodyTextSectionType): Seq[NonemptyString] = bodyText.filter(_.sectionType == sectionType).map(_.text).flatten

	// todo these are a short-term hack; need to figure out requirements and best representation.  Probably a whole new DisplayItem class,
	// like BodyTextSection
	val numFigures : Option[Int]            = None
	val numTables  : Option[Int]            = None
	val licenseType: Option[NonemptyString] = None

	// addresses that are not linked to authors
	val addresses: Seq[Address] = Nil

	val institutionTypes: Set[InstitutionType] = Set.empty
}

object TextWithLanguage extends Logging {
	val init = {
		val okFilenames = Language.majorLanguages.map(_.name).toList
		val profileUrl: URL = getClass.getResource("/profiles")
		val profileDir = new Directory(new java.io.File(profileUrl.getFile)) // hope that there are no competing items with that name on the classpath

		profileDir.files.filter(f => okFilenames.contains(f.name)).map(f => {
			logger.debug("Loading language: " + f)
			DetectorFactory.loadProfile(f.jfile)
		})
		logger.debug("Loaded language profiles")
	}

	def apply(specifiedLanguage: Option[Language], text: Option[NonemptyString]): Option[TextWithLanguage] =
		text.map(new TextWithLanguage(specifiedLanguage, _))

	def apply(specifiedLanguage: Option[Language], text: NonemptyString): TextWithLanguage =
		new TextWithLanguage(specifiedLanguage, text)

	//init
	//logger.info("Loaded language profiles")
}

class TextWithLanguage private(val specifiedLanguage: Option[Language], val text: NonemptyString) extends Logging {

	def cleanText = RichStructuredCitation.cleanup(text)

	def language: Option[Language] = {
		(specifiedLanguage, detectedLanguage) match {
			case (None, None) => None
			case (Some(a), None) => Some(a)
			case (None, Some(b)) => Some(b)
			case (Some(a), Some(b)) if a == b => Some(a)
			case (Some(a), Some(b)) => {
				logger.warn("Language disagreement. Using specified " + a + ", but detected " + b + ".")
				Some(a)
			}
		}
	}

	def detectedLanguage: Option[Language] = {
		try {
			val detector: Detector = DetectorFactory.create()
			detector.append(text)
			val l = detector.detect()
			Language.get(l)
		}
		catch {
			case e: LangDetectException => None
		}
	}
}

trait StructuredPatent extends StructuredCitation {

	override val doctype = Some(Patent)
	val priorityClaims        : Seq[StructuredCitation] = Nil
	val mainFamily            : Seq[StructuredCitation] = Nil
	val completeFamily        : Seq[StructuredCitation] = Nil
	val searchReportReferences: Seq[StructuredCitation] = Nil
}

trait BodyTextSection {

	import StringUtils.enrichString

	def ++(s: String): BodyTextSection = s.opt.map(x => new BasicBodyTextSection(sectionType, (text + " " + x).opt, header)).getOrElse(this)

	val sectionType: BodyTextSectionType
	val header     : Option[NonemptyString]
	val text       : Option[NonemptyString]
}

case class BasicBodyTextSection(override val sectionType: BodyTextSectionType, override val text: Option[NonemptyString],
                                override val header: Option[NonemptyString])
		extends BodyTextSection

case class UndifferentiatedBodyTextSection(override val text: Option[NonemptyString]) extends BodyTextSection {
	val sectionType = GeneralBodyText
	val header      = None
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

case object TechnicalField extends BodyTextSectionType

object CitationUtils {

	implicit def docTypeToOption(a: DocType) = Some(a)

	implicit def languageToOption(a: Language) = Some(a)

	implicit def containmentInfoToOption(a: ContainmentInfo) = Some(a)

	implicit def institutionToOption(a: Institution) = Some(a)
}

trait Keyword {
	val authority: Option[KeywordAuthority] = None
	val word: NonemptyString
}

// don't model hierarchical keywords, just leave them slash-delimited in the string
case class BasicKeyword(override val word: NonemptyString, override val authority: Option[KeywordAuthority] = None) extends Keyword {
	require(!word.contains("\n"))
	require(!word.contains("\t"))
}

sealed abstract class DocType(parent: Option[DocType] = None)

case object JournalArticle extends DocType

case object ResearchArticle extends DocType(Some(JournalArticle))

case object ReviewArticle extends DocType(Some(JournalArticle))

case object NoteArticle extends DocType(Some(JournalArticle))

case object Biographical extends DocType(Some(JournalArticle))

case object Correction extends DocType(Some(JournalArticle))

case object Bibliography extends DocType(Some(JournalArticle))

case object Editorial extends DocType(Some(JournalArticle))

case object Letter extends DocType(Some(JournalArticle))

// possibly ambiguous: a Science "letter" is really a ResearchArticle, not a letter to the editor
case object CriticalReview extends DocType

// theater, music, etc.
case object BookReview extends DocType(Some(CriticalReview))

case object ProductReview extends DocType

case object Creative extends DocType

// poetry, fiction etc.
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

case object Other extends DocType

sealed class Country

trait Identifier {
	val authority: Option[IdentifierAuthority] = None
	val value: NonemptyString

	def qualifiedValue = authority.map(_.shortName).getOrElse("Unknown") + ":" + value

	override def toString = qualifiedValue
}

object BasicIdentifier {
	def apply(value: Option[NonemptyString], authority: Option[IdentifierAuthority] = None): Option[BasicIdentifier] =
		value.map(new BasicIdentifier(_, authority))
}

case class BasicIdentifier(override val value: NonemptyString, override val authority: Option[IdentifierAuthority]) extends Identifier

trait Location {
	// are there other kinds of locations?  e.g., call numbers
	val hashes: Seq[Hash]
  def +(s:String) : Location
}

trait UrlLocation extends Location {
	val url: URL

	override def toString: String = url.toExternalForm
}

trait StringLocation extends Location {
	val name: NonemptyString

	override def toString: String = name
}

case class BasicUrlLocation(override val url: URL, override val hashes: Seq[Hash]) extends UrlLocation
{
  override def +(s:String) : Location = BasicUrlLocation(new URL(url,s),Nil)
}

case class BasicStringLocation(override val name: NonemptyString, override val hashes: Seq[Hash]) extends StringLocation
{
  override def +(s:String) : Location = BasicStringLocation((name + "#" + s).n ,Nil)
}

trait Hash {
	val hashType : HashType
	val hashValue: NonemptyString
}

case class BasicHash(override val hashType: HashType, override val hashValue: NonemptyString) extends Hash

sealed class HashType

case object CRC32 extends HashType

case object SHA1 extends HashType

case object SHA256 extends HashType

case object MD5 extends HashType

/**
 * We don't model issues in their own right (in which case volume and issue numbers would have to go in StructuredCitation).
 * Instead the container is considered to be the journal as a whole, and the volume and issue numbers act similarly to page numbers.
 */
trait ContainmentInfo {
	val container: StructuredCitation
	val series   : Option[NonemptyString]
	val volume   : Option[NonemptyString]
	// journal number, or chapter number.  Not necessarily integer?
	val number   : Option[NonemptyString]
	val pages    : Option[PageRange]
}

case class BasicContainmentInfo(override val container: StructuredCitation, override val series: Option[NonemptyString],
                                override val volume: Option[NonemptyString], override val number: Option[NonemptyString], override val pages: Option[PageRange])
		extends ContainmentInfo

object PageRange extends Logging {
	def apply(start: NonemptyString, end: Option[NonemptyString]): PageRange = {
		try {
			new BasicNormalPageRange(start.s.toInt, end.map(_.s.toInt))
		}
		catch {
			case e: NumberFormatException => new BasicStringPageRange(start, end)
		}
	}

	def apply(s: String): Option[PageRange] = {
		val tokens = s.split("-").map(_.trim.opt).filter(_.isDefined)
		tokens.size match {
			case 0 => None
			case 1 => Some(PageRange(tokens(1).get, None))
			case 2 => Some(PageRange(tokens(1).get, tokens(2)))
			case _ => {
				logger.warn("Can't parse page range: " + s)
				None
			}
		}
	}
}

trait PageRange {
	def numPages: Option[Int]
}

trait NormalPageRange extends PageRange {
	val start: Int
	val end: Option[Int] = None

	def numPages = end.map(_ - start + 1).filter(_ > 0)

	override def toString = start + end.map("-" + _).getOrElse("")
}

case class BasicNormalPageRange(override val start: Int, override val end: Option[Int]) extends NormalPageRange

trait StringPageRange extends PageRange {
	val start: NonemptyString
	val end: Option[NonemptyString] = None

	def numPages = None

	override def toString = start + end.map("-" + _).getOrElse("")
}

case class BasicStringPageRange(override val start: NonemptyString, override val end: Option[NonemptyString]) extends StringPageRange

/**
 * Enumeration of types of events that may occur in the lifecycle of a document.
 *
 * @param primaryPriority indicates the order in which entries should be consulted to determine the "primary" date of the document.  A value of zero
 *                        indicates that the date cannot be primary.
 */
sealed class EventType(val primaryPriority: Int, val shortName: NonemptyString)

case object Received extends EventType(6, "rec".n)

case object Revised extends EventType(7, "rev".n)

case object Epub extends EventType(8, "epub".n)

case object Published extends EventType(9, "pub".n)

case object Retracted extends EventType(0, "retr".n)

case object Captured extends EventType(0, "capt".n)

case object Begin extends EventType(10, "beg".n)

// for grants, patents
case object End extends EventType(0, "end".n)

trait CitationEvent {
	val eventType: EventType
	val date     : Option[PartialDate] // possibly we know that a given event type occurred but we don't know when
}

case class BasicCitationEvent(override val date: Option[PartialDate], override val eventType: EventType) extends CitationEvent

trait PartialDate extends Ordered[PartialDate] {
	// this could be a lot fancier
	val year : Option[Int]
	val month: Option[Int]
	val day  : Option[Int]

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
	val grant   : StructuredCitation
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
