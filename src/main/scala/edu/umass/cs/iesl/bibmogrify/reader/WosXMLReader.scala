package edu.umass.cs.iesl.bibmogrify.reader

import edu.umass.cs.iesl.scalacommons.DateUtils._
import edu.umass.cs.iesl.bibmogrify.model._
import com.typesafe.scalalogging.slf4j.Logging
import edu.umass.cs.iesl.bibmogrify.model.Authorities._
import edu.umass.cs.iesl.bibmogrify.pipeline.Transformer
import xml.Node
import collection.immutable.Seq
import edu.umass.cs.iesl.bibmogrify.{NamedInputStream, NamedPlugin, BibMogrifyException}
import edu.umass.cs.iesl.scalacommons.{StringUtils, XmlUtils}
import edu.umass.cs.iesl.namejuggler.PersonNameWithDerivations
import StringUtils._
import RichAddress._
import scala.Predef._
import edu.umass.cs.iesl.bibmogrify.model.BasicPersonIdentifier
import edu.umass.cs.iesl.bibmogrify.model.BasicCitationEvent
import scala.Some
import edu.umass.cs.iesl.bibmogrify.model.BasicContainmentInfo
import edu.umass.cs.iesl.bibmogrify.model.BasicKeyword
import edu.umass.cs.iesl.bibmogrify.model.BasicAddress
import edu.umass.cs.iesl.bibmogrify.model.AuthorInRole
import edu.umass.cs.iesl.scalacommons.NonemptyString
import edu.umass.cs.iesl.bibmogrify.model.BasicStringLocation
import edu.umass.cs.iesl.bibmogrify.model.BasicPartialDate
import edu.umass.cs.iesl.namejuggler.PersonNameParser.PersonNameParsingException

object WosXMLReader extends Transformer[NamedInputStream, StructuredCitation] with Logging with NamedPlugin {

	val name = "wosxml"

	def apply(nis: NamedInputStream): TraversableOnce[StructuredCitation] = new Traversable[StructuredCitation] {
		def foreach[U](f: (StructuredCitation) => U) {

			val s = nis.getInputStream
			val inLocation = new BasicStringLocation(nis.name, Nil)

			try {
				def ff(rec: Node) {
					assert(rec.label.equals("REC"))
					val result = parseDroppingErrors(inLocation, rec)
					result.foreach(f)
				}
				XmlUtils.firstLevelNodes(s).foreach(n => (n \\ "REC").foreach(ff))
			}
			catch {
				case e => {
					logger.error("Failed to parse " + nis.name, e)
					Nil
				}
			}
			finally {
				s.close()
			}
		}
	}

	def parseDroppingErrors(inLocation: Location, doc: Node): Seq[StructuredCitation] = {
		try {
			val c = parseRec(inLocation, doc)
			c
		}
		catch {
			case e: BibMogrifyException => {
				logger.error(e.getMessage)
				Nil
			}
			case e: PersonNameParsingException => {
				logger.error(e.getMessage)
				Nil
			}
		}
	}

	/**
	 * A rec may have multiple issues and items in it
	 * @param inLocation
	 * @param doc
	 * @return
	 */
	def parseRec(inLocation: Location, doc: Node): Seq[StructuredCitation] = {

		val issues = doc \ "issue"
		val items = doc \ "item"

		val issueRecords: Map[String, (StructuredCitation,Option[NonemptyString],Option[NonemptyString] )] =
			(for (issue <- issues) yield parseIssue(inLocation, issue)).toMap
		val itemRecords = for (item <- items) yield parseItem(inLocation, item, issueRecords)
		itemRecords
	}

	def parseIssue(inLocation: Location, issue: Node): (String, (StructuredCitation,Option[NonemptyString] ,Option[NonemptyString] )) = {

		val subjectNodes = (issue \ "subjects" \ "subject")
		val issueId = (issue \ "@recid").text
		//logger.debug("Found issue " + issueId + " with " + subjectNodes.size + " subject nodes")
		val subjectCodes = subjectNodes.flatMap(n => n \ "@code").flatMap(_.text.opt).toSet

		//logger.debug("Found issue " + issueId + " with subject codes " + subjectCodes.mkString(", "))
		val c = new StructuredCitation() {
			override val keywords: Set[Keyword]           = subjectCodes map (new BasicKeyword(_, WosKeywordAuthority))
			override val title   : Option[NonemptyString] = (issue \ "issue_title").text
		}

		val volumeNumber : Option[NonemptyString] = (issue \ "bib_vol" \ "@volume").text
		val issueNumber : Option[NonemptyString] = (issue \ "bib_vol" \ "@issue").text

		(issueId, (c,volumeNumber,issueNumber))
	}

	def parseItem(inLocation: Location, item: Node, issuesById: Map[String, (StructuredCitation,Option[NonemptyString] ,Option[NonemptyString] )]): StructuredCitation = {

		val issueRef = (item \ "@issue").text
		val issueInfo : Option[(StructuredCitation,Option[NonemptyString] ,Option[NonemptyString] )] = issuesById.get(issueRef)
		if (issueInfo.isEmpty) {
			logger.warn("Unresolved issue reference: " + issueRef + " for item " + (item \ "ut").text)
		}

		//else {
		//	logger.debug("Resolved issue reference " + issueRef + " with keywords " + venueMention.get.keywords.mkString(" "))
		//}
		val date: Some[BasicPartialDate] = {
			val month: Option[NonemptyString] = None //(doc \ "bib_date" \ "@month").text
			val yearS: Option[NonemptyString] = (item \ "bib_issue" \ "@year").text
			val year: Option[Int] = yearS.map(_.s.toInt)
			// val dayS: Option[String] = (doc \ "day").text
			// val day: Option[Int] = dayS.map(_.toInt)

			Some(BasicPartialDate(year, month.flatMap(parseMonthOneBased(_)), None))
		}

		val localVenueMention : Option[StructuredCitation] = (item \ "source_title").text.opt map (venueTitle => new StructuredCitation {
			// drop superscripts, subscripts, italics, and typewriter styles
			override val title: Option[NonemptyString] = Some(venueTitle)

			// todo interpret pubtype field in associated issue
			//val doctype = Journal
		})

		//val authorSplit = "(.+)( .*)? (.+)".r
		val c = new StructuredCitation() {

			// drop superscripts, subscripts, italics, and typewriter styles
			override val title: Option[NonemptyString] = (item \ "item_title").text
			override val dates                         = Seq(BasicCitationEvent(date, Published))

			override val abstractText: Iterable[TextWithLanguage] = TextWithLanguage(None, (item \ "abstract").text)

			// todo collect other identifiers?
			val wosUtId: String = (item \ "ut").text
			val wosDoi : String = (item \ "article_nos" \ "article_no").text

			override val identifiers = Seq(BasicIdentifier(wosUtId, WosUtAuthority), BasicIdentifier((item \ "@recid").text, WosRecidAuthority),
			                               BasicIdentifier((item \ "@refkey").text, WosRefkeyAuthority),
			                               BasicIdentifier((item \ "@refid").text, WosRefidAuthority), BasicIdentifier((item \ "i_cid").text,
			                                                                                                           WosCidAuthority),
			                               BasicIdentifier(wosDoi, DoiAuthority)).flatten

			override val authors = {
				val authorsNode = item \ "authors"

				// there are at least "primaryauthor" and/or "author" tags containing lastname, firstinitial
				val primaryAuthorNodes = (authorsNode \ "primaryauthor").filterNot(_.text.isEmpty)
				val basicAuthorNodes = (authorsNode \ "author").filterNot(_.text.isEmpty)

				val basicAuthors: Seq[AuthorInRole] = primaryAuthorNodes.map(x => {
					new AuthorInRole(new Person() {
						override val name       : Option[PersonNameWithDerivations] = x.text.opt.map(PersonNameWithDerivations(_))
						override val identifiers: Seq[PersonIdentifier]             = {
							val key = (x \ "@key").text
							if (!key.isEmpty) {
								Seq(new BasicPersonIdentifier(key, WosAuthorAuthority))
							}
							else Nil
						}
					}, List(FirstAuthor))
				}) ++ basicAuthorNodes.map(x => {
					new AuthorInRole(new Person() {
						override val name       : Option[PersonNameWithDerivations] = x.text.opt.map(PersonNameWithDerivations(_))
						override val identifiers: Seq[PersonIdentifier]             = {
							val key = (x \ "@key").text
							if (!key.isEmpty) {
								Seq(new BasicPersonIdentifier(key, WosAuthorAuthority))
							}
							else Nil
						}
					}, Nil)
				})

				// there may also be more detailed records with full names and addresses
				val fullAuthorNodes = authorsNode \ "fullauthorname"

				val fullAuthors = fullAuthorNodes.map(x => {
					val first = (x \ "AuFirstName").text
					val last = (x \ "AuLastName").text
					val assembled = new PersonNameWithDerivations() {
						override val givenNames = first.opt.toSeq
						override val surNames   = last.opt.toSet
					}
					val collective = (x \ "AuCollectiveName").text.opt.map(PersonNameWithDerivations(_))

					val mergedName = collective.map(PersonNameWithDerivations.merge(assembled, _)).getOrElse(assembled)

					new AuthorInRole(new Person() {
						override val name                    = Some(mergedName)
						// it is possible to have a completely empty PersonName
						override val addresses: Set[Address] = (x \ "address").map(f => BasicAddress(Seq(f.text))).toSet
					}, Nil)
				})

				// there may also be an email block
				/*
							<emails count="1">
				<email>
				<name>Crystal, RG</name>
				<email_addr>geneticmedicine@med.cornell.edu</email_addr>
				</email>
				</emails>m
							 */
				val emailsNode = item \ "emails"

				val emailAuthors = emailsNode.map(x => {
					new AuthorInRole(new Person() {
						override val name  = (x \ "name").text.opt.map(PersonNameWithDerivations(_))
						override val email = (x \ "email").text.opt
					}, Nil)
				})

				val m = basicAuthors.map(_.mergeWithMatching(fullAuthors)).map(_.mergeWithMatching(emailAuthors))

				assert(m.head.agent.hasName)

				m
			}

			override val addresses = {
				val addrNodes = item \ "research_addrs" \ "research"
				for (addr <- addrNodes) yield {
					val streetlines = addrNodes.map(n => {
						(n \ "rs_address").text
					})
					BasicAddress(streetlines)
				}
			}

			override val doctype    = decodeDocType((item \ "doctype").text)
			override val docSubtype = (item \ "doctype").text.opt

			override val institutionTypes: Set[InstitutionType] = {
				val allEmails = authors.map(_.agent).flatMap(_.email)
				val allAddresses = addresses ++ authors.map(_.agent).flatMap(_.addresses)

				if (allEmails.isEmpty && allAddresses.isEmpty) {Set.empty}
				else {
					val allEmailTypes = allEmails.flatMap(InstitutionType.infer(_)).toSet
					val allAddressTypes: Set[InstitutionType] = allAddresses.flatMap(_.inferredInstitutionType).toSet
					val result = allAddressTypes ++ allEmailTypes
					if (result.isEmpty) {Set[InstitutionType](Ambiguous)}
					else result
				}
			}

			override val containedIn = {

				val pages: Option[PageRange] = {
					val b = item \ "bib_pages"
					val providedStringRange = b.text
					val start = (b \ "@begin").text.opt
					val result: Option[PageRange] = start.map(s => {
						val end = (b \ "@end").text.opt
						val inferredStringRange = s + end.map("-" + _).getOrElse("")
						if (providedStringRange != inferredStringRange) {logger.warn(providedStringRange + " != " + inferredStringRange)}

						Some(PageRange(s, end))
					}).getOrElse(PageRange(providedStringRange))
					result
				}

				val localIssueInfo = localVenueMention.map((_, (item \ "bib_issue" \ "@vol").text.opt, None))
				issueInfo.orElse(localIssueInfo).map({

					case (venueMention:StructuredCitation, volumeNumber:Option[NonemptyString], issueNumber:Option[NonemptyString]) =>

					BasicContainmentInfo(venueMention, None, volumeNumber, issueNumber, pages)
				})
			}

			//override val keywords = issueKeywords  // subjectCodes map (new BasicKeyword(_, WosKeywordAuthority))
			override val locations = Seq(inLocation)

			override val references = (item \ "refs" \ "ref").zipWithIndex.map(parseRef(_, wosUtId))
		}
		c
	}

	def parseRef(nodeWithIndex: (Node, Int), idBasis: String): StructuredCitation = new StructuredCitation {

		val (node, index) = nodeWithIndex

		override val authors: Seq[AuthorInRole] = Seq(new AuthorInRole(new Person() {
			override val name: Option[PersonNameWithDerivations] = ((node \ "@auth").text).opt.map(n => PersonNameWithDerivations(n))
		}, List(FirstAuthor)))

		val venueMention = new StructuredCitation {
			override val title: Option[NonemptyString] = (node \ "@work").text
		}
		override val containedIn = {
			val pr = ((node \ "@page").text).opt.map(st => new StringPageRange() {
				override val start: NonemptyString = st
			})

			Some(BasicContainmentInfo(venueMention, None, (node \ "vol").text, None, pr))
		}
		val date: Some[BasicPartialDate] = {
			val yearS: Option[NonemptyString] = (node \ "@year").text
			val year: Option[Int] = try {
				yearS.map(_.s.toInt)
			}
			catch {case e: NumberFormatException => None}
			Some(BasicPartialDate(year, None, None))
		}
		override val dates       = Seq(BasicCitationEvent(date, Published))
		override val identifiers = Seq(BasicIdentifier(node.text), BasicIdentifier(idBasis + "-ref-" + index, WosUtRefIndexAuthority),
		                               BasicIdentifier((node \ "@recid").text, WosRecidAuthority), BasicIdentifier((node \ "@refkey").text,
		                                                                                                           WosRefkeyAuthority),
		                               BasicIdentifier((node \ "@refid").text, WosRefidAuthority), BasicIdentifier((node \ "@cid").text, WosCidAuthority),
		                               BasicIdentifier((node \ "@artno").text, DoiAuthority)).flatten
	}

	private val knownDocTypes: Map[String, DocType] = Map("Article" -> JournalArticle, "Review" -> JournalArticle, "Book Review" -> JournalArticle,
	                                                      "Meeting Abstract" -> ProceedingsArticle, "Meeting Abstr" -> ProceedingsArticle,
	                                                      "Proceedings Paper" -> ProceedingsArticle, "Book" -> Book, "Art Exhibit Review" -> CriticalReview,
	                                                      "Biographical-Item" -> Biographical, "Book Review" -> BookReview, "Chronology" -> Other,
	                                                      "Correction, Addition" -> Correction, "Correction" -> Correction,
	                                                      "Dance Performance Review" -> CriticalReview, "Discussion" -> Editorial,
	                                                      "Editorial Material" -> Editorial, "Excerpt" -> Other, "Fiction, Creative Prose" -> Creative,
	                                                      "Film Review" -> CriticalReview, "Hardware Review" -> ProductReview,
	                                                      "Item About an Individual" -> Biographical, "Letter" -> Letter,
	                                                      "Music Performance Review" -> CriticalReview, "Note" -> NoteArticle, "Poetry" -> Creative,
	                                                      "Record Review" -> CriticalReview, "Review" -> ReviewArticle, "Script" -> Creative,
	                                                      "Software Review" -> ProductReview, "Database Review" -> ProductReview,
	                                                      "TV Review, Radio Review, Video" -> CriticalReview, "Music Score Review" -> CriticalReview,
	                                                      "Theater Review" -> CriticalReview, "Bibliography" -> Bibliography)

	/*


case object JournalArticle extends DocType

case object ResearchArticle extends DocType(Some(JournalArticle))

case object ReviewArticle extends DocType(Some(JournalArticle))

case object NoteArticle extends DocType(Some(JournalArticle))

case object Biographical extends DocType(Some(JournalArticle))

case object Correction extends DocType(Some(JournalArticle))

case object Letter extends DocType(Some(JournalArticle))  // possibly ambiguous: a Science "letter" is really a ResearchArticle, not a letter to the editor

case object CriticalReview extends DocType  // theater, music, etc.

case object BookReview  extends DocType(Some(CriticalReview))

case object ProductReview extends DocType

case object Creative extends DocType  // poetry, fiction etc.

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

	 */
	/* From Wos data:

	Art Exhibit Review
	Article
	Biographical-Item
	Book Review
	Chronology
	Correction, Addition
	Dance Performance Review
	Discussion
	Editorial Material
	Excerpt
	Fiction, Creative Prose
	Film Review
	Hardware Review
	Item About an Individual
	Letter
	Meeting Abstr
	Meeting Abstract
	Music Performance Review
	Note
	Poetry
	Record Review
	Review
	Script
	Software Review
	Theater Review
	Bibliography
	Proceedings Paper

	 */
	private def decodeDocType(s: String): Option[DocType] = knownDocTypes.get(s).orElse({logger.warn("Unknown DocType: " + s); None})
}
