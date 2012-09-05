package edu.umass.cs.iesl.bibmogrify.reader

import edu.umass.cs.iesl.scalacommons.DateUtils._
import edu.umass.cs.iesl.bibmogrify.model._
import com.weiglewilczek.slf4s.Logging
import edu.umass.cs.iesl.bibmogrify.model.Authorities._
import edu.umass.cs.iesl.bibmogrify.pipeline.Transformer
import xml.Node
import collection.immutable.Seq
import edu.umass.cs.iesl.bibmogrify.{NamedInputStream, NamedPlugin, BibMogrifyException}
import edu.umass.cs.iesl.scalacommons.{NonemptyString, StringUtils, XmlUtils}
import edu.umass.cs.iesl.namejuggler.PersonNameWithDerivations
import StringUtils._
import RichAddress._
import edu.umass.cs.iesl.bibmogrify.model.DocType

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

	def parseDroppingErrors(inLocation: Location, doc: Node): Option[StructuredCitation] = {
		try {
			val c = parse(inLocation, doc)
			Some(c)
		}
		catch {
			case e: BibMogrifyException => logger.error(e.getMessage)
			None
		}
	}

	def parse(inLocation: Location, doc: Node): StructuredCitation = {

		val issue = doc \ "issue"
		val item = doc \ "item"

		val subjectCodes = ((issue \ "subjects" \ "subject" \ "@code")).flatMap(_.text.opt)


		val date: Some[BasicPartialDate] = {
			val month: Option[NonemptyString] = None //(doc \ "bib_date" \ "@month").text
			val yearS: Option[NonemptyString] = (item \ "bib_issue" \ "@year").text
			val year: Option[Int] = yearS.map(_.s.toInt)
			// val dayS: Option[String] = (doc \ "day").text
			// val day: Option[Int] = dayS.map(_.toInt)

			Some(BasicPartialDate(year, month.map(parseMonthOneBased(_)), None))
		}

		val venueMention = new StructuredCitation {
			// drop superscripts, subscripts, italics, and typewriter styles
			override val title: Option[NonemptyString] = (item \ "source_title").text

			// todo interpret pubtype field in associated issue
			//val doctype = Journal
		}

		//val authorSplit = "(.+)( .*)? (.+)".r
		val c = new StructuredCitation() {
			// todo interpret pubtype field
			//val doctype = JournalArticle
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
					val collective = PersonNameWithDerivations((x \ "AuCollectiveName").text)

					val mergedName = PersonNameWithDerivations.merge(assembled, collective)

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
						override val name  = Some(PersonNameWithDerivations((x \ "name").text))
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

			override val consensusInstitutionType = {
				val allEmailTypes = authors.map(_.agent).flatMap(_.email).flatMap(InstitutionType.infer(_)).toSet
				val allAddresses = addresses ++ authors.map(_.agent).flatMap(_.addresses)
				val foundInstitutionTypes = allAddresses.flatMap(_.inferredInstitutionType).toSet ++ allEmailTypes
				foundInstitutionTypes.size match {
					case 0 => None
					case 1 => foundInstitutionTypes.headOption
					case _ => Some(Mixed)
				}
			}

			// TODO implement parsePages, or just store the string
			def parsePages(s: String): Option[PageRange] = None

			override val containedIn = Some(BasicContainmentInfo(venueMention, None, None, None, None))

			override val keywords = subjectCodes map (new BasicKeyword(_, WosKeywordAuthority))

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
	                                                      "Meeting Abstract" -> Proceedings, "Proceedings Paper" -> ProceedingsArticle, "Book" -> Book)

	/*

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
	 */
	private def decodeDocType(s: String): Option[DocType] = knownDocTypes.get(s).orElse({logger.warn("Unknown DocType: " + s); None})
}
