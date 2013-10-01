package edu.umass.cs.iesl.bibmogrify.reader

import edu.umass.cs.iesl.scalacommons.DateUtils._
import edu.umass.cs.iesl.bibmogrify.model._
import edu.umass.cs.iesl.bibmogrify.model.Authorities._
import edu.umass.cs.iesl.bibmogrify.model.CitationUtils._
import com.typesafe.scalalogging.slf4j.Logging
import xml.Node
import java.net.URL
import edu.umass.cs.iesl.bibmogrify.pipeline.Transformer
import edu.umass.cs.iesl.bibmogrify.{NamedInputStream, NamedPlugin, BibMogrifyException}
import edu.umass.cs.iesl.scalacommons.{StringUtils, NonemptyString, XmlUtils}

object DBLPReader extends Transformer[NamedInputStream, StructuredCitation] with Logging with NamedPlugin
	{

	import ReaderUtils._
	import StringUtils._

	val name    = "dblp"
	val baseUrl = "http://www.informatik.uni-trier.de/∼ley/db/"

	def parse(inLocation: Location, doc: Node): StructuredCitation =
		{

		val doctypeX: DocType = doc.label match
		{
			case "article" => JournalArticle
			case "inproceedings" => ProceedingsArticle
			case "proceedings" => Proceedings
			case "book" => Book
			case "incollection" => CollectionArticle
			case "collection" => CollectionOfArticles // not in the dblp dtd, but should be??
			case "phdthesis" => PhdThesis
			case "mastersthesis" => MastersThesis
			case "www" => WwwArticle
			case _ =>
				{
				throw new BibMogrifyException("Unknown Doc Type: " + doc.label)
				}
		}


		val key = (doc \ "@key").text
		val id = BasicIdentifier(key, DblpAuthority)

		val authorsX: Seq[NonemptyString] = (doc \ "author").flatMap(_.text.opt)

		val editorsX: Seq[NonemptyString] = (doc \ "editor").flatMap(_.text.opt)

		// todo modify dtd to constrain some fields to appear only once?
		val date: Some[BasicPartialDate] =
			{
			val month: Option[NonemptyString] = (doc \ "month").text
			val yearS: Option[NonemptyString] = (doc \ "year").text
			val year: Option[Int] = yearS.map(_.s.toInt)
			val dayS: Option[NonemptyString] = (doc \ "day").text
			val day: Option[Int] = dayS.map(_.s.toInt)

			Some(BasicPartialDate(year, month.flatMap(parseMonthOneBased(_)), day))
			}

		val volume: Option[NonemptyString] = (doc \ "volume").text
		val series: Option[NonemptyString] = (doc \ "series").text
		val pages: Option[NonemptyString] = (doc \ "pages").text
		val number: Option[NonemptyString] = (doc \ "number").text

		val url: Option[NonemptyString] = (doc \ "url").text

		val ee: Option[NonemptyString] = (doc \ "ee").text
		// http://www.informatik.uni-trier.de/~ley/
		val address: Option[NonemptyString] = (doc \ "address").text

		// todo should the address be associated with the first author?
		val journalMention = new StructuredCitation
			{
			// drop superscripts, subscripts, italics, and typewriter styles
			override val title  : Option[NonemptyString] = (doc \ "journal").text
			override val doctype: Option[DocType]        = Journal
			}

		//val authorSplit = "(.+)( .*)? (.+)".r
		val c = new StructuredCitation()
			{
			override val doctype: Option[DocType] = doctypeX

			// drop superscripts, subscripts, italics, and typewriter styles
			override val title: Option[NonemptyString] = (doc \ "title").text
			override val authors                       = authorsX map (x => new AuthorInRole(Person(x), Nil))
			override val otherContributors             = editorsX map (x => new OtherContributorInRole(Person(x), List(Editor)))
			override val dates                         = Seq(BasicCitationEvent(date, Published))

			override val abstractText: Iterable[TextWithLanguage] = TextWithLanguage(None, (doc \ "abstract").stripTags)

			override val identifiers: Iterable[Identifier] = id

			// TODO implement parsePages, or just store the string
			def parsePages(s: String): Option[PageRange] = None

			override val containedIn = Some(BasicContainmentInfo(journalMention, series, volume, number, pages.flatMap(parsePages(_))))

			val loc = url.map(u =>
				                  {
				                  val uu = if (u.contains(":")) u.s else (baseUrl + u)
				                  new BasicUrlLocation(new URL(uu), Nil)
				                  })

			override val locations = Seq(inLocation) ++ loc
			}
		c
		}

	def parseDroppingErrors(inLocation: Location, doc: Node): Option[StructuredCitation] =
		{
		try
		{
		val c = parse(inLocation, doc)
		Some(c)
		}
		catch
		{
		case e: BibMogrifyException => logger.error(e.getMessage)
		None
		}
		}

	def apply(nis: NamedInputStream): TraversableOnce[StructuredCitation] =
		{
		//val xml = scala.xml.XML.load(f)
		// val xml = XMLIgnoreDTD.load(f)  // can't, because we need the entity declarations
		//XMLMapDTD.setGlobalXMLCatalogDir(getClass.getResource("/dblp.dtd").getPath)
		//val xmlloader = new XMLFilenameOnlyMappingDTDLoader(Map("dblp.dtd" -> new InputSource(getClass.getResource("/dblp.dtd").getPath)))
		// val xml = xmlloader.load(f)
		//XmlUtils.firstLevelNodes(s).flatMap(node => (node \\ "REC").flatMap(parseDroppingErrors(_)))
		val s = nis.getInputStream
		val inLocation = new BasicStringLocation(nis.name, Nil)
		try
		{
		XmlUtils.firstLevelNodes(s).flatMap(parseDroppingErrors(inLocation, _))
		}
		catch
		{
		case e =>
			{logger.error("Failed to parse " + nis.name, e); Nil}
		}
		finally
			{
			s.close()
			}
		}
	}
