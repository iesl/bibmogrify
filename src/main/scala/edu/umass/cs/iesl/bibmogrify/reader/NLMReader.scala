package edu.umass.cs.iesl.bibmogrify.reader

import edu.umass.cs.iesl.scalacommons.StringUtils._
import com.weiglewilczek.slf4s.Logging
import edu.umass.cs.iesl.bibmogrify.model._
import edu.umass.cs.iesl.bibmogrify.model.Authorities._
import edu.umass.cs.iesl.bibmogrify.model.CitationUtils._
import xml.{NodeSeq, Node}
import edu.umass.cs.iesl.bibmogrify.pipeline.Transformer
import edu.umass.cs.iesl.bibmogrify.{NamedInputStream, NamedPlugin, BibMogrifyException}
import edu.umass.cs.iesl.scalacommons.XMLIgnoreDTD

/**
 * @author <a href="mailto:dev@davidsoergel.com">David Soergel</a>
 * @version $Id$
 */
object NLMReader extends Transformer[NamedInputStream, StructuredCitation] with Logging with NamedPlugin
	{

	val name = "nlm"

	//def apply(s: InputStream): TraversableOnce[CitationMention] = XmlUtils.firstLevelNodes(s).flatMap(node => (node \ "publication").flatMap
	// (parsePublication(_)))
	def apply(nis: NamedInputStream): TraversableOnce[StructuredCitation] =
		{

		val a = nis.getInputStream
		val inLocation = new BasicStringLocation(nis.name, Nil)

		try
		{
		//XmlUtils.firstLevelNodes(s).flatMap(node => (node \ "@{http://www.elsevier.com/xml/document/schema}document").flatMap(parseDroppingErrors(url, _)))
		//XmlUtils.firstLevelNodes(s).flatMap(parseDroppingErrors(url, _))
		parseDroppingErrors(inLocation, XMLIgnoreDTD.load(a))
		}
		catch
		{
		case e =>
			{
			logger.error("Failed to parse " + nis.name, e);
			Nil
			}
		}
		finally
			{
			a.close()
			}
		}

	def parseDroppingErrors(inLocation: Location, doc: Node): Option[StructuredCitation] =
		{
		try
		{
		//logger.debug(doc.toString())
		val c = parse(inLocation, doc)
		Some(c)
		}
		catch
		{
		case e: BibMogrifyException =>
			{
			logger.error(e.getMessage)
			None
			}
		}
		}

	def parse(inLocation: Location, doc: Node): StructuredCitation =
		{

		val front: NodeSeq = doc \ "front"
		val articlemeta: NodeSeq = front \ "article-meta"
		val back: NodeSeq = doc \ "back"

		val date: Some[BasicPartialDate] =
			{
			//val month: Option[String] = None //(doc \ "bib_date" \ "@month").text
			val dates: NodeSeq = articlemeta \ "pub-date"
			def isCollectionDate(d: Node): Boolean =
				{
				val atts: Option[Seq[Node]] = d.attribute("pub-type")
				val atts2: Option[Seq[Node]] = atts.filter(_.text == "collection")
				atts2.isDefined
				}
			val datesCollection: NodeSeq = dates.filter(isCollectionDate)
			val yearS: Option[String] = (datesCollection \ "year").text
			val year: Option[Int] = yearS.map(_.toInt)
			// val dayS: Option[String] = (doc \ "day").text
			// val day: Option[Int] = dayS.map(_.toInt)

			Some(BasicPartialDate(year, None, None))
			}

		val journalMention = new StructuredCitation
			{
			// drop superscripts, subscripts, italics, and typewriter styles
			override val title: Option[String] = (front \ "journal-meta" \ "journal-title").text.trim

			// todo interpret pubtype fieldin associated issue
			override val doctype: Option[DocType] = Journal
			}

		val parseReferences = (back \ "ref-list" \ "ref") map (ref =>
			{
			val idNodes = (ref \ "citation" \ "pub-id")
			new StructuredCitation()
				{
				override val identifiers = {
				val pmidNodes: NodeSeq = idNodes.filter(_.attribute("pub-id-type").filter(_.text == "pmid").isDefined)
				val doiNodes: NodeSeq = idNodes.filter(_.attribute("pub-id-type").filter(_.text == "doi").isDefined)

				val pmids = pmidNodes.map(pm => BasicIdentifier(pm.text, PubmedAuthority))
				val dois = doiNodes.map(pm => BasicIdentifier(pm.text, DoiAuthority))

				pmids ++ dois
				}

				override val refMarker: Option[String] = (ref \ "label").text

				// ** sometimes these are tagged; we just ignore that for now
				override val unstructuredString: Option[String] = (ref \ "citation").filter(_.label != "pub-id").text
				}
			})

		//val authorSplit = "(.+)( .*)? (.+)".r
		val idNodes = (articlemeta \ "article-id")
		val c = new StructuredCitation()
			{
			// todo interpret pubtype field
			override val doctype: Option[DocType] = JournalArticle

			// drop superscripts, subscripts, italics, and typewriter styles
			override val title: Option[String] = (articlemeta \ "title-group" \ "article-title").text.trim
			override val dates                 = Seq(BasicCitationEvent(date, Published))

			override val abstractText = Seq(new TextWithLanguage(None, (articlemeta \ "abstract").text.trim))
			override val identifiers =
				{
				val pmidNodes: NodeSeq = idNodes.filter(_.attribute("pub-id-type").filter(_.text == "pmid").isDefined)
				val doiNodes: NodeSeq = idNodes.filter(_.attribute("pub-id-type").filter(_.text == "doi").isDefined)

				val pmids = pmidNodes.map(pm => BasicIdentifier(pm.text, PubmedAuthority))
				//val dois = doiNodes.map(pm => BasicIdentifier(pm.text, DoiAuthority))

				pmids
				}

			// TODO implement parsePages, or just store the string
			def parsePages(s: String): Option[PageRange] = None

			val volume = (articlemeta \ "volume").text
			override val containedIn = Some(BasicContainmentInfo(journalMention, None, volume, None, None))

			//override val keywords = subjectCodes map (new BasicKeyword(WOSKeywordAuthority, _))
			override val locations = Seq(inLocation)
			override val authors   = (articlemeta \ "contrib-group" \ "contrib" \ "name").map((c => new Person()
				{
				override val name = Some((c \ "given-names").text + " " + (c \ "surname").text)
				})).map(new AuthorInRole(_, Nil))

			override val references = parseReferences
			}
		c
		}
	}
