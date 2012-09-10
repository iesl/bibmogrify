package edu.umass.cs.iesl.bibmogrify.reader

import com.weiglewilczek.slf4s.Logging
import edu.umass.cs.iesl.bibmogrify.model._
import edu.umass.cs.iesl.bibmogrify.model.Authorities._
import edu.umass.cs.iesl.bibmogrify.model.CitationUtils._
import xml.{XML, NodeSeq, Node}
import edu.umass.cs.iesl.bibmogrify.pipeline.Transformer
import edu.umass.cs.iesl.bibmogrify.{NamedInputStream, NamedPlugin, BibMogrifyException}
import edu.umass.cs.iesl.scalacommons.StringUtils._
import edu.umass.cs.iesl.scalacommons.NonemptyString

/**
 * @author <a href="mailto:dev@davidsoergel.com">David Soergel</a>
 * @version $Id$
 */
object ElsevierReader extends Transformer[NamedInputStream, StructuredCitation] with Logging with NamedPlugin
	{

	import ReaderUtils._

	val name = "elsevier"

	//def apply(s: InputStream): TraversableOnce[CitationMention] = XmlUtils.firstLevelNodes(s).flatMap(node => (node \ "publication").flatMap
	// (parsePublication(_)))
	def apply(nis: NamedInputStream): TraversableOnce[StructuredCitation] =
		{

		val s = nis.getInputStream
		val inLocation = new BasicStringLocation(nis.name, Nil)

		try
		{
		//XmlUtils.firstLevelNodes(s).flatMap(node => (node \ "@{http://www.elsevier.com/xml/document/schema}document").flatMap(parseDroppingErrors(url, _)))
		//XmlUtils.firstLevelNodes(s).flatMap(parseDroppingErrors(url, _))
		parseDroppingErrors(inLocation, XML.load(s))
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
			s.close()
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
		case e: BibMogrifyException => logger.error(e.getMessage); None
		case f =>
			{
			logger.error("Could not parse " + inLocation);
			logger.error(f.getMessage)
			};
			None
		}
		}

	def parse(inLocation: Location, doc: Node): StructuredCitation =
		{

		//val doc = docp \ "@{http://www.elsevier.com/xml/document/schema}document"
		// val rdf = doc \ "@{http://www.w3.org/1999/02/22-rdf-syntax-ns#}RDF" \ "@{http://www.w3.org/1999/02/22-rdf-syntax-ns#}Description"
		val rdfp: NodeSeq = doc \ "RDF"

		// val rdfb: NodeSeq = doc \ "rdf:RDF"
		// val rdfp: NodeSeq = doc \ "@{http://www.w3.org/1999/02/22-rdf-syntax-ns#}RDF"
		val rdf = rdfp \ "Description"

		val date: Some[BasicPartialDate] =
			{
			//val month: Option[String] = None //(doc \ "bib_date" \ "@month").text
			val yearS: Option[NonemptyString] = (rdf \ "coverDate").text
			val year: Option[Int] = yearS.map(_.s.toInt)
			// val dayS: Option[String] = (doc \ "day").text
			// val day: Option[Int] = dayS.map(_.toInt)

			Some(BasicPartialDate(year, None, None))
			}

		val journalMention = new StructuredCitation
			{
			// drop superscripts, subscripts, italics, and typewriter styles
			override val title: Option[NonemptyString] = (rdf \ "publicationName").text.trim

			// todo interpret pubtype fieldin associated issue
			override val doctype: Option[DocType] = Journal
			}

		//val authorSplit = "(.+)( .*)? (.+)".r
		val c = new StructuredCitation()
			{
			// todo interpret pubtype field
			override val doctype: Option[DocType] = JournalArticle

			// drop superscripts, subscripts, italics, and typewriter styles
			override val title: Option[NonemptyString] = (rdf \ "title").text.trim
			override val dates                         = Seq(BasicCitationEvent(date, Published))

			override val abstractText : Iterable[TextWithLanguage]= TextWithLanguage(None, (doc \ "converted-article" \ "head" \ "abstract" \ "abstract-sec")
			                                                                       .stripTags)

			override val identifiers : Iterable[Identifier]= BasicIdentifier((rdf \ "doi").text, DoiAuthority)

			// TODO implement parsePages, or just store the string
			def parsePages(s: String): Option[PageRange] = None

			val volume = (rdf \ "volume").text
			override val containedIn = Some(BasicContainmentInfo(journalMention, None, volume, None, None))

			//override val keywords = subjectCodes map (new BasicKeyword(WOSKeywordAuthority, _))
			override val locations = Seq(inLocation)
			override val authors   = (rdf \ "creator" \ "Seq" \ "li").flatMap(nameS => nameS.text.opt).map(n => new AuthorInRole(Person(n), Nil))
			}
		c
		}
	}
