package edu.umass.cs.iesl.bibmogrify.reader

import edu.umass.cs.iesl.scalacommons.StringUtils._
import edu.umass.cs.iesl.bibmogrify.model.CitationUtils._
import edu.umass.cs.iesl.bibmogrify.model.Authorities._
import edu.umass.cs.iesl.scalacommons.DateUtils._
import edu.umass.cs.iesl.bibmogrify.model._
import com.weiglewilczek.slf4s.Logging
import xml.Node
import java.lang.String
import edu.umass.cs.iesl.bibmogrify.pipeline.Transformer
import edu.umass.cs.iesl.bibmogrify.{NamedInputStream, NamedPlugin, BibMogrifyException}
import edu.umass.cs.iesl.scalacommons.{NonemptyString, XMLIgnoreDTD}
import edu.umass.cs.iesl.namejuggler.PersonNameWithDerivations

object IEEEReader extends Transformer[NamedInputStream, StructuredCitation] with Logging with NamedPlugin
	{

	import ReaderUtils._

	val name = "ieee"

	def parse(doc: Node, journalMention: StructuredCitation, volume: Option[NonemptyString], date: Some[BasicPartialDate]): StructuredCitation =
		{
		val c = new StructuredCitation()
			{
			// todo interpret pubtype field
			override val doctype: Option[DocType] = JournalArticle

			override val title: Option[NonemptyString] = (doc \ "title").text
			override val dates                         = Seq(BasicCitationEvent(date, Published))
			override val abstractText                  = Seq(TextWithLanguage(None, (doc \ "articleinfo" \ "abstract").stripTags))
			override val identifiers                   =
				{
				val id: String = (doc \ "articleinfo" \ "articledoi").text.trim
				if (id.isEmpty)
					{
					Nil
					}
				else
					{
					List(new Identifier
						{
						override val authority = Some(DoiAuthority)
						override val value     = id
						})
					}
				}
			override val containedIn                   = Some(BasicContainmentInfo(journalMention, None, volume, None, None))
			override val authors                       = (doc \ "articleinfo" \ "authorgroup" \ "author").map((c =>
				new Person()
					{
					override val name =
						{
						val first: String = (c \ "firstname").text
						val last: String = (c \ "surname").text
						val norm: String = (c \ "normname").text
						if (first == last && last == norm) // broken name records, especially asian
							{
							Some(PersonNameWithDerivations(norm))
							}
						else
							{
							Some(new PersonNameWithDerivations
								{
								override val givenNames: Seq[NonemptyString] = emptyStringToNone(first).toSeq
								override val surNames  : Set[NonemptyString] = emptyStringToNone(last).toSet

								//** just ignore normname for now
								override val fullNames: Set[NonemptyString] = emptyStringToNone(first + " " + last).toSet
								})
							}
						}
					})).map(new AuthorInRole(_, Nil))
			}
		c
		}

	def parsePublication(pub: Node): TraversableOnce[StructuredCitation] =
		{
		// assume only one volume
		val journalMention = new StructuredCitation
			{
			override val title: Option[NonemptyString] = (pub \ "title").text

			// todo interpret pubtype field
			override val doctype: Option[DocType] = Journal
			}

		val date: Some[BasicPartialDate] =
			{
			val month: Option[NonemptyString] = None //(doc \ "bib_date" \ "@month").text
			val yearS: Option[NonemptyString] = (pub \ "volume" \ "volumeinfo" \ "year").text
			val year: Option[Int] = yearS.map(_.s.toInt)

			Some(BasicPartialDate(year, month.map(parseMonthOneBased(_)), None))
			}

		val volume = (pub \ "volume" \ "volumeinfo" \ "volumenum").text

		for (a <- pub \ "volume" \ "article") yield
			{
			parse(a, journalMention, volume, date)
			}
		}

	def parseDroppingErrors(inLocation: Location, doc: Node): TraversableOnce[StructuredCitation] =
		{
		try
		{
		//logger.debug(doc.toString())
		val c = parsePublication(doc)
		c
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

	//def apply(s: InputStream): TraversableOnce[CitationMention] = XmlUtils.firstLevelNodes(s).flatMap(node => (node \ "publication").flatMap
	// (parsePublication(_)))
	def apply(nis: NamedInputStream): TraversableOnce[StructuredCitation] =
		{
		//val xml = scala.xml.XML.load(f)
		// val xml = XMLIgnoreDTD.load(f)  // can't, because we need the entity declarations
		//XMLMapDTD.setGlobalXMLCatalogDir(getClass.getResource("/dblp.dtd").getPath)
		//val xmlloader = new XMLFilenameOnlyMappingDTDLoader(Map("dblp.dtd" -> new InputSource(getClass.getResource("/dblp.dtd").getPath)))
		// val xml = xmlloader.load(f)
		//XmlUtils.firstLevelNodes(s).flatMap(node => (node \\ "REC").flatMap(parseDroppingErrors(_)))
		val a = nis.getInputStream
		val inLocation = new BasicStringLocation(nis.name, Nil)

		try
		{
		//XmlUtils.firstLevelNodes(s).flatMap(node => (node \ "publication").flatMap(parsePublication(_)))
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
	}
