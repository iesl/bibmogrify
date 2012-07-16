package edu.umass.cs.iesl.bibmogrify.reader

import edu.umass.cs.iesl.scalacommons.StringUtils._
import com.weiglewilczek.slf4s.Logging
import edu.umass.cs.iesl.bibmogrify.model._
import edu.umass.cs.iesl.bibmogrify.model.Authorities._
import edu.umass.cs.iesl.bibmogrify.model.CitationUtils._
import xml.{NodeSeq, Node}
import edu.umass.cs.iesl.bibmogrify.pipeline.Transformer
import edu.umass.cs.iesl.bibmogrify.{NamedInputStream, NamedPlugin, BibMogrifyException}
import edu.umass.cs.iesl.scalacommons.{NonemptyString, StringUtils, XMLIgnoreDTD}

/**
 * @author <a href="mailto:dev@davidsoergel.com">David Soergel</a>
 * @version $Id$
 */
object NLMReader extends Transformer[NamedInputStream, StructuredCitation] with Logging with NamedPlugin
	{

	import ReaderUtils._

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
		val body: NodeSeq = doc \ "body"
		val back: NodeSeq = doc \ "back"

		if (articlemeta.isEmpty) throw new BibMogrifyException("No article found: " + inLocation)

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


			def isPpubDate(d: Node): Boolean =
				{
				val atts: Option[Seq[Node]] = d.attribute("pub-type")
				val atts2: Option[Seq[Node]] = atts.filter(_.text == "ppub")
				atts2.isDefined
				}
			val datesPpub: NodeSeq = dates.filter(isPpubDate)


			def isEpubDate(d: Node): Boolean =
				{
				val atts: Option[Seq[Node]] = d.attribute("pub-type")
				val atts2: Option[Seq[Node]] = atts.filter(_.text == "epub")
				atts2.isDefined
				}
			val datesEpub: NodeSeq = dates.filter(isEpubDate)


			val yearC: Option[NonemptyString] = (datesCollection \ "year").text
			val yearP: Option[NonemptyString] = (datesPpub \ "year").text
			val yearE: Option[NonemptyString] = (datesEpub \ "year").text
			val yearString: Option[NonemptyString] = yearC.orElse(yearE).orElse(yearP)

			val year: Option[Int] =
				try
				{
				yearString.map(s => s.s.toInt)
				}
				catch
				{case e: NumberFormatException => None}
			// val dayS: Option[String] = (doc \ "day").text
			// val day: Option[Int] = dayS.map(_.toInt)

			Some(BasicPartialDate(year, None, None))
			}

		val journalMention = new StructuredCitation
			{
			// drop superscripts, subscripts, italics, and typewriter styles
			override val title: Option[NonemptyString] = (front \ "journal-meta" \ "journal-title").text.trim

			// todo interpret pubtype fieldin associated issue
			override val doctype: Option[DocType] = Journal
			}

		val parseReferences = (back \ "ref-list" \ "ref") map (ref =>
			{
			val idNodes = (ref \ "citation" \ "pub-id")
			new StructuredCitation()
				{
				override val identifiers =
					{
					val pmidNodes: NodeSeq = idNodes.filter(_.attribute("pub-id-type").filter(_.text == "pmid").isDefined)
					val doiNodes: NodeSeq = idNodes.filter(_.attribute("pub-id-type").filter(_.text == "doi").isDefined)

					val pmids = pmidNodes.map(pm => BasicIdentifier(pm.text.trim.removeNewlinesAndTabs, PubmedAuthority))
					val dois = doiNodes.map(pm => BasicIdentifier(pm.text.trim.removeNewlinesAndTabs, DoiAuthority))

					pmids ++ dois
					}

				override val refMarker: Option[NonemptyString] = (ref \ "label").text.trim.removeNewlinesAndTabs

				// ** sometimes these are tagged; we just ignore that for now
				override val unstructuredString: Option[NonemptyString] = (ref \ "citation").filter(_.label != "pub-id").text.trim.removeNewlinesAndTabs
				}
			})

		//val authorSplit = "(.+)( .*)? (.+)".r
		val idNodes = (articlemeta \ "article-id")
		val c = new StructuredCitation()
			{
			override val doctype   : Option[DocType]        = JournalArticle
			override val docSubtype: Option[NonemptyString] = ((articlemeta \ "article-categories" \ "subj-group")
			                                                   .filter(_.attribute("subj-group-type").filter(_.text == "heading").isDefined) \ "subject").text
			                                                  .trim.removeNewlinesAndTabs

			// drop superscripts, subscripts, italics, and typewriter styles
			override val title: Option[NonemptyString] = (articlemeta \ "title-group" \ "article-title").text.trim.removeNewlinesAndTabs
			override val dates                         = Seq(BasicCitationEvent(date, Published))

			override val abstractText = Seq(TextWithLanguage(None, (articlemeta \ "abstract").stripTags))

			// todo distinguish section types
			override val bodyText = (body \ "sec").map(s => UndifferentiatedBodyTextSection(s.stripTags))

			// todo check that these don't double-count, and that they catch all the cases
			override val numFigures                          = Some((body \\ "fig").size + (back \\ "fig").size)
			override val numTables                           = Some((body \\ "table-wrap").size + (back \\ "table-wrap").size)
			override val licenseType: Option[NonemptyString] = (front \ "permissions" \ "license" \ "@license-type").text.trim.removeNewlinesAndTabs

			override val identifiers =
				{
				val pmidNodes: NodeSeq = idNodes.filter(_.attribute("pub-id-type").filter(_.text == "pmid").isDefined)
				val doiNodes: NodeSeq = idNodes.filter(_.attribute("pub-id-type").filter(_.text == "doi").isDefined)

				val pmids = pmidNodes.map(pm => BasicIdentifier(pm.text.trim.removeNewlinesAndTabs, PubmedAuthority))
				//val dois = doiNodes.map(pm => BasicIdentifier(pm.text, DoiAuthority))

				pmids
				}

			val volume = (articlemeta \ "volume").text.trim.removeNewlinesAndTabs
			val number = (articlemeta \ "issue").text.trim.removeNewlinesAndTabs

			val pages: Option[PageRange] =
				{
				val fpage: Option[NonemptyString] = (articlemeta \ "fpage").text.trim.removeNewlinesAndTabs
				val lpage: Option[NonemptyString] = (articlemeta \ "lpage").text.trim.removeNewlinesAndTabs
				try
				{
				val fpagei = fpage.map(_.s.toInt)
				val lpagei = lpage.map(_.s.toInt)
				fpagei.map(fp => BasicNormalPageRange(fp, lpagei))
				}
				catch
				{
				case e: NumberFormatException => fpage.map(fp => BasicStringPageRange(fp, lpage))
				}
				}

			override val containedIn = Some(BasicContainmentInfo(journalMention, None, volume, number, pages))

			//override val keywords = subjectCodes map (new BasicKeyword(WOSKeywordAuthority, _))
			override val locations = Seq(inLocation)
			override val authors   = (articlemeta \ "contrib-group" \ "contrib" \ "name").map((c =>
				new Person()
					{
					override val name = Some(new PersonNameWithDerivations
						{
						val first: String = (c \ "given-names").text.trim.removeNewlinesAndTabs
						val last          = (c \ "surname").text.trim.removeNewlinesAndTabs

						override val givenNames: Seq[NonemptyString] = StringUtils.emptyStringToNone(first).toSeq

						override val surNames: Set[NonemptyString] = StringUtils.emptyStringToNone(last).toSet

						override val fullNames = emptyStringToNone(first + " " + last).toSet
						})
					})).map(new AuthorInRole(_, Nil))

			override val references = parseReferences
			}
		c
		}
	}
