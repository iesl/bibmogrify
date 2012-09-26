package edu.umass.cs.iesl.bibmogrify.reader

import com.weiglewilczek.slf4s.Logging
import edu.umass.cs.iesl.bibmogrify.model._
import edu.umass.cs.iesl.bibmogrify.model.Authorities._
import edu.umass.cs.iesl.bibmogrify.model.CitationUtils._
import edu.umass.cs.iesl.scalacommons.XmlUtils
import xml.{NodeSeq, Node}
import edu.umass.cs.iesl.bibmogrify.pipeline.Transformer
import edu.umass.cs.iesl.bibmogrify.{NamedInputStream, NamedPlugin, BibMogrifyException}
import edu.umass.cs.iesl.scalacommons.StringUtils._
import edu.umass.cs.iesl.scalacommons.DateUtils._
import edu.umass.cs.iesl.bibmogrify.model.BasicIdentifier
import edu.umass.cs.iesl.bibmogrify.model.AuthorInRole
import edu.umass.cs.iesl.scalacommons.NonemptyString
import edu.umass.cs.iesl.bibmogrify.model.BasicCitationEvent
import scala.Some
import edu.umass.cs.iesl.bibmogrify.model.BasicPartialDate
import edu.umass.cs.iesl.bibmogrify.model.BasicNormalPageRange
import edu.umass.cs.iesl.bibmogrify.model.BasicStringLocation
import edu.umass.cs.iesl.bibmogrify.model.BasicContainmentInfo

/**
 * @author <a href="mailto:dev@davidsoergel.com">David Soergel</a>
 * @version $Id$
 */
object MedlineReader extends Transformer[NamedInputStream, StructuredCitation] with Logging with NamedPlugin
	{

	import ReaderUtils._

	val name = "medline"

	def apply(nis: NamedInputStream): TraversableOnce[StructuredCitation] =
		{
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

	def parseDroppingErrors(inLocation: Location, doc: Node): Option[StructuredCitation] =
		{
		try
		{
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

		val article: NodeSeq = doc \ "Article"

		val journal = article \ "Journal"

		val journalMention = new StructuredCitation
			{

			val date: Some[BasicPartialDate] =
				{
				val d = journal \ "JournalIssue" \ "PubDate"
				val yearS: Option[NonemptyString] = (d \ "Year").text
				val year: Option[Int] = yearS.map(_.s.toInt)
				val monthS: Option[NonemptyString] = (d \ "Month").text
				val month: Option[Int] = monthS.map(parseMonthOneBased(_))
				val dayS: Option[NonemptyString] = (d \ "Day").text
				val day: Option[Int] = dayS.map(_.s.toInt)

				Some(BasicPartialDate(year, month, day))
				}

			// drop superscripts, subscripts, italics, and typewriter styles
			override val title: Option[NonemptyString] = (journal \ "Title").text.trim

			// todo interpret pubtype fieldin associated issue
			override val doctype: Option[DocType] = Journal

			override val dates = Seq(BasicCitationEvent(date, Published))
			}


		//val authorSplit = "(.+)( .*)? (.+)".r
		val c = new StructuredCitation()
			{
			// todo interpret pubtype field
			override val doctype: Option[DocType] = JournalArticle

			// drop superscripts, subscripts, italics, and typewriter styles
			override val title: Option[NonemptyString] = (article \ "ArticleTitle").text.trim
			override val dates                         = journalMention.dates

			override val abstractText : Iterable[TextWithLanguage]   = TextWithLanguage(None, (article \ "Abstract" \ "AbstractText").stripTags)

			override val identifiers : Iterable[Identifier]   = BasicIdentifier((doc \ "PMID").text, PubmedAuthority)

			val volume = (journal \ "JournalIssue" \ "Volume").text
			val issue  = (journal \ "JournalIssue" \ "Issue").text
			val pagesS = (article \ "Pagination" \ "MedlinePgn").text.trim

			val pages: Option[PageRange] =
				{
				val ps = pagesS.split("-")
				if (ps.size == 1)
					{
					try
					{
					val fpagei: Int = ps(0).trim.toInt
					Some(BasicNormalPageRange(fpagei, None))
					}
					catch
					{
					case e: NumberFormatException => None
					}
					}
				else if (ps.size == 2)
					{
					val fpage: String = ps(0).trim
					val lpage: String = ps(1).trim
					try
					{
					val fpagei: Int = fpage.toInt
					val lpagei: Int = lpage.toInt
					Some(BasicNormalPageRange(fpagei, Some(lpagei)))
					}
					catch
					{
					case e: NumberFormatException => Some(BasicStringPageRange(NonemptyString(fpage), lpage))
					}
					}
				else None
				}

			override val containedIn = Some(BasicContainmentInfo(journalMention, None, volume, issue, pages))

			//override val keywords = subjectCodes map (new BasicKeyword(WOSKeywordAuthority, _))
			override val locations = Seq(inLocation)


			override val authors   = (article \ "AuthorList" \ "Author").map(a =>
				                                                                 {
				                                                                 new AuthorInRole(Person((a \ "ForeName").text.trim,(a \ "LastName").text.trim), Nil)
				                                                                 		                                                                 })

			// in this variant we just make a full-name string and then parse it again downstream
		/*
			override val authors   = (article \ "AuthorList" \ "Author").map(a =>
				                                                                 {
				                                                                 val fullname = (a \ "ForeName").text.trim + " " + (a \ "LastName").text.trim
				                                                                 new AuthorInRole(Person(fullname), Nil)
				                                                                 })
				                                                                 */
			}
		c
		}
	}
