/*
 * Copyright (c) 2013  University of Massachusetts Amherst
 * Licensed under the Apache License, Version 2.0
 * http://www.apache.org/licenses/LICENSE-2.0
 */

package edu.umass.cs.iesl.bibmogrify.reader

import edu.umass.cs.iesl.scalacommons.StringUtils._
import edu.umass.cs.iesl.scalacommons.DateUtils._
import edu.umass.cs.iesl.bibmogrify.model._
import edu.umass.cs.iesl.bibmogrify.model.Authorities._
import edu.umass.cs.iesl.bibmogrify.model.CitationUtils._
import com.typesafe.scalalogging.slf4j.Logging
import xml.Node
import java.net.URL
import edu.umass.cs.iesl.bibmogrify.pipeline.Transformer
import edu.umass.cs.iesl.bibmogrify.{NamedInputStream, NamedPlugin, BibMogrifyException}
import java.io.InputStream
import io.Source
import scala.collection.mutable

object BibTeXReader //extends Transformer[NamedInputStream, StructuredCitation] with Logging with NamedPlugin
	{
/*
	val name    = "dblp"
	val baseUrl = "http://www.informatik.uni-trier.de/∼ley/db/"

	def parse(inLocation: Location, doc: Node): StructuredCitation =
		{

		val doctypeX: DocType = doc.label match
		{
			case "article"       => JournalArticle
			case "inproceedings" => ProceedingsArticle
			case "proceedings"   => Proceedings
			case "book"          => Book
			case "incollection"  => CollectionArticle
			case "collection"    => CollectionOfArticles // not in the dblp dtd, but should be??
			case "phdthesis"     => PhdThesis
			case "mastersthesis" => MastersThesis
			case "www"           => WwwArticle
			case _               =>
				{
				throw new BibMogrifyException("Unknown Doc Type: " + doc.label)
				}
		}


		val key = (doc \ "@key").text
		val id = new BasicIdentifier(key, DblpAuthority)


		// these "X" variables may be empty Strings; they are implicitly converted to Option[String]
		val authorsX: Seq[String] = (doc \ "author").map(_.text)

		val editorsX: Seq[String] = (doc \ "editor").map(_.text)

		// todo modify dtd to constrain some fields to appear only once?
		val date: Some[BasicPartialDate] =
			{
			val month: Option[String] = (doc \ "month").text
			val yearS: Option[String] = (doc \ "year").text
			val year: Option[Int] = yearS.map(_.toInt)
			val dayS: Option[String] = (doc \ "day").text
			val day: Option[Int] = dayS.map(_.toInt)

			Some(BasicPartialDate(year, month.map(parseMonthOneBased(_)), day))
			}

		val volume: Option[String] = (doc \ "volume").text
		val series: Option[String] = (doc \ "series").text
		val pages: Option[String] = (doc \ "pages").text
		val number: Option[String] = (doc \ "number").text



		val url: Option[String] = (doc \ "url").text

		val ee: Option[String] = (doc \ "ee").text
		// http://www.informatik.uni-trier.de/~ley/
		val address: Option[String] = (doc \ "address").text

		// todo should the address be associated with the first author?
		val journalMention = new StructuredCitation
			{
			// drop superscripts, subscripts, italics, and typewriter styles
			override val title  : Option[String]  = (doc \ "journal").text
			override val doctype: Option[DocType] = Journal
			}

		//val authorSplit = "(.+)( .*)? (.+)".r
		val c = new StructuredCitation()
			{
			override val doctype: Option[DocType] = doctypeX

			// drop superscripts, subscripts, italics, and typewriter styles
			override val title: Option[String] = (doc \ "title").text
			override val authors               = authorsX map (x => new AuthorInRole(new Person
				{
				override val name = Some(x)
				}, Nil))
			override val otherContributors     = editorsX map (x => new OtherContributorInRole(new Person
				{
				override val name = Some(x)
				}, List(Editor)))
			override val dates                 = Seq(BasicCitationEvent(date, Published))

			override val abstractText: Iterable[TextWithLanguage] = Seq(new TextWithLanguage(None, (doc \ "abstract").text))

			override val identifiers = Seq(id)

			// TODO implement parsePages, or just store the string
			def parsePages(s: String): Option[PageRange] = None

			override val containedIn = Some(BasicContainmentInfo(journalMention, series, volume, number, pages.flatMap(parsePages(_))))

			val loc = url.map(u =>
				                  {
				                  val uu = if (u.contains(":")) u else (baseUrl + u)
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
		{
		val c = parse(inLocation, doc)
		Some(c)
		}
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
		{
		BibTeXTraversible(s).flatMap(parseDroppingErrors(inLocation, _))
		}
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
		}*/
	}

object BibTeXTraversible
	{/*
	def apply(stream: InputStream): TraversableOnce[String] = new Traversable[String]()
		{
		private val st = Source.fromInputStream(stream)

		def foreach[U](f: (String) => U)
			{
			// find the beginning of the next record
			st.dropWhile(_ != '@')
			// consume the at sign itself
			st.next()
			val result = "@" + st.takeWhile(_ != '@')

			// remove CRs
			result.filterNot(_ == "\n")
			}
		}*/
	}
