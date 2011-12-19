package edu.umass.cs.iesl.bibmogrify.reader

import edu.umass.cs.iesl.scalacommons.StringUtils._
import edu.umass.cs.iesl.scalacommons.DateUtils._
import edu.umass.cs.iesl.bibmogrify.model._
import com.weiglewilczek.slf4s.Logging
import xml.Node
import edu.umass.cs.iesl.scalacommons.XmlUtils
import edu.umass.cs.iesl.bibmogrify.{BibMogrifyException, CitationStreamReader}
import java.io.InputStream

object WosXMLReader extends CitationStreamReader with Logging
  {

  def parse(doc: Node): CitationMention =
    {

    val date: Some[BasicPartialDate] =
      {
      val month: Option[String] = None //(doc \ "bib_date" \ "@month").text
      val yearS: Option[String] = (doc \ "bib_issue" \ "@year").text
      val year: Option[Int] = yearS.map(_.toInt)
      // val dayS: Option[String] = (doc \ "day").text
      // val day: Option[Int] = dayS.map(_.toInt)

      Some(BasicPartialDate(year, month.map(parseMonthOneBased(_)), None))
      }

    val journalMention = new CitationMention
      {
      // drop superscripts, subscripts, italics, and typewriter styles
      val title = (doc \ "source_title").text


      // todo interpret pubtype fieldin associated issue
      val doctype = Journal
      }

    //val authorSplit = "(.+)( .*)? (.+)".r
    val c = new CitationMention()
      {
      // todo interpret pubtype field
      val doctype = JournalArticle

      // drop superscripts, subscripts, italics, and typewriter styles
      override val title: String = (doc \ "item_title").text
      override val dates = Seq(BasicCitationEvent(date, Published))

      override val abstractText: Option[String] = (doc \ "abstract").text

      //override val identifiers = Seq( BasicIdentifier(authority, (doc \ "article_no").text))
      // TODO implement parsePages, or just store the string
      def parsePages(s: String): Option[PageRange] = None

      override val containedIn = Some(BasicContainmentInfo(journalMention, None, None, None, None))
      }
    c
    }

  def parseDroppingErrors(doc: Node): Option[CitationMention] =
    {
    try
    {
    val c = parse(doc)
    Some(c)
    }
    catch
    {
    case e: BibMogrifyException => logger.error(e.getMessage)
    None
    }
    }

  def apply(s: InputStream): TraversableOnce[CitationMention] =
    {
    //val xml = scala.xml.XML.load(f)
    // val xml = XMLIgnoreDTD.load(f)  // can't, because we need the entity declarations
    //XMLMapDTD.setGlobalXMLCatalogDir(getClass.getResource("/dblp.dtd").getPath)
    //val xmlloader = new XMLFilenameOnlyMappingDTDLoader(Map("dblp.dtd" -> new InputSource(getClass.getResource("/dblp.dtd").getPath)))
    // val xml = xmlloader.load(f)

    XmlUtils.firstLevelNodes(s).flatMap(node => (node \\ "item").flatMap(parseDroppingErrors(_)))
    }
  }
