package edu.umass.cs.iesl.bibmogrify.reader

import edu.umass.cs.iesl.scalacommons.StringUtils._
import edu.umass.cs.iesl.scalacommons.DateUtils._
import edu.umass.cs.iesl.bibmogrify.model._
import com.weiglewilczek.slf4s.Logging
import xml.Node
import edu.umass.cs.iesl.scalacommons.XmlUtils
import edu.umass.cs.iesl.bibmogrify.{BibMogrifyException, CitationStreamReader}
import java.io.InputStream

object DBLPReader extends CitationStreamReader with Logging
  {

  def parse(doc: Node): CitationMention =
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
        {throw new BibMogrifyException("Unknown Doc Type: " + doc.label)}
    }


    val key = (doc \ "@key").text
    val id = new BasicIdentifier(DBLPAuthority, key)


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
    val journalMention = new CitationMention
      {
      // drop superscripts, subscripts, italics, and typewriter styles
      val title = (doc \ "journal").text
      val doctype = Journal
      }

    //val authorSplit = "(.+)( .*)? (.+)".r
    val c = new CitationMention()
      {
      val doctype = doctypeX

      // drop superscripts, subscripts, italics, and typewriter styles
      override val title: String = (doc \ "title").text
      override val authors = authorsX map (x => new AuthorInRole(BasicPerson(name = Some(x)), Nil))
      override val otherContributors = editorsX map (x => new OtherContributorInRole(BasicPerson(name = Some(x)), Seq(Editor)))
      override val dates = Seq(BasicCitationEvent(date, Published))

      override val abstractText: Option[String] = (doc \ "abstract").text

      override val identifiers = Seq(id)

      // TODO implement parsePages, or just store the string
      def parsePages(s: String): Option[PageRange] = None

      override val containedIn = Some(BasicContainmentInfo(journalMention, series, volume, number, pages.flatMap(parsePages(_))))
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

    XmlUtils.firstLevelNodes(s).flatMap(parseDroppingErrors(_))
    }
  }

object DBLPAuthority extends BasicIdentifierAuthority("dblp")


