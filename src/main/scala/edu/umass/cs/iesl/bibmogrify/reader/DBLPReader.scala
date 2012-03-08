package edu.umass.cs.iesl.bibmogrify.reader

import edu.umass.cs.iesl.scalacommons.StringUtils._
import edu.umass.cs.iesl.scalacommons.DateUtils._
import edu.umass.cs.iesl.bibmogrify.model._
import edu.umass.cs.iesl.bibmogrify.model.Authorities._
import edu.umass.cs.iesl.bibmogrify.model.CitationUtils._
import com.weiglewilczek.slf4s.Logging
import xml.Node
import java.net.URL
import edu.umass.cs.iesl.scalacommons.XmlUtils
import edu.umass.cs.iesl.bibmogrify.pipeline.Transformer
import edu.umass.cs.iesl.bibmogrify.{NamedPlugin, BibMogrifyException}

object DBLPReader extends Transformer[URL, StructuredCitation] with Logging with NamedPlugin {

  val name = "dblp"
  val baseUrl = " http://www.informatik.uni-trier.de/∼ley/db/"

  def parse(inurl: URL, doc: Node): StructuredCitation = {

    val doctypeX: DocType = doc.label match {
      case "article" => JournalArticle
      case "inproceedings" => ProceedingsArticle
      case "proceedings" => Proceedings
      case "book" => Book
      case "incollection" => CollectionArticle
      case "collection" => CollectionOfArticles // not in the dblp dtd, but should be??
      case "phdthesis" => PhdThesis
      case "mastersthesis" => MastersThesis
      case "www" => WwwArticle
      case _ => {
        throw new BibMogrifyException("Unknown Doc Type: " + doc.label)
      }
    }


    val key = (doc \ "@key").text
    val id = new BasicIdentifier(key, DblpAuthority)


    // these "X" variables may be empty Strings; they are implicitly converted to Option[String]
    val authorsX: Seq[String] = (doc \ "author").map(_.text)

    val editorsX: Seq[String] = (doc \ "editor").map(_.text)

    // todo modify dtd to constrain some fields to appear only once?
    val date: Some[BasicPartialDate] = {
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
    val journalMention = new StructuredCitation {
      // drop superscripts, subscripts, italics, and typewriter styles
      override val title: Option[String] = (doc \ "journal").text
      override val doctype: Option[DocType] = Journal
    }

    //val authorSplit = "(.+)( .*)? (.+)".r
    val c = new StructuredCitation() {
      override val doctype: Option[DocType] = doctypeX

      // drop superscripts, subscripts, italics, and typewriter styles
      override val title: Option[String] = (doc \ "title").text
      override val authors = authorsX map (x => new AuthorInRole(new Person {
        override val name = Some(x)
      }, Nil))
      override val otherContributors = editorsX map (x => new OtherContributorInRole(new Person {
        override val name = Some(x)
      }, List(Editor)))
      override val dates = Seq(BasicCitationEvent(date, Published))

      override val abstractText: Option[String] = (doc \ "abstract").text

      override val identifiers = Seq(id)

      // TODO implement parsePages, or just store the string
      def parsePages(s: String): Option[PageRange] = None

      override val containedIn = Some(BasicContainmentInfo(journalMention, series, volume, number, pages.flatMap(parsePages(_))))

      val loc = url.map(u => {
        val uu = if (u.contains(":")) u else (baseUrl + u)
        new BasicLocation(new URL(uu), Nil)
      })


      override val locations = Seq(new BasicLocation(inurl, Nil)) ++ loc
    }
    c
  }

  def parseDroppingErrors(url: URL, doc: Node): Option[StructuredCitation] = {
    try {
      val c = parse(url, doc)
      Some(c)
    }
    catch {
      case e: BibMogrifyException => logger.error(e.getMessage)
      None
    }
  }


  def apply(url: URL): TraversableOnce[StructuredCitation] = {
    //val xml = scala.xml.XML.load(f)
    // val xml = XMLIgnoreDTD.load(f)  // can't, because we need the entity declarations
    //XMLMapDTD.setGlobalXMLCatalogDir(getClass.getResource("/dblp.dtd").getPath)
    //val xmlloader = new XMLFilenameOnlyMappingDTDLoader(Map("dblp.dtd" -> new InputSource(getClass.getResource("/dblp.dtd").getPath)))
    // val xml = xmlloader.load(f)
    //XmlUtils.firstLevelNodes(s).flatMap(node => (node \\ "REC").flatMap(parseDroppingErrors(_)))
    val s = url.openStream()
    try {
      XmlUtils.firstLevelNodes(s).flatMap(parseDroppingErrors(url, _))
    }
    finally {
      s.close()
    }
  }
}
