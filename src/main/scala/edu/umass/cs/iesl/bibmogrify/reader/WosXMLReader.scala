package edu.umass.cs.iesl.bibmogrify.reader

import edu.umass.cs.iesl.scalacommons.StringUtils._
import edu.umass.cs.iesl.scalacommons.DateUtils._
import edu.umass.cs.iesl.bibmogrify.model._
import com.weiglewilczek.slf4s.Logging
import edu.umass.cs.iesl.bibmogrify.model.Authorities._
import xml.Node
import edu.umass.cs.iesl.scalacommons.XMLIgnoreDTD
import java.net.URL
import edu.umass.cs.iesl.bibmogrify.pipeline.Transformer
import edu.umass.cs.iesl.bibmogrify.{NamedPlugin, BibMogrifyException}

object WosXMLReader extends Transformer[URL, StructuredCitation] with Logging with NamedPlugin {

  val name = "wosxml"


  def apply(url: URL): TraversableOnce[StructuredCitation] = {
    //val xml = scala.xml.XML.load(f)
    // val xml = XMLIgnoreDTD.load(f)  // can't, because we need the entity declarations
    //XMLMapDTD.setGlobalXMLCatalogDir(getClass.getResource("/dblp.dtd").getPath)
    //val xmlloader = new XMLFilenameOnlyMappingDTDLoader(Map("dblp.dtd" -> new InputSource(getClass.getResource("/dblp.dtd").getPath)))
    // val xml = xmlloader.load(f)
    //XmlUtils.firstLevelNodes(s).flatMap(node => (node \\ "REC").flatMap(parseDroppingErrors(_)))
    val s = url.openStream()
    try {
      //XMLIgnoreDTD.load(s).flatMap(node => (node \\ "REC").flatMap(parseDroppingErrors(url, _)))

      for {
        node <- XMLIgnoreDTD.load(s)
        rec <- (node \\ "REC")
        result <- parseDroppingErrors(url, rec)
      } yield result
    }
    finally {
      s.close()
    }
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

  def parse(url: URL, doc: Node): StructuredCitation = {

    val issue = doc \ "issue"
    val item = doc \ "item"

    val subjectCodes = (issue \ "subjects" \ "subject" \ "@code") map (_.text)


    val date: Some[BasicPartialDate] = {
      val month: Option[String] = None //(doc \ "bib_date" \ "@month").text
      val yearS: Option[String] = (item \ "bib_issue" \ "@year").text
      val year: Option[Int] = yearS.map(_.toInt)
      // val dayS: Option[String] = (doc \ "day").text
      // val day: Option[Int] = dayS.map(_.toInt)

      Some(BasicPartialDate(year, month.map(parseMonthOneBased(_)), None))
    }

    val venueMention = new StructuredCitation {
      // drop superscripts, subscripts, italics, and typewriter styles
      override val title: Option[String] = (item \ "source_title").text


      // todo interpret pubtype fieldin associated issue
      //val doctype = Journal
    }

    //val authorSplit = "(.+)( .*)? (.+)".r
    val c = new StructuredCitation() {
      // todo interpret pubtype field
      //val doctype = JournalArticle

      // drop superscripts, subscripts, italics, and typewriter styles
      override val title: Option[String] = (item \ "item_title").text
      override val dates = Seq(BasicCitationEvent(date, Published))

      override val abstractText: Option[String] = (item \ "abstract").text

      // todo collect other identifiers
      override val identifiers = Seq(BasicIdentifier((item \ "ut").text, WOSIDAuthority))

      // TODO implement parsePages, or just store the string
      def parsePages(s: String): Option[PageRange] = None

      override val containedIn = Some(BasicContainmentInfo(venueMention, None, None, None, None))

      override val keywords = subjectCodes map (new BasicKeyword(_, WOSKeywordAuthority))

      override val locations = Seq(new BasicLocation(url, Nil))


      override val references = (item \ "refs" \ "ref").map(parseRef(_))
    }
    c
  }


  def parseRef(node: Node): StructuredCitation = new StructuredCitation {
    override val authors = Seq(new AuthorInRole(new Person() {
      override val name: Option[String] = (node \ "@auth").text
    }, Seq(FirstAuthor)))

    val venueMention = new StructuredCitation {
      override val title: Option[String] = (node \ "@work").text
    }
    override val containedIn = Some(BasicContainmentInfo(venueMention, None, (node \ "vol").text, None, Some(new StringPageRange() {
      override val start = (node \ "@page").text
    })))
    val date: Some[BasicPartialDate] = {
      val yearS: Option[String] = (node \ "@year").text
      val year: Option[Int] = yearS.map(_.toInt)
      Some(BasicPartialDate(year, None, None))
    }
    override val dates = Seq(BasicCitationEvent(date, Published))
    override val identifiers = Seq(
      new BasicIdentifier(node.text),
      new BasicIdentifier((node \ "recid").text),
      new BasicIdentifier((node \ "refkey").text),
      new BasicIdentifier((node \ "refid").text),
      new BasicIdentifier((node \ "cid").text),
      new BasicIdentifier((node \ "artno").text, DoiAuthority)
    )
  }

}
