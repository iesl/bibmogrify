package edu.umass.cs.iesl.bibmogrify.reader

import edu.umass.cs.iesl.scalacommons.StringUtils._
import java.net.URL
import com.weiglewilczek.slf4s.Logging
import edu.umass.cs.iesl.bibmogrify.model._
import edu.umass.cs.iesl.bibmogrify.model.Authorities._
import edu.umass.cs.iesl.bibmogrify.model.CitationUtils._
import xml.{NodeSeq, Node}
import edu.umass.cs.iesl.scalacommons.XMLIgnoreDTD
import edu.umass.cs.iesl.bibmogrify.pipeline.{Transformer}
import edu.umass.cs.iesl.bibmogrify.{NamedPlugin, BibMogrifyException}

/**
 * @author <a href="mailto:dev@davidsoergel.com">David Soergel</a>
 * @version $Id$
 */

object NLMReader extends Transformer[URL, StructuredCitation] with Logging with NamedPlugin {

  val name = "nlm"


  //def apply(s: InputStream): TraversableOnce[CitationMention] = XmlUtils.firstLevelNodes(s).flatMap(node => (node \ "publication").flatMap(parsePublication(_)))
  def apply(url: URL): TraversableOnce[StructuredCitation] = {
    val s = url.openStream()
    try {
      //XmlUtils.firstLevelNodes(s).flatMap(node => (node \ "@{http://www.elsevier.com/xml/document/schema}document").flatMap(parseDroppingErrors(url, _)))
      //XmlUtils.firstLevelNodes(s).flatMap(parseDroppingErrors(url, _))
      parseDroppingErrors(url, XMLIgnoreDTD.load(url))
    }
    finally {
      s.close()
    }
  }


  def parseDroppingErrors(url: URL, doc: Node): Option[StructuredCitation] = {
    try {
      //logger.debug(doc.toString())
      val c = parse(url, doc)
      Some(c)
    }
    catch {
      case e: BibMogrifyException => logger.error(e.getMessage)
      None
    }
  }


  def parse(url: URL, doc: Node): StructuredCitation = {

    val front: NodeSeq = doc \ "front"
    val articlemeta: NodeSeq = front \ "article-meta"

    val date: Some[BasicPartialDate] = {
      //val month: Option[String] = None //(doc \ "bib_date" \ "@month").text
      val dates: NodeSeq = articlemeta \ "pub-date"
      def isCollectionDate(d: Node): Boolean = {
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

    val journalMention = new StructuredCitation {
      // drop superscripts, subscripts, italics, and typewriter styles
      override val title: Option[String] = (front \ "journal-meta" \ "journal-title").text.trim

      // todo interpret pubtype fieldin associated issue
      override val doctype: Option[DocType] = Journal
    }

    //val authorSplit = "(.+)( .*)? (.+)".r
    val c = new StructuredCitation() {
      // todo interpret pubtype field
      override val doctype : Option[DocType]= JournalArticle

      // drop superscripts, subscripts, italics, and typewriter styles
      override val title: Option[String] = (articlemeta \ "title-group" \ "article-title").text.trim
      override val dates = Seq(BasicCitationEvent(date, Published))

      override val abstractText: Option[String] = (articlemeta \ "abstract").text.trim

      override val identifiers = Seq(BasicIdentifier((articlemeta \ "article-id").filter(_.attribute("pub-id-type").filter(_ == "doi").isDefined).text, DoiAuthority))
      // TODO implement parsePages, or just store the string
      def parsePages(s: String): Option[PageRange] = None

      val volume = (articlemeta \ "volume").text
      override val containedIn = Some(BasicContainmentInfo(journalMention, None, volume, None, None))

      //override val keywords = subjectCodes map (new BasicKeyword(WOSKeywordAuthority, _))

      override val locations = Seq(new BasicLocation(url, Nil))
      override val authors = (articlemeta \ "contrib-group" \ "contrib" \ "name").map((c => new Person() {
        override val name = Some((c \ "given-names").text + " " + (c \ "surname").text)

      })).map(new AuthorInRole(_, Nil))


    }
    c
  }
}
