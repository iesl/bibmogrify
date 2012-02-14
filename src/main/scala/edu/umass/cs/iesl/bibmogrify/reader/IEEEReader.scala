package edu.umass.cs.iesl.bibmogrify.reader

import edu.umass.cs.iesl.scalacommons.StringUtils._
import edu.umass.cs.iesl.bibmogrify.model.CitationUtils._
import edu.umass.cs.iesl.bibmogrify.model.Authorities._
import edu.umass.cs.iesl.scalacommons.DateUtils._
import edu.umass.cs.iesl.bibmogrify.model._
import com.weiglewilczek.slf4s.Logging
import xml.Node
import java.net.URL
import edu.umass.cs.iesl.scalacommons.XMLIgnoreDTD
import java.lang.String
import com.sun.org.apache.xerces.internal.impl.io.MalformedByteSequenceException
import edu.umass.cs.iesl.bibmogrify.pipeline.{Transformer}
import edu.umass.cs.iesl.bibmogrify.{NamedPlugin, BibMogrifyException}

object IEEEReader extends Transformer[URL, StructuredCitation] with Logging with NamedPlugin{

  val name = "ieee"

  def parse(doc: Node, journalMention: StructuredCitation, volume: Option[String], date: Some[BasicPartialDate]): StructuredCitation = {
    val c = new StructuredCitation() {
      // todo interpret pubtype field
      override val doctype: Option[DocType] = JournalArticle

      override val title: Option[String] = (doc \ "title").text
      override val dates = Seq(BasicCitationEvent(date, Published))
      override val abstractText: Option[String] = (doc \ "articleinfo" \ "abstract").text.trim
      override val identifiers = {
        val id: String = (doc \ "articleinfo" \ "articledoi").text.trim
        if (id.isEmpty) { Nil }
        else {
          List(new Identifier {
            override val authority = Some(DoiAuthority)
            override val value = id
          })
        }
      }
      override val containedIn = Some(BasicContainmentInfo(journalMention, None, volume, None, None))
      override val authors = (doc \ "articleinfo" \ "authorgroup" \ "author").map((c => new Person() {
        override val name: Option[String] = Some((c \ "firstname").text + " " + (c \ "surname").text)

      })).map(new AuthorInRole(_, Nil))
    }
    c
  }

  def parsePublication(pub: Node): TraversableOnce[StructuredCitation] = {
    // assume only one volume
    val journalMention = new StructuredCitation {
      override val title: Option[String] = (pub \ "title").text

      // todo interpret pubtype field
      override val doctype: Option[DocType] = Journal
    }

    val date: Some[BasicPartialDate] = {
      val month: Option[String] = None //(doc \ "bib_date" \ "@month").text
      val yearS: Option[String] = (pub \ "volume" \ "volumeinfo" \ "year").text
      val year: Option[Int] = yearS.map(_.toInt)

      Some(BasicPartialDate(year, month.map(parseMonthOneBased(_)), None))
    }

    val volume = (pub \ "volume" \ "volumeinfo" \ "volumenum").text

    for (a <- pub \ "volume" \ "article") yield {
      parse(a, journalMention, volume, date)
    }
  }


  def parseDroppingErrors(url: URL, doc: Node): TraversableOnce[StructuredCitation] = {
    try {
      //logger.debug(doc.toString())
      val c = parsePublication(doc)
      c
    }
    catch {
      case e: BibMogrifyException => logger.error(e.getMessage)
      None
    }
  }


  //def apply(s: InputStream): TraversableOnce[CitationMention] = XmlUtils.firstLevelNodes(s).flatMap(node => (node \ "publication").flatMap(parsePublication(_)))
  def apply(url: URL): TraversableOnce[StructuredCitation] = {
    //val xml = scala.xml.XML.load(f)
    // val xml = XMLIgnoreDTD.load(f)  // can't, because we need the entity declarations
    //XMLMapDTD.setGlobalXMLCatalogDir(getClass.getResource("/dblp.dtd").getPath)
    //val xmlloader = new XMLFilenameOnlyMappingDTDLoader(Map("dblp.dtd" -> new InputSource(getClass.getResource("/dblp.dtd").getPath)))
    // val xml = xmlloader.load(f)
    //XmlUtils.firstLevelNodes(s).flatMap(node => (node \\ "REC").flatMap(parseDroppingErrors(_)))
    val s = url.openStream()
    try {
      //XmlUtils.firstLevelNodes(s).flatMap(node => (node \ "publication").flatMap(parsePublication(_)))
      parseDroppingErrors(url, XMLIgnoreDTD.load(url))
    }
      catch {
        case e: MalformedByteSequenceException => { logger.error("Failed to parse " + url, e) ; Nil }
      }
    finally {
      s.close()
    }
  }
}
