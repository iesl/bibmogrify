package edu.umass.cs.iesl.bibmogrify.reader

import edu.umass.cs.iesl.scalacommons.StringUtils._
import edu.umass.cs.iesl.scalacommons.DateUtils._
import edu.umass.cs.iesl.bibmogrify.model._
import edu.umass.cs.iesl.bibmogrify.model.CitationUtils._
import com.weiglewilczek.slf4s.Logging
import xml.Node
import java.net.URL
import edu.umass.cs.iesl.scalacommons.XMLIgnoreDTD
import edu.umass.cs.iesl.bibmogrify.pipeline.Transformer
import edu.umass.cs.iesl.bibmogrify.{NamedPlugin, BibMogrifyException}

object PatentST36Reader extends Transformer[URL, StructuredCitation] with Logging with NamedPlugin
  {


  val name = "st36"

  def parse(doc: Node): StructuredCitation =
    {

    val date: Some[BasicPartialDate] =
      {
      val parseDate = "(....)(..)(..)".r
      val parseDate(yearS, monthS, dayS) = (doc \ "@date-published").text.trim

      val year: Option[Int] = Some(yearS.toInt)
      // val dayS: Option[String] = (doc \ "day").text
      // val day: Option[Int] = dayS.map(_.toInt)

      Some(BasicPartialDate(year, Some(parseMonthOneBased(monthS)), Some(dayS.toInt)))
      }


    val c = new StructuredCitation()
      {
      override val doctype : Option[DocType]= Patent
      override val title: Option[String] = (doc \ "bibliographic-data" \ "invention-title").text.trim
      override val dates = Seq(BasicCitationEvent(date, Published))

      override val abstractText: Option[String] = (doc \ "abstract").text.trim
      }
    c
    }

  def parseDroppingErrors(doc: Node): Option[StructuredCitation] =
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

  /*  def apply(s: InputStream): TraversableOnce[CitationMention] =
      {
      //val xmlloader = new XMLFilenameOnlyMappingDTDLoader(Map("wo-patent-document-v1-3.dtd" -> new InputSource(getClass.getResource("/wo-patent-document-v1-3.dtd").getPath)))
      val xmlloader = XMLIgnoreDTD

      // always one per file
      parseDroppingErrors(xmlloader.load(s))
      //XmlUtils.firstLevelNodes(s).flatMap(node => (node \\ "wopatent-document").flatMap(parseDroppingErrors(_)))
      }*/
  def apply(url: URL): TraversableOnce[StructuredCitation] =
    {
    //val xml = scala.xml.XML.load(f)
    // val xml = XMLIgnoreDTD.load(f)  // can't, because we need the entity declarations
    //XMLMapDTD.setGlobalXMLCatalogDir(getClass.getResource("/dblp.dtd").getPath)
    //val xmlloader = new XMLFilenameOnlyMappingDTDLoader(Map("dblp.dtd" -> new InputSource(getClass.getResource("/dblp.dtd").getPath)))
    // val xml = xmlloader.load(f)
    //XmlUtils.firstLevelNodes(s).flatMap(node => (node \\ "REC").flatMap(parseDroppingErrors(_)))
    val s = url.openStream()
    try
    {
    XMLIgnoreDTD.load(s).flatMap(parseDroppingErrors(_))
    }
    finally
      {
      s.close()
      }
    }
  }
