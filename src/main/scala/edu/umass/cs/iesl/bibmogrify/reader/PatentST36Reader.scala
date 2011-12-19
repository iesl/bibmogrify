package edu.umass.cs.iesl.bibmogrify.reader

import edu.umass.cs.iesl.scalacommons.StringUtils._
import edu.umass.cs.iesl.scalacommons.DateUtils._
import edu.umass.cs.iesl.bibmogrify.model._
import com.weiglewilczek.slf4s.Logging
import xml.Node
import edu.umass.cs.iesl.bibmogrify.{BibMogrifyException, CitationStreamReader}
import java.io.InputStream
import edu.umass.cs.iesl.scalacommons.XMLIgnoreDTD

object PatentST36Reader extends CitationStreamReader with Logging
  {

  def parse(doc: Node): CitationMention =
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


    val c = new CitationMention()
      {
      val doctype = Patent
      override val title: String = (doc \ "bibliographic-data" \ "invention-title").text.trim
      override val dates = Seq(BasicCitationEvent(date, Published))

      override val abstractText: Option[String] = (doc \ "abstract").text.trim
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
    //val xmlloader = new XMLFilenameOnlyMappingDTDLoader(Map("wo-patent-document-v1-3.dtd" -> new InputSource(getClass.getResource("/wo-patent-document-v1-3.dtd").getPath)))
    val xmlloader = XMLIgnoreDTD

    // always one per file
    parseDroppingErrors(xmlloader.load(s))
    //XmlUtils.firstLevelNodes(s).flatMap(node => (node \\ "wopatent-document").flatMap(parseDroppingErrors(_)))
    }
  }
