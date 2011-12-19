package edu.umass.cs.iesl.bibmogrify.reader

import edu.umass.cs.iesl.scalacommons.StringUtils._
import edu.umass.cs.iesl.scalacommons.DateUtils._
import edu.umass.cs.iesl.bibmogrify.model._
import com.weiglewilczek.slf4s.Logging
import xml.Node
import edu.umass.cs.iesl.scalacommons.XmlUtils
import edu.umass.cs.iesl.bibmogrify.CitationStreamReader
import java.io.InputStream

object IEEEReader extends CitationStreamReader with Logging
  {

  def parse(doc: Node, journalMention: CitationMention, date: Some[BasicPartialDate]): CitationMention =
    {
    val c = new CitationMention()
      {
      // todo interpret pubtype field
      val doctype = JournalArticle

      override val title: String = (doc \ "title").text
      override val dates = Seq(BasicCitationEvent(date, Published))
      override val abstractText: Option[String] = (doc \ "articleInfo" \ "abstract").text
      override val containedIn = Some(BasicContainmentInfo(journalMention, None, None, None, None))
      }
    c
    }

  def parsePublication(pub: Node): TraversableOnce[CitationMention] =
    {
    // assume only one volume
    val journalMention = new CitationMention
      {
      val title = (pub \ "title").text

      // todo interpret pubtype field
      val doctype = Journal
      }

    val date: Some[BasicPartialDate] =
      {
      val month: Option[String] = None //(doc \ "bib_date" \ "@month").text
      val yearS: Option[String] = (pub \ "volume" \ "volumeinfo" \ "year").text
      val year: Option[Int] = yearS.map(_.toInt)

      Some(BasicPartialDate(year, month.map(parseMonthOneBased(_)), None))
      }

    for (a <- pub \ "volume" \ "article") yield
      {
      parse(a, journalMention, date)
      }
    }

  def apply(s: InputStream): TraversableOnce[CitationMention] = XmlUtils.firstLevelNodes(s).flatMap(node => (node \ "publication").flatMap(parsePublication(_)))
  }
