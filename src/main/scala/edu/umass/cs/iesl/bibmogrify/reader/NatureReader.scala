package edu.umass.cs.iesl.bibmogrify.reader

import edu.umass.cs.iesl.scalacommons.StringUtils._
import edu.umass.cs.iesl.bibmogrify.model.CitationUtils._
import edu.umass.cs.iesl.bibmogrify.model.Authorities._
import edu.umass.cs.iesl.scalacommons.DateUtils._
import edu.umass.cs.iesl.bibmogrify.model._
import com.weiglewilczek.slf4s.Logging
import xml.{NodeSeq, Node}
import java.lang.String
import edu.umass.cs.iesl.bibmogrify.pipeline.Transformer
import edu.umass.cs.iesl.bibmogrify.{NamedInputStream, NamedPlugin, BibMogrifyException}
import edu.umass.cs.iesl.scalacommons.XMLIgnoreDTD
import scala.Predef._
import edu.umass.cs.iesl.bibmogrify.model.AuthorInRole
import edu.umass.cs.iesl.scalacommons.NonemptyString
import edu.umass.cs.iesl.bibmogrify.model.BasicStringPageRange
import edu.umass.cs.iesl.bibmogrify.model.BasicCitationEvent
import scala.Some
import edu.umass.cs.iesl.bibmogrify.model.BasicStringLocation
import edu.umass.cs.iesl.bibmogrify.model.BasicNormalPageRange
import edu.umass.cs.iesl.bibmogrify.model.BasicContainmentInfo
import edu.umass.cs.iesl.bibmogrify.model.BasicPartialDate

object NatureReader extends Transformer[NamedInputStream, StructuredCitation] with Logging with NamedPlugin {

  val name = "nature"

  import ReaderUtils._

  def apply(nis: NamedInputStream): TraversableOnce[StructuredCitation] = {
    val a = nis.getInputStream
    val inLocation = new BasicStringLocation(nis.name, Nil)

    try parseDroppingErrors(inLocation, XMLIgnoreDTD.load(a))
    catch {
      case e => {
        logger.error("Failed to parse " + nis.name, e)
        Nil
      }
    }
    finally {
      a.close()
    }
  }

  def parseDroppingErrors(inLocation: Location, doc: Node): TraversableOnce[StructuredCitation] = {
    try {
      val c = parse(inLocation, doc)
      Some(c)
    }
    catch {
      case e: BibMogrifyException => logger.error(e.getMessage); None
      case f => {
        logger.error("Could not parse " + inLocation)
        logger.error(f.getMessage)
      }
      None
    }
  }

  def parse(inLocation: Location, doc: Node): StructuredCitation = {

    val article: NodeSeq = doc \ "article"

    val pubfm = article \ "pubfm"
    val fm = article \ "fm"

    val journalMention = new StructuredCitation {

      val date: Some[BasicPartialDate] = {
        val d = (pubfm \ "idt").text
        val yearS: Option[NonemptyString] = d.substring(4)
        val year: Option[Int] = yearS.map(_.s.toInt)
        val monthS: Option[NonemptyString] = d.substring(4, 6)
        val month: Option[Int] = monthS.map(parseMonthOneBased(_))
        //val dayS: Option[NonemptyString] = (d \ "Day").text
        val day: Option[Int] = None // dayS.map(_.s.toInt)

        Some(BasicPartialDate(year, month, day))
      }

      // drop superscripts, subscripts, italics, and typewriter styles
      override val title: Option[NonemptyString] = (pubfm \ "jtl").text.trim

      // todo interpret pubtype fieldin associated issue
      override val doctype: Option[DocType] = Journal

      override val dates = Seq(BasicCitationEvent(date, Published))
    }


    //val authorSplit = "(.+)( .*)? (.+)".r
    val c = new StructuredCitation() {
      // todo interpret pubtype field
      override val doctype: Option[DocType] = JournalArticle

      // drop superscripts, subscripts, italics, and typewriter styles
      override val title: Option[NonemptyString] = (fm \ "atl").text.trim
      override val dates = journalMention.dates // could grab received, accepted, aop dates too

      override val abstractText: Iterable[TextWithLanguage] = TextWithLanguage(None, (article \ "abs").stripTags)

      override val identifiers: Iterable[Identifier] = BasicIdentifier((pubfm \ "doi").text, DoiAuthority)

      val volume = (pubfm \ "vol").text
      val issue = (pubfm \ "iss").text

      val pages: Option[PageRange] = {

        val fpage: String = (pubfm \ "pp" \ "spn").text.trim
        val lpage: String = (pubfm \ "pp" \ "epn").text.trim
        try {
          val fpagei: Int = fpage.toInt
          val lpagei: Int = lpage.toInt
          Some(BasicNormalPageRange(fpagei, Some(lpagei)))
        }
        catch {
          case e: NumberFormatException => Some(BasicStringPageRange(NonemptyString(fpage), lpage))
        }

      }

      override val containedIn = Some(BasicContainmentInfo(journalMention, None, volume, issue, pages))

      //override val keywords = subjectCodes map (new BasicKeyword(WOSKeywordAuthority, _))
      override val locations = Seq(inLocation)


      override val authors = {
        val corresponding = (fm \ "aug" \ "cau").map(a => {
          new AuthorInRole(Person((a \ "fnm").text.trim, (a \ "snm").text.trim), Seq(FirstAuthor))
        })
        val other =
          (fm \ "aug" \ "au").map(a => {
            new AuthorInRole(Person((a \ "fnm").text.trim, (a \ "snm").text.trim), Nil)
          })
        corresponding ++ other
      }

      // in this variant we just make a full-name string and then parse it again downstream
      /*
             override val authors   = (article \ "AuthorList" \ "Author").map(a =>
                                                {
                                                val fullname = (a \ "ForeName").text.trim + " " + (a \ "LastName").text.trim
                                                new AuthorInRole(Person(fullname), Nil)
                                                })
                                                */
    }
    c
  }
}