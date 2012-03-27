package edu.umass.cs.iesl.bibmogrify.reader

import edu.umass.cs.iesl.scalacommons.StringUtils._
import edu.umass.cs.iesl.scalacommons.DateUtils._
import edu.umass.cs.iesl.bibmogrify.model._
import com.weiglewilczek.slf4s.Logging
import edu.umass.cs.iesl.bibmogrify.model.Authorities._
import edu.umass.cs.iesl.bibmogrify.pipeline.Transformer
import edu.umass.cs.iesl.scalacommons.XmlUtils
import xml.Node
import collection.immutable.Seq
import edu.umass.cs.iesl.bibmogrify.{NamedInputStream, NamedPlugin, BibMogrifyException}

object WosXMLReader extends Transformer[NamedInputStream, StructuredCitation] with Logging with NamedPlugin {

  val name = "wosxml"


  def apply(nis: NamedInputStream): TraversableOnce[StructuredCitation] = new Traversable[StructuredCitation] {
    def foreach[U](f: (StructuredCitation) => U) {

      val s = nis.getInputStream
      val inLocation = new BasicStringLocation(nis.name, Nil)

      try {
        def ff(rec: Node) {
          assert(rec.label.equals("REC"));
          val result = parseDroppingErrors(inLocation, rec)
          result.foreach(f)
        }
        XmlUtils.firstLevelNodes(s).foreach(n => (n \\ "REC").foreach(ff))
      } catch {
        case e => {
          logger.error("Failed to parse " + nis.name, e); Nil
        }
      }
      finally {
        s.close()
      }
    }
  }

  def parseDroppingErrors(inLocation: Location, doc: Node): Option[StructuredCitation] = {
    try {
      val c = parse(inLocation, doc)
      Some(c)
    }
    catch {
      case e: BibMogrifyException => logger.error(e.getMessage)
      None
    }
  }

  def parse(inLocation: Location, doc: Node): StructuredCitation = {

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

      // todo interpret pubtype field in associated issue
      //val doctype = Journal
    }

    //val authorSplit = "(.+)( .*)? (.+)".r
    val c = new StructuredCitation() {
      // todo interpret pubtype field
      //val doctype = JournalArticle

      // drop superscripts, subscripts, italics, and typewriter styles
      override val title: Option[String] = (item \ "item_title").text
      override val dates = Seq(BasicCitationEvent(date, Published))

      override val abstractText= Seq(new TextWithLanguage(None,(item \ "abstract").text))

      // todo collect other identifiers?
      val wosUtId: String = (item \ "ut").text
      val wosDoi: String = (item \ "article_nos" \ "article_no").text

      override val identifiers = Seq(
        BasicIdentifier(wosUtId, WosUtAuthority),
        new BasicIdentifier((item \ "@recid").text, WosRecidAuthority),
        new BasicIdentifier((item \ "@refkey").text, WosRefkeyAuthority),
        new BasicIdentifier((item \ "@refid").text, WosRefidAuthority),
        new BasicIdentifier((item \ "i_cid").text, WosCidAuthority),
        new BasicIdentifier(wosDoi, DoiAuthority)
      ).filter(!_.value.isEmpty)


      override val authors = {
        val authorsNode = item \ "authors"

        // there are at least "primaryauthor" and/or "author" tags containing lastname, firstinitial
        val primaryAuthorNodes = (authorsNode \ "primaryauthor").filterNot(_.text.isEmpty)
        val basicAuthorNodes = (authorsNode \ "author").filterNot(_.text.isEmpty)

        val basicAuthors: Seq[AuthorInRole] = primaryAuthorNodes.map(x => {

          new AuthorInRole(new Person() {
            override val name: Option[String] = Person.cleanupName(x.text)
            override val identifiers: Seq[PersonIdentifier] = {
              val key = (x \ "@key").text
              if (!key.isEmpty) {
                Seq(new BasicPersonIdentifier(key, WosAuthorAuthority))
              } else Nil
            }
          }, List(FirstAuthor)
          )
        }) ++ basicAuthorNodes.map(x => {
          new AuthorInRole(new Person() {
            override val name: Option[String] = Person.cleanupName(x.text)
            override val identifiers: Seq[PersonIdentifier] = {
              val key = (x \ "@key").text
              if (!key.isEmpty) {
                Seq(new BasicPersonIdentifier(key, WosAuthorAuthority))
              } else Nil
            }
          }, Nil
          )
        })

        // there may also be more detailed records with full names and addresses
        val fullAuthorNodes = authorsNode \ "fullauthorname"

        val fullAuthors = fullAuthorNodes.map(x => {
          val first = (x \ "AuFirstName").text
          val last = (x \ "AuLastName").text
          val assembled = Person.cleanupName(first + " " + last)
          val collective = Person.cleanupName((x \ "AuCollectiveName").text)

          // choose the longer of the assembled vs collective names, on the assumption that it's more informative
          val chosenFullName = if (assembled.length() >= collective.length()) assembled else collective;

          new AuthorInRole(new Person() {
            override val name: Option[String] = chosenFullName
            // override val address:
          }, Nil
          )
        })

        val m = basicAuthors.map(_.mergeWithMatching(fullAuthors))

        assert(!m.head.person.name.isEmpty)

        m


      }

      // TODO implement parsePages, or just store the string
      def parsePages(s: String): Option[PageRange] = None

      override val containedIn = Some(BasicContainmentInfo(venueMention, None, None, None, None))

      override val keywords = subjectCodes map (new BasicKeyword(_, WosKeywordAuthority))

      override val locations = Seq(inLocation)


      override val structuredReferences = (item \ "refs" \ "ref").zipWithIndex.map(parseRef(_, wosUtId))
    }
    c
  }


  def parseRef(nodeWithIndex: (Node, Int), idBasis: String): StructuredCitation = new StructuredCitation {

    val (node, index) = nodeWithIndex

    override val authors: Seq[AuthorInRole] = Seq(new AuthorInRole(new Person() {
      override val name: Option[String] = (node \ "@auth").text
    }, List(FirstAuthor)))

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
      new BasicIdentifier(idBasis + "-ref-" + index, WosUtRefIndexAuthority),
      new BasicIdentifier((node \ "@recid").text, WosRecidAuthority),
      new BasicIdentifier((node \ "@refkey").text, WosRefkeyAuthority),
      new BasicIdentifier((node \ "@refid").text, WosRefidAuthority),
      new BasicIdentifier((node \ "@cid").text, WosCidAuthority),
      new BasicIdentifier((node \ "@artno").text, DoiAuthority)
    ).filter(!_.value.isEmpty)
  }

}
