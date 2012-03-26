package edu.umass.cs.iesl.bibmogrify.reader

import edu.umass.cs.iesl.scalacommons.StringUtils._
import edu.umass.cs.iesl.scalacommons.DateUtils._
import edu.umass.cs.iesl.bibmogrify.model._
import edu.umass.cs.iesl.bibmogrify.model.Authorities._
import com.weiglewilczek.slf4s.Logging
import edu.umass.cs.iesl.scalacommons.XMLIgnoreDTD
import edu.umass.cs.iesl.bibmogrify.pipeline.Transformer
import xml.{NodeSeq, Node}
import edu.umass.cs.iesl.bibmogrify.{NamedInputStream, NamedPlugin, BibMogrifyException}

object PatentST36Reader extends Transformer[NamedInputStream, StructuredPatent] with Logging with NamedPlugin {


  val name = "st36"
  /*
  private def parseLanguage(s: String): Option[Language] = {
    // ** since these are standardized, they should go in some more general place
    s match {
      case "en" => English
      case "eng" => English
      case "de" => German
      case "cn" => Chinese
      case "jp" => Japanese
      case "ru" => Russian
      case l => {
        logger.warn("Ignoring unknown language: " + l);
        None
      }
    }
  }*/

  private def parseIdentifierAndDate(d: Option[Node], ct: EventType): (Option[Identifier], Option[CitationEvent]) = {
    (d map ((c: Node) => {
      val id = new BasicIdentifier((c \ "doc-number").text, Some(new BasicIdentifierAuthority("patent-" + (c \ "country").text.trim + "-" + ct.shortName)))
      val date = parseDate(c)
      val event = new BasicCitationEvent(date, ct)
      (Some(id), Some(event))
    })).getOrElse((None, None))
  }

  private val parseDateR = "(....)(..)(..)".r

  private def parseDate(c: Node): Option[BasicPartialDate] = {
    val dateString: String = (c \ "date").text.trim

    dateString match {
      case "" => None
      case d => {

        val parseDateR(yearS, monthS, dayS) = d

        val year: Option[Int] = Some(yearS.toInt)
        // val dayS: Option[String] = (doc \ "day").text
        // val day: Option[Int] = dayS.map(_.toInt)

        Some(BasicPartialDate(year, Some(parseMonthOneBased(monthS)), Some(dayS.toInt)))
      }
    }
  }


  def parse(inLocation: Location, doc: Node): StructuredPatent = {


    def getBodyText: Seq[BodyTextSection] = {
      val s = (doc \ "summary").text match {
        case "" => None
        case t => Some(new BasicBodyTextSection(Summary, t))
      }
      val g = (doc \ "description").text match {
        case "" => None
        case t => Some(new BasicBodyTextSection(GeneralBodyText, t))
      }
      val c = (doc \ "claims").text match {
        case "" => None
        case t => Some(new BasicBodyTextSection(Claims, t))
      }
      Seq(s, g, c).flatten
    }

    // IDs and dates are confounded in the source data; we separate and regroup them
    def getIdentifiersAndDates: (Seq[Identifier], Seq[CitationEvent]) = {
      val (pubId, pubEvent) = parseIdentifierAndDate((doc \ "bibliographic-data" \ "publication-reference" \ "document-id").headOption, Published) // assume exactly one
      val (recId, recEvent) = parseIdentifierAndDate((doc \ "bibliographic-data" \ "application-reference" \ "document-id").headOption, Received) // assume exactly one

      // ignore dates-of-public-availability for now

      val ids = Seq(pubId, recId).flatten
      val events = Seq(pubEvent, recEvent).flatten
      (ids, events)
    }

    // ** legal info, e.g. new owner


    def parseKeywordGroup(seq: NodeSeq, auth: KeywordAuthority): Seq[Keyword] = {
      seq map ((n: Node) => {
        new Keyword {
          override val authority = Some(auth)
          override val word = (n \ "text").text.trim //** ignoring lots of structured data in here
        }
      })
    }

    val c = new StructuredPatent() {
      //override val doctype: Option[DocType] = Patent
      override val locations = Seq(inLocation)
      override val title: Option[String] = (doc \ "bibliographic-data" \ "invention-title").map(_.text.trim).mkString(" ")
      override val (identifiers, dates) = getIdentifiersAndDates

      val abstracts = (doc \ "abstract");
      val abstractsByLanguage = abstracts groupBy {
        n: Node => {
          val langName = (n \ "@lang").text
          val lang = Language.get(langName)
          if (lang == None) {
            logger.warn("Unknown language: " + langName)
          }
          lang
        }
      }

      override val abstractLanguages: Seq[Option[Language]] = abstractsByLanguage.keys.toSeq

      override val abstractText: Option[String] = {

        for ((lang, abs) <- abstractsByLanguage) {
          if (abs.length != 1) logger.error(abs.length + " abstracts for language " + lang.getOrElse("None"))
        }
        val englishAbstracts: Option[NodeSeq] = abstractsByLanguage.get(Some(English))
        val s = englishAbstracts.map(ns => Some(ns.text.trim)).getOrElse(abstractsByLanguage.get(None).map(_.text.trim))
        s
      }
      override val sourceLanguage = Language.get((doc \ "bibliographic-data" \ "language-of-filing").text.trim)
      override val language = Language.get((doc \ "bibliographic-data" \ "language-of-publication").text.trim)

      def parseReferenceGroup(seq: NodeSeq): Seq[StructuredPatent] = {
        seq map ((n: Node) => {
          val (id, event) = parseIdentifierAndDate(Some(n), Published) // ** Hmm: do priority claims refer to the filing date?
          new StructuredPatent {
            override val identifiers = Seq(id).flatten
            override val dates = Seq(event).flatten
          }
        })
      }


      def parseFamily(seq: NodeSeq): Seq[StructuredPatent] = {
        (seq \ "family-member") map ((n: Node) => {
          val d = (n \ "document-id").headOption

          val (id, pubEvent) = parseIdentifierAndDate(d, Published)
          val recEvent: Option[CitationEvent] = d.map(r => (r \ "application-date").headOption.map(q => new BasicCitationEvent(parseDate(q), Received))).getOrElse(None)

          new StructuredPatent {
            override val identifiers = Seq(id).flatten
            override val dates = Seq(pubEvent, recEvent).flatten
          }
        })
      }

      override val keywords = {
        val ipc = parseKeywordGroup(doc \\ "classification-ipc", IpcKeywordAuthority)
        val ipcr = parseKeywordGroup(doc \\ "classification-ipcr", IpcrKeywordAuthority)
        val ecla = parseKeywordGroup(doc \\ "classification-ecla", EclaKeywordAuthority)
        val fterm = parseKeywordGroup(doc \\ "classification-f-term", FtermKeywordAuthority)

        val nationalNodes: NodeSeq = doc \\ "classification-national"
        val nationalKeywords = for (c <- nationalNodes) yield {
          val country = (c \ "country").text.trim
          val auth = new BasicKeywordAuthority(country)
          val ks = parseKeywordGroup(c, auth)
          ks
        }

        var result = Seq(ipc, ipcr, ecla, fterm).flatten ++ nationalKeywords.flatten.toSeq
        result
      }

      override val priorityClaims = parseReferenceGroup(doc \ "bibliographic-data" \ "priority-claims" \ "priority-claim")
      override val structuredReferences = parseReferenceGroup(doc \\ "bibliographic-data" \\ "patcit") ++ parseReferenceGroup(doc \\ "description" \\ "patcit")
      override val searchReportReferences = parseReferenceGroup(doc \\ "srep-citations" \\ "patcit")
      override val mainFamily = parseFamily(doc \ "bibliographic-data" \ "patent-family" \ "main-family")
      override val completeFamily = parseFamily(doc \ "bibliographic-data" \ "patent-family" \ "complete-family")

      override val bodyText = getBodyText
    }
    c
  }

  def parseDroppingErrors(inLocation: Location, doc: Node): Option[StructuredPatent] = {
    try {
      val c = parse(inLocation, doc)
      Some(c)
    }
    catch {
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
  def apply(nis: NamedInputStream): TraversableOnce[StructuredPatent] = {
    //val xml = scala.xml.XML.load(f)
    // val xml = XMLIgnoreDTD.load(f)  // can't, because we need the entity declarations
    //XMLMapDTD.setGlobalXMLCatalogDir(getClass.getResource("/dblp.dtd").getPath)
    //val xmlloader = new XMLFilenameOnlyMappingDTDLoader(Map("dblp.dtd" -> new InputSource(getClass.getResource("/dblp.dtd").getPath)))
    // val xml = xmlloader.load(f)
    //XmlUtils.firstLevelNodes(s).flatMap(node => (node \\ "REC").flatMap(parseDroppingErrors(_)))

    val s = nis.getInputStream
    val inLocation = new BasicStringLocation(nis.name, Nil)
    try {
      XMLIgnoreDTD.load(s).flatMap(parseDroppingErrors(inLocation, _))
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
