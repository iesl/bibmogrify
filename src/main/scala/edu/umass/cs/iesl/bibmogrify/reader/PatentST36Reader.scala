package edu.umass.cs.iesl.bibmogrify.reader

import edu.umass.cs.iesl.scalacommons.DateUtils._
import edu.umass.cs.iesl.bibmogrify.model._
import edu.umass.cs.iesl.bibmogrify.model.Authorities._
import com.typesafe.scalalogging.slf4j.Logging
import edu.umass.cs.iesl.bibmogrify.pipeline.Transformer
import edu.umass.cs.iesl.bibmogrify.{NamedInputStream, NamedPlugin, BibMogrifyException}
import xml.{Elem, NodeSeq, Node}
import edu.umass.cs.iesl.scalacommons.{NonemptyString, XMLIgnoreDTD}

import edu.umass.cs.iesl.scalacommons.StringUtils._
import edu.umass.cs.iesl.namejuggler.PersonNameWithDerivations

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
			val id = BasicIdentifier((c \ "doc-number").text,
			                         Some(new BasicIdentifierAuthority(("patent-" + (c \ "country").text.trim + "-" + ct.shortName).n)))
			val date = parseDate(c)
			val event = new BasicCitationEvent(date, ct)
			(id, Some(event))
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

				Some(BasicPartialDate(year, parseMonthOneBased(monthS), Some(dayS.toInt)))
			}
		}
	}

	def inferBodyTypeFromHeading(s: String, defaultType: BodyTextSectionType): BodyTextSectionType = {
		s match {
			case q: String if q.toLowerCase.contains("field") => TechnicalField
			case q: String if q.toLowerCase.contains("background") => IntroductionAndBackground
			case q: String if q.toLowerCase.contains("introduction") => IntroductionAndBackground
			case q: String if q.toLowerCase.contains("summary") => Summary
			case q: String if q.toLowerCase.contains("figure") => FigureCaptions
			case q: String if q.toLowerCase.contains("drawing") => FigureCaptions
			case q: String => defaultType
			//** could add more here if needed
		}
	}

	def parse(inLocation: Location, doc: Node): StructuredPatent = {

		import ReaderUtils._

		def splitBodyText(n: Node, defaultType: BodyTextSectionType): Seq[BodyTextSection] = {
			val init: List[BodyTextSection] = List[BodyTextSection](new BasicBodyTextSection(GeneralBodyText, "", None))
			// start the fold with a general block, which may enf up empty if we immediately get a heading
			val result = n.child.foldLeft(init)((accum: List[BodyTextSection], n: Node) => {
				n match {
					case np: Elem if np.label == "heading" => {
						new BasicBodyTextSection(inferBodyTypeFromHeading(np.text.trim, defaultType), None, np.text.trim) ::
						accum
					}
					case np: Elem if np.label == "p" => (accum.head ++ np.text.trim) :: accum.tail
					case np => accum //ignore
				}
			})
			result.filterNot(_.text.isEmpty).reverse
			// "background of the invention" or "background art" or just "background"
			//"detailed description of the invention" or "disclosure of the invention" or just "description"
			//"summary"
			//"brief description of the figures"
		}

		/**
		 * Descriptions may contain paragraphs and headers, and/or subsections like "summary" which in turn contain paragraphs and headers.  Here we just
		 * return everything.
		 * @param n
		 * @return
		 */
		def parseDescription(n: Node): Seq[BodyTextSection] = {
			/*
			<xs:element ref="summary" />
                                                <xs:element ref="related-apps" />
                                                <xs:element ref="govt-interest" />
                                                <xs:element ref="detailed-desc" />
                                                <xs:element ref="technical-field" />
                                                <xs:element ref="background-art" />
                                                <xs:element ref="disclosure" />
                                                <xs:element ref="description-of-drawings" />
                                                <xs:element ref="best-mode" />
                                                <xs:element ref="mode-for-invention" />
                                                <xs:element ref="industrial-applicability" />
                                                <xs:element ref="sequence-list-text" />
			 */
			val descriptionElements = Map("summary" -> Summary, "govt-interest" -> GeneralBodyText, "detailed-desc" -> GeneralBodyText,
			                              "technical-field" -> TechnicalField, "background-art" -> IntroductionAndBackground, "disclosure" -> GeneralBodyText,
			                              "description-of-drawings" -> FigureCaptions, "best-mode" -> GeneralBodyText,
			                              "mode-for-inventions" -> GeneralBodyText,
			                              "industrial-applicability" -> GeneralBodyText)

			val result = descriptionElements.toSeq.flatMap {case (k, v) => (n \ k) flatMap (c => splitBodyText(c, v))}

			val nt = splitBodyText(n, GeneralBodyText)

			Seq(result, nt).flatten
		}

		def getBodyText: Seq[BodyTextSection] = {
			// ignore "invention-title" here
			val desc = (doc \ "description") flatMap parseDescription

			val claims = (doc \ "claims").text match {
				case "" => None
				case t => Some(new BasicBodyTextSection(Claims, t, None))
			}
			desc ++ Seq(claims).flatten
		}

		// IDs and dates are confounded in the source data; we separate and regroup them
		def getIdentifiersAndDates: (Seq[Identifier], Seq[CitationEvent]) = {
			val (pubId, pubEvent) = parseIdentifierAndDate((doc \ "bibliographic-data" \ "publication-reference" \ "document-id").headOption,
			                                               Published) // assume exactly one
			val (recId, recEvent) = parseIdentifierAndDate((doc \ "bibliographic-data" \ "application-reference" \ "document-id").headOption,
			                                               Received) // assume exactly one
			// ignore dates-of-public-availability for now
			val ids = Seq(pubId, recId).flatten
			val events = Seq(pubEvent, recEvent).flatten
			(ids, events)
		}

		// ** legal info, e.g. new owner
		def parseKeywordGroup(seq: NodeSeq, auth: KeywordAuthority): Seq[Keyword] = {
			seq flatMap ((n: Node) => {
				val word: Option[NonemptyString] = (n \ "text").stripTags //** ignoring lots of structured data in here
				word.map(x => new BasicKeyword(x, Some(auth)))
			})
		}

		val c = new StructuredPatent() {
      //override val doctype: Option[DocType] = Patent
			override val locations                     = Seq(inLocation)
			override val title: Option[NonemptyString] = (doc \ "bibliographic-data" \ "invention-title").stripTags
			override val (identifiers, dates)          = getIdentifiersAndDates

			val abstracts           = (doc \ "abstract")
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

			// override val abstractLanguages: Seq[Option[Language]] = abstractsByLanguage.keys.toSeq
			override val abstractText: Iterable[TextWithLanguage] = {
				for ((lang, abs) <- abstractsByLanguage) {
					if (abs.length != 1) logger.error(abs.length + " abstracts for language " + lang.getOrElse("None"))
				}
				val withoutHeaders: Map[Option[Language], Option[NonemptyString]] = abstractsByLanguage.map {
					                                                                                            case (l, n) => (l, (n \ "p").stripTags
					                                                                                                               .opt) // exclude headers,
					                                                                                            // as these are likely general
					                                                                                            // uninformative things like "background"
				                                                                                            }

				withoutHeaders.flatMap {
					                       case (l, Some(n)) => Some(TextWithLanguage(l, n))
					                       case _ => None
				                       }
				//val englishAbstracts: Option[NodeSeq] = abstractsByLanguage.get(Some(English))
				//val s = englishAbstracts.map(ns => Some(ns.text.trim)).getOrElse(abstractsByLanguage.get(None).map(_.text.trim))
				//s
			}
			override val sourceLanguage                           = Language.get((doc \ "bibliographic-data" \ "language-of-filing").text.trim)
			override val language                                 = Language.get((doc \ "bibliographic-data" \ "language-of-publication").text.trim)

			def parsePatentCitationGroup(seq: NodeSeq): Seq[StructuredPatent] = {
				seq map ((n: Node) => {
					val (id, event) = parseIdentifierAndDate(Some(n), Published) // ** Hmm: do priority claims refer to the filing date?
					new StructuredPatent {
						override val identifiers = Seq(id).flatten
						override val dates       = Seq(event).flatten
					}
				})
			}
      
      def parseNonPatentCitationGroup(seq: NodeSeq): Seq[String] = {
        seq map ((n: Node) => (n \ "text").text)
      }

			def parseFamily(seq: NodeSeq): Seq[StructuredPatent] = {
				(seq \ "family-member") map ((n: Node) => {
					val d = (n \ "document-id").headOption

					val (id, pubEvent) = parseIdentifierAndDate(d, Published)
					val recEvent: Option[CitationEvent] = d.map(r => (r \ "application-date").headOption
					                                                 .map(q => new BasicCitationEvent(parseDate(q), Received))).getOrElse(None)

					new StructuredPatent {
						override val identifiers = Seq(id).flatten
						override val dates       = Seq(pubEvent, recEvent).flatten
					}
				})
			}

      override val authors = (doc \\ "inventors" \ "inventor").map { inventorNode =>
        new AuthorInRole(new Person() {
          override val name = "%s %s".format((inventorNode \\ "first-name").text, (inventorNode \\ "last-name").text).opt.map{PersonNameWithDerivations(_)}
          // this was giving me grief so I changed it (empty iterator on PersonName.combineGivenNames)
          /*Some(new PersonNameWithDerivations {
            override val firstName = (inventorNode \ "first-name").text.opt
            override val surNames = (inventorNode \ "last-name").text.opt.toSet
          })                    */
        }, Nil)
      }

			override val keywords = {
				val ipc = parseKeywordGroup(doc \\ "classification-ipc", IpcKeywordAuthority)
				val ipcr = parseKeywordGroup(doc \\ "classification-ipcr", IpcrKeywordAuthority)
				val ecla = parseKeywordGroup(doc \\ "classification-ecla", EclaKeywordAuthority)
				val fterm = parseKeywordGroup(doc \\ "classification-f-term", FtermKeywordAuthority)

				val nationalNodes: NodeSeq = doc \\ "classification-national"

				val nationalKeywords = for {c <- nationalNodes
				                             country <- (c \ "country").text.opt  // if country isn't given the node is dropped
				                             auth <- new BasicKeywordAuthority(country)}
				yield parseKeywordGroup(c, auth)



				var result = Set(ipc, ipcr, ecla, fterm).flatten ++ nationalKeywords.flatten
				result
			}

			override val priorityClaims         = parsePatentCitationGroup(doc \ "bibliographic-data" \ "priority-claims" \ "priority-claim")
      
			override val patentCitations             = parsePatentCitationGroup(doc \\ "bibliographic-data" \\ "patcit") ++
			                                      parsePatentCitationGroup(doc \\ "description" \\ "patcit")
      override val nonPatentCitations             = parseNonPatentCitationGroup(doc \\ "bibliographic-data" \\ "nplcit") ++
        parseNonPatentCitationGroup(doc \\ "description" \\ "nplcit")

      lazy val forwardCitations             = parsePatentCitationGroup(doc \\ "bibliographic-data" \\ "fwdcit") ++
        parsePatentCitationGroup(doc \\ "description" \\ "fwdcit")
      
      override val references = patentCitations //++ nonPatentCitations
      
			override val searchReportPatentCitations = parsePatentCitationGroup(doc \\ "srep-citations" \\ "patcit")

      override val searchReportNonPatentCitations = parseNonPatentCitationGroup(doc \\ "srep-citations" \\ "nplcit")
			override val mainFamily             = parseFamily(doc \ "bibliographic-data" \ "patent-family" \ "main-family")
			override val completeFamily         = parseFamily(doc \ "bibliographic-data" \ "patent-family" \ "complete-family")

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
	  //val xmlloader = new XMLFilenameOnlyMappingDTDLoader(Map("wo-patent-document-v1-3.dtd" -> new InputSource(getClass.getResource
	  ("/wo-patent-document-v1-3.dtd").getPath)))
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
		}
		catch {
			case e => {
				logger.error("Failed to parse " + nis.name, e);
				Nil
			}
		}
		finally {
			s.close()
		}
	}
}
