package edu.umass.cs.iesl.bibmogrify.tagmodel

import edu.umass.cs.iesl.bibmogrify.pipeline.Transformer
import java.net.URL
import edu.umass.cs.iesl.bibmogrify.NamedPlugin
import edu.umass.cs.iesl.bibmogrify.model._
import com.weiglewilczek.slf4s.Logging
import edu.umass.cs.iesl.scalacommons.NonemptyString

import edu.umass.cs.iesl.scalacommons.StringUtils._

/**
 * @author <a href="mailto:dev@davidsoergel.com">David Soergel</a>
 * @version $Id$
 */
object StandardLabels extends LabelSet with Logging {
	val validLabels = Seq("authors", "author", "date", "title", "volume", "pages", "booktitle", "editor", "institution", "conference", "journal", "location",
	                      "note", "publisher", "tech");

	val headerSectionLabels    = Seq()
	val referenceSectionLabels = Seq()
	val referenceLabels        = Seq()
	val mergeableLabels        = List[String]("": String)

	val topLevelRecordLabels = Seq("REC", "doc", "NEWREFERENCE")

	val yearRE = """.*((19|20)\d\d).*""".r

	def toStructuredCitation(cwr: TaggedCitationWithReferences) = {
		val c = cwr.parent
		//throw new NotImplementedException()
		new StructuredCitation {
			override val title   = c.get("title").headOption.map(NonemptyString(_))
			override val authors = c.get("author").flatMap(n => n.trimPunctuation.opt).map(n => new AuthorInRole(Person(n), Nil))

			val year: Option[Int] = {
				val od: Option[String] = c.get("date").headOption
				od.flatMap(d => {
					try {
						val yearRE(y, ignore) = d
						Some(y.toInt)
					}
					catch {
						case e: MatchError => {
							logger.error("bad date:" + d)
							None
						}
					}
				})
			}
			override val abstractText: Iterable[TextWithLanguage] = c.get("abstract").headOption.flatMap(x => TextWithLanguage(None, x.opt))

			override val dates = Seq(new BasicCitationEvent(Some(new BasicPartialDate(year, None, None)), Published))

			val venueMention = new StructuredCitation {
				// drop superscripts, subscripts, italics, and typewriter styles
				override val title: Option[NonemptyString] = c.get("journal").headOption match {
					case None => c.get("conference").headOption.map(NonemptyString(_))
					case x => x.map(NonemptyString(_))
				}

				// todo interpret pubtype field in associated issue
				//val doctype = Journal
			}
			override val containedIn = Some(BasicContainmentInfo(venueMention, None, None, None, None))

			override val references = cwr.references.map(_.toStructuredCitation)
		}
	}
}

// with Transformer[URL, TaggedCitation] needs to be explicit for the sake of the plugin discovery
object StandardLabelXMLReader extends TaggedCitationXMLReader(StandardLabels) with Transformer[URL, TaggedCitation] with NamedPlugin {
	val name = "standardLabels"
}

object ExtendedLabelXMLReader extends TaggedCitationXMLReader(new ExtendedLabels()) with Transformer[URL, TaggedCitation] with NamedPlugin {
	val name = "extendedLabels"
}

object ExtendedLabelXMLReaderHlabeled
		extends TaggedCitationXMLReader(new ExtendedLabels(Seq("reference-hlabeled"))) with Transformer[URL, TaggedCitation] with NamedPlugin {
	val name = "extendedLabels"
}

// todo abstract away cut-and-paste
class ExtendedLabels(val referenceLabels: Seq[String] = Seq("reference", "reference-hlabeled")) extends LabelSet with Logging {
	val validLabels = StandardLabels.validLabels ++ Seq(// "author-affix", "author-first", "author-middle", "author-last",
	                                                    "abstract", "address",
	                                                    //"biblio-hlabeled",
	                                                    "biblioEpilogue", "biblioPrologue", "body", "email",
	                                                    //"headers-hlabeled",
	                                                    "keyword", "number", "phone", "ref-marker",
	                                                    //"reference-hlabeled",
	                                                    //"reference",
	                                                    "series", "thesis", "web")

	val headerSectionLabels    = Seq("headers-hlabeled")
	val referenceSectionLabels = Seq("biblio-hlabeled")
	//val referenceLabels = Seq("reference", "reference-hlabeled")
	val mergeableLabels        = List[String]("": String)
	//val untagged : String = ""
	val topLevelRecordLabels   = Seq("REC", "doc", "NEWREFERENCE")

	val yearRE = """.*((19|20)\d\d).*""".r

	def toStructuredCitation(cwr: TaggedCitationWithReferences) = {
		val c = cwr.parent
		//throw new NotImplementedException()
		new StructuredCitation {
			override val title = c.get("title").headOption.map(NonemptyString(_))

			override val authors = {
				//** need to parse authors/author/author-first etc., taking the grouping into account
				//** As a work'round we just ignore the subtags for now by commenting them out above
				val individualAuthors = (c.get("author") ++ c.get("authors/author")).flatMap(n => n.trimPunctuation.opt).map(n => new AuthorInRole(Person(n), Nil))
				val result = if (!individualAuthors.isEmpty) individualAuthors
				else {
					val combinedAuthors = c.get("authors").flatMap(s => s.split("\\band\\b").flatMap(_.split(",")))
					                      .flatMap(n => n.trimPunctuation.opt).map(n => new AuthorInRole(Person(n), Nil))

					combinedAuthors
				}
				result
			}

			val year: Option[Int] = {
				val od: Option[String] = c.get("date").headOption
				od.flatMap(d => {
					try {
						val yearRE(y, ignore) = d
						Some(y.toInt)
					}
					catch {
						case e: MatchError => {
							logger.error("bad date:" + d)
							None
						}
					}
				})
			}

			override val abstractText: Iterable[TextWithLanguage] = c.get("abstract").headOption.flatMap(x => TextWithLanguage(None, x.opt))

			override val dates = year.map(y => new BasicCitationEvent(Some(new BasicPartialDate(Some(y), None, None)), Published)).toSeq

			val venueMention = new StructuredCitation {
				// drop superscripts, subscripts, italics, and typewriter styles
				override val title: Option[NonemptyString] = c.get("journal").headOption match {
					case None => {
						c.get("conference").headOption match {
							case None => {
								if (!c.get("tech").isEmpty) c.get("institution").headOption.map(NonemptyString(_)) else None
							}
							case y => y.map(NonemptyString(_))
						}
					}
					case x => x.map(NonemptyString(_))
				}

				// todo interpret pubtype field in associated issue
				//val doctype = Journal
			}
			override val containedIn = Some(BasicContainmentInfo(venueMention, None, None, None, None))

			override val references = cwr.references.map(_.toStructuredCitation)
		}
	}
}

/* <abstract>
<address>
<author-affix>
<author-first>
<author-last>
<author-middle>
<author>
<authors>
<biblio-hlabeled>
<biblioEpilogue>
<biblioPrologue>
<body>
<booktitle>
<conference>
<date>
<editor>
<email>
<headers-hlabeled>
<institution>
<journal>
<keyword>
<note>
<number>
<phone>
<publisher>
<ref-marker>
<reference-hlabeled>
<reference>
<series>
<tech>
<thesis>
<title>
<volume>
<web>*/
