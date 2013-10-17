package edu.umass.cs.iesl.bibmogrify.reader

import edu.umass.cs.iesl.scalacommons.DateUtils._
import edu.umass.cs.iesl.bibmogrify.model._
import com.typesafe.scalalogging.slf4j.Logging
import edu.umass.cs.iesl.bibmogrify.model.Authorities._
import edu.umass.cs.iesl.bibmogrify.pipeline.Transformer
import xml.Node
import collection.immutable.Seq
import edu.umass.cs.iesl.bibmogrify.{NamedInputStream, NamedPlugin, BibMogrifyException}
import edu.umass.cs.iesl.scalacommons.{StringUtils, XmlUtils}
import edu.umass.cs.iesl.namejuggler.PersonNameWithDerivations
import StringUtils._
import RichAddress._
import scala.Predef._
import edu.umass.cs.iesl.bibmogrify.model.BasicPersonIdentifier
import edu.umass.cs.iesl.bibmogrify.model.BasicCitationEvent
import scala.Some
import edu.umass.cs.iesl.bibmogrify.model.BasicContainmentInfo
import edu.umass.cs.iesl.bibmogrify.model.BasicKeyword
import edu.umass.cs.iesl.bibmogrify.model.BasicAddress
import edu.umass.cs.iesl.bibmogrify.model.AuthorInRole
import edu.umass.cs.iesl.scalacommons.NonemptyString
import edu.umass.cs.iesl.bibmogrify.model.BasicStringLocation
import edu.umass.cs.iesl.bibmogrify.model.BasicPartialDate
import edu.umass.cs.iesl.namejuggler.PersonNameParser.PersonNameParsingException

object WosXMLReaderAuthorsOnly extends Transformer[NamedInputStream, StructuredCitation] with Logging with NamedPlugin {

	val name = "wosxmlauthors"

	def apply(nis: NamedInputStream): TraversableOnce[StructuredCitation] = new Traversable[StructuredCitation] {
		def foreach[U](f: (StructuredCitation) => U) {

			val s = nis.getInputStream
			val inLocation = new BasicStringLocation(nis.name, Nil)

			try {
				def ff(rec: Node) {
					assert(rec.label.equals("REC"))
					val result = parseDroppingErrors(inLocation, rec)
					result.foreach(f)
				}
				XmlUtils.firstLevelNodes(s).foreach(n => (n \\ "REC").foreach(ff))
			}
			catch {
				case e => {
					logger.error("Failed to parse " + nis.name, e)
					Nil
				}
			}
			finally {
				s.close()
			}
		}
	}

	def parseDroppingErrors(inLocation: Location, doc: Node): Seq[StructuredCitation] = {
		try {
			val c = parseRec(inLocation, doc)
			c
		}
		catch {
			case e: BibMogrifyException => {
				logger.error(e.getMessage)
				Nil
			}
			case e: PersonNameParsingException => {
				logger.error(e.getMessage)
				Nil
			}
		}
	}

	/**
	 * A rec may have multiple issues and items in it
	 * @param inLocation
	 * @param doc
	 * @return
	 */
	def parseRec(inLocation: Location, doc: Node): Seq[StructuredCitation] = {

		val issues = doc \ "issue"
		val items = doc \ "item"

		val issueRecords: Map[String, (StructuredCitation,Option[NonemptyString],Option[NonemptyString] )] =
			(for (issue <- issues) yield parseIssue(inLocation, issue)).toMap
		val itemRecords = for (item <- items) yield parseItem(inLocation, item, issueRecords)
		itemRecords
	}

	def parseIssue(inLocation: Location, issue: Node): (String, (StructuredCitation,Option[NonemptyString] ,Option[NonemptyString] )) = {

		val subjectNodes = (issue \ "subjects" \ "subject")
		val issueId = (issue \ "@recid").text
		//logger.debug("Found issue " + issueId + " with " + subjectNodes.size + " subject nodes")
		val subjectCodes = subjectNodes.flatMap(n => n \ "@code").flatMap(_.text.opt).toSet

		//logger.debug("Found issue " + issueId + " with subject codes " + subjectCodes.mkString(", "))
		val c = new StructuredCitation() {
			override val keywords: Set[Keyword]           = subjectCodes map (new BasicKeyword(_, WosKeywordAuthority))
			override val title   : Option[NonemptyString] = (issue \ "issue_title").text
		}

		val volumeNumber : Option[NonemptyString] = (issue \ "bib_vol" \ "@volume").text
		val issueNumber : Option[NonemptyString] = (issue \ "bib_vol" \ "@issue").text

		(issueId, (c,volumeNumber,issueNumber))
	}

	def parseItem(inLocation: Location, item: Node, issuesById: Map[String, (StructuredCitation,Option[NonemptyString] ,Option[NonemptyString] )]): StructuredCitation = {

		//val authorSplit = "(.+)( .*)? (.+)".r
		val c = new StructuredCitation() {

			// todo collect other identifiers?
			val wosUtId: String = (item \ "ut").text

			override val identifiers = Seq(BasicIdentifier(wosUtId, WosUtAuthority)).flatten

			override val authors = {
				val authorsNode = item \ "authors"

				// there are at least "primaryauthor" and/or "author" tags containing lastname, firstinitial
				val primaryAuthorNodes = (authorsNode \ "primaryauthor").filterNot(_.text.isEmpty)
				val basicAuthorNodes = (authorsNode \ "author").filterNot(_.text.isEmpty)

				val basicAuthors: Seq[AuthorInRole] = primaryAuthorNodes.map(x => {
					new AuthorInRole(new Person() {
						override val name       : Option[PersonNameWithDerivations] = x.text.opt.map(PersonNameWithDerivations(_))
						override val identifiers: Seq[PersonIdentifier]             = {
							val key = (x \ "@key").text
							if (!key.isEmpty) {
								Seq(new BasicPersonIdentifier(key, WosAuthorAuthority))
							}
							else Nil
						}
					}, List(FirstAuthor))
				}) ++ basicAuthorNodes.map(x => {
					new AuthorInRole(new Person() {
						override val name       : Option[PersonNameWithDerivations] = x.text.opt.map(PersonNameWithDerivations(_))
						override val identifiers: Seq[PersonIdentifier]             = {
							val key = (x \ "@key").text
							if (!key.isEmpty) {
								Seq(new BasicPersonIdentifier(key, WosAuthorAuthority))
							}
							else Nil
						}
					}, Nil)
				})

				// there may also be more detailed records with full names and addresses
				val fullAuthorNodes = authorsNode \ "fullauthorname"

				val fullAuthors = fullAuthorNodes.map(x => {
					val first = (x \ "AuFirstName").text
					val last = (x \ "AuLastName").text
					val assembled = new PersonNameWithDerivations() {
						override val givenNames = first.opt.toSeq
						override val surNames   = last.opt.toSet
					}
					val collective = (x \ "AuCollectiveName").text.opt.map(PersonNameWithDerivations(_))

					val mergedName = collective.map(PersonNameWithDerivations.merge(assembled, _)).getOrElse(assembled)

					new AuthorInRole(new Person() {
						override val name                    = Some(mergedName)
						// it is possible to have a completely empty PersonName
						override val addresses: Set[Address] = (x \ "address").map(f => BasicAddress(Seq(f.text))).toSet
					}, Nil)
				})

				// there may also be an email block
				/*
							<emails count="1">
				<email>
				<name>Crystal, RG</name>
				<email_addr>geneticmedicine@med.cornell.edu</email_addr>
				</email>
				</emails>m
							 */
				val emailsNode = item \ "emails"

				val emailAuthors = emailsNode.map(x => {
					new AuthorInRole(new Person() {
						override val name  = (x \ "name").text.opt.map(PersonNameWithDerivations(_))
						override val email = (x \ "email").text.opt
					}, Nil)
				})

				val m = basicAuthors.map(_.mergeWithMatching(fullAuthors)).map(_.mergeWithMatching(emailAuthors))

				assert(m.head.agent.hasName)

				m
			}

		}
		c
	}

}
