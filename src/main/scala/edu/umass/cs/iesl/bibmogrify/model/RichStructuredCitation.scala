package edu.umass.cs.iesl.bibmogrify.model

import actors.threadpool.AtomicInteger
import com.weiglewilczek.slf4s.Logging
import edu.umass.cs.iesl.scalacommons.{StringUtils, NonemptyString}
import StringUtils.unwrapNonemptyString
import StringUtils.enrichString
import StringUtils.toOptionNonempty
import java.util.UUID

object RichStructuredCitation {
	//DetectorFactory.loadProfiles(Language.majorLanguages.map(_.name).toList: _*);
	implicit def enrichStructuredCitation(cm: StructuredCitation): RichStructuredCitation = new RichStructuredCitation(cm)

	implicit def iterableTextWithLanguageToMap(i: Iterable[TextWithLanguage]): Map[Option[Language], NonemptyString] = {
		i.map {
			      x => (x.language, x.text)
		      }.toMap
	}

	def cleanup(s: NonemptyString): String = s.toLowerCase.maskPunctuation.maskNewlines.collapseWhitespace.trim

	def cleanup(os: Option[NonemptyString]): String = os.map(cleanup).getOrElse("")

	def cleanupJoined(ss: Iterable[NonemptyString]): String = cleanup(ss.mkString(" "))

	//def cleanup(ss: Iterable[String]): String = cleanup(wrapNonemptyString(ss.mkString(" ")))
}

class RichStructuredCitation(cm: StructuredCitation) extends Logging {

	import RichStructuredCitation.enrichStructuredCitation
	import RichStructuredCitation.iterableTextWithLanguageToMap
	import RichStructuredCitation.cleanup
	import RichStructuredCitation.cleanupJoined
	import RichPerson.enrichPerson

	//val cleanAbstract = paperAbstract.toLowerCase.replaceAll("\\s", " ").replaceAll("[^\\w ]", " ").split(" +").mkString(" ")
	//val cleanBody = body.toLowerCase.replaceAll("\\s", " ").replaceAll("[^\\w ]", " ").split(" +").mkString(" ")
	lazy val cleanTitle = cleanup(cm.title)
	val englishAbstract: Option[NonemptyString] = cm.abstractText.get(Some(English)).orElse(cm.abstractText.get(None).orElse(None))

	lazy val cleanAbstract      = cleanup(englishAbstract)
	lazy val cleanAbstractWords = cleanAbstract.split(" ").filter(_.nonEmpty).length

	lazy val cleanTitleAndAbstract = (cleanTitle + " " + cleanAbstract).trim

	lazy val cleanSummary = cleanupJoined(cm.textOfType(Summary))
	lazy val cleanClaims  = cleanupJoined(cm.textOfType(Claims))

	lazy val cleanField              = cleanupJoined(cm.textOfType(TechnicalField))
	lazy val cleanBackground         = cleanupJoined(cm.textOfType(IntroductionAndBackground))
	lazy val cleanFieldAndBackground = (cleanField + " " + cleanBackground).trim

	lazy val cleanGeneralBody = cleanupJoined(cm.textOfType(GeneralBodyText))
	//lazy val cleanIntro = cm.textOfType(IntroductionAndBackground).map(_.replaceAll("\\s", " ")).mkString("").trim
	lazy val cleanBody        = cleanupJoined(cm.bodyText.map(_.text).flatten)

	lazy val cleanBodyWords = {
		val words = cleanBody.split(" ").filter(_.nonEmpty).length
	}
	lazy val cleanTotal     = (cleanTitleAndAbstract + " " + cleanSummary + " " + cleanBody).trim

	def totalTextSize = cleanTotal.size

	def allPrimaryEvents = cm.dates.filter(e => (e.eventType.primaryPriority != 0)).toSeq.sortBy(e => -e.eventType.primaryPriority)

	lazy val bestPrimaryPriority = allPrimaryEvents.headOption.map(_.eventType.primaryPriority).getOrElse(-1)

	def bestPrimaryEvents = allPrimaryEvents.filter(e => (e.eventType.primaryPriority == bestPrimaryPriority && e.date != None)).sortBy(_.date.get)

	def primaryEvent = bestPrimaryEvents.headOption

	def primaryDate = primaryEvent.flatMap(_.date)

	def year: Option[Int] = primaryDate.flatMap(_.year)

	// todo parameterize/refactor authority short names; attach priorities directly to authorities; etc
	val authorityPriority: Map[String, Int] = Map("wos-ut" -> 200, "wos-ut-ref" -> 190, "wos-cid" -> 180, "doi" -> 100, "pii" -> 90, "dc" -> 1,
	                                              "pubmed" -> 10, "" -> 0)

	def qualifiedIdsInOrder = cm.identifiers.toSeq.sortBy(x => {
		val sortOrder: Option[Int] = authorityPriority.get(x.authority.map(_.shortName.s).getOrElse(""))
		-sortOrder.getOrElse(0)
	}).map(_.qualifiedValue)

	def primaryId = {
		qualifiedIdsInOrder.headOption.orElse(cm.locations.headOption).getOrElse("adhoc:" + UUID.randomUUID())
	}

	def authorFullNames: Seq[String] = cm.authors.map(_.agent.toString)

	// get the first author if it is a person
	def personFirstAuthor: Option[Person] = cm.authors.map(_.agent).filter(_.isInstanceOf[Person]).headOption.asInstanceOf[Option[Person]]

	def firstAuthorLastName: Option[NonemptyString] = personFirstAuthor.flatMap(_.name).flatMap(_.longestSurName)

	//bestFullName)
	def authorFullNamesWithId: Seq[String] = cm.authors.map(air => {
		val a: Agent = air.agent
		a match {
			case p: Person => {
				val id = p.primaryId

				val r: String = p + id.map(" [" + _ + "]").getOrElse("") // p.bestFullName
				r
			}
			case i: Institution => {
				// ** institutions don't have identifiers.  Perhaps all the ID stuff should be at the
				// Agent level
				i.toString
			}
		}
	})

	/*
		def detectAbstractLanguages: Iterable[String] = {
		  try {
			cm.abstractText map { case (l:Language,a:String) => {
			  val detector: Detector = DetectorFactory.create();
			  detector.append(a);
			  detector.detect()
			}}
		  } catch {
			case e: LangDetectException => None
		  }
		}
	  */
	def listAbstractLanguages: String = cm.abstractText.keys.flatten.map(_.toString).toSeq.sorted.mkString(",")

	/*
		def listAbstractLanguages: String = {
		  cm.abstractLanguages.map({
			case None => "None"
			case Some(x) => x.toString
		  }).sorted.mkString(",")
		}
	  */
	def allKeywordsCountByAuthority: String = {
		val counts = cm.allKeywords.groupBy(k => k.authority).map {
			                                                       case (auth, ks) => (auth.get.name, ks.size)
		                                                       }
		counts.map(x => x._1 + ":" + x._2).mkString(",")
	}

	def allKeywordsByAuthority: String = {
		cm.allKeywords.map(x => {
			val w = x.word
			// ensure result is easy to parse
			val v = if (w.contains(":") || w.contains(",")) {
				if (w.contains("\"") || w.contains("'")) {
					logger.warn("Keyword contains quotes: " + w)
				}
				"'" + w + "'"
			}
			else w
			x.authority.get.name + ":" + v
		}).mkString(",")
	}

	def allKeywords: Iterable[Keyword] = cm.keywords ++ (cm.containedIn.map(_.container.allKeywords).flatten)

	def allAddresses: Seq[Address] = cm.addresses ++ cm.authors.flatMap(_.agent.addresses)

	def institutionRatios: Map[InstitutionType, Double] = {
		import RichAddress._
		val instTypes: Map[InstitutionType, Int] = allAddresses.flatMap(_.inferredInstitutionType).groupBy(x => x).mapValues(_.size)
		val total: Double = instTypes.values.sum
		instTypes.mapValues(_ / total)
	}

	def rootContainedIn: StructuredCitation = {
		cm.containedIn.map(_.container.rootContainedIn).getOrElse(cm)
	}

	def rootContainedInNotSelf: Option[StructuredCitation] = {
		cm.containedIn.map(x => Some(x.container.rootContainedIn)).getOrElse(None)
	}

	def volume: Option[NonemptyString] = {
		cm.containedIn.flatMap(_.volume)
	}

	def numPages: Option[Int] = {
		cm.containedIn.flatMap(_.pages.flatMap(_.numPages))
	}

	/*
		public void init(String profileDirectory) throws LangDetectException {
		  DetectorFactory.loadProfile(profileDirectory);
		}
	  */
	// + "\t" + referenceIds.mkString(", ")
}

object RichPerson {

	implicit def enrichPerson(p: Person): RichPerson = new RichPerson(p)

	val adhocIdIncrementor: AtomicInteger = new AtomicInteger(0)
}

class RichPerson(p: Person) {
	val authorityPriority: Map[String, Int] = Map("wos-author" -> 200, "" -> 0)

	def qualifiedIdsInOrder = p.identifiers.sortBy(x => {
		val sortOrder: Option[Int] = authorityPriority.get(x.authority.map(_.shortName.s).getOrElse(""))
		-sortOrder.getOrElse(0)
	}).map(_.qualifiedValue)

	def primaryId = qualifiedIdsInOrder.headOption

	//.getOrElse("adhoc:" + RichPerson.adhocIdIncrementor.getAndIncrement)
}
