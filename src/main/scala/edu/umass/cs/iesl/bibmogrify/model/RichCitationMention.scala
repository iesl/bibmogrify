package edu.umass.cs.iesl.bibmogrify.model

import actors.threadpool.AtomicInteger
import com.cybozu.labs.langdetect.DetectorFactory
import edu.umass.cs.iesl.scalacommons.StringUtils

object RichCitationMention {
  DetectorFactory.loadProfiles(Language.majorLanguages.map(_.name).toList: _*);

  implicit def enrichCitationMention(cm: StructuredCitation): RichCitationMention = new RichCitationMention(cm)

  val adhocIdIncrementor: AtomicInteger = new AtomicInteger(0)

  implicit def iterableTextWithLanguageToMap(i: Iterable[TextWithLanguage]): Map[Option[Language], String] = {
    i.map {
      x => (x.language, x.text)
    }.toMap
  }

  import StringUtils.enrichString

  def cleanup(s: String): String = s.toLowerCase.removePunctuation.removeNewlines.collapseWhitespace.trim

  def cleanup(os: Option[String]): String = os.map(cleanup).getOrElse("")

  def cleanup(ss: Iterable[String]): String = cleanup(ss.mkString(" "))
}

class RichCitationMention(cm: StructuredCitation) {

  import RichCitationMention.enrichCitationMention
  import RichCitationMention.iterableTextWithLanguageToMap
  import RichCitationMention.cleanup
  import RichPerson.enrichPerson

  //val cleanAbstract = paperAbstract.toLowerCase.replaceAll("\\s", " ").replaceAll("[^\\w ]", " ").split(" +").mkString(" ")
  //val cleanBody = body.toLowerCase.replaceAll("\\s", " ").replaceAll("[^\\w ]", " ").split(" +").mkString(" ")

  lazy val cleanTitle = cleanup(cm.title)
  val englishAbstract: String = cm.abstractText.get(Some(English)).getOrElse(cm.abstractText.get(None).getOrElse(""))

  lazy val cleanAbstract = cleanup(englishAbstract)

  lazy val cleanTitleAndAbstract = cleanTitle + " " + cleanAbstract
  lazy val cleanSummary = cleanup(cm.textOfType(Summary))
  lazy val cleanClaims = cleanup(cm.textOfType(Claims))
  lazy val cleanGeneralBody = cleanup(cm.textOfType(GeneralBodyText))
  //lazy val cleanIntro = cm.textOfType(IntroductionAndBackground).map(_.replaceAll("\\s", " ")).mkString("").trim
  lazy val cleanBody = cleanup(cm.bodyText.map(_.text))
  lazy val cleanTotal = cleanTitleAndAbstract + " " + cleanSummary + " " + cleanBody

  def totalTextSize = cleanTotal.size

  def allPrimaryEvents = cm.dates.filter(e => (e.eventType.primaryPriority != 0)).toSeq.sortBy(e => -e.eventType.primaryPriority)

  lazy val bestPrimaryPriority = allPrimaryEvents.headOption.map(_.eventType.primaryPriority).getOrElse(-1)

  def bestPrimaryEvents = allPrimaryEvents.filter(e => (e.eventType.primaryPriority == bestPrimaryPriority && e.date != None)).sortBy(_.date.get)

  def primaryEvent = bestPrimaryEvents.headOption

  def primaryDate = primaryEvent.flatMap(_.date)

  def year: Option[Int] = primaryDate.flatMap(_.year)

  // todo parameterize/refactor authority short names; attach priorities directly to authorities; etc
  val authorityPriority: Map[String, Int] = Map("wos-ut" -> 200, "wos-ut-ref" -> 190, "wos-cid" -> 180, "doi" -> 100, "pubmed" -> 10, "" -> 0)

  def qualifiedIdsInOrder = cm.identifiers.toSeq.sortBy(x => {
    val sortOrder: Option[Int] = authorityPriority.get(x.authority.map(_.shortName).getOrElse(""))
    -sortOrder.getOrElse(0)
  }).map(_.qualifiedValue)


  def primaryId = qualifiedIdsInOrder.headOption.getOrElse("adhoc:" + RichCitationMention.adhocIdIncrementor.getAndIncrement)

  def authorFullNames: Seq[String] = cm.authors.flatMap(_.person.name)

  def authorFullNamesWithId: Seq[String] = cm.authors.map(air => {
    val p: Person = air.person
    val id = p.primaryId

    val r: String = p.name.getOrElse("") + id.map(" [" + _ + "]").getOrElse("")
    r
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
  def keywordsCountByAuthority: String = {
    val counts = cm.keywords.groupBy(k => k.authority).map {
      case (auth, ks) => (auth.get.name, ks.size)
    }
    counts.map(x => x._1 + ":" + x._2).mkString(",")
  }

  def rootContainedIn: StructuredCitation = {
    cm.containedIn.map(_.container.rootContainedIn).getOrElse(cm)
  }

  def rootContainedInNotSelf: Option[StructuredCitation] = {
    cm.containedIn.map(x => Some(x.container.rootContainedIn)).getOrElse(None)
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
    val sortOrder: Option[Int] = authorityPriority.get(x.authority.map(_.shortName).getOrElse(""))
    -sortOrder.getOrElse(0)
  }).map(_.qualifiedValue)


  def primaryId = qualifiedIdsInOrder.headOption

  //.getOrElse("adhoc:" + RichPerson.adhocIdIncrementor.getAndIncrement)

}
