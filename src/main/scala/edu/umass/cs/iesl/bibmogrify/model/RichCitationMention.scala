package edu.umass.cs.iesl.bibmogrify.model

import actors.threadpool.AtomicInteger

class RichCitationMention(cm: CitationMention)
  {

  //val cleanAbstract = paperAbstract.toLowerCase.replaceAll("\\s", " ").replaceAll("[^\\w ]", " ").split(" +").mkString(" ")
  //val cleanBody = body.toLowerCase.replaceAll("\\s", " ").replaceAll("[^\\w ]", " ").split(" +").mkString(" ")

  lazy val cleanTitle = cm.title.replaceAll("\\s", " ").trim
  lazy val cleanAbstract = cm.abstractText.map(_.replaceAll("\\s", " ")).getOrElse("").trim
  lazy val cleanTitleAndAbstract = cleanTitle + " " + cleanAbstract
  lazy val cleanIntro = cm.introText.map(_.replaceAll("\\s", " ")).getOrElse("").trim
  lazy val cleanBody = cm.bodyText.map(_.replaceAll("\\s", " ")).getOrElse("").trim
  lazy val cleanTotal = cleanTitleAndAbstract + " " + cleanIntro + " " + cleanBody
  def totalTextSize = cleanTotal.size

  def allPrimaryEvents = cm.dates.filter(e => (e.eventType.primaryPriority != 0)).sortBy(e => -e.eventType.primaryPriority)
  lazy val bestPrimaryPriority = allPrimaryEvents.headOption.map(_.eventType.primaryPriority).getOrElse(-1)
  def bestPrimaryEvents = allPrimaryEvents.filter(e => (e.eventType.primaryPriority == bestPrimaryPriority && e.date != None)).sortBy(_.date.get)
  def primaryEvent = bestPrimaryEvents.headOption
  def primaryDate = primaryEvent.flatMap(_.date)
  def year: Option[Int] = primaryDate.flatMap(_.year)

  // todo parameterize/refactor authority short names
  val authorityPriority : Map[String, Int] = Map("doi"->100,"pubmed"->10)

  def qualifiedIdsInOrder = cm.identifiers.sortBy(x => authorityPriority(x.authority.shortName)).map(_.qualifiedValue)

  val adhocIdIncrementor : AtomicInteger = new AtomicInteger(0)

  def primaryId = qualifiedIdsInOrder.headOption.getOrElse("adhoc:" + adhocIdIncrementor.getAndIncrement)

  def authorFullNames : Seq[String] = cm.authors.flatMap(_.person.name)

  // + "\t" + referenceIds.mkString(", ")

  }

object RichCitationMention
  {
  implicit def enrichCitationMention(cm: CitationMention): RichCitationMention = new RichCitationMention(cm)
  }
