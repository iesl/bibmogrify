package edu.umass.cs.iesl.bibmogrify.writer

import java.lang.String
import edu.umass.cs.iesl.bibmogrify.pipeline.Transformer
import edu.umass.cs.iesl.bibmogrify.NamedPlugin
import edu.umass.cs.iesl.bibmogrify.model._

import RichCitationMention._

/**
 * just writes the fields needed for Rexa2 import for now
 */
object BibTexWriter extends Transformer[StructuredCitation, String] with NamedPlugin {

  val name = "bibtex"

  def apply(cm: StructuredCitation) = {
    val opener = cm.doctype.map(
    _ match {
      case JournalArticle => "@article {"
      case ProceedingsArticle => "@inproceedings {"
      case Proceedings => "@proceedings {"
      case BookChapter => "@inbook {"
      case Book => "@book {"
      case TechnicalReport => "@technicalreport {"
      case PhdThesis => "@phdthesis {"
      case MastersThesis => "@mastersthesis {"


      // following may be nonstandard
      case Patent => "@patent {"
      case Grant => "@grant {"
      case WwwArticle => "@wwwarticle {"
      case Journal => "@journal {"
      case CollectionArticle => "@collection {"
      case CollectionOfArticles => "@incollection {"

    }).getOrElse("@article {")

    val id : String = cm.primaryId
    val title: Option[(String, String)] = cm.title.map(("title", _))

    // no need to filter author roles, because editors etc. are otherContributors
    val authors: Option[(String, String)] = Some("authors", cm.authors.flatMap(_.person.name).mkString(" and "))
    val year: Option[(String, String)] = cm.dates.filter(_.eventType == Published).flatMap(_.date.flatMap(_.year)).headOption.map(y => ("year", y.toString))
    val venue: Option[(String, String)] = cm.containedIn.flatMap(_.container.title).map(("venue", _))

    val closer = "},\n"

    val fields: Seq[(String, String)] = Seq(title, authors, year, venue).flatten
    val fieldsString = fields.map(x => "\t" + x._1 + " = {" + x._2.replace("{","\\{").replace("}","\\}") + "}").mkString(",\n")
    val result = opener + id + ",\n" + fieldsString + closer
    Some(result)
  }
}
