package edu.umass.cs.iesl.bibmogrify.writer

import java.lang.String
import edu.umass.cs.iesl.bibmogrify.pipeline.Transformer
import edu.umass.cs.iesl.bibmogrify.NamedPlugin
import edu.umass.cs.iesl.bibmogrify.model._

import RichStructuredCitation._
import edu.umass.cs.iesl.scalacommons.NonemptyString
import edu.umass.cs.iesl.scalacommons.StringUtils._

/**
 * just writes the fields needed for Rexa2 import for now
 */
object BibTexWriter extends Transformer[StructuredCitation, String] with NamedPlugin
	{

	// ** add collection open and close tag
	val name = "bibtex"

	def apply(cm: StructuredCitation) =
		{
		val opener = cm.doctype.map(_ match
		                            {
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

		val id: String = cm.primaryId
		val title: Option[(String, NonemptyString)] = cm.title.map(("title", _))

		// no need to filter author roles, because editors etc. are otherContributors
		val authors: Option[(String, NonemptyString)] =
			{
			val s = cm.authors.map(_.agent).mkString(" and ")
			emptyStringToNone(s).map(("author", _))
			}
		val year: Option[(String, NonemptyString)] = cm.dates.filter(_.eventType == Published).flatMap(_.date.flatMap(_.year)).headOption
		                                             .map(y => ("year", NonemptyString(y.toString)))
		val venue: Option[(String, NonemptyString)] = cm.containedIn.flatMap(_.container.title).map(("journal", _))

		val closer = "},\n"

		val fields: Seq[(String, NonemptyString)] = Seq(title, authors, year, venue).flatten
		val fieldsString = fields.map(x => "\t" + x._1 + " = {" + x._2.s.replace("{", "\\{").replace("}", "\\}") + "}").mkString(",\n")
		val result = opener + id + ",\n" + fieldsString + closer
		Some(result)
		}
	}
