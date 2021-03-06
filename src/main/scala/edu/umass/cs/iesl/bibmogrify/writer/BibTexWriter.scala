/*
 * Copyright (c) 2013  University of Massachusetts Amherst
 * Licensed under the Apache License, Version 2.0
 * http://www.apache.org/licenses/LICENSE-2.0
 */

package edu.umass.cs.iesl.bibmogrify.writer

import java.lang.String
import edu.umass.cs.iesl.bibmogrify.pipeline.Transformer
import edu.umass.cs.iesl.bibmogrify.NamedPlugin
import edu.umass.cs.iesl.bibmogrify.model._

import RichStructuredCitation._
import edu.umass.cs.iesl.scalacommons.NonemptyString
import edu.umass.cs.iesl.scalacommons.StringUtils._
import scala.collection.GenIterable

/**
 * just writes the fields needed for Rexa2 import for now
 */
object BibTexWriter extends Transformer[StructuredCitation, String] with NamedPlugin {

	// ** add collection open and close tag
	val name = "bibtex"

  val fromType = "StructuredCitation"
  val toType = "String"

	def apply(cm: StructuredCitation) = {
		val opener = cm.doctype.map(_ match {
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
			                            case _ => "@article {"
		                            }).getOrElse("@article {")

		val id: String = cm.primaryId
		val title: Option[(String, NonemptyString)] = cm.title.map(("title", _))

		// no need to filter author roles, because editors etc. are otherContributors
		val authors: Option[(String, NonemptyString)] = {
			val s = cm.authors.map(_.agent).mkString(" and ")
			s.opt.map(("author", _))
		}
		val year: Option[(String, NonemptyString)] = cm.dates.filter(_.eventType == Published).flatMap(_.date.flatMap(_.year)).headOption
		                                             .map(y => ("year", NonemptyString(y.toString)))
		val venue: Option[(String, NonemptyString)] = cm.containedIn.flatMap(_.container.title).map(("journal", _))

		val closer = "},\n"
    
    val keywords : GenIterable[NonemptyString] = cm.containedIn.map(x => x.container.keywords.map(y=>y.word)).getOrElse(Nil)
    val keywordsString = keywords.mkString(", ").opt
    
    val note: Option[(String, NonemptyString)] = keywordsString.map(("note", _)) // cm.containedIn.map(_.container.keywords.map(_.word).mkString(", ")).map(("note", _))

		val fields: Seq[(String, NonemptyString)] = Seq(title, authors, year, venue, note).flatten
		val fieldsString = fields.map(x => "\t" + x._1 + " = {" + x._2.s.replace("{", "\\{").replace("}", "\\}") + "}").mkString(",\n")
		val result = opener + id + ",\n" + fieldsString + closer
		Some(result)
	}
}
