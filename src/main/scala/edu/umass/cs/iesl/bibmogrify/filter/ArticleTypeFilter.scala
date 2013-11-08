/*
 * Copyright (c) 2013  University of Massachusetts Amherst
 * Licensed under the Apache License, Version 2.0
 * http://www.apache.org/licenses/LICENSE-2.0
 */

package edu.umass.cs.iesl.bibmogrify.filter

import edu.umass.cs.iesl.bibmogrify.model._
import com.typesafe.scalalogging.slf4j.Logging
import edu.umass.cs.iesl.bibmogrify.NamedPlugin
import edu.umass.cs.iesl.bibmogrify.pipeline.Transformer

object ArticleTypeFilter extends Transformer[StructuredCitation, StructuredCitation] with NamedPlugin with Logging {
	val name = "wosphysical"

  val fromType = "StructuredCitation"
  val toType = "StructuredCitation"
  
	/*
	case object JournalArticle extends DocType
	case object ResearchArticle extends DocType(Some(JournalArticle))
	case object ReviewArticle extends DocType(Some(JournalArticle))
	case object NoteArticle extends DocType(Some(JournalArticle))
	case object Biographical extends DocType(Some(JournalArticle))
	case object Correction extends DocType(Some(JournalArticle))
	case object Bibliography extends DocType(Some(JournalArticle))
	case object Editorial extends DocType(Some(JournalArticle))
	case object Letter extends DocType(Some(JournalArticle))
	// possibly ambiguous: a Science "letter" is really a ResearchArticle, not a letter to the editor
	case object CriticalReview extends DocType
	// theater, music, etc.
	case object BookReview extends DocType(Some(CriticalReview))
	case object ProductReview extends DocType
	case object Creative extends DocType
	// poetry, fiction etc.
	case object Journal extends DocType
	case object ProceedingsArticle extends DocType
	case object Proceedings extends DocType
	case object CollectionArticle extends DocType
	case object CollectionOfArticles extends DocType
	case object BookChapter extends DocType
	case object Book extends DocType
	case object TechnicalReport extends DocType
	case object Patent extends DocType
	case object PhdThesis extends DocType
	case object MastersThesis extends DocType
	case object Grant extends DocType
	case object WwwArticle extends DocType
	case object Other extends DocType
	 */
	// just accept things that could be papers,
	val okTypes = List(JournalArticle, ResearchArticle, ReviewArticle, NoteArticle, Editorial, // debateable
	                   Letter, // debateable
	                   Journal, ProceedingsArticle, Proceedings, CollectionArticle, CollectionOfArticles, BookChapter, Book, TechnicalReport, Patent,
	                   PhdThesis,
	                   MastersThesis, Grant)

	def apply(cm: StructuredCitation) = {
		val result = okTypes.contains(cm.doctype)
		logger.debug("Filter " + (if (result) "PASSED" else "FAILED") + ": " + cm.locations.head.toString)
		if (result) {
			Some(cm)
		}
		else None
	}
}
