package edu.umass.cs.iesl.bibmogrify.writer

import edu.umass.cs.iesl.bibmogrify.NamedPlugin
import edu.umass.cs.iesl.bibmogrify.model.RichCitationMention._
import edu.umass.cs.iesl.bibmogrify.pipeline.{StringMetadata, TransformerMetadata, Transformer}
import edu.umass.cs.iesl.bibmogrify.model.{English, StructuredPatent}
import collection.Iterable

/**
 * @author <a href="mailto:dev@davidsoergel.com">David Soergel</a>
 * @version $Id$
 */

object PatentStatsWriter extends Transformer[StructuredPatent, String] with NamedPlugin {
  val name = "patentstats"

  override def metadata: Option[TransformerMetadata] = {
    val fields = Seq(
      "location",
      "id",
      "year",
      "orig lang",
      "pub lang",
      "abstract langs",
      "num priority claims",
      "num main family",
      "num complete family",
      "num references",
      "num search refs",
      "keywords per authority"
    )
    Some(new StringMetadata(fields.mkString("\t") + "\n"))
  }

  def apply(cm: StructuredPatent) = {

    val fields = Seq(
      cm.locations.headOption.map(_.toString),
      Some(cm.primaryId),
      cm.year,
      cm.sourceLanguage,
      cm.language,
      Some(cm.listAbstractLanguages),
      Some(cm.priorityClaims.length),
      Some(cm.mainFamily.length),
      Some(cm.completeFamily.length),
      Some(cm.structuredReferences.length),
      Some(cm.searchReportReferences.length),
      Some(cm.keywordsCountByAuthority)
    )
    val fieldsUnpacked = fields.map(_.getOrElse(""))
    Some(fieldsUnpacked.mkString("\t") + "\n")
  }
}


object PatentMultiLanguageAbstractsWriter extends Transformer[StructuredPatent, String] with NamedPlugin {
  val name = "patentstats"

  override def metadata: Option[TransformerMetadata] = {
    val fields = Seq(
      "location",
      "id",
      "year",
      "keywords per authority",
      "english abstract",
      "other lang",
      "other abstract"
    )
    Some(new StringMetadata(fields.mkString("\t") + "\n"))
  }

  def apply(cm: StructuredPatent) = {

    val extraLanguageAbstracts: Iterable[Option[String]] = cm.abstractText.map(twl => {
        twl.language match {
          case Some(English) => Nil
          case None => Nil
          case Some(l) => Seq(Some(l.toString), Some(twl.text))
        }
      }).flatten


    val fields: Iterable[Option[String]] = Seq(
      cm.locations.headOption.map(_.toString),
      Some(cm.primaryId),
      cm.year.map(_.toString),
      Some(cm.keywordsCountByAuthority),
      Some(cm.englishAbstract)
    ) ++ extraLanguageAbstracts

    val fieldsUnpacked = fields.map(_.getOrElse(""))
    Some(fieldsUnpacked.mkString("\t") + "\n")
  }
}
