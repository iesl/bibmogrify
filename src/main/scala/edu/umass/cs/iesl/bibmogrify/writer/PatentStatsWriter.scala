package edu.umass.cs.iesl.bibmogrify.writer

import edu.umass.cs.iesl.bibmogrify.NamedPlugin
import edu.umass.cs.iesl.bibmogrify.model.RichCitationMention._
import edu.umass.cs.iesl.bibmogrify.pipeline.{StringMetadata, TransformerMetadata, Transformer}
import collection.Iterable
import edu.umass.cs.iesl.bibmogrify.model._

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
  val name = "patentabstractlangs"

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
      Some(cm.cleanAbstract)
    ) ++ extraLanguageAbstracts

    val fieldsUnpacked = fields.map(_.getOrElse(""))
    Some(fieldsUnpacked.mkString("\t") + "\n")
  }
}


object PatentTokensPerSectionWriter extends Transformer[StructuredPatent, String] with NamedPlugin {
  val name = "patenttokens"

  override def metadata: Option[TransformerMetadata] = {
    val fields = Seq(
      "location",
      "id",
      "year",
      "english abstract",
      "summary",
      "claims",
      "fulltext",
      "total tokens",
      "abstract tokens",
      "summary tokens",
      "claims tokens",
      "body toks",
      "abstract vs summary",
      "abstract vs claims",
      "abstract vs body",
      "summary vs claims",
      "summary vs body",
      "claims vs body"
    )
    Some(new StringMetadata(fields.mkString("\t") + "\n"))
  }

  def apply(cm: StructuredPatent) = {

    val theAbstract: String = cm.cleanTitleAndAbstract
    val theSummary: String = cm.cleanSummary
    val theClaims: String = cm.cleanClaims
    val theBody: String = cm.cleanGeneralBody

    val theAbstractTokens: Set[String] = theAbstract.split(" ").toSet - ""
    val theSummaryTokens: Set[String] = theSummary.split(" ").toSet - ""
    val theClaimsTokens: Set[String] = theClaims.split(" ").toSet - ""
    val theBodyTokens: Set[String] = theBody.split(" ").toSet - ""

    val totalTokens: Set[String] = theAbstractTokens ++ theSummaryTokens ++ theClaimsTokens ++ theBodyTokens

    val fields = Seq(
      cm.locations.headOption.map(_.toString),
      Some(cm.primaryId),
      cm.year,
      Some(theAbstract),
      Some(theSummary),
      Some(theClaims),
      Some(theBody),
      Some(totalTokens.size),
      Some(theAbstractTokens.size),
      Some(theSummaryTokens.size),
      Some(theClaimsTokens.size),
      Some(theBodyTokens.size),
      Some(theAbstractTokens.intersect(theSummaryTokens).size),
      Some(theAbstractTokens.intersect(theClaimsTokens).size),
      Some(theAbstractTokens.intersect(theBodyTokens).size),
      Some(theSummaryTokens.intersect(theClaimsTokens).size),
      Some(theSummaryTokens.intersect(theBodyTokens).size),
      Some(theClaimsTokens.intersect(theBodyTokens).size)
    )
    val fieldsUnpacked = fields.map(_.getOrElse(""))
    Some(fieldsUnpacked.mkString("\t") + "\n")
  }
}
