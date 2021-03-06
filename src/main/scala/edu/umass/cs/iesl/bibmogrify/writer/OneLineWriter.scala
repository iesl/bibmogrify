/*
 * Copyright (c) 2013  University of Massachusetts Amherst
 * Licensed under the Apache License, Version 2.0
 * http://www.apache.org/licenses/LICENSE-2.0
 */

package edu.umass.cs.iesl.bibmogrify.writer

import edu.umass.cs.iesl.bibmogrify.model.RichStructuredCitation._
import edu.umass.cs.iesl.bibmogrify.NamedPlugin
import edu.umass.cs.iesl.bibmogrify.pipeline.{StringMetadata, TransformerMetadata, Transformer}
import edu.umass.cs.iesl.bibmogrify.model.Authorities.{DoiAuthority, PubmedAuthority}
import edu.umass.cs.iesl.bibmogrify.model.{Person, Identifier, AuthorInRole, StructuredCitation}
import edu.umass.cs.iesl.scalacommons.StringUtils

object OneLineWriter extends Transformer[StructuredCitation, String] with NamedPlugin {
  val name = "oneline"
  val fromType = "StructuredCitation"
  val toType = "String"

  def apply(cm: StructuredCitation) = {
    Some(cm.locations.headOption.getOrElse("") + "\t" + cm.primaryId + "\t" + cm.year.getOrElse("") + "\t" + cm.authorFullNames.mkString(", ") + "\t" +
      cm.title + "\t" + cm.cleanAbstract +
      "\n")
  }
}

object IdMapWriter extends Transformer[StructuredCitation, String] with NamedPlugin {
  val name = "idmap"
  val fromType = "StructuredCitation"
  val toType = "String"

  def apply(cm: StructuredCitation) = {
    Some(cm.identifiers.mkString("\t") + "\t" + cm.locations.mkString("\t") + "\n")
  }
}

object MalletAbstractWriter extends Transformer[StructuredCitation, String] with NamedPlugin {
  val name = "mallet"
  val fromType = "StructuredCitation"
  val toType = "String"

  def apply(cm: StructuredCitation) = {
    Some(cm.primaryId + "\t" + cm.year.getOrElse("") + "\t" + cm.cleanTitleAndAbstract + "\n")
  }
}

// redundant computation, but more reusable
object abstractNotShort extends Transformer[StructuredCitation, StructuredCitation] with NamedPlugin {
  val name = "notShort"
  val fromType = "StructuredCitation"
  val toType = "StructuredCitation"
  def apply(cm: StructuredCitation) = {
    val ca = cm.cleanTitleAndAbstract
    if(cm.cleanAbstractWords == 0 || ca.split(" ").length < 20)
      None
    else
      Some(cm)
  }
}

// use notShort, mallet
/*
object MalletAbstractNotShortWriter extends Transformer[StructuredCitation, String] with NamedPlugin {
  val name = "malletNotShort"

  def apply(cm: StructuredCitation) = {
    val ca = cm.cleanTitleAndAbstract
    if(cm.cleanAbstractWords == 0 || ca.split(" ").length < 20) 
      None 
    else
      Some(cm.primaryId + "\t" + cm.year.getOrElse("") + "\t" + ca + "\n")
  }
}
*/

object MalletAntonAbstractWriter extends Transformer[StructuredCitation, String] with NamedPlugin {
  val name = "malletAnton"
  val fromType = "StructuredCitation"
  val toType = "String"

  def apply(cm: StructuredCitation) = {
    Some(cm.primaryId + "\t" + cm.year.getOrElse("") + "\t" + cm.cleanTitleAndAbstractAnton + "\n")
  }
}

object MalletMikeAbstractWriter extends Transformer[StructuredCitation, String] with NamedPlugin {
  val name = "malletMike"
  val fromType = "StructuredCitation"
  val toType = "String"

  def apply(cm: StructuredCitation) = {
    Some(cm.primaryId + "\t" + cm.year.getOrElse("") + "\t" + cm.cleanVenue + "\t" + cm.cleanTitle + "\t" + cm.cleanAbstract + "\n")
  }
}

object MalletFullWriter extends Transformer[StructuredCitation, String] with NamedPlugin {
  val name = "malletfull"
  val fromType = "StructuredCitation"
  val toType = "String"

  def apply(cm: StructuredCitation) = {
    Some(cm.primaryId + "\t" + cm.year.getOrElse("") + "\t" + cm.cleanTotal + "\n")
  }
}

object PatentBackgroundWriter extends Transformer[StructuredCitation, String] with NamedPlugin {
  val name = "patback"
  val fromType = "StructuredCitation"
  val toType = "String"

  def apply(cm: StructuredCitation) = {
    Some(cm.primaryId + "\t" + cm.year.getOrElse("") + "\t" + cm.cleanFieldAndBackground + "\n")
  }
}

object PatentAbstractAndBackgroundWriter extends Transformer[StructuredCitation, String] with NamedPlugin {
  val name = "patabsback"
  val fromType = "StructuredCitation"
  val toType = "String"

  def apply(cm: StructuredCitation) = {
    Some(cm.primaryId + "\t" + cm.year.getOrElse("") + "\t" + cm.cleanTitleAndAbstract + " " + cm.cleanFieldAndBackground + "\n")
  }
}

object InstitutionTypeWriter extends Transformer[StructuredCitation, String] with NamedPlugin {
  val name = "institutiontype"
  val fromType = "StructuredCitation"
  val toType = "String"

  def apply(cm: StructuredCitation) = {
    //val ir = cm.institutionRatios
    Some(cm.primaryId + "\t" + cm.doctype.getOrElse("") + "\t" + cm.docSubtype.getOrElse("") + "\t" + cm.institutionTypes.mkString("; ") + "\t" +
      cm.allKeywords.map(_.word).mkString("; ") + "\n")
  }
}

object CorefWriter extends Transformer[StructuredCitation, String] with NamedPlugin {
  val name = "corefoneline"
  val fromType = "StructuredCitation"
  val toType = "String"

  import StringUtils._

  def apply(cm: StructuredCitation) = {
    val venue: String = cm.rootContainedInNotSelf.flatMap(_.title).unwrap
    Some(cm.primaryId + "\t" + cm.year.getOrElse("") + "\t" + cm.authorFullNamesWithId.mkString(", ") + "\t" + cm.title.getOrElse("") + "\t" + venue +
      "\n")
  }
}

object ReferenceStringWriter extends Transformer[StructuredCitation, String] with NamedPlugin {
  val name = "refstrings"
  val fromType = "StructuredCitation"
  val toType = "String"

  def apply(cm: StructuredCitation) = {
    // extra CR for now for easy viewing in terminal
    Some(cm.references.map(_.unstructuredString).mkString("\n\n") + "\n")
  }
}

object PmidRefMapWriter extends Transformer[StructuredCitation, String] with NamedPlugin {
  val name = "pmidrefs"
  val fromType = "StructuredCitation"
  val toType = "String"

  private def pmid(cm: StructuredCitation) = cm.identifiers.filter(_.authority == Some(PubmedAuthority)).headOption

  def apply(cm: StructuredCitation) = {
    val op = pmid(cm)

    val result = op.map(p => (p.value +: cm.references.flatMap(pmid(_)).map(_.value)).mkString("\t") + "\n")
    result
  }
}

object PmidRefPairWriter extends Transformer[StructuredCitation, String] with NamedPlugin {
  val name = "pmidrefpairs"
  val fromType = "StructuredCitation"
  val toType = "String"

  override def metadata: Option[TransformerMetadata] = {
    val fields = Seq("citer", "citee")
    Some(new StringMetadata(fields.mkString("\t") + "\n"))
  }

  private def pmid(cm: StructuredCitation) = cm.identifiers.filter(_.authority == Some(PubmedAuthority)).headOption

  def apply(cm: StructuredCitation) = {
    val op = pmid(cm)

    val result = op.map(p => (cm.references.flatMap(pmid(_)).map(p.value + "\t" + _.value)).mkString("\n"))
    result.map(_ + "\n")
  }
}

object PaperStatsWriter extends Transformer[StructuredCitation, String] with NamedPlugin {
  val name = "paperstats"
  val fromType = "StructuredCitation"
  val toType = "String"

  private def pmid(cm: StructuredCitation) = cm.identifiers.filter(_.authority == Some(PubmedAuthority)).headOption

  private def doi(cm: StructuredCitation) = cm.identifiers.filter(_.authority == Some(DoiAuthority)).headOption

  override def metadata: Option[TransformerMetadata] = {
    val fields = Seq("pmid", "doi", "title", "year", "volume", "numpages", "abstractwords", "totalwords", "category", "license", "numFigures",
      "numTables")
    Some(new StringMetadata(fields.mkString("\t") + "\n"))
  }

  def apply(cm: StructuredCitation) = {

    val fields = Seq(pmid(cm).map(_.qualifiedValue), doi(cm), cm.title, cm.year, cm.volume, cm.numPages, Some(cm.cleanAbstractWords),
      Some(cm.cleanBodyWords), cm.docSubtype, cm.licenseType, cm.numFigures, cm.numTables)
    val fieldsUnpacked = fields.map(_.getOrElse(""))
    Some(fieldsUnpacked.mkString("\t") + "\n")
  }
}

object AuthorStatsWriter extends Transformer[StructuredCitation, String] with NamedPlugin {
  val name = "authorstats"
  val fromType = "StructuredCitation"
  val toType = "String"

  private def pmid(cm: StructuredCitation) = cm.identifiers.filter(_.authority == Some(PubmedAuthority)).headOption

  override def metadata: Option[TransformerMetadata] = {
    val fields = Seq("pmid", "name", "email", "institution")
    Some(new StringMetadata(fields.mkString("\t") + "\n"))
  }

  def apply(cm: StructuredCitation) = {

    val op = pmid(cm)

    def authorToLine(p: Identifier, a: AuthorInRole): String = {
      a.agent match {
        case per: Person => {
          // per.bestFullName
          p.qualifiedValue + "\t" + per + "\t" + per.email.getOrElse("") + "\t" +
            per.affiliations.headOption.map(_.name).getOrElse("")
        }
        // ** just drop institutional authors for now
      }
    }


    def pmidToManyLines(p: Identifier): String = cm.authors.map(authorToLine(p, _)).mkString("\n")

    val result = op.map(pmidToManyLines).filter(_.nonEmpty).map(_ + "\n")
    result
  }
}
