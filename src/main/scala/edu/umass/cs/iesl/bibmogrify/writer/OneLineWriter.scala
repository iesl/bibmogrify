package edu.umass.cs.iesl.bibmogrify.writer

import edu.umass.cs.iesl.bibmogrify.model.StructuredCitation
import edu.umass.cs.iesl.bibmogrify.model.RichCitationMention._
import edu.umass.cs.iesl.bibmogrify.pipeline.Transformer
import edu.umass.cs.iesl.bibmogrify.NamedPlugin

object OneLineWriter extends Transformer[StructuredCitation, String] with NamedPlugin {
  val name = "oneline"

  def apply(cm: StructuredCitation) = {
    Some(cm.locations.headOption.getOrElse("") + "\t" + cm.primaryId + "\t" + cm.year.getOrElse("") + "\t" + cm.authorFullNames.mkString(", ") + "\t" + cm.title + "\t" + cm.cleanAbstract +
      "\n")

  }
}

object MalletAbstractWriter extends Transformer[StructuredCitation, String] with NamedPlugin {
  val name = "mallet"

  def apply(cm: StructuredCitation) = {
    Some(cm.primaryId + "\t" + cm.year.getOrElse("") + "\t" + cm.cleanTitleAndAbstract + "\n")
  }
}


object MalletFullWriter extends Transformer[StructuredCitation, String] with NamedPlugin {
  val name = "malletfull"


  def apply(cm: StructuredCitation) = {
    Some(cm.primaryId + "\t" + cm.year.getOrElse("") + "\t" + cm.cleanTotal + "\n")
  }
}

