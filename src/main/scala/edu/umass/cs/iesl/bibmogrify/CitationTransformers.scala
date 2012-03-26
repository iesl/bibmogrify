package edu.umass.cs.iesl.bibmogrify

import model.StructuredCitation
import pipeline.Transformer
import java.net.URL
import java.io.File
import tagmodel.{TaggedCitation, TaggedCitationWithReferences}

/**
 * @author <a href="mailto:dev@davidsoergel.com">David Soergel</a>
 * @version $Id$
 */

object StructuredWithReferences extends Transformer[StructuredCitation, StructuredCitation] with NamedPlugin {

  val name = "withReferences"

  def apply(cm: StructuredCitation) = cm +: cm.structuredReferences
}

object StructuredOnlyReferences extends Transformer[StructuredCitation, StructuredCitation] with NamedPlugin {

  val name = "onlyReferences"

  def apply(cm: StructuredCitation) = cm.structuredReferences
}


object TaggedWithReferences extends Transformer[TaggedCitationWithReferences, TaggedCitation] with NamedPlugin {

  val name = "taggedWithReferences"

  def apply(cm: TaggedCitationWithReferences) = cm.parent +: cm.references
}

object TaggedOnlyReferences extends Transformer[TaggedCitationWithReferences, TaggedCitation] with NamedPlugin {

  val name = "taggedOnlyReferences"

  def apply(cm: TaggedCitationWithReferences) = cm.references
}
