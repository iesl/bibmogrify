package edu.umass.cs.iesl.bibmogrify

import model.StructuredCitation
import pipeline.Transformer
import java.net.URL
import java.io.File

/**
 * @author <a href="mailto:dev@davidsoergel.com">David Soergel</a>
 * @version $Id$
 */

object WithReferences extends Transformer[StructuredCitation, StructuredCitation] with NamedPlugin {

  val name = "withReferences"

  def apply(cm: StructuredCitation) = cm +: cm.references
}

object OnlyReferences extends Transformer[StructuredCitation, StructuredCitation] with NamedPlugin {

  val name = "onlyReferences"

  def apply(cm: StructuredCitation) = cm.references
}
