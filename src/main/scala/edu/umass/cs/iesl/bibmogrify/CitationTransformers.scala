/*
 * Copyright (c) 2013  University of Massachusetts Amherst
 * Licensed under the Apache License, Version 2.0
 * http://www.apache.org/licenses/LICENSE-2.0
 */

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

  val fromType = "StructuredCitation"
  val toType = "StructuredCitation"
  
  def apply(cm: StructuredCitation) = cm +: cm.references
}

object StructuredOnlyReferences extends Transformer[StructuredCitation, StructuredCitation] with NamedPlugin {

  val name = "onlyReferences"
  val fromType = "StructuredCitation"
  val toType = "StructuredCitation"
  
  def apply(cm: StructuredCitation) = cm.references
}


object TaggedWithReferences extends Transformer[TaggedCitationWithReferences, TaggedCitation] with NamedPlugin {

  val name = "taggedWithReferences"
  val fromType = "TaggedCitationWithReferences"
  val toType = "TaggedCitation"
  
  def apply(cm: TaggedCitationWithReferences) = cm.parent +: cm.references
}

object TaggedOnlyReferences extends Transformer[TaggedCitationWithReferences, TaggedCitation] with NamedPlugin {

  val name = "taggedOnlyReferences"
  val fromType = "TaggedCitationWithReferences"
  val toType = "TaggedCitation"

  def apply(cm: TaggedCitationWithReferences) = cm.references
}
