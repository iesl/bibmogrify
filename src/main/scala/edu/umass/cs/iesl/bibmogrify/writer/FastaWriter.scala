/*
 * Copyright (c) 2013  University of Massachusetts Amherst
 * Licensed under the Apache License, Version 2.0
 * http://www.apache.org/licenses/LICENSE-2.0
 */

package edu.umass.cs.iesl.bibmogrify.writer

import edu.umass.cs.iesl.bibmogrify.pipeline.Transformer
import edu.umass.cs.iesl.bibmogrify.model.StructuredCitation
import edu.umass.cs.iesl.bibmogrify.NamedPlugin
import edu.umass.cs.iesl.scalacommons.NonemptyString
import edu.umass.cs.iesl.bibmogrify.model.RichStructuredCitation._

/**
 * @author <a href="mailto:dev@davidsoergel.com">David Soergel</a>
 * @version $Id$
 */
object FastaWriter extends Transformer[(NonemptyString, StructuredCitation), String] with NamedPlugin {
  val name = "fasta"

  val fromType = "(NonemptyString, StructuredCitation)"
  val toType = "String"

  def apply(x: (NonemptyString, StructuredCitation)) = {
    x match {
      case (hash, sc) => {
        val allIds = (sc.locations.map(_.toString) ++ sc.qualifiedIdsInOrder).toSeq.distinct.mkString(";")
        Some(">" + allIds + "\n" + hash + "\n")
      }
    }
  }
}
