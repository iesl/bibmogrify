package edu.umass.cs.iesl.bibmogrify.filter

import edu.umass.cs.iesl.bibmogrify.model.StructuredCitation
import com.weiglewilczek.slf4s.Logging
import edu.umass.cs.iesl.bibmogrify.pipeline.Transformer
import edu.umass.cs.iesl.bibmogrify.NamedPlugin

object WosPhysicalFilter extends Transformer[StructuredCitation, StructuredCitation] with NamedPlugin with Logging {
  val name = "wosphysical"

  val okKeywords = List(
    "AA", "BU", "AC", "DA", "EA", "DW", "EC", "DX", "DY", "EE", "EI", "EP", "ER", "ES", "ET", "EV", "EW", "EX", "FA", "FI", "GU", "HQ", "ID", "AI", "IG", "II", "IM", "IQ", "IH", "IX", "IJ",
    "IK", "IL", "IU", "IF", "IO", "IP", "JA", "JB", "JY", "KA", "GC", "KU", "KV", "KY", "LE", "UE", "NU", "OA", "OY", "OU", "OT", "QE", "PK", "QF", "QG", "QH", "PM", "PJ", "QJ", "PQ", "PN",
    "PO", "PU", "PZ", "QM", "QQ", "RA", "RE", "ZQ", "RO", "NS", "RY", "SI", "PE", "SY", "UB", "UH", "UK", "UF", "UR", "UI", "UN", "UP", "UY", "SR", "RB", "XQ", "YE", "DT", "YQ", "YR", "ZR"
  )

  def apply(cm: StructuredCitation) = {
    val result = !cm.keywords.map(_.word).toSeq.intersect(okKeywords).isEmpty
    logger.debug("Filter " + (if (result) "PASSED" else "FAILED") + ": " + cm.locations.head.toString)
    if (result) {
      Some(cm)
    } else None
  }
}
