package edu.umass.cs.iesl.bibmogrify.tagmodel

import edu.umass.cs.iesl.bibmogrify.model.StructuredCitation
import edu.umass.cs.iesl.bibmogrify.pipeline.Transformer
import edu.umass.cs.iesl.bibmogrify.NamedPlugin

/**
 * @author <a href="mailto:dev@davidsoergel.com">David Soergel</a>
 * @version $Id$
 */

/**
 * this is not just a map because a tag may recur (e.g., author) and order matters.
 * What about nested tags, e.g. authors/author?  Really we care only about the leaves,
 * so the parser should ignore the authors container.
 *
 * taggedTokens is (text, label)
 */
case class TaggedCitation(taggedTokens: Seq[(String, String)], labelset: LabelSet) {
  lazy val asMap = taggedTokens.groupBy(_._2).map {
    case (k, v) => (k, v.map(_._1))
  }

  def get(label: String): Seq[String] = asMap.getOrElse(label, Nil)

  def toStructuredCitation: StructuredCitation = {
    labelset.toStructuredCitation(this)
  }
}

//trait Label extends String

trait LabelSet {
  val validLabels: Seq[String]
  // each label set is mapped differently into a CitationMention
  def toStructuredCitation(c: TaggedCitation): StructuredCitation

  //def read(s: String): TaggedCitation
}

object toStructured extends Transformer[TaggedCitation, StructuredCitation] with NamedPlugin {
  def apply(v1: TaggedCitation) = Some(v1.toStructuredCitation)

  val name = "tagToStruct"
}


object toString extends Transformer[TaggedCitation, String] with NamedPlugin {
  def apply(v1: TaggedCitation) = Some(

    (
      for (label <- v1.labelset.validLabels)
      yield {
        val vals: Seq[String] = v1.get(label)
        if (vals.isEmpty) { "NONE" } else vals.mkString(" | ")
      }
      ).mkString("\t") + "\n"
  )

  /*Some("------------\n" +
    (for {
      (label, values) <- v1.asMap
    }
    yield (label + " -> {" + values.mkString("}, {") + "}")).mkString("\n") + "\n")*/

  val name = "tagToString"
}
