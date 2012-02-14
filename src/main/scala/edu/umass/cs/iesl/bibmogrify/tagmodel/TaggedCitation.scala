package edu.umass.cs.iesl.bibmogrify.tagmodel

import edu.umass.cs.iesl.bibmogrify.model.StructuredCitation
import java.io.BufferedWriter
import java.net.URL
import edu.umass.cs.iesl.scalacommons.XmlUtils

/**
 * @author <a href="mailto:dev@davidsoergel.com">David Soergel</a>
 * @version $Id$
 */

/**
 * this is not just a map because a tag may recur (e.g., author) and order matters.
 * What about nested tags, e.g. authors/author?  Really we care only about the leaves,
 * so the parser should ignore the authors container.
 */
case class TaggedCitation(taggedTokens : Seq[(String, String)], labelset: LabelSet)
{
  lazy val asMap = taggedTokens.groupBy(_._1).map { case (k,v) => (k,v.map(_._2)) }
  def get(label : String) : Seq[String] = asMap.getOrElse(label,Nil)
}

//trait Label extends String

trait LabelSet {
  val validLabels : Set[String]
  // each label set is mapped differently into a CitationMention
  def toCitationMention(c: TaggedCitation): StructuredCitation

  //def read(s: String): TaggedCitation
}
