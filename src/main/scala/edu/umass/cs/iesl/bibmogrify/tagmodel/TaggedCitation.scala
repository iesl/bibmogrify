/*
 * Copyright (c) 2013  University of Massachusetts Amherst
 * Licensed under the Apache License, Version 2.0
 * http://www.apache.org/licenses/LICENSE-2.0
 */

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
 *
 * Note there should be an "untagged" label, so that the entire citation is represented by the union of the (text, tag) pairs.
 *
 * Oops: grouping does matter in the case of authors/author/author-first etc.
 */
case class TaggedCitation(taggedTokens: Seq[(String, String)], labelset: LabelSet)
	{

	lazy val asMap = taggedTokens.groupBy(_._2).map
	                 {
	                 case (k, v) => (k, v.map(_._1))
	                 }

	def get(label: String): Seq[String] = asMap.getOrElse(label, Nil)

	def toStructuredCitation: StructuredCitation =
		{
		labelset.toStructuredCitation(new TaggedCitationWithReferences(this, Nil))
		}
	}

class TaggedCitationWithReferences(val parent: TaggedCitation, val references: Seq[TaggedCitation]) extends TaggedCitation(parent.taggedTokens,
                                                                                                                           parent.labelset)
	{
	override def toStructuredCitation: StructuredCitation = labelset.toStructuredCitation(this)
	}

//trait Label extends String
trait LabelSet
	{

	val topLevelRecordLabels: Seq[String]

	val validLabels           : Seq[String]
	val headerSectionLabels   : Seq[String]
	val referenceSectionLabels: Seq[String]
	val referenceLabels       : Seq[String]
	val mergeableLabels       : Seq[String]

	// each label set is mapped differently into a CitationMention
	def toStructuredCitation(c: TaggedCitationWithReferences): StructuredCitation

	//def read(s: String): TaggedCitation
	}

object toStructured extends Transformer[TaggedCitation, StructuredCitation] with NamedPlugin
	{
	def apply(v1: TaggedCitation) = Some(v1.toStructuredCitation)

	val name = "tagToStruct"

  val fromType = "TaggedCitation"
  val toType = "StructuredCitation"
	}

object toString extends Transformer[TaggedCitation, String] with NamedPlugin
	{
	def apply(v1: TaggedCitation) = Some((for (label <- v1.labelset.validLabels)
	yield
		{
		val vals: Seq[String] = v1.get(label)
		if (vals.isEmpty)
			{
			"NONE"
			}
		else vals.mkString(" | ")
		}).mkString("\t") + "\n")

	/*Some("------------\n" +
		(for {
		  (label, values) <- v1.asMap
		}
		yield (label + " -> {" + values.mkString("}, {") + "}")).mkString("\n") + "\n")*/
	val name = "tagToString"
  val fromType = "TaggedCitation"
  val toType = "String"
	}
