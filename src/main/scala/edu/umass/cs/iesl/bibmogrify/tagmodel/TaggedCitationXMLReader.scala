package edu.umass.cs.iesl.bibmogrify.tagmodel

import java.net.URL
import edu.umass.cs.iesl.scalacommons.XmlUtils
import edu.umass.cs.iesl.bibmogrify.BibMogrifyException
import com.weiglewilczek.slf4s.Logging
import edu.umass.cs.iesl.bibmogrify.pipeline.Transformer
import collection.TraversableOnce
import xml.{Elem, Text, Node}

/**
 * @author <a href="mailto:dev@davidsoergel.com">David Soergel</a>
 * @version $Id$
 */

class TaggedCitationXMLReader(labels: LabelSet) extends Transformer[URL, TaggedCitationWithReferences ] with Logging {

  def apply(url: URL): TraversableOnce[TaggedCitationWithReferences ] = {
    val s = url.openStream()
    try {
      val allNodes: TraversableOnce[Node] = XmlUtils.nodesMatching(s, labels.topLevelRecordLabels).toList
      allNodes.flatMap(parseDroppingErrors(_))
    }
    finally {
      s.close()
    }
  }

  def parseDroppingErrors(doc: Node): Option[TaggedCitationWithReferences ] = {
    try Some(parseWithReferences(doc))
    catch {
      case e: BibMogrifyException => logger.error(e.getMessage)
      None
    }
  }


  def parseWithReferences(node: Node): TaggedCitationWithReferences = {
    // because of the untagged-text problem, we have to just walk the tree.
    // This is a little tricky because we want to slash-separate nested but valid tags,
    // while merging all text in invalid tags (regardless of level) into "untagged" nodes,
    // all while maintaining order.

    // labelPath is whatever we want to see in front of the label at this level, not including the delimiter.
    // the reason not to include the delimiter is that a PCDATA section should be recorded at the parent path with no trailing delimiter
    // this basically requires that "untagged" == "".

    // a further complication is that all text in a valid node that has no valid descendants should be merged.  That is, we want to ignore invalid nodes entirely.
    // at the same time, two neighboring valid nodes should not be merged; that distinction is legit.

    // so: perhaps we should:
    // 0) pull out reference sections
    // 1) remove all invalid nodes, flattening their contents upwards
    // 2) merge neighboring PCDATA
    // 3) traverse tree, collecting TaggedCitation elements.

    val references = extractReferences(node)
    new TaggedCitationWithReferences(parse(node),references)
  }
    def parse(node: Node): TaggedCitation = {


    // hmmm... good idiom for flatMap on a tree?
    val validNode =
      new Elem(node.prefix, node.label, node.attributes, node.scope, flattenValid(node): _*)

    val mergedNode = mergeText(validNode)

    val resultElements = mergedNode.child.map(traverse(_, Nil)).flatten  // avoid including the top-level label

    new TaggedCitation(resultElements,labels)

  }

  def extractReferences(node: Node): Seq[TaggedCitation] = {
    val q = for (r <- labels.referenceSectionLabels;
                 x <- (node \\ r)
    ) yield
    {
      val refs = x.child.toList
      refs.map(parse)
    }

    q.flatten
  }

  def flattenValid(node: Node): Seq[Node] = {
    if(labels.referenceSectionLabels contains node.label) Nil
    else if (node.isInstanceOf[Text]) {
      if(node.text.trim().isEmpty) Nil else Seq(node)
    }
    else if (labels.validLabels contains node.label) {
      new Elem(node.prefix, node.label, node.attributes, node.scope, node.child.map(flattenValid).flatten: _*)
    }
    else {
      node.child.map(flattenValid).flatten
    }
  }

  def mergeText(node: Node): Node = {
    if (node.isInstanceOf[Text]) node
    else {
      val mergedChildren = node.child.map(mergeText).flatten
      val mergedReverse = mergedChildren.foldLeft(Seq[Node]())((a: Seq[Node], b: Node) => {
        if (a == Nil) Seq(b)
        else
          (a.head, b) match {
            case (ah: Text, bh: Text) => new Text(ah.text + " " + bh.text) ++ a.tail
            case (ah, bh) => b ++ a
          }
      })
      new Elem(node.prefix, node.label, node.attributes, node.scope, mergedReverse.reverse: _*)
    }
  }


  def traverse(node: Node, revpath: Seq[String]): Seq[(String, String)] = {
    if(node.isInstanceOf[Text]) Seq((node.text, revpath.reverse.mkString("/")))
    else node.child.flatMap(traverse(_, node.label +: revpath))
  }

}
