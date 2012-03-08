package edu.umass.cs.iesl.bibmogrify.tagmodel

import java.net.URL
import edu.umass.cs.iesl.scalacommons.XmlUtils
import edu.umass.cs.iesl.bibmogrify.BibMogrifyException
import com.weiglewilczek.slf4s.Logging
import collection.immutable.Seq
import edu.umass.cs.iesl.bibmogrify.pipeline.Transformer
import collection.TraversableOnce
import xml.{Text, Node}

/**
 * @author <a href="mailto:dev@davidsoergel.com">David Soergel</a>
 * @version $Id$
 */

class TaggedCitationXMLReader(labels: LabelSet) extends Transformer[URL, TaggedCitation] with Logging {

  def apply(url: URL): TraversableOnce[TaggedCitation] = {
    val s = url.openStream()
    try {
      val allNodes: TraversableOnce[Node] = XmlUtils.nodesMatching(s, labels.topLevelRecordLabels).toList
      allNodes.flatMap(parseDroppingErrors(_))
    }
    finally {
      s.close()
    }
  }

  def parseDroppingErrors(doc: Node): Option[TaggedCitation] = {
    try {
      val c = parse(doc)
      Some(c)
    }
    catch {
      case e: BibMogrifyException => logger.error(e.getMessage)
      None
    }
  }

  def parse(node: Node): TaggedCitation = {
    // can't just pull the desired tagged ranges from the tree at whatever hierarchy level they may be, because there may be a contained reference section

    val allNonReferenceSectionDescendants = XmlUtils.descendantExcluding(node, labels.referenceSectionLabels)

    // this is tricky: any text that is not in an accepted tag should be "untagged".  e.g. <bogus> this is untagged text <title>this is tagged</title> this is untagged again</bogus>

    val taggedTokens: scala.Seq[(String, String)] = allNonReferenceSectionDescendants.map(f => {
      val parts: Seq[String] = f.descendant.filter(_.isInstanceOf[Text]).map(_.text)
      val comb: String = parts.mkString(" ")
      val s: String = comb.replaceAll("\\s|\"", " ").trim
      (s, f.label)
    })

    /*

    val headerSections = labels.headerSectionLabels.flatMap(node \\ _)
    val allowedRootNodes = node +: headerSections

    node.descendant.filter(n => n.)

    val taggedTokens: scala.Seq[(String, String)] = allowedRootNodes.flatMap(r => r.nonEmptyChildren.toList.map(f => {
      val parts: Seq[String] = f.descendant.filter(_.isInstanceOf[Text]).map(_.text)
      val comb: String = parts.mkString(" ")
      val s: String = comb.replaceAll("\\s|\"", " ").trim
      (s, f.label)
    }))
*/
    val validTaggedTokens = taggedTokens.filter(a => {
      val result = labels.validLabels.contains(a._2)
      /*    if (!result) {
        logger.warn("Ignored label: " + a._2)
      }*/
      result
    })

    val referenceSections = labels.referenceSectionLabels.flatMap(node.descendant \\ _)
    val references = referenceSections.map(parse(_))
    new TaggedCitation(validTaggedTokens, references, labels)
  }

}
