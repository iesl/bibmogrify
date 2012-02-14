package edu.umass.cs.iesl.bibmogrify.tagmodel

import java.net.URL
import edu.umass.cs.iesl.scalacommons.XmlUtils
import xml.Node
import edu.umass.cs.iesl.bibmogrify.BibMogrifyException
import com.weiglewilczek.slf4s.Logging
import collection.immutable.Seq

/**
 * @author <a href="mailto:dev@davidsoergel.com">David Soergel</a>
 * @version $Id$
 */

class TaggedCitationXMLReader(labels: LabelSet) extends Logging {

  def apply(url: URL): TraversableOnce[TaggedCitation] = {
    val s = url.openStream()
    try {
      XmlUtils.firstLevelNodes(s).flatMap(parseDroppingErrors(_))
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
    // just pull the desired tagged ranges from the tree at whatever hierarchy level they may be

    val taggedTokens: Seq[(String, String)] = node.descendant.map(f => (f.text, f.label))
    val validTaggedTokens = taggedTokens.filter(a => {
      val result = labels.validLabels.contains(a._2)
      if (!result) {
        logger.warn("Ignored label: " + a._2)
      }
      result
    })
    new TaggedCitation(validTaggedTokens, labels)
  }

}
