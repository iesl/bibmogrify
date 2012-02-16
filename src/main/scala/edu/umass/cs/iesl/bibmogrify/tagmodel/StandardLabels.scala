package edu.umass.cs.iesl.bibmogrify.tagmodel

import org.apache.commons.lang.NotImplementedException
import edu.umass.cs.iesl.bibmogrify.pipeline.Transformer
import java.net.URL
import edu.umass.cs.iesl.bibmogrify.model.StructuredCitation
import edu.umass.cs.iesl.bibmogrify.NamedPlugin

/**
 * @author <a href="mailto:dev@davidsoergel.com">David Soergel</a>
 * @version $Id$
 */

object StandardLabels extends LabelSet {
  val validLabels = Seq(
    "author",
    "date",
    "title",
    "volume",
    "pages",
    "booktitle",
    "editor",
    "institution",
    "conference",
    "journal",
    "location",
    "note",
    "publisher",
    "tech");

  def toStructuredCitation(c: TaggedCitation) = {
    throw new NotImplementedException()
    /*   new CitationMention {
      override val title = c.get("title")
      override val authors = c.get("authors")
      override val dates = c.get("date")

    }*/
  }
}

// with Transformer[URL, TaggedCitation] needs to be explicit for the sake of the plugin discovery
object StandardLabelXMLReader extends TaggedCitationXMLReader(StandardLabels) with Transformer[URL, TaggedCitation] with NamedPlugin {
  val name = "standardLabels"
}
object ExtendedLabelXMLReader extends TaggedCitationXMLReader(ExtendedLabels) with Transformer[URL, TaggedCitation] with NamedPlugin {
  val name = "extendedLabels"
}

object ExtendedLabels extends LabelSet {
  val validLabels = StandardLabels.validLabels ++ Seq(
    "abstract",
    "address",
    "biblio-hlabeled",
    "biblioEpilogue",
    "biblioPrologue",
    "body",
    "email",
    "headers-hlabeled",
    "keyword",
    "number",
    "phone",
    "ref-marker",
    "reference-hlabeled",
    "reference",
    "series",
    "thesis",
    "web"
  )

  def toStructuredCitation(c: TaggedCitation) = {
    throw new NotImplementedException()
    /*   new CitationMention {
      override val title = c.get("title")
      override val authors = c.get("authors")
      override val dates = c.get("date")

    }*/
  }
}

/* <abstract>
<address>
<author-affix>
<author-first>
<author-last>
<author-middle>
<author>
<authors>
<biblio-hlabeled>
<biblioEpilogue>
<biblioPrologue>
<body>
<booktitle>
<conference>
<date>
<editor>
<email>
<headers-hlabeled>
<institution>
<journal>
<keyword>
<note>
<number>
<phone>
<publisher>
<ref-marker>
<reference-hlabeled>
<reference>
<series>
<tech>
<thesis>
<title>
<volume>
<web>*/
