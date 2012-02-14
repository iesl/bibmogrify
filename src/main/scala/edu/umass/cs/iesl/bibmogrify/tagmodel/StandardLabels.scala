package edu.umass.cs.iesl.bibmogrify.tagmodel

import org.apache.commons.lang.NotImplementedException

/**
 * @author <a href="mailto:dev@davidsoergel.com">David Soergel</a>
 * @version $Id$
 */

object StandardLabels extends LabelSet {
  val validLabels = Set(
    "author",
    "booktitle",
    "date",
    "author",
    "date",
    "editor",
    "institution",
    "journal",
    "location",
    "note",
    "pages",
    "publisher",
    "tech",
    "title",
    "volume");

  def toCitationMention(c: TaggedCitation) = {
    throw new NotImplementedException()
    /*   new CitationMention {
      override val title = c.get("title")
      override val authors = c.get("authors")
      override val dates = c.get("date")

    }*/
  }
}

object StandardLabelXMLReader extends TaggedCitationXMLReader(StandardLabels)
object ExtendedLabelXMLReader extends TaggedCitationXMLReader(ExtendedLabels)

object ExtendedLabels extends LabelSet {
  val validLabels = StandardLabels.validLabels ++ Set(
    "abstract",
    "address",
    "biblio-hlabeled",
    "biblioEpilogue",
    "biblioPrologue",
    "body",
    "conference",
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

  def toCitationMention(c: TaggedCitation) = {
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
