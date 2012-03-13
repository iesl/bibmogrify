package edu.umass.cs.iesl.bibmogrify.reader

import org.scalatest.{BeforeAndAfter, FunSuite}
import edu.umass.cs.iesl.bibmogrify.model.Published
import com.weiglewilczek.slf4s.Logging
import edu.umass.cs.iesl.bibmogrify.tagmodel.{ExtendedLabelXMLReaderHlabeled, ExtendedLabelXMLReader, StandardLabelXMLReader}


class GaryhuangTaggedCitationReaderTestSuite extends FunSuite with BeforeAndAfter with Logging {
  val file = getClass.getResource("/examples/garyhuang/garyhuang-example.xml")

  // todo: detailed tests of all fields

  test("Title is parsed") {
    val citationList = ExtendedLabelXMLReader(file)
    val tagged = citationList.toIterator.next()
    val c = tagged.toStructuredCitation
    assert(c.title === Some("Building frameworks through specialisable nested objects."))
  }
  test("Abstract is parsed") {
    val citationList = ExtendedLabelXMLReader(file)
    val tagged = citationList.toIterator.next()
    val c = tagged.toStructuredCitation
    assert(c.abstractText.get.size > 50)
  }

  /*  test("Authors are parsed")
  {
  val citationList = WosXMLReader(Source.fromURL(file))
  val c = citationList.toIterator.next()
  assert(c.authors.size === 1)
  assert(c.authors.head.roles.isEmpty)
  assert(c.authors.head.person.name === Some("E. F. Codd"))
  }
*/
  test("Journal is parsed") {
    val citationList = ExtendedLabelXMLReader(file)
    val tagged = citationList.toIterator.next()
    val c = tagged.toStructuredCitation
    val cont = c.containedIn.get
    assert(cont.container.title === Some("Vrije Universiteit Brussel Faculteit Wetenschappen"))
    //assert(cont.volume === Some("146"))
  }

 /* test("Partial date is parsed") {
    val citationList = ExtendedLabelXMLReader(file)
    val tagged = citationList.toIterator.next()
    val c = tagged.toStructuredCitation
    val ce = c.dates.head
    assert(ce.eventType === Published)
    assert(ce.date.get.year === Some(2007))
  }*/


  test("Every record has a title") {
    val citationList = ExtendedLabelXMLReader(file)
  //  val tagged = citationList.toIterator.next()
  //  val c = tagged.toStructuredCitation
    for (t <- citationList) {
      val cc = t.toStructuredCitation
      assert(!cc.title.isEmpty)
      logger.info("Title : " + cc.title)
    }
  }

  test("References are parsed") {
    val citationList = ExtendedLabelXMLReader(file)
   // val tagged = citationList.toIterator.next()
  //  val c = tagged.toStructuredCitation
    for (t <- citationList) {
      val cc = t.toStructuredCitation
      assert(!cc.references.isEmpty)
      assert(cc.references.size == 29)
      for (r <- cc.references) {
        assert(!r.authors.isEmpty)
        val authorName: Option[String] = r.authors.head.person.name
        logger.info("Reference first author : " + authorName)
      }
    }
  }


  test("References are parsed with h-labeled-only option") {
    val citationList = ExtendedLabelXMLReaderHlabeled(file)
    // val tagged = citationList.toIterator.next()
    //  val c = tagged.toStructuredCitation
    for (t <- citationList) {
      val cc = t.toStructuredCitation
      assert(!cc.references.isEmpty)
      assert(cc.references.size == 1)
      for (r <- cc.references) {
        assert(!r.authors.isEmpty)
        val authorName: Option[String] = r.authors.head.person.name
        logger.info("Reference first author : " + authorName)
      }
    }
  }


  test("Untagged text gets merged") {
    val input = <bogus>some text<title>the title</title>some more<squee>more untagged<authors><author>hello</author> <author>world</author></authors></squee>the end</bogus>
    val expected = Seq(("some text", ""), ("the title","title"), ("some more more untagged", ""), ("hello", "authors/author"), ("world", "authors/author"), ("the end", ""))
    val result = ExtendedLabelXMLReader.parse(input)
    val actual: Seq[(String, String)] = result.taggedTokens
    assert(actual.toList == expected.toList)
  }
}
