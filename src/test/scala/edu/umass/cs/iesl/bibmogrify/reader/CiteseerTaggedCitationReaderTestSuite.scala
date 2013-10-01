package edu.umass.cs.iesl.bibmogrify.reader

import org.scalatest.{BeforeAndAfter, FunSuite}
import edu.umass.cs.iesl.bibmogrify.model.Published
import com.typesafe.scalalogging.slf4j.Logging
import edu.umass.cs.iesl.bibmogrify.tagmodel.StandardLabelXMLReader
import edu.umass.cs.iesl.scalacommons.StringUtils._

class CiteseerTaggedCitationReaderTestSuite extends FunSuite with BeforeAndAfter with Logging {
  val file = getClass.getResource("/examples/citeseer/citeseer-example.xml")

  // todo: detailed tests of all fields

  test("Title is parsed") {
    val citationList = StandardLabelXMLReader(file)
    val tagged = citationList.toIterator.next()
    val c = tagged.toStructuredCitation

    assert(c.title === "Implementation issues in the development of the parsec parser .".opt)
  }
  /* test("Abstract is parsed") {
    assert(c.abstractText.get.size > 50)
  }*/

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
    val citationList = StandardLabelXMLReader(file)
    val tagged = citationList.toIterator.next()
    val c = tagged.toStructuredCitation

    val cont = c.containedIn.get
    assert(cont.container.title === "SOFTWARE - Practice and Experience".opt)
    //assert(cont.volume === Some("146"))
  }

  test("Partial date is parsed") {
    val citationList = StandardLabelXMLReader(file)
    val tagged = citationList.toIterator.next()
    val c = tagged.toStructuredCitation

    val ce = c.dates.head
    assert(ce.eventType === Published)
    assert(ce.date.get.year === Some(1996))
  }

  test("Every record has a title") {
    val citationList = StandardLabelXMLReader(file)
    val tagged = citationList.toIterator.next()
    val c = tagged.toStructuredCitation

    for (t <- citationList) {
      val cc = t.toStructuredCitation
      assert(!cc.title.isEmpty)
      logger.info("Title : " + cc.title)
    }
  }
/*
  test("References are parsed") {
    val citationList = StandardLabelXMLReader(file)
    val tagged = citationList.toIterator.next()
    val c = tagged.toStructuredCitation

    for (t <- citationList) {
      val cc = t.toStructuredCitation
      assert(!cc.references.isEmpty)
      for (r <- cc.references) {
        assert(!r.authors.isEmpty)
        logger.info("Reference first author : " + r.authors.head.person.name)
      }
    }
  }*/
}
