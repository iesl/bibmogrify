package edu.umass.cs.iesl.bibmogrify.reader

import org.scalatest.{BeforeAndAfter, FunSuite}
import com.weiglewilczek.slf4s.Logging
import edu.umass.cs.iesl.bibmogrify.UrlNamedInputStream
import edu.umass.cs.iesl.bibmogrify.model._
import edu.umass.cs.iesl.scalacommons.StringUtils._

class PatentST36ReaderTestSuite extends FunSuite with BeforeAndAfter with Logging
	{
	val file = getClass.getResource("/examples/patentST36/patentST36.xml")

	val citationList = PatentST36Reader(new UrlNamedInputStream(file.toExternalForm.n, file))
	val c            = citationList.toIterator.next()

	// todo: detailed tests of all fieldstest


	test("Title is parsed")
	{
	assert(c.title === "GOLF TEE DEVICE".opt)
	}

	test("ID is parsed")
	{
	assert(c.identifiers.head.authority.get.name === "patent-WO-pub".opt)
	}

	import RichStructuredCitation.enrichStructuredCitation

	test("Abstract is parsed")
	{
	assert(c.englishAbstract.unwrap.size > 50)
	}

	/*
	  test("Summary is parsed")
	  {
		assert(c.textOfType(Summary).mkString.size > 50)
	  }
	  */

	test("Description is parsed")
	{
	assert(c.textOfType(GeneralBodyText).mkString.size > 50)
	}

	test("Claims are parsed")
	{
	assert(c.textOfType(Claims).mkString.size > 50)
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
	/*  test("Journal is parsed")
	  {
	  val cont = c.containedIn.get
	  assert(cont.container.title === "Patent: WO")
	  //assert(cont.volume === Some("146"))
	  }*/

	test("Partial date is parsed")
	{
	val ce = c.dates.head
	assert(ce.eventType === Published)
	assert(ce.date.get.year === Some(2004))
	}

	test("Every record has a title")
	{
	for (c <- citationList)
		{
		assert(!c.title.isEmpty)
		logger.info("Title : " + c.title)
		}
	}
	}
