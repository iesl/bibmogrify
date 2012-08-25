package edu.umass.cs.iesl.bibmogrify.reader

import org.scalatest.{BeforeAndAfter, FunSuite}
import com.weiglewilczek.slf4s.Logging
import edu.umass.cs.iesl.bibmogrify.UrlNamedInputStream
import edu.umass.cs.iesl.bibmogrify.model.{FirstAuthor, RichStructuredCitation, Published}
import edu.umass.cs.iesl.scalacommons.StringUtils._

class WosXmlReaderTestSuite extends FunSuite with BeforeAndAfter with Logging
	{
	val file = getClass.getResource("/examples/wosxml/wosxml.xml")

	val citationList = WosXMLReader(new UrlNamedInputStream(file.toExternalForm.n, file))
	val c            = citationList.toIterator.next()

	// todo: detailed tests of all fields

	test("Title is parsed")
	{
	assert(c.title === "Universal non-equilibrium phenomena at submicrometric surfaces and interfaces".opt)
	}

	import RichStructuredCitation.enrichStructuredCitation

	test("Abstract is parsed")
	{
	assert(c.englishAbstract.unwrap.size > 50)
	}

	  test("Authors are parsed")
   {
   assert(c.authors.size === 5)
   assert(c.authors.head.roles.size == 1)
   assert(c.authors.head.roles.head == FirstAuthor )
   assert(c.authors.head.agent.toString === "RoTEST R. Q. Cuerno")  // note initials -> given names merging is not done yet
   }

	test("Journal is parsed")
	{
	val cont = c.containedIn.get
	assert(cont.container.title === "EUROPEAN PHYSICAL JOURNAL-SPECIAL TOPICS".opt)
	//assert(cont.volume === Some("146"))
	}

	test("Partial date is parsed")
	{
	val ce = c.dates.head
	assert(ce.eventType === Published)
	assert(ce.date.get.year === Some(2007))
	}

	test("Every record has a title")
	{
	for (c <- citationList)
		{
		assert(!c.title.isEmpty)
		logger.info("Title : " + c.title)
		}
	}

	test("References are parsed")
	{
	for (c <- citationList)
		{
		assert(!c.references.isEmpty)
		/*      for (r <- c.references) {
				assert(!r.authors.isEmpty)
				assert(!r.authors.head.person.name.isEmpty)
				logger.info("Reference first author : " + r.authors.head.person.name)
			  }*/
		}
	}
	}
