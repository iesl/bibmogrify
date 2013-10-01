package edu.umass.cs.iesl.bibmogrify.reader

import org.scalatest.{BeforeAndAfter, FunSuite}
import com.typesafe.scalalogging.slf4j.Logging
import edu.umass.cs.iesl.bibmogrify.UrlNamedInputStream
import edu.umass.cs.iesl.bibmogrify.model.{FirstAuthor, RichStructuredCitation, Published}
import edu.umass.cs.iesl.scalacommons.StringUtils._
import edu.umass.cs.iesl.bibmogrify.writer.BibJSONWriter

class NatureReaderTestSuite extends FunSuite with BeforeAndAfter with Logging
	{
	val file = getClass.getResource("/examples/nature/test.xml")

	val citationList = NatureReader(new UrlNamedInputStream(file.toExternalForm.n, file))
	val c            = citationList.toIterator.next()

	// todo: detailed tests of all fields
	/*

		<article mdate="2002-01-03" key="persons/Codd71a">
		<author>E. F. Codd</author>
		<title>Further Normalization of the Data Base Relational Model.</title>
		<journal>IBM Research Report, San Jose, California</journal>
		<volume>RJ909</volume>
		<month>August</month>
		<year>1971</year>
		<cdrom>ibmTR/rj909.pdf</cdrom>f
		<ee>db/labs/ibm/RJ909.html</ee>
		 */
	test("Title is parsed")
	{
	assert(c.title === "Biology Algal synchronized swimming".opt)
	}

	test("Authors are parsed")
	{
	assert(c.authors.size === 3)
	assert(c.authors.head.roles.head == FirstAuthor)
	//assert(c.authors.head.agent.name.flatMap(_.inferFully.bestFullName)  === emptyStringToNone("Heping Zhang"))
	assert(c.authors.head.agent.toString === "Kermit Frog")
	}

	test("Journal is parsed")
	{
	val cont = c.containedIn.get
	assert(cont.container.title === "Nature".opt)
	assert(cont.volume === "474".opt)
	}

	test("Partial date is parsed")
	{
	val ce = c.dates.head
	assert(ce.eventType === Published)
	assert(ce.date.get.year === Some(2011))
	assert(ce.date.get.month === Some(6))
	assert(ce.date.get.day === Some(30))
	}

	import RichStructuredCitation.enrichStructuredCitation

	test("Every record has a title")
	{
	for (c <- citationList)
		{
		assert(!c.title.isEmpty)
		logger.info("Title : " + c.title)
		}
	}

  test("JSON representation ends with brace")
  {
  val result =  BibJSONWriter(c)
   result.map(logger.info(_))
    result.map(x=>assert(x.endsWith("}")))
  }
	}
