package edu.umass.cs.iesl.bibmogrify.reader

import org.scalatest.{BeforeAndAfter, FunSuite}
import com.weiglewilczek.slf4s.Logging
import edu.umass.cs.iesl.bibmogrify.UrlNamedInputStream
import edu.umass.cs.iesl.bibmogrify.model.{RichStructuredCitation, Published}
import edu.umass.cs.iesl.scalacommons.StringUtils._

class ElsevierReaderTestSuite extends FunSuite with BeforeAndAfter with Logging
	{
	val file = getClass.getResource("/examples/elsevier/example.xml")

	val citationList = ElsevierReader(new UrlNamedInputStream(file.toExternalForm, file))
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
	assert(c.title === "Effects of excessive cookie consumption in garbage-can-dwelling shagbeasts".opt)
	}

	test("Authors are parsed")
	{
	assert(c.authors.size === 2)
	assert(c.authors.head.roles.isEmpty)
	//assert(c.authors.head.agent.name.flatMap(_.inferFully.bestFullName) === emptyStringToNone("Kermit T. Frog"))
	assert(c.authors.head.agent.toString === "Kermit T. Frog")
	}

	test("Journal is parsed")
	{
	val cont = c.containedIn.get
	assert(cont.container.title === "Acta Sesamae".opt)
	assert(cont.volume === "50".opt)
	}

	test("Partial date is parsed")
	{
	val ce = c.dates.head
	assert(ce.eventType === Published)
	assert(ce.date.get.year === Some(2002))
	assert(ce.date.get.month === None)
	assert(ce.date.get.day === None)
	}

	import RichStructuredCitation.enrichStructuredCitation

	test("Abstract is parsed")
	{
	val ce = c.englishAbstract
	assert(ce.split(" ").head === "Domestication")
	assert(ce.split(" ").last === "cans.")
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
