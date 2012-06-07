package edu.umass.cs.iesl.bibmogrify.model

import edu.umass.cs.iesl.scalacommons.StringUtils._
import edu.umass.cs.iesl.scalacommons.{StringUtils, NonemptyString, OptionUtils, SeqUtils}

object PersonName
	{

	def merge(primary: PersonName, secondary: PersonName): PersonName =
		{
		new PersonName
			{
			override val prefix      = OptionUtils.mergeWarn(primary.prefix, secondary.prefix)
			override val firstName   = OptionUtils.mergeWarn(primary.firstName, secondary.firstName)
			override val middleNames = SeqUtils.mergeWarn(primary.middleNames, secondary.middleNames)
			override val lastName    = OptionUtils.mergeWarn(primary.lastName, secondary.lastName)
			override val suffix      = OptionUtils.mergeWarn(primary.suffix, secondary.suffix)
			override val degree      = OptionUtils.mergeWarn(primary.degree, secondary.degree)
			}
		}
	}

/**
 * subclasses propagate name fragments around the various representations, in an attempt to provide some reasonable value for each field.
 */
trait PersonName
	{
	def prefix: Option[NonemptyString] = None

	def firstName: Option[NonemptyString] = None

	def middleNames: Seq[NonemptyString] = Nil

	def lastName: Option[NonemptyString] = None

	// e.g., Jr. or III
	def suffix: Option[NonemptyString] = None

	def degree: Option[NonemptyString] = None
	}

// allow declaring a record canonical, to ensure that there are no explicit derivations
trait CanonicalPersonName extends PersonName
	{
	def withDerivations = new CanonicalPersonNameWithDerivations(this)
	}

object PersonNameWithDerivations
	{
	def apply(s: String): PersonNameWithDerivations =
		{
		new PersonNameWithDerivations
			{
			override val fullNames = Set(NonemptyString(s))
			}
		}

	/**
	 * Make a new PersonNameWithDerivations.  Generally we should take the local values as primary, and back off to the values provided in the argument.
	 * However
	 * sometimes we may judge that the backup value is superior (e.g., full name vs initial).

	 * @return
	 */
	def merge(primary: PersonNameWithDerivations, secondary: PersonNameWithDerivations): PersonNameWithDerivations =
		{
		val mergedCanonical = PersonName.merge(primary, secondary)

		new PersonNameWithDerivations
			{
			override lazy val prefix      = mergedCanonical.prefix
			override lazy val firstName   = mergedCanonical.firstName
			override lazy val middleNames = mergedCanonical.middleNames
			override lazy val lastName    = mergedCanonical.lastName
			override lazy val suffix      = mergedCanonical.suffix
			override lazy val degree      = mergedCanonical.degree

			// last names
			override lazy val lastInitial = OptionUtils.mergeWarn(primary.lastInitial, secondary.lastInitial)

			//given (first and middle) names
			override lazy val firstInitial = OptionUtils.mergeWarn(primary.firstInitial, secondary.firstInitial)

			// typically first name + middle initial, etc.
			override lazy val givenNames = OptionUtils.mergeWarn(primary.givenNames, secondary.givenNames)

			// not just a char; could be Ja. for Jacques, etc.
			override lazy val middleInitials = OptionUtils.mergeWarn(primary.middleInitials, secondary.middleInitials)

			override lazy val initials = OptionUtils.mergeWarn(primary.initials, secondary.initials)

			// various representations of full name may be present in a single mention (e.g., a metadata xml file)
			override lazy val fullNames = primary.fullNames ++ secondary.fullNames
			}
		}
	}

trait PersonNameWithDerivations extends PersonName
	{
	// a person mention may distinguish name components, or not.
	// here, just store whatever we get (i.e. if we get first and last, leave fullname blank).
	// then we can do whatever normalizations are needed later.
	// not just a char; could be Ja. for Jacques, etc.
	def firstInitial: Option[NonemptyString] = None

	def middleInitials: Option[NonemptyString] = None

	// van Dyke = v.D. ?
	def lastInitial: Option[NonemptyString] = None

	// various representations of full name may be present in a single mention (e.g., a metadata xml file)
	def fullNames: Set[NonemptyString] = Set.empty

	// assume that the longest is the most informative
	def bestFullName: Option[NonemptyString] = fullNames.toSeq.sortBy(-_.s.size).headOption

	def initials: Option[NonemptyString] = None

	// coming from PubMed, these are just for the given namez
	def givenNames: Option[NonemptyString] = None

	// typically first name + middle initial, etc.
	def inferFully: PersonNameWithDerivations =
		{
		val canonical: CanonicalPersonName = new InferredCanonicalPersonName(this)
		val rederived = canonical.withDerivations
		val result = PersonNameWithDerivations.merge(this, rederived)
		result
		}

	override def toString = bestFullName.map(_.s).getOrElse("")
	}

// derive all derivable fields solely from provided canonical fields
class CanonicalPersonNameWithDerivations(n: CanonicalPersonName) extends CanonicalPersonName with PersonNameWithDerivations
	{

	override def degree = n.degree

	override def firstName = n.firstName

	override def lastName = n.lastName

	override def middleNames = n.middleNames

	override def prefix = n.prefix

	// e.g., Jr. or III
	override def suffix = n.suffix

	override val firstInitial: Option[NonemptyString] = firstName.map(x=> NonemptyString(x.s(0) + "."))

	override val middleInitials: Option[NonemptyString] = middleNames.headOption.map(_.s(0) + ".").mkString(" ").trim

	override val lastInitial: Option[NonemptyString] = lastName.map(x=> NonemptyString(x.s(0) + "."))

	// van Dyke = v.D. ?
	// various representations of full name may be present in a single mention (e.g., a metadata xml file)
	override val fullNames: Set[NonemptyString] =
		{
		val middleString: Option[NonemptyString] = middleNames.mkString(" ").trim
		// typically first name + middle test
		// initial, etc.
		val rebuiltFullName: Option[NonemptyString] = Seq[Option[NonemptyString]](prefix, firstName, middleString, lastName, suffix,
		                                                                   degree).flatten.mkString(" ").trim
		rebuiltFullName.toSet
		//if(rebuiltFullName.nonEmpty) Set(rebuiltFullName) else Set.empty
		}

	// coming from PubMed, these are just for the given names
	override val initials: Option[NonemptyString] = List(firstInitial, middleInitials, lastInitial).flatten.mkString(" ").trim

	override val givenNames: Option[NonemptyString] = (n.firstName.getOrElse("") + " " + n.middleNames.mkString(" ")).trim
	}

/**
 * Infer any empty canonical fields, if possible, from provided derived fields
 * @param n
 */
class InferredCanonicalPersonName(n: PersonNameWithDerivations) extends CanonicalPersonName
	{
	private lazy val nParsedFullNames = n.fullNames.map(n => PersonNameParser.parseFullName(n))

	override val prefix = n.prefix.orElse(nParsedFullNames.map(_.prefix).flatten.headOption)

	// e.g., Jr. or III
	override val suffix = n.suffix.orElse(nParsedFullNames.map(_.suffix).flatten.headOption)

	override val degree = n.degree.orElse(nParsedFullNames.map(_.degree).flatten.headOption)

	override val lastName = n.lastName.orElse(nParsedFullNames.map(_.lastName).flatten.headOption).orElse(n.lastInitial)

	override val firstName                = {
	 var gf :Option[NonemptyString]= n.givenNames.map(r => NonemptyString(r.split(" ").head))
	var asd:Option[NonemptyString] = nParsedFullNames.map(_.firstName).flatten.headOption.map(NonemptyString(_))
	n.firstName.orElse(gf).orElse(asd).orElse(n.firstInitial)
	}
	override val middleNames: Seq[NonemptyString] =
		{
		import SeqUtils.emptyCollectionToNone
		val mid: Option[Seq[NonemptyString]] = n.givenNames.map(_.split(" ").tail.filter(_.nonEmpty).map(new NonemptyString(_)))
		val fromInitials: Option[Seq[NonemptyString]] = n.middleInitials.map(_.split(" ").filter(_.nonEmpty).map(new NonemptyString(_)))
		val result: Seq[NonemptyString] = emptyCollectionToNone[Seq[NonemptyString]](n.middleNames).orElse(mid).orElse(fromInitials).orElse(Nil).getOrElse(Nil)
		result
		}
	}

/**
 * This could be a crf...
 * @return
 */
object PersonNameParser
	{

	def parseFullName(s: String): PersonName =
		{
		val toks = uninvertName(s).split(" ").map(_.trim).filter(_.nonEmpty)

		new PersonName
			{
			}
		}

	val invertedNamePattern = """^([^,]*),([^,]*)$""".r

	/**
	 * if there is exactly one comma in the name, reverse the order, e.g. "lastname, firstname" -> "firstname lastname".  In any other case just return the
	 * string as is.  Careful: the prefix, suffix, and degree may be confounded with the inversion, e.g. Dr. Soergel, David, Ph.D.
	 *
	 * @param s
	 */
	def uninvertName(s: String): String =
		{
		assert(!s.isEmpty)
		if (s.count(_ == ',') > 1)
			{
			throw new PersonNameParsingException("Too many commas: " + s)
			}
		val q = try
		{
		val invertedNamePattern(lastname, firstname) = s
		if (lastname != null && firstname != null)
			{
			firstname.trim + " " + lastname.trim
			}
		else s
		}
		catch
		{
		case e: MatchError => s
		}
		val r = q.replace("  ", " ").trim
		assert(!r.isEmpty)
		r
		}
	}

class PersonNameParsingException(s: String) extends Exception(s)

object PersonNameUtils
	{

	/**
	 * Replace periods with spaces.
	 * if there is exactly one comma in the name, reverse the order, e.g. "lastname, firstname" -> "firstname lastname".  In any other case just return the
	 * string as is.
	 */
	/*	def cleanupNameNoPeriods(s: String): String =
	   {
	   assert(!s.isEmpty)
	   val r = cleanupName(s).replace(".", " ").replace("  ", " ").trim
	   assert(!r.isEmpty)
	   r
	   }

   /**
	* could two names conceivably refer to the same person?  Largely for use within a single WOS record, not for coref
	*/
   def compatibleName(oa: Option[PersonName], ob: Option[PersonName]): Boolean =
	   {
	   // the usual case is that the last names match, but the first name may be an initial
	   // but there may be additional stuff with prefixes, suffixes, middle names, etc. etc.
	   // or just initials
	   // we don't want to do coref here!  Just look for contradictions
	   // two names are compatible iff
	   // a) any string in one name longer than 3 chars is matched in the other string either exactly or by first initial
	   // b) any s

	   (oa, ob) match
	   {
		   case (Some(a), Some(b)) =>
			   {
			   compatibleName(a, b)
			   }
		   case default => false
	   }
	   }

   def compatibleName(a: PersonName, b: PersonName): Boolean =
	   {
	   val aToks = cleanupNameNoPeriods(a).toLowerCase.split(" ").reverse.map(_.trim).filterNot(_.isEmpty)
	   val bToks = cleanupNameNoPeriods(b).toLowerCase.split(" ").reverse.map(_.trim).filterNot(_.isEmpty)
	   if (aToks.isEmpty || bToks.isEmpty)
		   {
		   false
		   }
	   else
		   {
		   val try1 = compatibleTokens(aToks, bToks)

		   val try2 = if (try1) true
		   else
			   {
			   // special case: see if separating a two- or three-character name into initials helps
			   val aToks2: Array[String] = aToks
										   .flatMap(tok => (if (tok.length == 2 || tok.length == 3) tok.toCharArray.reverse.map(_.toString) else Some(tok)))
			   val bToks2: Array[String] = bToks.flatMap(tok => if (tok.length == 2 || tok.length == 3) tok.toCharArray.reverse.map(_.toString)
			   else Some(tok))

			   compatibleTokens(aToks2, bToks2)
			   }

		   try2
		   }
	   }

   private def compatibleTokens(aToks: Array[String], bToks: Array[String]): Boolean =
	   {
	   // basically a simple alignment.  don't bother with DP, just a couple heuristics for the common cases
	   // don't support suffixes; the last names must match, (allowing initials)
	   // suffixes after a comma may be OK due to uninvert
	   val headA = aToks.head
	   val headB = bToks.head
	   if (!((headA equals headB) || (headA equals headB(0).toString) || (headB equals headA(0).toString)))
		   {
		   false
		   }
	   else
		   {
		   // choose the minimum set of first & middle names & initials
		   if (aToks.length <= bToks.length)
			   {
			   compatibleFirstMiddle(aToks.tail, bToks.tail)
			   }
		   else
			   {
			   compatibleFirstMiddle(bToks.tail, aToks.tail)
			   }
		   }
	   }

   // remember tokens are reversed
   private def compatibleFirstMiddle(fewerToks: Array[String], moreToks: Array[String]): Boolean =
	   {

	   if (fewerToks.isEmpty || moreToks.isEmpty) true
	   else
		   {
		   val headX = fewerToks.head
		   val headY = moreToks.head
		   if (!((headX equals headY) || (headX equals headY(0).toString) || (headY equals headX(0).toString)))
			   {
			   // mismatch in first token; try to drop middle name/initial
			   // note this means that "A J Smith" and "J Smith" are compatible; oh well
			   if (fewerToks.length < moreToks.length)
				   compatibleFirstMiddle(fewerToks, moreToks.tail)
			   else false
			   }
		   else
			   {
			   //first initial equal; proceed
			   compatibleFirstMiddle(fewerToks.tail, moreToks.tail)
			   }
		   }
	   }
	   */
	}
