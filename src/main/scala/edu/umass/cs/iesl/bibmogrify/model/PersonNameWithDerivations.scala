package edu.umass.cs.iesl.bibmogrify.model

import edu.umass.cs.iesl.scalacommons.{SeqUtils, StringUtils, OptionUtils}
import StringUtils._
import edu.umass.cs.iesl.scalacommons.NonemptyString

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
	 * However sometimes we may judge that the backup value is superior (e.g., full name vs initial).

	 * @return
	 */
	def merge(primary: PersonNameWithDerivations, secondary: PersonNameWithDerivations): PersonNameWithDerivations =
		{
		val mergedCanonical = PersonName.merge(primary, secondary)

		new PersonNameWithDerivations
			{
			// first set the canonical fields
			override lazy val prefix         = mergedCanonical.prefix
			override lazy val givenNames     = mergedCanonical.givenNames
			override lazy val nickNames      = mergedCanonical.nickNames
			override lazy val surNames       = mergedCanonical.surNames
			override lazy val hereditySuffix = mergedCanonical.hereditySuffix
			override lazy val degrees        = mergedCanonical.degrees

			// then merge the derived fields.  The result may be different from just rederiving them!
			override lazy val firstInitial   = OptionUtils.mergeWarn(primary.firstInitial, secondary.firstInitial)
			override lazy val middleInitials = OptionUtils.mergeWarn(primary.middleInitials, secondary.middleInitials)
			override lazy val lastInitial    = OptionUtils.mergeWarn(primary.lastInitial, secondary.lastInitial)
			override lazy val givenInitials  = OptionUtils.mergeWarn(primary.givenInitials, secondary.givenInitials)
			override lazy val allInitials    = OptionUtils.mergeWarn(primary.allInitials, secondary.allInitials)

			override lazy val firstName   = OptionUtils.mergeWarn(primary.firstName, secondary.firstName)
			override lazy val middleNames = SeqUtils.mergeWarn(primary.middleNames, secondary.middleNames)

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
	// firstInitial is not just a char; could be Ja. for Jacques, etc.
	def firstInitial: Option[NonemptyString] = None

	def middleInitials: Option[NonemptyString] = None

	// van Dyke = v.D. ?
	def lastInitial: Option[NonemptyString] = None

	// initials may be for full name, or just given names (e.g., from PubMed)
	def givenInitials: Option[NonemptyString] = None

	def allInitials: Option[NonemptyString] = None

	// typically first name + middle initial, etc.
	//def givenNames: Option[NonemptyString] = None
	def firstName: Option[NonemptyString] = None

	def middleNames: Seq[NonemptyString] = Nil

	// various representations of full name may be present in a single mention (e.g., a metadata xml file)
	def fullNames: Set[NonemptyString] = Set.empty

	// assume that the longest is the most informative
	final def longestSurName: Option[NonemptyString] = surNames.toSeq.sortBy(-_.s.size).headOption

	// assume that the longest is the most informative
	final def longestFullName: Option[NonemptyString] = fullNames.toSeq.sortBy(-_.s.size).headOption

	final def bestFullName = preferredFullName.orElse(longestFullName)

	def inferFully: PersonNameWithDerivations =
		{
		// first infer canonical fields only from derived fields
		val proposedCanonical: CanonicalPersonName = new InferredCanonicalPersonName(this)
		// override those with explicit canonical fields
		val canonical = PersonName.merge(this, proposedCanonical)
		// rederive all fields
		val rederived = canonical.withDerivations
		// override those with explicit derived fields
		val result = PersonNameWithDerivations.merge(this, rederived)
		result
		}

	override def toString = bestFullName.map(_.s).getOrElse("")

	/*
//.orElse(n.lastInitial)
override val firstName                        =
	{
	var gf: Option[NonemptyString] = n.givenNames.map(r => NonemptyString(r.split(" ").head))
	var asd: Option[NonemptyString] = nParsedFullNames.map(_.firstName).flatten.headOption.map(NonemptyString(_))
	n.firstName.orElse(gf).orElse(asd).orElse(n.firstInitial)
	}
override val middleNames: Seq[NonemptyString] =
	{
	import SeqUtils.emptyCollectionToNone
	val mid: Option[Seq[NonemptyString]] = n.givenNames.map(_.split(" ").tail.filter(_.nonEmpty).map(new NonemptyString(_)))
	val fromInitials: Option[Seq[NonemptyString]] = n.middleInitials.map(_.split(" ").filter(_.nonEmpty).map(new NonemptyString(_)))
	val result: Seq[NonemptyString] =
		emptyCollectionToNone[Seq[NonemptyString]](n.middleNames).orElse(mid).orElse(fromInitials).orElse(Nil).getOrElse(Nil)
	result
	}*/
	}

/**derive all derivable fields solely from provided canonical fields
 *
 * @param n
 */
class CanonicalPersonNameWithDerivations(n: CanonicalPersonName) extends CanonicalPersonName with PersonNameWithDerivations
	{
	// first copy the canonical fields
	override lazy val prefix         = n.prefix
	override lazy val givenNames     = n.givenNames
	override lazy val nickNames      = n.nickNames
	override lazy val surNames       = n.surNames
	override lazy val hereditySuffix = n.hereditySuffix
	override lazy val degrees        = n.degrees


// ** lots not implemented and generally broken

	// then derive the remaining fields
//	override lazy val firstName   = n.givenNames.headOption
//	override lazy val middleNames = n.givenNames.tail

//	override lazy val firstInitial   = firstName.map(x => NonemptyString(x.s(0) + "."))
//	override lazy val middleInitials = middleNames.map(_.s(0) + ".").mkString(" ").trim

	// ** We just take the first uppercase letter from the longest surname,
	// desJardins?  drop all particles?  etc. etc.
	// yuck
	/*
	 override lazy val lastInitial                           = longestSurName.map(x => NonemptyString(x.s(0) + "."))
	 override lazy val givenInitials: Option[NonemptyString] = List(firstInitial, middleInitials).flatten.mkString(" ").trim
	 override lazy val allInitials  : Option[NonemptyString] = List(firstInitial, middleInitials, lastInitial).flatten.mkString(" ").trim
 */
	// van Dyke = v.D. ?
	// various representations of full name may be present in a single mention (e.g., a metadata xml file)
	override lazy val fullNames: Set[NonemptyString] =
		{
		val givenString: Option[NonemptyString] = givenNames.mkString(" ").trim
		val degreesString: Option[NonemptyString] = degrees.mkString(" ").trim
		val rebuiltFullName: Option[NonemptyString] = Seq[Option[NonemptyString]](prefix, givenString, nickNamesInQuotes, longestSurName, hereditySuffix,
		                                                                          degreesString).flatten.mkString(" ").trim
		rebuiltFullName.toSet
		//if(rebuiltFullName.nonEmpty) Set(rebuiltFullName) else Set.empty
		}
	}
