package edu.umass.cs.iesl.bibmogrify.model

import edu.umass.cs.iesl.scalacommons.StringUtils._
import edu.umass.cs.iesl.scalacommons.{NonemptyString, OptionUtils}
import org.apache.commons.lang.NotImplementedException

object PersonName
	{
	/**
	 * Whenever one field is empty and the other is full, use the full one.  If both are full but don't match, emit a warning.
	 * @param primary
	 * @param secondary
	 * @return
	 */
	def merge(primary: PersonName, secondary: PersonName): CanonicalPersonName =
		{
		new CanonicalPersonName
			{
			override val preferredFullName = OptionUtils.mergeWarn(primary.preferredFullName, secondary.preferredFullName)
			override val prefix            = OptionUtils.mergeWarn(primary.prefix, secondary.prefix)
			// ** careful: what if one variant has just the first name, and the other also has middle names?  Or, middle name vs. middle initial, etc.
			override val givenNames        = combineGivenNames(Set(primary.givenNames, secondary.givenNames))
			//SeqUtils.mergeWarn(primary.givenNames, secondary.givenNames)
			override val nickNames         = primary.nickNames ++ secondary.nickNames
			//OptionUtils.mergeWarn(primary.nickName, secondary.nickName)
			override val surNames          = primary.surNames ++ secondary.surNames
			//SetUtils.mergeWarn(primary.surNames,
			// secondary.surNames)
			override val hereditySuffix    = OptionUtils.mergeWarn(primary.hereditySuffix, secondary.hereditySuffix)
			override val degrees           = primary.degrees ++ secondary.degrees //SeqUtils.mergeWarn(primary.degrees, secondary.degrees)
			}
		}

	def merge(s: Seq[PersonName]): PersonName = s.reduceLeft[PersonName](merge)

	def combineGivenNames(set: Set[Seq[NonemptyString]]): Seq[NonemptyString] =
		{
		// ** align all the sequences, allowing initials to align with full names, and output the consensus sequence
		throw new NotImplementedException
		}
	}

/**
 * An attempt to represent names as a set of canonical atomic fields.  Being really comprehensive and accurate about this is not possible due to too many
 * cultural variations and ambiguities.  Still this should cover most of the cases we care about re authorship of journal articles.
 *
 * A name is not a fixed thing; it is a probabilistic cloud of strings, all denoting the same person.  Here we don't cover the case that a person changes
 * names completely; in that case there are two disjoint clouds of strings, so that should be modeled by allowing a Person to have multiple PersonNames.
 *
 * Here we try to model different representations of "the same name".  Variations may include: omitting some components; using initials for some components;
 * reordering; etc.  The most "different" case to model is that of married names vs. maiden names.  Since one or both of these may appear,
 * but the other name components are not affected, we consider this a case of multiple surnames within one name.
 *
 * Subclasses propagate name fragments around the various representations, in an attempt to provide some reasonable value for each field.
 *
 * Here we want to take multiple name variants as input and coordinate them into a single record.  For instance,
 * if we assert that Amanda Jones and A. Jones-Archer are the same person, then we should later recognize Amanda Archer as a valid variant.
 */

trait PersonName
	{
	/**Out of the cloud of possible name representations, the person probably prefers one variant.  This is how we know to use the first-initial form,
	 * or a married name, etc.
	 * @return
	 */
	def preferredFullName: Option[NonemptyString] = None

	def prefix: Option[NonemptyString] = None

	// typically first name + middle initial, etc.
	def givenNames: Seq[NonemptyString] = Nil

	def nickNames: Set[NonemptyString] = Set.empty

	final def nickNamesInQuotes: Option[NonemptyString] = nickNames.map(s => NonemptyString("'" + s.toString + "'")).mkString(" ")

	/**
	 * Each element of this list should be a complete and valid surname (i.e., using only one should produce a valid full name)
	 *
	 * Includes single surnames, multiple sequential surnames (in joined form), hyphenated names (in joined form), maiden names, and married names.
	 *
	 * The point is to facilitate recognizing both "Jorge Martinez" and "Ivan Renteria" as variants of "Jorge Ivan Renteria Martinez".
	 *
	 * Also: Camille Rosenthal-Sabroux Lamsade might have surnames "Rosenthal-Sabroux Lamsade", "Rosenthal-Sabroux", "Rosenthal", "Sabroux",
	 * and "Lamsade".  Here we should record only those variants that are actually observed, but these may later be tokenized for matching purposes.
	 *
	 * Names with particles should simply include the particle, e.g. "de la Mouliere".  The later tokenization for matching should produce at least
	 * "Mouliere" and "la Mouliere", since it's not predictable which particles are "dropping" and which are not.
	 *
	 * Surnames may also include known alternate spellings, e.g. Jouline/Zhulin
	 */
	def surNames: Set[NonemptyString] = Set.empty

	// e.g., Jr. or III
	def hereditySuffix: Option[NonemptyString] = None

	// ** Careful: don't duplicate degrees when merging, but also don't assume they're unique (Kermit the Frog, Ph.D., Ph.D.)
	// ** no problem: assume unique for now, and assume order doesn't matter
	def degrees: Set[NonemptyString] = Set.empty
	}

/**
 * allow declaring a record canonical, to ensure that there are no explicit derivations
  */
trait CanonicalPersonName extends PersonName
	{
	def withDerivations = new CanonicalPersonNameWithDerivations(this)
	//def inferFully: PersonNameWithDerivations = withDerivations.inferFully
	}

/**
 * Infer any empty canonical fields, if possible, from provided derived fields.
 *
 * The approach is to generate full names from the derived fields, and then parse those full names back to canonical fields.  On the one hand,
 * that risks losing information.  On the other hand, this is generally used upstream of a merge where the "correct" derived fields will take priority anyway.
 * Also, this may help clean up mistagged data.
 *
 * In cases of single-value fields, nonempty explicit data overrides implicit data resulting from full-name parsing.  Thus, e.g.,
 * an explicit Mr. overrides an implicit Dr.  Should there be precedence rules?
 *
 * Set-valued fields are just merged.
 *
 * Note relationship with PersonName.merge().  Really we want to a) derive canonical fields only from derived fields; b) merge those with existing canonical
 * fields.
 *
 *
 * @param n
 */
class InferredCanonicalPersonName(n: PersonNameWithDerivations) extends CanonicalPersonName
	{
	private lazy val nParsedFullNames = n.fullNames.map(n => PersonNameParser.parseFullName(n))

// ** if the prefix is populated in more than one input, pick a random one.  Better: emit warning, choose best (?)
	override val prefix            = nParsedFullNames.map(_.prefix).flatten.headOption
	override val givenNames        = PersonName.combineGivenNames(nParsedFullNames.map(_.givenNames))
	override val nickNames         = nParsedFullNames.map(_.nickNames).flatten
	override val surNames          = nParsedFullNames.map(_.surNames).flatten.toSet
	// e.g., Jr. or III

// ** if the hereditySuffix is populated in more than one input, pick a random one.  Better: emit warning, choose best (?)
	override val hereditySuffix    = nParsedFullNames.map(_.hereditySuffix).flatten.headOption
	override val degrees           = nParsedFullNames.toSeq.map(_.degrees).flatten.toSet
	}
