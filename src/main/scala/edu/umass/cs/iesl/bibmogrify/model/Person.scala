package edu.umass.cs.iesl.bibmogrify.model

import java.net.URL
import edu.umass.cs.iesl.scalacommons.{SeqUtils, Lexicon, OptionUtils, NonemptyString}
import edu.umass.cs.iesl.namejuggler.{PersonNameWithDerivations, CanonicalPersonName}
import edu.umass.cs.iesl.scalacommons.StringUtils._
import com.weiglewilczek.slf4s.Logging

object Person {

	def withIdentifiers(p: Person, newIds: Seq[PersonIdentifier]): Person = new Person {
		override val name         = p.name
		override val address      = p.address
		override val email        = p.email
		override val phone        = p.phone
		override val affiliations = p.affiliations
		override val homepages    = p.homepages
		override val identifiers  = newIds ++ p.identifiers
	}

	def apply(fullname: String): Person =
		new Person {
			override val name = Some(PersonNameWithDerivations(fullname))
			//StringUtils.emptyStringToNone(fullname).map(n => PersonNameWithDerivations(n))
		}

	def apply(givenNames: String, surName: String): Person =
		new Person {
			val fs                        = givenNames.split(" ").toSeq
			//val f   = fs.flatMap(t => emptyStringToNone(t))
			val l: Option[NonemptyString] = surName

			override val name = Some((new CanonicalPersonName() {
				override val givenNames: Seq[NonemptyString] = fs
				override val surNames                        = l.toSet
			}).withDerivations)
		}

	def merge(primary: Person, secondary: Person) = new Person {
		override val name         = OptionUtils.merge(primary.name, secondary.name, PersonNameWithDerivations.merge)
		override val address      = OptionUtils.mergeWarn(primary.address, secondary.address)
		override val email        = OptionUtils.mergeWarn(primary.email, secondary.email)
		override val phone        = OptionUtils.mergeWarn(primary.phone, secondary.phone)
		override val affiliations = primary.affiliations ++ secondary.affiliations
		override val homepages    = primary.homepages ++ secondary.homepages
		override val identifiers  = primary.identifiers ++ secondary.identifiers
	}
}

/**an entity that may be an author (i.e., a person or institution or named collaboration)
 *
 */
trait Agent {
	type Self <: Agent

	/**
	 * Could this agent and the other agent possibly be the same?  Generally this will depend on name compatibility only,
	 * since addresses and whatnot could easily change.
	 * @param other
	 */
	def compatibleWith(other: Agent): Boolean

	//** in the case of PersonNameWithDerivations we just put the merge function in the companion; but here we want this subtyping fanciness.
	def mergeWith[T <: Self](other: T): Self

	def hasName: Boolean

	val address: Option[Address] = None

	val phone    : Option[NonemptyString] = None
	val email    : Option[NonemptyString] = None
	val homepages: Seq[URL]               = Nil
}

trait Person extends Agent {
	override type Self = Person
	val name        : Option[PersonNameWithDerivations] = None
	val affiliations: Seq[Institution]                  = Nil
	val identifiers : Seq[PersonIdentifier]             = Nil

	def hasName: Boolean = name.isDefined

	//	def bestFullName: String = name.flatMap(_.inferFully.bestFullName).map(_.s).getOrElse("")
	override def toString = name.map(_.toString).getOrElse("")

	//bestFullName
	override def compatibleWith(other: Agent): Boolean = other match {
		case p: Person => (name, p.name) match {
			case (Some(a), Some(b)) => a.toCanonical.compatibleWith(b.toCanonical)
			case _ => true // a Person with an empty name is compatible with all others
		}
		case _ => false // a Person can only be compatible with a Person
	}

	override def mergeWith[T <: Person](secondary: T): Person = Person.merge(this, secondary)
}

trait PersonIdentifier {
	val authority: Option[PersonIdentifierAuthority] = None
	val value: String

	def qualifiedValue = authority.map(_.shortName).getOrElse("Unknown") + ":" + value
}

/*
case class BasicPerson(override val name: Option[String] = None, //
                       address: Option[Address] = None, //
                       email: Option[String] = None, //
                       phone: Option[String] = None, //
                       affiliations: Seq[Institution] = Nil, //
                       homepages: Seq[URL] = Nil) extends Person*/
case class AuthorInRole(agent: Agent, roles: Seq[AuthorRole]) {

	def mergeWithMatching(fullAuthors: Seq[AuthorInRole]): AuthorInRole = {
		val matches = fullAuthors.filter(other => agent.compatibleWith(other.agent))
		if (matches.size == 1) {
			val other: AuthorInRole = matches.head
			other.agent match {
				//** this sucks: it's not typesafe due to erasure
				// this test will always pass because agent.compatibleWith checked it already, but we need it for type safety
				case otherAgent: agent.Self => new AuthorInRole(agent mergeWith otherAgent, roles ++ other.roles)
				case _ => throw new Error("impossible")
			}
		}
		else {
			// if there are no matches or ambiguous matches, just drop it
			this
		}
	}
}

case class OtherContributorInRole(person: Person, roles: Seq[OtherContributorRole])

sealed class AuthorRole

// a middle author has an empty "role" but is still an author
case object FirstAuthor extends AuthorRole

case object EqualContribution extends AuthorRole

// (could be represented as multiple first author)
case object WroteThePaper extends AuthorRole

case object ProvidedMaterials extends AuthorRole

case object ProvidedData extends AuthorRole

case object PerformedExperiment extends AuthorRole

case object PerformedAnalysis extends AuthorRole

case object ConceivedExperiment extends AuthorRole

case object PrincipalInvestigator extends AuthorRole

case object CoPrincipalInvestigator extends AuthorRole

case object Corresponding extends AuthorRole

sealed class OtherContributorRole

case object Editor extends OtherContributorRole

case object Communicator extends OtherContributorRole

// e.g., for PNAS
case object Acknowledged extends OtherContributorRole

case object Reviewer extends OtherContributorRole

case object ProgramManager extends OtherContributorRole

object Institution {
	def merge(primary: Institution, secondary: Institution): Institution = new Institution {
		//override val institutionType      = OptionUtils.mergeWarn(primary.institutionType, secondary.institutionType)
		override val name      = OptionUtils.mergeWarn(primary.name, secondary.name)
		override val address   = OptionUtils.mergeWarn(primary.address, secondary.address)
		override val email     = OptionUtils.mergeWarn(primary.email, secondary.email)
		override val phone     = OptionUtils.mergeWarn(primary.phone, secondary.phone)
		override val parent    = OptionUtils.mergeWarn(primary.parent, secondary.parent)
		override val homepages = primary.homepages ++ secondary.homepages
	}
}

trait Institution extends Agent {
	override type Self = Institution
	//val institutionType : Option[InstitutionType]
	val name  : Option[NonemptyString]
	val parent: Option[Institution]

	def hasName: Boolean = name.isDefined

	override def compatibleWith(other: Agent): Boolean = other match {
		case p: Institution => (name, p.name) match {
			case (Some(a), Some(b)) => a == b
			case _ => true // an Institution with an empty name is compatible with all others
		}
		case _ => false // an Institution can only be compatible with an Institution
	}

	// sensible naming to access shadowed variables
	var primary = this

	override def mergeWith[T <: Institution](secondary: T): Institution = Institution.merge(this, secondary)
}

case class BasicInstitution(override val name: Option[NonemptyString], override val address: Option[Address], override val phone: Option[NonemptyString],
                            override val email: Option[NonemptyString], override val homepages: Seq[URL], override val parent: Option[Institution])
//override val institutionType : Option[Institution])
		extends Institution

trait IdentifierAuthority extends Institution {
	val shortName: NonemptyString // for prefixing the ID to establish uniqueness in some string context, e.g. "pubmed:838387"
}

case class BasicIdentifierAuthority(override val shortName: NonemptyString) extends IdentifierAuthority {
	val name   = Some(shortName)
	val parent = None
}

trait InstitutionIdentifierAuthority extends IdentifierAuthority with Institution

/**
 * A file constitutes an "authority" for incrementing ids based on the record order
 */
trait LocationIdentifierAuthority extends IdentifierAuthority with Location

trait KeywordAuthority extends Institution {
	val shortName: NonemptyString // for prefixing the ID to establish uniqueness in some string context, e.g. "pubmed:838387"
}

case class BasicKeywordAuthority(override val shortName: NonemptyString) extends KeywordAuthority {
	val name   = Some(shortName)
	val parent = None
}

sealed class AddressType

case object University extends AddressType

case object Hospital extends AddressType

case object Government extends AddressType

case object Nonprofit extends AddressType

case object Industry extends AddressType

object RichAddress {
	implicit def toRichAddress(address: Address): RichAddress = new RichAddress(address)

	val universityWords = new Lexicon("university")
	val hospitalWords   = new Lexicon("hospital")
	val governmentWords = new Lexicon("government")
	val nonprofitWords  = new Lexicon("nonprofit")
	val industryWords   = new Lexicon("industry")
}

class RichAddress(address: Address) extends Logging {

	import RichAddress._

	// this belongs in some inference module, not in the middle of the model?  Or int RichAddress, at least...
	def inferredAddressType: Option[AddressType] = address.addressType.orElse(inferAddressType(address.streetLines.mkString(" ")))

	private def inferAddressType(a: String): Option[AddressType] = {
		val countsByType: Map[AddressType, Map[String, Int]] = Map(
				University -> universityWords.substringMatchesLC(a), Hospital -> hospitalWords.substringMatchesLC(a), Government ->
				                                                                                                      governmentWords.substringMatchesLC(a),
				Nonprofit -> nonprofitWords.substringMatchesLC(a), Industry -> industryWords.substringMatchesLC(a)
				)

		val populatedTypes: Map[AddressType, Map[String, Int]] = countsByType.filter(!_._2.isEmpty)
		populatedTypes.size match {
			case 0 => None
			case 1 => Some(populatedTypes.head._1)
			case _ => {
				val best = SeqUtils.argMax[AddressType,Int](populatedTypes.keys, x=>populatedTypes(x).values.sum)
				logger.warn("Address type ambiguity: " + populatedTypes + "; chose " + best)
				None
			}
		}
	}
}

trait Address {
	val streetLines: Seq[String]
	val city       : String
	val country    : Country
	val addressType: Option[AddressType]
}

case class BasicAddress(override val streetLines: Seq[String], override val city: String, override val country: Country,
                        override val addressType: Option[AddressType])
		extends Address

trait PersonIdentifierAuthority extends Institution {
	val shortName: NonemptyString // for prefixing the ID to establish uniqueness in some string context, e.g. "pubmed:838387"
}

case class BasicPersonIdentifierAuthority(override val shortName: NonemptyString) extends PersonIdentifierAuthority {
	val name   = Some(shortName)
	val parent = None
}

case class BasicPersonIdentifier(override val value: String, override val authority: Option[PersonIdentifierAuthority] = None) extends PersonIdentifier
