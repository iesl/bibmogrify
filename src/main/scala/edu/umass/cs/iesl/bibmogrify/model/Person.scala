package edu.umass.cs.iesl.bibmogrify.model

import java.net.URL
import collection.immutable.Seq
import edu.umass.cs.iesl.scalacommons.{StringUtils, NonemptyString}
import StringUtils.emptyStringToNone

object Person
	{

	def withIdentifiers(p: Person, newIds: Seq[PersonIdentifier]): Person = new Person
		{
		override val name         = p.name
		override val address      = p.address
		override val email        = p.email
		override val phone        = p.phone
		override val affiliations = p.affiliations
		override val homepages    = p.homepages
		override val identifiers  = newIds ++ p.identifiers
		}

	def apply(fullname: String): Person =
		new Person
			{
			override val name = PersonNameWithDerivations(fullname)
			//StringUtils.emptyStringToNone(fullname).map(n => PersonNameWithDerivations(n))
			}

	def apply(givenNames: String, surName: String): Person =
		new Person
			{
			val f: Option[NonemptyString] = givenNames
			val l: Option[NonemptyString] = surName

			override val name = new CanonicalPersonName()
				{
				override val givenNames = f
				override val surNames   = l.toSet
				}
			}
	}

/**an entity that may be an author (i.e., a person or institution or named collaboration)
 *
 */
trait Agent

trait Person extends Agent
	{
	val name        : Option[PersonNameWithDerivations] = None
	val address     : Option[Address]                   = None
	val email       : Option[NonemptyString]            = None
	val phone       : Option[NonemptyString]            = None
	val affiliations: Seq[Institution]                  = Nil
	val homepages   : Seq[URL]                          = Nil
	val identifiers : Seq[PersonIdentifier]             = Nil

	//	def bestFullName: String = name.flatMap(_.inferFully.bestFullName).map(_.s).getOrElse("")
	override def toString = name.map(_.toString).getOrElse("") //bestFullName
	}

trait PersonIdentifier
	{
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
case class AuthorInRole(agent: Agent, roles: Seq[AuthorRole])
	{
	/*
	def mergeWithMatching(fullAuthors: Seq[AuthorInRole]): AuthorInRole =
		{
		val matches = fullAuthors.filter(other => PersonName.compatibleName(person.name, other.person.name))
		if (matches.size == 1)
			{
			// assume the "full" record is more complete, but merge the role and id info
			val full: AuthorInRole = matches.head
			new AuthorInRole(Person.withIdentifiers(full.person, person.identifiers), roles ++ full.roles)
			}
		else
			{
			// if there are no matches or ambiguous matches, just drop it
			this
			}
		}*/
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

trait Institution extends Agent
	{
	val name     : String
	val address  : Option[Address]
	val phone    : Option[NonemptyString]
	val email    : Option[NonemptyString]
	val homepages: Seq[URL]
	val parent   : Option[Institution]
	}

case class BasicInstitution(override val name: String, override val address: Option[Address], override val phone: Option[NonemptyString],
                            override val email: Option[NonemptyString], override val homepages: Seq[URL], override val parent: Option[Institution])
		extends Institution

trait IdentifierAuthority extends Institution
	{
	val shortName: String // for prefixing the ID to establish uniqueness in some string context, e.g. "pubmed:838387"
	}

case class BasicIdentifierAuthority(override val shortName: String) extends IdentifierAuthority
	{
	val address   = None
	val email     = None
	val homepages = Nil
	val name      = shortName
	val parent    = None
	val phone     = None
	}

trait InstitutionIdentifierAuthority extends IdentifierAuthority with Institution

/**
 * A file constitutes an "authority" for incrementing ids based on the record order
 */
trait LocationIdentifierAuthority extends IdentifierAuthority with Location

trait KeywordAuthority extends Institution
	{
	val shortName: String // for prefixing the ID to establish uniqueness in some string context, e.g. "pubmed:838387"
	}

case class BasicKeywordAuthority(override val shortName: String) extends KeywordAuthority
	{
	val address   = None
	val email     = None
	val homepages = Nil
	val name      = shortName
	val parent    = None
	val phone     = None
	}

trait Address
	{
	val streetLines: Seq[String]
	val city       : String
	val country    : Country
	}

case class BasicAddress(override val streetLines: Seq[String], override val city: String, override val country: Country) extends Address

trait PersonIdentifierAuthority extends Institution
	{
	val shortName: String // for prefixing the ID to establish uniqueness in some string context, e.g. "pubmed:838387"
	}

case class BasicPersonIdentifierAuthority(override val shortName: String) extends PersonIdentifierAuthority
	{
	val address   = None
	val email     = None
	val homepages = Nil
	val name      = shortName
	val parent    = None
	val phone     = None
	}

case class BasicPersonIdentifier(override val value: String, override val authority: Option[PersonIdentifierAuthority] = None) extends PersonIdentifier
