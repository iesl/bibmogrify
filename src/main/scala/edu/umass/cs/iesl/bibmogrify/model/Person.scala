package edu.umass.cs.iesl.bibmogrify.model

import java.net.URL

// Don't even try to split author names into components.  First of all, it's a rabbit hole of edge cases.  Second, it's largely useless for our purposes.  We have to do coreference separately
// anyway, and split author names don't really help with that.  Thu only other thing it might be good for is sort order.  So identifying the last name for that purpose could conceivably be useful.
// But there's just no use case I can think of that requires knowing someone's first name, or pedigree, etc.
// OK: these might be needed for styling, e.g. converting names to initials.
/*case class Person(firstNameInitial: Option[Char] = None, // used only for J. Harrison Ford
                  firstName: Option[String] = None, // preferred name goes here too, e.g. Harrison
                  middleName: Option[String] = None, // often just middle initial // combine as "givenNames"?
                  givenInitials: Option[Seq[Char]] = None, //
                  lastName: Option[String] = None, //
                  pedigree: Option[String] = None, //
                  degree: Option[String] = None, //
                  // droppingParticle: Option[String] = None, //
                  nonDroppingParticle: Option[String] = None, //
                  address: Option[Address] = None, //
                  email: Option[String] = None, //
                  phone: Option[String] = None, //
                  affiliations: Seq[Institution] = Nil, //
                  homepages: Seq[URL] = Nil) //
  {
  // val droppingParticles = Map(("de la" ->("de", "la")))  // ignore for now
  val particles = List("st", "de", "la", "de la", "du", "des", "del", "di", "van", "van den", "von", "zu", "der", "ter")
  val authorSplit = "(\\S+ )?(.*? )?((" + particles.mkString("|") + ") )?(\\S+)".r
  def Person(x: String)
    {
    val authorSplit(f: String, m: String, p: String, l: String) = x
    val solidCaps = (x == x.toUpperCase)

    f.length match
    {
      case 0 => // Prufrock
        {
        assert(m.isEmpty)
        }
      case 1 if m.length > 1 => // J. Alfred X. Prufrock
        {
        firstName = Some(f); // we have only the initial, but call it the "name"
        middleName = Some(m);
        givenInitials = Some(f.head :: m.split(" .").map(_.head).toList)
        }
      case 2 =>
        {
        if (solidCaps)
          {
          if (m.isEmpty) // JA PRUFROCK.  ED GREEN is interpreted as E.D. Green, not Ed Green.
            {
            givenInitials = Some(f.toCharArray.toSeq)
            }
          else // AL J PRUFROCK interpreted as Al J. Prufrock
            {
            firstName = Some(f);
            middleName = Some(m);
            givenInitials = Some(f.head :: m.split(" .").map(_.head).toList)
            }
          }
        }
      case _ => firstName = Some(f);
    }



    /* val tokensR = x.split(" .").reverse;
    lastName = tokensR.headOption
    val givenNames = tokensR.tail.reverse
    val maybeFirstName = givenNames.head
    if(maybeFirstName.contains()*/
    }
  //override def toString = firstName.map(_ + " ").getOrElse("") + lastName
  def fullName = firstName.map(_ + " " + middleName.map(_ + " ").getOrElse("")).getOrElse(givenInitials.map(_ + " ").getOrElse("")) + lastName
  }
*/
trait Person
  {
  val name: Option[String] = None
  val address: Option[Address]= None
  val email: Option[String]= None
  val phone: Option[String]= None
  val affiliations: Seq[Institution]= Nil
  val homepages: Seq[URL]= Nil
  }

/*
case class BasicPerson(override val name: Option[String] = None, //
                       address: Option[Address] = None, //
                       email: Option[String] = None, //
                       phone: Option[String] = None, //
                       affiliations: Seq[Institution] = Nil, //
                       homepages: Seq[URL] = Nil) extends Person*/

case class AuthorInRole(person: Person, roles: Seq[AuthorRole])

case class OtherContributorInRole(person: Person, roles: Seq[OtherContributorRole])

sealed class AuthorRole
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

trait Institution
  {
  val name: String
  val address: Option[Address]
  val phone: Option[String]
  val email: Option[String]
  val homepages: Seq[URL]
  val parent: Option[Institution]
  }

case class BasicInstitution(override val name: String, override val address: Option[Address], override val phone: Option[String], override val email: Option[String], override val homepages: Seq[URL],
                            override val parent: Option[Institution]) extends Institution

trait IdentifierAuthority extends Institution
  {
  val shortName: String // for prefixing the ID to establish uniqueness in some string context, e.g. "pubmed:838387"
  }

case class BasicIdentifierAuthority(override val shortName: String) extends IdentifierAuthority
  {
  val address = None
  val email = None
  val homepages = Nil
  val name = shortName
  val parent = None
  val phone = None
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
  val address = None
  val email = None
  val homepages = Nil
  val name = shortName
  val parent = None
  val phone = None
  }

trait Address
  {
  val streetLines: Seq[String]
  val city: String
  val country: Country
  }

case class BasicAddress(override val streetLines: Seq[String], override val city: String, override val country: Country) extends Address
