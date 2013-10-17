package edu.umass.cs.iesl.bibmogrify.compare

import edu.umass.cs.iesl.bibmogrify.pipeline.Transformer
import edu.umass.cs.iesl.bibmogrify.model.{Person, StructuredCitation}
import edu.umass.cs.iesl.namejuggler.PersonNameWithDerivations
import edu.umass.cs.iesl.bibmogrify.{BibMogrifyException, NamedPlugin}
import com.typesafe.scalalogging.slf4j.Logging
import edu.umass.cs.iesl.scalacommons.NonemptyString

import edu.umass.cs.iesl.bibmogrify.model.RichStructuredCitation._

import edu.umass.cs.iesl.scalacommons.NonemptyString
import edu.umass.cs.iesl.scalacommons.StringUtils._

/**
 * @author <a href="mailto:dev@davidsoergel.com">David Soergel</a>
 */

object AuthorsWithSubjects extends Transformer[StructuredCitation, (PersonNameWithDerivations,Int,StructuredCitation)]  with NamedPlugin with Logging {
  val name = "AuthorsWithSubjects"
  def apply(cm: StructuredCitation) = {
    cm.authors.map(_.agent).zipWithIndex.collect({case (x:Person,position:Int)=>(x,position)}).map({case  (x:Person,position:Int) if x.name.isDefined => (x.name.get, position, cm) })
  }
}


object AminoAcidAuthorsHash extends Transformer[(PersonNameWithDerivations,Int,StructuredCitation), String] with NamedPlugin with Logging {
  import AminoAcidUtils._


  val name = "AAAuthorHash"


  def apply(x: (PersonNameWithDerivations, Int, StructuredCitation)) = {
    x match {
      case (pname, position, sc) => {
        val n = pname.inferFully
        val lastName = n.longestSurName.getOrElse(throw new BibMogrifyException("Dropping author with no surname"))
        val namePart = Seq(n.firstInitial.getOrElse(""), n.firstName.getOrElse(""), n.middleInitials.getOrElse(""), n.middleNames.mkString(" "), n.lastInitial.getOrElse(""), lastName).mkString("\t")
        val subjectCodes = sc.allKeywords.map(_.word).toSeq.seq.sorted

        val yearRange = sc.year.map(aaizeYearRange(_, 5, 10)).getOrElse("")

        val venue = sc.containedIn.map(_.container.title.unwrap)

        // we're going to bin by first initial and last name before doing the hash-based clustering anyway, so they don't need to be included in the hash.

        val hash: NonemptyString = (aaizeComplete(subjectCodes.mkString("") + sc.authorFirstInitialLastNamesSorted.mkString("") + venue.getOrElse("")) + yearRange).n

        val nameId = sc.primaryId + "-" + position

        // fasta transformation will happen downstream
        // Some(">" + nameId + "\n" + hash + "\n")
        val result = Seq(nameId, namePart, hash).mkString("\t")
        Some(result+"\n")
      }
    }
  }
}

object AllAuthorNames extends Transformer[StructuredCitation, (String,Int,StructuredCitation)]  with NamedPlugin with Logging {
  val name = "AllAuthorNames"
  def apply(cm: StructuredCitation) = {
    cm.authors.map(_.agent).zipWithIndex.collect({case (x:Person,position:Int)=>(x,position)}).map({case  (x:Person,position:Int) if x.name.isDefined => (x.name.get.bestFullName.get.s, position, cm) })
  }
}

object AuthorNamesWithLength extends Transformer[(String,Int,StructuredCitation), String] with NamedPlugin with Logging {

  val name = "AuthorNamesWithLength"

  def apply(x: (String, Int,StructuredCitation)) = {
    x match {
      case (name, position, sc) => {
        val nameId = sc.primaryId + "-" + position
        val result = Seq(name.length, nameId, name).mkString("\t")
        Some(result+"\n")
      }
    }
  }
}

