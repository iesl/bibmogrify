package edu.umass.cs.iesl.bibmogrify.writer

import edu.umass.cs.iesl.bibmogrify.model.RichCitationMention._
import edu.umass.cs.iesl.bibmogrify.pipeline.Transformer
import edu.umass.cs.iesl.bibmogrify.NamedPlugin
import edu.umass.cs.iesl.bibmogrify.model.StructuredCitation
import edu.umass.cs.iesl.bibmogrify.model.Authorities.PubmedAuthority

object OneLineWriter extends Transformer[StructuredCitation, String] with NamedPlugin
	{
	val name = "oneline"

	def apply(cm: StructuredCitation) =
		{
		Some(cm.locations.headOption.getOrElse("") + "\t" + cm.primaryId + "\t" + cm.year.getOrElse("") + "\t" + cm.authorFullNames.mkString(", ") + "\t" +
		     cm.title + "\t" + cm.cleanAbstract +
		     "\n")
		}
	}

object MalletAbstractWriter extends Transformer[StructuredCitation, String] with NamedPlugin
	{
	val name = "mallet"

	def apply(cm: StructuredCitation) =
		{
		Some(cm.primaryId + "\t" + cm.year.getOrElse("") + "\t" + cm.cleanTitleAndAbstract + "\n")
		}
	}

object MalletFullWriter extends Transformer[StructuredCitation, String] with NamedPlugin
	{
	val name = "malletfull"

	def apply(cm: StructuredCitation) =
		{
		Some(cm.primaryId + "\t" + cm.year.getOrElse("") + "\t" + cm.cleanTotal + "\n")
		}
	}

object CorefWriter extends Transformer[StructuredCitation, String] with NamedPlugin
	{
	val name = "corefoneline"

	def apply(cm: StructuredCitation) =
		{
		val venue: String = cm.rootContainedInNotSelf.flatMap(_.title).getOrElse("")
		Some(cm.primaryId + "\t" + cm.year.getOrElse("") + "\t" + cm.authorFullNamesWithId.mkString(", ") + "\t" + cm.title.getOrElse("") + "\t" + venue +
		     "\n")
		}
	}

object ReferenceStringWriter extends Transformer[StructuredCitation, String] with NamedPlugin
	{
	val name = "refstrings"

	def apply(cm: StructuredCitation) =
		{
		// extra CR for now for easy viewing in terminal
		Some(cm.references.map(_.unstructuredString).mkString("\n\n") + "\n")
		}
	}

object PmidRefMapWriter extends Transformer[StructuredCitation, String] with NamedPlugin
	{
	val name = "pmidrefs"

	private def pmid(cm: StructuredCitation) = cm.identifiers.filter(_.authority == Some(PubmedAuthority)).headOption

	def apply(cm: StructuredCitation) =
		{
		val op = pmid(cm)

		val result = op.map(p => (p.value +: cm.references.flatMap(pmid(_)).map(_.value)).mkString("\t") + "\n")
		result
		}
	}
