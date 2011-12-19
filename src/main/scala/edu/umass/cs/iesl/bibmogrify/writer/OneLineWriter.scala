package edu.umass.cs.iesl.bibmogrify.writer

import edu.umass.cs.iesl.bibmogrify.model.CitationMention
import edu.umass.cs.iesl.bibmogrify.model.RichCitationMention._
import edu.umass.cs.iesl.bibmogrify.CitationStreamWriter
import java.io.BufferedWriter

object OneLineWriter extends CitationStreamWriter
  {
  def apply(cms: TraversableOnce[CitationMention], out: BufferedWriter) =
    {

    for (cm <- cms)
      {
      val line = cm.locations.headOption.getOrElse("") + "\t" + cm.primaryId + "\t" + cm.year.getOrElse("") + "\t" + cm.authorFullNames.mkString(", ") + "\t" + cm.title + "\t" + cm.cleanAbstract +
                 "\n"

      out.write(line)
      }
    }
  }

object MalletAbstractWriter extends CitationStreamWriter
  {
  def apply(cms: TraversableOnce[CitationMention], out: BufferedWriter) =
    {

    for (cm <- cms)
      {
      val line = cm.primaryId + "\t" + cm.year.getOrElse("") + "\t" + cm.cleanTitleAndAbstract + "\n"

      out.write(line)
      }
    }
  }


object MalletFullWriter extends CitationStreamWriter
  {
  def apply(cms: TraversableOnce[CitationMention], out: BufferedWriter) =
    {

    for (cm <- cms)
      {
      val line = cm.primaryId + "\t" + cm.year.getOrElse("") + "\t" + cm.cleanTotal + "\n"
      out.write(line)
      }
    }
  }

