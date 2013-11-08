/*
 * Copyright (c) 2013  University of Massachusetts Amherst
 * Licensed under the Apache License, Version 2.0
 * http://www.apache.org/licenses/LICENSE-2.0
 */

package edu.umass.cs.iesl.bibmogrify.pipeline

import com.typesafe.scalalogging.slf4j.Logging
import collection.GenTraversableOnce

/**
 * @author <a href="mailto:dev@davidsoergel.com">David Soergel</a>
 * @version $Id$
 */
trait Transformer[S, +T] extends ((S) => GenTraversableOnce[T]) {
  def metadata: Option[TransformerMetadata] = None

  // introspecting the types is a mess; just hardcode the strings
  val fromType: String  // S
  val toType: String  // T
}

trait TransformerMetadata

// for tracking provenance, headers, etc.
case class StringMetadata(value: String) extends TransformerMetadata {
  override def toString = value
}

//trait Source[T] extends TraversableOnce[T]
trait Sink[T] {
  def put(c: T)

  def putMetadata(m: Option[TransformerMetadata])

  def close()
}

object Pump extends Logging {
  def apply[T](source: TraversableOnce[T], sink: Sink[T]) {
    source.foreach(sink.put(_))
  }

/*
  def apply[T](source: TraversableOnce[T], sink: Sink[T], before:Option[T], between: Option[T], after:Option[T]) {
    //val x = source.toIterable.par //toSeq // no clue why toSeq is needed here, but otherwise the map below doesn't work ??  BAD memory use.  even
    // toIterable makes a Stream.
    //logger.warn("Pumping...")

    before.map(sink.put(_))

    var first = true
    source.foreach(x=>{
      if (!first) {
        between.map(sink.put(_))
        first = false
      }
      sink.put(x)
    })

    after.map(sink.put(_))
    //logger.warn("Done Pumping!")
  }
*/
}

/*
object ParPump extends Logging
  {
  def apply[T](source: TraversableOnce[T], sink: Sink[T])
    {
    source.toIterator.sliding(100,100).foreach(
    i=>i.par.foreach(sink.put(_))
    )

    //val x = source.toIterable.par //toSeq // no clue why toSeq is needed here, but otherwise the map below doesn't work ??  BAD memory use.  even
    // toIterable makes a Stream.
    //logger.warn("Pumping...")
    //source.foreach(sink.put(_))
    //logger.warn("Done Pumping!")
    }
  }
*/
class CompositeTransformer[T, U, V](first: Transformer[T, U], second: Transformer[U, V]) extends Transformer[T, V] with Logging {
  //def apply(t: T) = first(t).flatMap(second)  // this is strict and memoizes?
  def apply(t: T): TraversableOnce[V] = new Traversable[V] {
    // ** side effect error handling for now; would prefer an error monad
    def foreach[Z](f: (V) => Z) {
      try { {
        val a = first(t)
        a.foreach(y => {
          try { {
            val b = second(y)
            b.foreach(f)
          }
          }
          catch {
            case e => logger.error("Error", e)
          }
        })
      }
      }
      catch {
        case e => logger.error("Error", e)
      }
    }
  }

  //XmlUtils.generatorToTraversable(second).foreach(first(t))
  override def metadata = {
    (first.metadata, second.metadata) match {
      case (None, None) => None
      case (a, b) => {
        Some(new CompositeMetadata(a, b))
      }
    }
  }

  val fromType = "T"
  val toType = "V"
}

class CompositeMetadata(a: Option[TransformerMetadata], b: Option[TransformerMetadata]) extends TransformerMetadata {
  override def toString = a.map(_.toString).getOrElse("") + b.map(_.toString).getOrElse("")
}
