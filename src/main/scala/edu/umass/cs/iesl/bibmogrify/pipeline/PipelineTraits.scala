package edu.umass.cs.iesl.bibmogrify.pipeline

import com.weiglewilczek.slf4s.Logging

/**
 * @author <a href="mailto:dev@davidsoergel.com">David Soergel</a>
 * @version $Id$
 */
trait Transformer[S, +T] extends ((S) => TraversableOnce[T])
	{
	def metadata: Option[TransformerMetadata] = None
	}

trait TransformerMetadata

// for tracking provenance, headers, etc.
case class StringMetadata(value: String) extends TransformerMetadata
	{
	override def toString = value
	}

//trait Source[T] extends TraversableOnce[T]
trait Sink[T]
	{
	def put(c: T)

	def putMetadata(m: Option[TransformerMetadata])

	def close()
	}

object Pump extends Logging
	{
	def apply[T](source: TraversableOnce[T], sink: Sink[T])
		{
		//val x = source.toIterable.par //toSeq // no clue why toSeq is needed here, but otherwise the map below doesn't work ??  BAD memory use.  even
		// toIterable makes a Stream.
		//logger.warn("Pumping...")
		source.foreach(sink.put(_))
		//logger.warn("Done Pumping!")
		}
	}

class CompositeTransformer[T, U, V](first: Transformer[T, U], second: Transformer[U, V]) extends Transformer[T, V] with Logging
	{
	//def apply(t: T) = first(t).flatMap(second)  // this is strict and memoizes?
	def apply(t: T): TraversableOnce[V] = new Traversable[V]
		{
		// ** side effect error handling for now; would prefer an error monad
		def foreach[Z](f: (V) => Z)
			{
			try
			{
			{
			val a = first(t)
			a.foreach(y =>
				          {
				          try
				          {
				          {
				          val b = second(y)
				          b.foreach(f)
				          }
				          }
				          catch
				          {
				          case e => logger.error("Error", e)
				          }
				          })
			}
			}
			catch
			{
			case e => logger.error("Error", e)
			}
			}
		}

	//XmlUtils.generatorToTraversable(second).foreach(first(t))
	override def metadata =
		{
		(first.metadata, second.metadata) match
		{
			case (None, None) => None
			case (a, b)       =>
				{
				Some(new CompositeMetadata(a, b))
				}
		}
		}
	}

class CompositeMetadata(a: Option[TransformerMetadata], b: Option[TransformerMetadata]) extends TransformerMetadata
	{
	override def toString = a.map(_.toString).getOrElse("") + b.map(_.toString).getOrElse("")
	}
