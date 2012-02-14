package edu.umass.cs.iesl.bibmogrify.pipeline


import com.weiglewilczek.slf4s.Logging

/**
 * @author <a href="mailto:dev@davidsoergel.com">David Soergel</a>
 * @version $Id$
 */

trait Transformer[S, T] extends ((S) => TraversableOnce[T])

//trait Source[T] extends TraversableOnce[T]

trait Sink[T] {
  def put(c: T)
}

object Pump extends Logging {
  def apply[T](source: TraversableOnce[T], sink: Sink[T])  {
    val x = source.toSeq  // no clue why toSeq is needed here, but otherwise the map below doesn't work
    x.map(sink.put(_))
  }
}

class CompositeTransformer[T, U, V](first: Transformer[T, U], second: Transformer[U, V]) extends Transformer[T, V] {
  def apply(t: T) = first(t).flatMap(second)
}
