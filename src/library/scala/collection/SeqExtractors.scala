package scala.collection

/** An extractor used to head/tail deconstruct sequences. */
object +: {
  def unapply[T,Coll <: SeqLike[T, Coll]](
      t: Coll with SeqLike[T, Coll]): Opt[(T, Coll)] =
    if(t.isEmpty) Opt.None
    else Opt(t.head -> t.tail)
}

/** An extractor used to init/last deconstruct sequences. */
object :+ {
  /** Splits a sequence into init :+ tail.
   * @return Some((init, tail)) if sequence is non-empty. None otherwise.
   */
  def unapply[T,Coll <: SeqLike[T, Coll]](
      t: Coll with SeqLike[T, Coll]): Opt[(Coll, T)] =
    if(t.isEmpty) Opt.None
    else Opt(t.init -> t.last)
}

// Dummy to fool ant
private abstract class SeqExtractors
