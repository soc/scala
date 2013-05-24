package scala
package coll

import java.util.concurrent.LinkedBlockingQueue

trait Foreach[+A] {
  def foreach(f: A => Any): Unit
  def toString(implicit fmt: Show[A] = Show.ToString): String = fmt showAll this
}

object Foreach {
  private[this] object Empty extends Foreach[Any] { def foreach(f: Any => Any) { } }
  def empty[A] : Foreach[A] = Empty.asInstanceOf[Foreach[A]]

  final class WithFilter[+A](xs: Foreach[A], p: A => Boolean) extends Foreach[A] {
    def withFilter(q: A => Boolean) = new WithFilter(xs, (x: A) => p(x) && q(x))
    def filter(q: A => Boolean) = withFilter(q)
    def foreach(f: A => Any) = xs foreach (x => if (p(x)) f(x))
  }

  final class ForeachClass[+A](g: (A => Any) => Unit) extends Foreach[A] {
    def foreach(f: A => Any): Unit = g(f)
  }

  final class ForeachIterator[A](val xs: Foreach[A]) extends Iterator[A] {
    private[this] val queue = new LinkedBlockingQueue[A]
    xs foreach queue.put

    def hasNext = !queue.isEmpty
    def next = queue.take()
  }

  def apply[A](xs: java.lang.Iterable[A]): Foreach[A] = new ForeachClass[A](iterator(xs) foreach _)
  def apply[A](f: (A => Any) => Unit): Foreach[A]     = new ForeachClass[A](f)

  def iterator[A](xs: Foreach[A]): Iterator[A] = new ForeachIterator(xs)
  def iterator[A](xs: java.lang.Iterable[A]): Iterator[A] = iterator(xs.iterator)
  def iterator[A](xs: java.util.Iterator[A]): Iterator[A] = new Iterator[A] {
    def hasNext = xs.hasNext
    def next()  = xs.next()
  }

  def map[A, B](xs: Foreach[A], g: A => B): Foreach[B] =
    new ForeachClass((f: B => Any) => xs foreach (x => f(g(x))))

  def join[A](xs: Foreach[A], ys: Foreach[A]): Foreach[A] =
    new ForeachClass((f: A => Any) => { xs foreach f ; ys foreach f })
}
