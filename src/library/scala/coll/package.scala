package scala

import scala.collection.convert.DecorateAsScala

package object coll extends DecorateAsScala {
  type tailrec = scala.annotation.tailrec
  type switch  = scala.annotation.switch
  type uV      = scala.annotation.unchecked.uncheckedVariance

  type ClassTag[A]            = scala.reflect.ClassTag[A]
  type CBF[-From, -Elem, +To] = scala.collection.generic.CanBuildFrom[From, Elem, To]
  type Clearable              = scala.collection.generic.Clearable
  type jSet[A]                = java.util.Set[A]
  type Builder[-Elem, +To]    = scala.collection.mutable.Builder[Elem, To]

  def builderForThis[A, Coll](xs: Coll with Traversable[A])(implicit cbf: CBF[Nothing, A, Coll], ev: Coll <:< Traversable[A]): Builder[A, Coll] = {
    val builder = cbf()
    builder.asInstanceOf[Builder[A, Coll]]
  }

  implicit def stringToStringSeq(s: String): StringSeq = new StringSeq(s)

  final implicit class ArrayOps[A](val xs: Array[A]) extends AnyVal {
    def mapSelf(f: A => A): Array[A] = {
      xs.indices foreach (i => xs(i) = f(xs(i)))
      xs
    }
  }

  final implicit class ForeachForeachOps[A](val xss: Foreach[Foreach[A]]) extends AnyVal {
    def flatten: Foreach[A] = xss reduceLeft (_ ++ _)
  }

  final implicit class ForeachOps[A](val xs: Foreach[A]) extends AnyVal {
    def map[B](f: A => B): Foreach[B] = Foreach.map(xs, f)
    def filter(p: A => Boolean) = withFilter(p)
    def withFilter(p: A => Boolean) = new Foreach.WithFilter(xs, p)
    def mkString(sep: String): String = iterator mkString sep

    def to[Col[_]](implicit cbf: CBF[Nothing, A, Col[A @uV]]): Col[A @uV] = {
      val b = cbf()
      xs foreach (b += _)
      b.result()
    }

    def isEmpty: Boolean = {
      xs foreach (_ => return false)
      true
    }
    def nonEmpty = !isEmpty
    def exists(p: A => Boolean): Boolean = {
      xs foreach (x => if (p(x)) return true)
      false
    }
    def forall(p: A => Boolean): Boolean = {
      xs foreach (x => if (!p(x)) return false)
      true
    }
    def contains(elem: A): Boolean = exists(_ == elem)
    def reduceLeft(f: (A, A) => A): A = {
      var acc: A = null.asInstanceOf[A]
      xs foreach (x => if (acc == null) acc = x else acc = f(acc, x))
      if (acc == null) sys.error("empty.reduceLeft")
      acc
    }
    def foldLeft[B](zero: B)(f: (B, A) => B): B = {
      var acc = zero
      xs foreach (x => acc = f(acc, x))
      acc
    }
    def iterator: Iterator[A] = Foreach iterator xs
    def ++(ys: Foreach[A]): Foreach[A] = Foreach.join(xs, ys)

    def indexed: ImmutableArraySeq[A] = toIndexedSeq
    def linear: List[A] = toList

    def toList: List[A] = {
      var buf: List[A] = Nil
      xs foreach (buf ::= _)
      buf.reverse
    }
    def toScalaList = to[scala.List]
    def toScalaSeq = to[scala.Seq]
    def toIndexedSeq: ImmutableArraySeq[A] = ImmutableArraySeq(toScalaSeq: _*)
  }

  /** Operations which need to know how to deconstruct. */
  final implicit class LinearOps[A, Repr <: Linear[A, Repr]](val xs: Repr with Linear[A, Repr]) extends AnyVal {
    def nonEmpty = !xs.isEmpty
    def take(n: Int): Repr = {
      def loop(xs: Repr, n: Int): Repr = (
        if (n <= 0 || xs.isEmpty) xs
        else loop(xs.tail, n - 1)
      )
      loop(xs, n)
    }
    def drop(n: Int): Repr = {
      def loop(xs: Repr, n: Int): Repr = (
        if (n <= 0 || xs.isEmpty) xs
        else loop(xs.tail, n - 1)
      )
      loop(xs, n)
    }
    def exists(p: A => Boolean): Boolean = {
      def loop(xs: Repr): Boolean = (
        if (xs.isEmpty) false
        else p(xs.head) || loop(xs.tail)
      )
      loop(xs)
    }
  }

  final implicit class SetOps[A, Repr](val xs: Set[A, Repr]) extends AnyVal {
    def contains(elem: A) = xs unsafeContains elem
    def apply(elem: A) = contains(elem)
  }

  final implicit class ForeachIntOps(val xs: Foreach[Int]) extends AnyVal {
    def sum: Int     = xs.foldLeft(0)(_ + _)
    def product: Int = xs.foldLeft(1)(_ * _)
    def max: Int     = xs.reduceLeft((x, y) => if (x > y) x else y)
    def min: Int     = xs.reduceLeft((x, y) => if (x < y) x else y)
  }

  /** Operations which need to know how to construct. */
  final implicit class ListOps[A](val xs: List[A]) extends AnyVal {
    def ::(elem: A): List[A] = new ::[A](elem, xs)
    def :::(elems: List[A]): List[A] = {
      var in  = elems.reverse
      var out = xs
      while (!in.isEmpty) {
        out ::= in.head
        in = in.tail
      }
      out
    }

    def filter(p: A => Boolean): List[A] = {
      def loop(xs: List[A]): List[A] = xs match {
        case Nil             => Nil
        case x :: xs if p(x) => x :: loop(xs)
        case _ :: xs         => loop(xs)
      }
      loop(xs)
    }

    def flatMap[B](f: A => Foreach[B]): List[B] = {
      def loop(in: List[A], out: List[B]): List[B] = in match {
        case Nil     => out
        case x :: xs => loop(xs, out ::: f(x).toList)
      }
      loop(xs, Nil)
    }

    def map[B](f: A => B): List[B] = {
      def loop(in: List[A], out: List[B]): List[B] = in match {
        case Nil     => out.reverse
        case x :: xs => loop(xs, f(x) :: out)
      }
      loop(xs, Nil)
    }

    def reverse: List[A] = {
      var in: List[A]  = xs
      var out: List[A] = Nil
      while (!in.isEmpty) {
        out ::= in.head
        in = in.tail
      }
      out
    }
  }
}
