package scala

final class FList[N <: Int with Singleton, T](val elems: List[T]) extends AnyVal {
  def foreach(f: T => Any): Unit = elems foreach f
  def map[U](f: T => U): FList[N, U] = new FList[N, U](elems map f)
  def zip[U](that: FList[N, U]): FList[N, (T, U)] = new FList[N, (T, U)](elems zip that.elems)
}
