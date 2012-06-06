def foreachFast(f: A => Unit): Unit
def mapFast[B](f: A => B): IterableOnce[B]
def flatMapFast[B](f: A => IterableOnce[B]): IterableOnce[B]

def isIterableAgain: Boolean
def toIterator: Iterator[A]
def foreach[U](f: A => U): Unit
def isEmpty: Boolean
def hasDefiniteSize: Boolean
def forall(p: A => Boolean): Boolean
def exists(p: A => Boolean): Boolean
def find(p: A => Boolean): Option[A]
def copyToArray[B >: A](xs: Array[B], start: Int, len: Int): Unit
def size: Int
def nonEmpty: Boolean
def count(p: A => Boolean): Int
def collectFirst[B](pf: PartialFunction[A, B]): Option[B]
def /:[B](z: B)(op: (B, A) => B): B
def :\[B](z: B)(op: (A, B) => B): B
def foldLeft[B](z: B)(op: (B, A) => B): B
def foldRight[B](z: B)(op: (A, B) => B): B =
def reduceLeft[B >: A](op: (B, A) => B): B
def reduceRight[B >: A](op: (A, B) => B): B
def reduce[A1 >: A](op: (A1, A1) => A1): A1
def fold[A1 >: A](z: A1)(op: (A1, A1) => A1): A1
def aggregate[B](z: B)(seqop: (B, A) => B, combop: (B, B) => B): B
def copyToBuffer[B >: A](dest: Buffer[B]): Unit
def copyToArray[B >: A](xs: Array[B], start: Int): Unit =
def copyToArray[B >: A](xs: Array[B]): Unit =
def toArray[B >: A : ArrayTag]: Array[B]
def toList: immutable.List[A]
def toSeq: immutable.Seq[A]
def toIndexedSeq: immutable.IndexedSeq[A]
def toBuffer[B >: A]: mutable.Buffer[B]
def toSet[B >: A]: immutable.Set[B]
def toMap[T, U](implicit ev: A <:< (T, U)): immutable.Map[T, U]
def mkString(sep: String): String
