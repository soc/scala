package scala

import java.nio.file._
import java.nio.file.attribute.BasicFileAttributes
import java.io.{ IOException, File => JFile }
import java.net.URI
import scala.collection.JavaConverters._
import scala.collection.{ mutable, immutable }

trait PathVisitor extends FileVisitor[Path] {
  def preVisitDirectory(dir: Path, attrs: BasicFileAttributes): FileVisitResult
  def postVisitDirectory(dir: Path, exc: java.io.IOException): FileVisitResult
  def visitFile(file: Path, attrs: BasicFileAttributes): FileVisitResult
  def visitFileFailed(file: Path, exc: java.io.IOException): FileVisitResult
}
object PathVisitor {
  class Simple extends SimpleFileVisitor[Path] with PathVisitor { }

  val Continue     = FileVisitResult.CONTINUE
  val SkipSiblings = FileVisitResult.SKIP_SIBLINGS
  val SkipSubtree  = FileVisitResult.SKIP_SUBTREE
  val Terminate    = FileVisitResult.TERMINATE

  def apply(f: (Path, BasicFileAttributes) => FileVisitResult): PathVisitor = new Simple {
    override def visitFile(file: Path, attrs: BasicFileAttributes): FileVisitResult = f(file, attrs)
  }
  def apply(f: Path => Unit): PathVisitor = new Simple {
    override def visitFile(file: Path, attrs: BasicFileAttributes): FileVisitResult = {
      f(file)
      Continue
    }
  }
}

package object io {
  type Path = java.nio.file.Path
  type File = java.io.File

  implicit class JavaPathOps[T <: Path](val path: T) extends AnyVal {
    def / (segment: String): Path = path resolve segment
    def parent                    = path.getParent
    def name                      = path.getFileName
    def bytes                     = Files readAllBytes path

    def walk(visitor: PathVisitor): Unit = Files.walkFileTree(path, visitor)
    def walk(f: (Path, BasicFileAttributes) => FileVisitResult): Unit = walk(PathVisitor(f))
    def map[U](f: Path => U): Seq[U] = {
      val buf = mutable.ListBuffer[U]()
      walk(PathVisitor((x: Path) => buf += f(x)))
      buf.toList
    }

    // def walkable: Iterable[Path] =

    def lines(implicit codec: Codec): Vector[String] = Files.readAllLines(path, codec.charSet).asScala.to[Vector]
  }

  def Path(s: String, ss: String*): Path = Paths.get(s, ss: _*)
  def Path(uri: URI): Path               = Paths get uri
  def File(s: String, ss: String*): File = Path(s, ss: _*).toFile
}
