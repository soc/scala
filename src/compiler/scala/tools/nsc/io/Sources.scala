package scala.tools.nsc
package io

import util.ClassPath
import java.util.concurrent.{ Future, ConcurrentHashMap, ExecutionException }
import java.util.zip.ZipException
import collection.JavaConverters._
import Properties.{ envOrElse, propOrElse }

class Sources(val path: String) {
  val expandedPath        = ClassPath.join(ClassPath expandPath path: _*)
  val cache               = new ConcurrentHashMap[String, List[Fileish]]
  def allNames            = cache.keys.asScala.toList.sorted
  def apply(name: String) = get(name)
  def size                = cache.asScala.values map (_.length) sum
  def isEmpty             = path == ""

  private val partitioned = ClassPath toPaths expandedPath partition (_.isDirectory)

  val dirs   = partitioned._1 map (_.toDirectory)
  val jars   = partitioned._2 filter Jar.isJarOrZip map (_.toFile)
  val (isDone, force) = (
    if (path == "") (() => true, () => ())
    else {
      val f1  = spawn(calculateDirs())
      val f2  = spawn(calculateJars())
      val fn1 = () => { f1.isDone() && f2.isDone() }
      val fn2 = () => { f1.get() ; f2.get() ; () }

      (fn1, fn2)
    }
  )

  private def catchZip(body: => Unit): Unit = {
    try body
    catch { case x: ZipException => }
  }

  private def calculateDirs() =
    dirs foreach { d => catchZip(addSources(d.deepFiles map (x => Fileish(x)))) }

  private def calculateJars() =
    jars foreach { j => catchZip(addSources(new Jar(j).fileishIterator)) }

  private def addSources(fs: IterableOnce[Fileish]) =
    fs foreach { f => if (f.isSourceFile) add(f.name, f) }

  private def get(key: String): List[Fileish] =
    if (cache containsKey key) cache.get(key) else Nil

  private def add(key: String, value: Fileish) = {
    if (cache containsKey key) cache.replace(key, value :: cache.get(key))
    else cache.put(key, List(value))
  }
  override def toString = "Sources(%d dirs, %d jars, %d sources)".format(
    dirs.size, jars.size, cache.asScala.values map (_.length) sum
  )
}

trait LowPrioritySourcesImplicits {
  self: Sources.type =>

  implicit def fallbackSources: Sources = defaultSources
}

object Sources extends LowPrioritySourcesImplicits {
  val empty = new Sources("")

  private def libraryInits      = ClassPath.scalaLibrary.toList flatMap (_.toAbsolute.parents)
  private def librarySourceDir  = libraryInits map (_ / "src") find (_.isDirectory)
  private def expandedSourceDir = librarySourceDir.toList flatMap (ClassPath expandDir _.path)

  private val initialPath    = sys.props.traceSourcePath.value
  private val initialSources = apply(expandedSourceDir :+ initialPath: _*)

  def defaultSources = {
    val path = sys.props.traceSourcePath.value
    if (path == "") empty
    else if (path == initialPath) initialSources
    else apply(expandedSourceDir :+ path: _*)
  }

  def apply(paths: String*): Sources = new Sources(ClassPath.join(paths: _*))
}
