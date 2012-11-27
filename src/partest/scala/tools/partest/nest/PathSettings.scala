/* NEST (New Scala Test)
 * Copyright 2007-2013 LAMP/EPFL
 */

package scala.tools.partest
package nest

import scala.tools.nsc.Properties.{ setProp, propOrEmpty, propOrNone, propOrElse }
import scala.tools.nsc.util.ClassPath
import scala.tools.nsc.io
import io.{ Path, File, Directory }

trait PartestPaths {
  private def findJar(d: Directory, name: String): Option[SFile] = findJar(d.files, name)
  private def findJar(files: Iterator[SFile], name: String): Option[SFile] =
    files filter (_ hasExtension "jar") find { _.name startsWith name }

  // Directory <root>/test
  // lazy val testRoot: Directory = testRootDir getOrElse {
  //   val candidates: List[Directory] = (cwd :: cwd.parents) flatMap (d => List(d, Directory(d / "test")))

  //   candidates find isPartestDir getOrElse sys.error("Directory 'test' not found.")
  // }

  // Directory <root>/test/files
  lazy val srcDir = Directory(testRoot / srcDirName toCanonical)

  // Directory <root>/test/files/lib
  lazy val srcLibDir = Directory(srcDir / "lib")

  // Directory <root>/test/files/speclib
  lazy val srcSpecLibDir = Directory(srcDir / "speclib")

  lazy val srcSpecLib: SFile = findJar(srcSpecLibDir, "instrumented") getOrElse {
    sys.error("No instrumented.jar found in %s".format(srcSpecLibDir))
  }

  // Directory <root>/test/files/codelib
  lazy val srcCodeLibDir = Directory(srcDir / "codelib")

  lazy val srcCodeLib: SFile = (
    findJar(srcCodeLibDir, "code")
      orElse findJar(Directory(testRoot / "files" / "codelib"), "code") // work with --srcpath pending
      getOrElse sys.error("No code.jar found in %s".format(srcCodeLibDir))
  )

  lazy val instrumentationAgentLib: SFile = {
    findJar(buildPackLibDir.files, "scala-partest-javaagent") getOrElse {
      sys.error("No partest-javaagent jar found in '%s' or '%s'".format(buildPackLibDir, srcLibDir))
    }
  }

  // Directory <root>/build
  lazy val buildDir: Directory = {
    val bases      = testRoot :: testRoot.parents
    // In the classic "ant" build, the relevant subdirectory is called build,
    // but in the postmodern "sbt" build, it is called target.  Look for both.
    val dirs = Path.onlyDirs(bases flatMap (x => List(x / "build", x / "target")))

    dirs.headOption getOrElse sys.error("Neither 'build' nor 'target' dir found under test root " + testRoot + ".")
  }

  // Directory <root>/build/pack/lib
  lazy val buildPackLibDir = Directory(buildDir / "pack" / "lib")

  def extraTestLibs: List[URL] =
    srcCodeLib :: Directory(srcDir / "lib").files.filter(_ hasExtension "jar").toList map (_.toURL)
  // lazy val scalaCheck: SFile = {
  //   SFile(testRoot / "files" / "lib" / "scalacheck.jar")
  // }
    // findJar(buildPackLibDir.files ++ srcLibDir.files, "scalacheck") getOrElse {
    //   sys.error("No scalacheck jar found in '%s' or '%s'".format(buildPackLibDir, srcLibDir))
    // }
}
