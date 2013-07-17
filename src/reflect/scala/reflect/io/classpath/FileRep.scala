package scala
package reflect
package io
package classpath

// final case class FileRep(file: jFile) extends AnyVal {
final case class FileRep(path: String) extends AnyVal {
  def file = classpath.file(path)
  def / (sub: String): FileRep = fileRep(classpath.file(file, sub))

  def filename       = file.getName
  def isDir          = file.isDirectory
  def isReadableFile = !isDir && file.canRead
  def isClass        = (extension equalsIgnoreCase "class")
  def isJavaOrScala  = (extension equalsIgnoreCase "scala") || (extension equalsIgnoreCase "java")
  def isJarOrZip     = (extension equalsIgnoreCase "jar") || (extension equalsIgnoreCase "zip")
  def uri            = file.toURI
  def name           = stripExtension(filename)
  def extension      = extensionOf(filename)
  def packageName    = packageOf(path)
  def className      = slashesToDots(path, path.length - 6)
  def isEmpty        = path == ""

  def fileLength: Int   = file.length.toInt
  def fileInputStream() = new BufferedInputStream(new FileInputStream(file))

  def foreachZipEntry(f: ZipEntry => Unit): Unit = {
    def traverse(entries: java.util.Enumeration[_ <: ZipEntry]) {
      while (entries.hasMoreElements)
        f(entries.nextElement)
    }
    val jar = new JarFile(file)
    try traverse(jar.entries) finally jar.close()
  }

  def contents: Seq[FileRep] = file.listFiles match {
    case null => Nil
    case xs   => xs.toSeq map fileRep
  }
  def containersIn: Seq[FileRep] = contents filter (_.isJarOrZip)
  override def toString = path
}
