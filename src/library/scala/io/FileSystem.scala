package scala
package io

import java.nio.file.{ FileSystem => JFileSystem, Path => JPath, _ }
import java.nio.file.spi._ // FileSystemProvider

object FileSystem {
  def providers                           = FileSystemProvider.installedProviders
  def default                             = FileSystems.getDefault()
  def get(uri: java.net.URI): JFileSystem = FileSystems.getFileSystem(uri)
}
