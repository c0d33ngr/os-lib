package os

import java.io.{InputStream, OutputStream}
import java.net.URI
import java.nio.file._
import java.nio.file.attribute._
import java.util.zip.{ZipEntry, ZipInputStream, ZipOutputStream}
import scala.collection.JavaConverters._
import scala.util.matching.Regex

object zip {

  /**
   * Opens a zip file as a filesystem root that you can operate on using `os.*` APIs.
   * Note that you need to call `close()` on the returned `ZipRoot` when you are done with it,
   * to avoid leaking filesystem resources.
   */
  def open(path: Path): ZipRoot = {
    val uri = new URI("jar", path.wrapped.toUri.toString, null)
    val env = Map("create" -> "true").asJava
    new ZipRoot(FileSystems.newFileSystem(uri, env))
  }

  /**
   * Zips the provided list of files and directories into a single ZIP archive.
   *
   * If `dest` already exists and is a zip, performs modifications to `dest` in place
   * rather than creating a new zip.
   *
   * @param dest             The path to the destination ZIP file.
   * @param sources          A list of paths to files and directories to be zipped.
   * @param excludePatterns  A list of regular expression patterns to exclude files from the ZIP archive.
   * @param includePatterns  A list of regular expression patterns to include files in the ZIP archive.
   * @param preserveMtimes   Whether to preserve modification times (mtimes) of the files.
   * @param deletePatterns   A list of regular expression patterns to delete files from an existing ZIP archive before appending new ones.
   * @param compressionLevel Compression level from 0-9, where 0 is no compression and 9 is best compression. Defaults to -1 (default compression).
   * @return The path to the created ZIP archive.
   */
  def apply(
      dest: os.Path,
      sources: Seq[ZipSource] = List(),
      excludePatterns: Seq[Regex] = List(),
      includePatterns: Seq[Regex] = List(),
      preserveMtimes: Boolean = false,
      deletePatterns: Seq[Regex] = List(),
      compressionLevel: Int = java.util.zip.Deflater.DEFAULT_COMPRESSION
  ): os.Path = {
    checker.value.onWrite(dest)
    sources.foreach(source => checker.value.onRead(source.src))

    if (os.exists(dest)) {
      val opened = open(dest)
      try {
        // Delete files matching deletePatterns
        os.walk(opened)
          .filter(path => anyPatternsMatch(path.relativeTo(opened).toString, deletePatterns))
          .foreach(os.remove.all)

        // Add new files
        createNewZip(
          sources,
          excludePatterns,
          includePatterns,
          preserveMtimes,
          compressionLevel,
          opened
        )
      } finally {
        opened.close()
      }
    } else {
      val out = Files.newOutputStream(dest.toNIO)
      try {
        val zipOut = new ZipOutputStream(out)
        zipOut.setLevel(compressionLevel)
        createNewZip(
          sources,
          excludePatterns,
          includePatterns,
          preserveMtimes,
          zipOut
        )
        zipOut.close()
      } finally {
        out.close()
      }
    }
    dest
  }

  private def createNewZip(
      sources: Seq[ZipSource],
      excludePatterns: Seq[Regex],
      includePatterns: Seq[Regex],
      preserveMtimes: Boolean,
      compressionLevel: Int,
      zipFs: FileSystem
  ): Unit = {
    sources.foreach { source =>
      if (os.isDir(source.src)) {
        os.walk(source.src)
          .filter(path => os.isFile(path) && shouldInclude(path.toString, excludePatterns, includePatterns))
          .foreach { path =>
            val destPath = zipFs.getPath(source.dest.getOrElse(os.sub) / path.subRelativeTo(source.src)).toString
            Files.copy(path.toNIO, zipFs.getPath(destPath), StandardCopyOption.REPLACE_EXISTING)
            if (preserveMtimes) {
              val destFile = zipFs.getPath(destPath)
              val srcTime = Files.getLastModifiedTime(path.toNIO)
              Files.setLastModifiedTime(destFile, srcTime)
            }
            // Preserve POSIX permissions
            if (Files.getFileAttributeView(path.toNIO, classOf[PosixFileAttributeView]) != null) {
              val permissions = Files.getPosixFilePermissions(path.toNIO)
              Files.setPosixFilePermissions(destFile, permissions)
            }
            // Preserve symbolic links
            if (Files.isSymbolicLink(path.toNIO)) {
              val target = Files.readSymbolicLink(path.toNIO)
              Files.createSymbolicLink(destFile, target)
            }
          }
      } else if (shouldInclude(source.src.last, excludePatterns, includePatterns)) {
        val destPath = zipFs.getPath(source.dest.getOrElse(os.sub / source.src.last)).toString
        Files.copy(source.src.toNIO, zipFs.getPath(destPath), StandardCopyOption.REPLACE_EXISTING)
        if (preserveMtimes) {
          val destFile = zipFs.getPath(destPath)
          val srcTime = Files.getLastModifiedTime(source.src.toNIO)
          Files.setLastModifiedTime(destFile, srcTime)
        }
        // Preserve POSIX permissions
        if (Files.getFileAttributeView(source.src.toNIO, classOf[PosixFileAttributeView]) != null) {
          val permissions = Files.getPosixFilePermissions(source.src.toNIO)
          Files.setPosixFilePermissions(destFile, permissions)
        }
        // Preserve symbolic links
        if (Files.isSymbolicLink(source.src.toNIO)) {
          val target = Files.readSymbolicLink(source.src.toNIO)
          Files.createSymbolicLink(destFile, target)
        }
      }
    }
  }

  private def createNewZip(
      sources: Seq[ZipSource],
      excludePatterns: Seq[Regex],
      includePatterns: Seq[Regex],
      preserveMtimes: Boolean,
      zipOut: ZipOutputStream
  ): Unit = {
    sources.foreach { source =>
      if (os.isDir(source.src)) {
        os.walk(source.src)
          .filter(path => os.isFile(path) && shouldInclude(path.toString, excludePatterns, includePatterns))
          .foreach { path =>
            makeZipEntry(path, source.dest.getOrElse(os.sub) / path.subRelativeTo(source.src), preserveMtimes, zipOut)
          }
      } else if (shouldInclude(source.src.last, excludePatterns, includePatterns)) {
        makeZipEntry(source.src, source.dest.getOrElse(os.sub / source.src.last), preserveMtimes, zipOut)
      }
    }
  }

  private def makeZipEntry(
      file: os.Path,
      sub: os.SubPath,
      preserveMtimes: Boolean,
      zipOut: ZipOutputStream
  ): Unit = {
    val zipEntry = new ZipEntry(sub.toString)
    if (preserveMtimes) {
      zipEntry.setTime(Files.getLastModifiedTime(file.toNIO).toMillis)
    }
    zipOut.putNextEntry(zipEntry)
    val fis = os.read.inputStream(file)
    try {
      os.Internals.transfer(fis, zipOut, close = false)
    } finally {
      fis.close()
    }
    zipOut.closeEntry()
  }

  private def anyPatternsMatch(fileName: String, patterns: Seq[Regex]): Boolean = {
    patterns.exists(_.findFirstIn(fileName).isDefined)
  }

  private def shouldInclude(
      fileName: String,
      excludePatterns: Seq[Regex],
      includePatterns: Seq[Regex]
  ): Boolean = {
    val isExcluded = anyPatternsMatch(fileName, excludePatterns)
    val isIncluded = includePatterns.isEmpty || anyPatternsMatch(fileName, includePatterns)
    !isExcluded && isIncluded
  }

  /**
   * A filesystem root representing a zip file.
   */
  class ZipRoot private[os] (fs: FileSystem) extends Path(fs.getRootDirectories.iterator().next())
      with AutoCloseable {
    def close(): Unit = fs.close()
  }

  /**
   * A file or folder you want to include in a zip file.
   */
  class ZipSource private[os] (val src: os.Path, val dest: Option[os.SubPath])
  object ZipSource {
    implicit def fromPath(src: os.Path): ZipSource = new ZipSource(src, None)
    implicit def fromSeqPath(srcs: Seq[os.Path]): Seq[ZipSource] = srcs.map(fromPath)
    implicit def fromPathTuple(tuple: (os.Path, os.SubPath)): ZipSource =
      new ZipSource(tuple._1, Some(tuple._2))
  }
}

object unzip {

  /**
   * Lists the contents of the given zip file without extracting it.
   */
  def list(
      source: os.Path,
      excludePatterns: Seq[Regex] = List(),
      includePatterns: Seq[Regex] = List()
  ): Generator[os.SubPath] = {
    for {
      (zipEntry, zipInputStream) <- streamRaw(os.read.stream(source), excludePatterns, includePatterns)
    } yield os.SubPath(zipEntry.getName)
  }

  /**
   * Extract the given zip file into the destination directory.
   *
   * @param source          An `os.Path` containing a zip file.
   * @param dest            The path to the destination directory for extracted files.
   * @param excludePatterns A list of regular expression patterns to exclude files during extraction.
   */
  def apply(
      source: os.Path,
      dest: os.Path,
      excludePatterns: Seq[Regex] = List(),
      includePatterns: Seq[Regex] = List()
  ): os.Path = {
    stream(os.read.stream(source), dest, excludePatterns, includePatterns)
    dest
  }

  /**
   * Unzips a ZIP data stream represented by a geny.Readable and extracts it to a destination directory.
   *
   * @param source          A geny.Readable object representing the ZIP data stream.
   * @param dest            The path to the destination directory for extracted files.
   * @param excludePatterns A list of regular expression patterns to exclude files during extraction.
   */
  def stream(
      source: geny.Readable,
      dest: os.Path,
      excludePatterns: Seq[Regex] = List(),
      includePatterns: Seq[Regex] = List()
  ): Unit = {
    checker.value.onWrite(dest)
    for ((zipEntry, zipInputStream) <- streamRaw(source, excludePatterns, includePatterns)) {
      val newFile = dest / os.SubPath(zipEntry.getName)
      if (zipEntry.isDirectory) os.makeDir.all(newFile)
      else {
        val outputStream = os.write.outputStream(newFile, createFolders = true)
        os.Internals.transfer(zipInputStream, outputStream, close = false)
        outputStream.close()
        // Preserve POSIX permissions
        if (Files.getFileAttributeView(newFile.toNIO, classOf[PosixFileAttributeView]) != null) {
          val permissions = PosixFilePermissions.fromString(zipEntry.getExtra.toString)
          Files.setPosixFilePermissions(newFile.toNIO, permissions)
        }
        // Preserve symbolic links
        if (zipEntry.getExtra != null && zipEntry.getExtra.toString.startsWith("SYMLINK:")) {
          val target = Paths.get(zipEntry.getExtra.toString.substring(8))
          Files.createSymbolicLink(newFile.toNIO, target)
        }
      }
    }
  }

  /**
   * Low-level API that streams the contents of the given zip file.
   */
  def streamRaw(
      source: geny.Readable,
      excludePatterns: Seq[Regex] = List(),
      includePatterns: Seq[Regex] = List()
  ): geny.Generator[(ZipEntry, java.io.InputStream)] = {
    new Generator[(ZipEntry, java.io.InputStream)] {
      override def generate(handleItem: ((ZipEntry, java.io.InputStream)) => Generator.Action)
          : Generator.Action = {
        var lastAction: Generator.Action = Generator.Continue
        source.readBytesThrough { inputStream =>
          val zipInputStream = new ZipInputStream(inputStream)
          try {
            var zipEntry: ZipEntry = zipInputStream.getNextEntry
            while (lastAction == Generator.Continue && zipEntry != null) {
              if (os.zip.shouldInclude(zipEntry.getName, excludePatterns, includePatterns)) {
                lastAction = handleItem((zipEntry, zipInputStream))
              }
              zipEntry = zipInputStream.getNextEntry
            }
          } finally {
            zipInputStream.closeEntry()
            zipInputStream.close()
          }
        }
        lastAction
      }
    }
  }
}
