package org.mcvly.tags

import java.nio.file.{Paths, Path}
import java.io.File

/**
 * @author mcvly
 * @since 08.04.15
 */
object Main {
  def main(args: Array[String]) = {
    if (args.length != 1) throw new Error("wrong arguments")
    countExecutionTime(processFiles)(args(0))
  }

  def countExecutionTime(f: (String) => Unit)(s: String) = {
    val start = System.currentTimeMillis()
    f(s)
    val stop = System.currentTimeMillis()
    println(s"Execution took ${stop - start} millis")
  }

  def processFiles(dirPath: String) = {
    val mp3Files = getFileTree(new File(dirPath)).filter(_.toLowerCase.endsWith(".mp3"))
    mp3Files.foreach(workWithTag)
  }

  def getFileTree(f: File): Stream[String] =
    f.getAbsolutePath #:: (if (f.isDirectory) f.listFiles().toStream.flatMap(getFileTree) else Stream.empty)

  def workWithTag(filePath: String) = {
    val parser = new TagParser(filePath)
    val tag = parser.readTags
    ///    val id3Tag = mp3File.getId3v2Tag
    //    println(s"${id3Tag.getArtist} - ${id3Tag.getTitle}")
    println(tag)
  }
}
