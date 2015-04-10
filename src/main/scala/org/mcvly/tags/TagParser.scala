package org.mcvly.tags

import java.nio.ByteBuffer
import java.nio.channels.FileChannel
import java.nio.file.{Path, StandardOpenOption}
import resource._

/**
 * @author mcvly
 * @since 09.04.15
 */
case class Tags(artist: Option[String], album: Option[String], title: Option[String], fileName: String)
class TagParser(val file: Path) {
  def readTags: Tags = {
    val header = managed(FileChannel.open(file, StandardOpenOption.READ)) map { fc =>
      val buffer = ByteBuffer.allocate(10)
      var nread = 0
      do {
        nread = fc.read(buffer)
      } while (nread != -1 && buffer.hasRemaining)
      buffer
    }
  }
}