package org.mcvly.tags

import java.nio.ByteBuffer
import java.nio.channels.FileChannel
import java.nio.charset.{Charset, StandardCharsets}
import java.nio.file.{Paths, StandardOpenOption}
import resource._

/**
 * @author mcvly
 * @since 09.04.15
 */
class TagParser(val file: String) {

  def readTags: Tags = {
    managed(FileChannel.open(Paths.get(file), StandardOpenOption.READ)) map { fc =>
      parseHeader(fc) match {
        case Some(header) => new ID3v2Parser(fc, header).parseTags
        case None => new ID3v1Parser(fc).parseTags
      }
    }
  }.opt match {
    case Some(tag) => tag
    case _ => Tags(None, None, None)
  }

  private def parseHeader(fileChannel: FileChannel): Option[ID3v2Header] = {
    val buffer = ByteBuffer.allocate(10)
    fileChannel.read(buffer)
    if (id3v2Header(buffer)) Some(ID3v2Header(buffer.get(3), headerLength(buffer), (buffer.get(5) & 0x40) != 0))
    else None
  }

  private def id3v2Header(buffer: ByteBuffer): Boolean = {
    buffer.get(0) == 'I' && buffer.get(1) == 'D' && buffer.get(2) == '3'
  }

  /* read tag length (7-10) bit with ByteBuffer and unsynchronize it */
  private def headerLength(headerBuffer: ByteBuffer) = {
    headerBuffer.position(6)
    IntUtils.unsync(headerBuffer.getInt)
  }

}
