package org.mcvly.tags

import java.nio.ByteBuffer
import java.nio.channels.FileChannel
import java.nio.charset.StandardCharsets

/**
 * @author mcvly
 * @since 13.04.15
 */

class ID3v1Parser(private val fileChannel: FileChannel) {

  def parseTags = parseV1Tag(readV1Tag)

  private def readV1Tag: Array[Byte] = {
    val byteBuffer = ByteBuffer.allocate(128)
    fileChannel.position(fileChannel.size() - 128)
    fileChannel.read(byteBuffer)
    byteBuffer.flip()
    byteBuffer.array()
  }

  private def parseV1Tag(bytes: Array[Byte]): Tags = {
    val track = new String(bytes.slice(3, 33), StandardCharsets.UTF_8).trim
    val artist = new String(bytes.slice(33, 63), StandardCharsets.UTF_8).trim
    val album = new String(bytes.slice(63, 93), StandardCharsets.UTF_8).trim
    //    val year = new String(bytes.slice(93, 97), StandardCharsets.UTF_8)
    Tags(if (track.isEmpty) None else Some(track),
      if (artist.isEmpty) None else Some(artist),
      if (album.isEmpty) None else Some(album))
  }
}
