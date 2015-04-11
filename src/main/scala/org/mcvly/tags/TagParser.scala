package org.mcvly.tags

import java.nio.ByteBuffer
import java.nio.channels.FileChannel
import java.nio.charset.StandardCharsets
import java.nio.file.{Paths, Path, StandardOpenOption}
import resource._

/**
 * @author mcvly
 * @since 09.04.15
 */
case class Tag(title: Option[String], artist: Option[String], album: Option[String], year: Option[Int], fileName: String)
class TagParser(val file: String) {

  def readTags: Option[Tag] = {
    managed(FileChannel.open(Paths.get(file), StandardOpenOption.READ)) map { fc =>
      parseHeader(fc) match {
        case Some((version, length)) => parseV2Tag(fc, version, length)
        case None => parseV1Tag(readV1Tag(fc))
      }
    }
  }.opt

  def parseHeader(fileChannel: FileChannel): Option[(Int, Int)] = {
    val buffer = ByteBuffer.allocate(10)
    fileChannel.read(buffer)
    buffer.flip()
    val headerBytes = buffer.array()
    if (headerBytes(0) == 'I' && headerBytes(1) == 'D' && headerBytes(2) == '3') Some(headerBytes(3).toInt, length(headerBytes))
    else None
  }

  def length(headerbuf: Array[Byte]) = {
    (headerbuf(9) & 0xFF) | ((headerbuf(8) & 0xFF) << 7 ) | ((headerbuf(7) & 0xFF) << 14 ) | ((headerbuf(6) & 0xFF) << 21 ) + 10
  }

  def readV1Tag(channel: FileChannel): Array[Byte] = {
    val byteBuffer = ByteBuffer.allocate(128)
    channel.position(channel.size() - 128)
    channel.read(byteBuffer)
    byteBuffer.flip()
    byteBuffer.array()
  }

  def parseV1Tag(bytes: Array[Byte]): Tag = {
    val track = new String(bytes.slice(3, 33), StandardCharsets.UTF_8).trim
    val artist = new String(bytes.slice(33, 63), StandardCharsets.UTF_8).trim
    val album = new String(bytes.slice(63, 93), StandardCharsets.UTF_8).trim
    val year = new String(bytes.slice(93, 97), StandardCharsets.UTF_8)
    Tag(if (track.isEmpty) None else Some(track),
        if (artist.isEmpty) None else Some(artist),
        if (album.isEmpty) None else Some(album),
        if (year.isEmpty) None else Some(year.toInt),
        file)
  }

  def parseV2Tag(fileChannel: FileChannel, version: Int, length: Int): Tag = {
    ???
  }

}
