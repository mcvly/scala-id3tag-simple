package org.mcvly.tags

import java.nio.ByteBuffer
import java.nio.channels.FileChannel
import java.nio.charset.{StandardCharsets, Charset}

import scala.collection.mutable

/**
 * @author mcvly
 * @since 13.04.15
 */
case class ID3v2Header(version: Int, length: Int, extHeader: Boolean) {
  def frameHeaderSize = if (isV2Tag) 6 else 10
  def frameComponentSize = if (isV2Tag) 3 else 4
  def isV2Tag = version < 3

}

class ID3v2Parser(private val fileChannel: FileChannel, private val header: ID3v2Header) {
  val regularCharset = Charset.forName(StandardCharsets.ISO_8859_1.name())

  def parseTags: Tags = {
    if (header.extHeader) readExtHeader(fileChannel)
    val trackTags = tags(fileChannel, header, 0)
    val map = new mutable.HashMap[String, String]()

    trackTags.foreach(r => r._1 match {
      case "TIT2" | "TIT" => map.put("title", r._2.tagValue)
      case "TPE1" | "TPE" => map.put("artist", r._2.tagValue)
      case "TALB" | "TAL" => map.put("album", r._2.tagValue)
      case _ => ()
    })

    Tags(map.get("title"), map.get("artist"), map.get("album"))
  }

  private def tags(fileChannel: FileChannel, header: ID3v2Header, tagRead: Int): Seq[(String, LazyTagValue)] = {
    var byteBuffer = ByteBuffer.allocate(header.frameComponentSize)
    val n = fileChannel.read(byteBuffer)
    // if EOF or PADDING or MPEG data or whole tag's been read
    if (n == -1 || byteBuffer.get(0) == 0x00 || byteBuffer.get(0) == 0xFF || tagRead >= header.length) Nil
    else {
      byteBuffer.position(0)
      val tagName = regularCharset.decode(byteBuffer).toString
      byteBuffer = ByteBuffer.allocate(header.frameComponentSize)
      fileChannel.read(byteBuffer)
      byteBuffer.position(0)
      val tagValLength = frameLength(byteBuffer, header)
      val tagValPosition = fileChannel.position + 2 // 2 bytes for flags
      fileChannel.position(tagValPosition + tagValLength)
      (tagName, LazyTagValue(fileChannel, tagValPosition, tagValLength)) +:
        tags(fileChannel, header, tagRead + header.frameHeaderSize + tagValLength)
    }
  }

  /* read frame length (depends on tag version) */
  private def frameLength(byteBuffer: ByteBuffer, iD3v2Header: ID3v2Header): Int = {
    val undecodedSize = byteBuffer.getInt
    if (iD3v2Header.version == 4) IntUtils.unsync(undecodedSize) else undecodedSize
  }

  private def readExtHeader(fileChannel: FileChannel) = {
    val startPos = fileChannel.position()
    val extHeader = ByteBuffer.allocate(4)
    fileChannel.read(extHeader)
    val extHeaderSize = IntUtils.unsync(extHeader.getInt)
    fileChannel.position(startPos + 4 + extHeaderSize)
  }

}

private case class LazyTagValue(fileChannel: FileChannel, private val tagPosition: Long, private val tagLength: Int) {
  lazy val tagValue: String = {
    val valueBuffer = ByteBuffer.allocate(tagLength)
    fileChannel.position(tagPosition)
    fileChannel.read(valueBuffer)
    val charset =
      if (valueBuffer.get(0) == 0) Charset.forName("CP1251")
      else if (valueBuffer.get(0) == 3) Charset.forName(StandardCharsets.UTF_8.name())
      else Charset.forName(StandardCharsets.UTF_16.name())
    valueBuffer.position(1)
    charset.decode(valueBuffer).toString.trim
  }
}
