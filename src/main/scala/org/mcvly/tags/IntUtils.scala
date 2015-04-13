package org.mcvly.tags

/**
 * @author mcvly
 * @since 13.04.15
 */
object IntUtils {
  /* Convert synchronization safe integer into normal integer */
  def unsync(x: Int): Int = {
    var x_final = 0x00
    val a = x & 0xff
    val b = (x >> 8) & 0xff
    val c = (x >> 16) & 0xff
    val d = (x >> 24) & 0xff

    x_final = x_final | a
    x_final = x_final | (b << 7)
    x_final = x_final | (c << 14)
    x_final = x_final | (d << 21)
    x_final
  }
}
