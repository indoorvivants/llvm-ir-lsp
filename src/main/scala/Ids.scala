package llvm_lsp

import java.util.Base64
import java.util.SplittableRandom

object Ids {
  private val rng = new SplittableRandom()

  def randomId(): String = {
    val bytes = new Array[Byte](8)

    var i = 0
    while (i < bytes.length) {
      val x = rng.nextLong()
      var j = 0
      while (j < 8 && i < bytes.length) {
        bytes(i) = (x >>> (j * 8)).toByte
        i += 1
        j += 1
      }
    }

    Base64.getUrlEncoder.withoutPadding.encodeToString(bytes)
  }
}
