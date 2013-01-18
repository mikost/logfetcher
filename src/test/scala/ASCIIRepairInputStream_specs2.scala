import org.specs2.mutable._
import name.mikkoostlund.utils.logfetcher.ASCIIRepairFilterInputStream
import java.io.InputStream
import org.specs2.matcher.MatchResult
import java.io.ByteArrayInputStream

class Spec2Test extends Specification {

  val brokenASCIIBytes = {
    "ABCD".getBytes.
      updated(1, 127.toByte).   // insert non-ASCII
      updated(2, 255.toByte)    // insert non-ASCII
  }

  "An ASCIIRepairFilterInputStream" should {
    "replace non-ASCII chars with dollar-signs" in {

      val brokenInputStream = new ByteArrayInputStream(brokenASCIIBytes)
      val repairValue: Byte = '$'
      val repairedStream = new ASCIIRepairFilterInputStream(brokenInputStream, repairValue)

      repairedStream.read mustEqual 'A'
      repairedStream.read mustEqual '$'
      repairedStream.read mustEqual '$'
      repairedStream.read mustEqual 'D'
      repairedStream.read mustEqual -1   // End-of-file
    }

    "replace non-ASCII chars with dollar-signs" in {

      val brokenInputStream = new ByteArrayInputStream(brokenASCIIBytes)
      val repairValue: Byte = '$'
      val repairedStream = new ASCIIRepairFilterInputStream(brokenInputStream, repairValue)
      val repairedASCIIBytes = Array.ofDim[Byte](4)
      repairedStream.read(repairedASCIIBytes, 0, 4)
      repairedASCIIBytes mustEqual Array[Byte]('A','$','$','D')
    }
  }
  
}