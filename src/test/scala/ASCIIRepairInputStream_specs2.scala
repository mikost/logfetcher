package name.mikkoostlund.utils.logfetcher

import org.specs2._
import org.junit.runner.RunWith
import org.specs2.runner.JUnitRunner
import java.io.ByteArrayInputStream
import java.io.InputStream
import org.specs2.mutable.Specification
import org.specs2.specification.Scope

@RunWith(classOf[JUnitRunner])
class Spec2Test extends Specification {

  trait TestData extends Scope {
    val brokenASCIIBytes = {
      "ABCD".getBytes.
        updated(1, 127.toByte).   // insert non-ASCII
        updated(2, 255.toByte)    // insert non-ASCII
    }
  
    val brokenInputStream: InputStream = new ByteArrayInputStream(brokenASCIIBytes)
    val repairValue: Byte = '$'
  }

  "An ASCIIRepairFilterInputStream" should {
    "replace non-ASCII chars with dollar-signs" in new TestData {
      val repairedStream = new ASCIIRepairFilterInputStream(brokenInputStream, repairValue)

      repairedStream.read mustEqual 'A'
      repairedStream.read mustEqual repairValue
      repairedStream.read mustEqual repairValue
      repairedStream.read mustEqual 'D'
      repairedStream.read mustEqual -1   // End-of-file
      success
    }

    "replace non-ASCII chars with dollar-signs" in new TestData {
      val repairedStream = new ASCIIRepairFilterInputStream(brokenInputStream, repairValue)
      val repairedASCIIBytes = Array.ofDim[Byte](4)
      repairedStream.read(repairedASCIIBytes, 0, 4) mustEqual 4
      repairedASCIIBytes mustEqual Array[Byte]('A', repairValue, repairValue, 'D')
    }
  }  
}