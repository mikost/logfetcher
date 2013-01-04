package name.mikkoostlund.utils.logfetcher

import java.io.FilterInputStream
import java.io.InputStream
import java.nio.ByteBuffer

class ASCIIRepairFilterInputStream(in: InputStream, repairValue: Byte) extends FilterInputStream(in) {
	require(repairValue > 0)
	var buffer: ByteBuffer = ByteBuffer.allocateDirect(0)

	override def read: Int = {
		val retval = in.read
		if (retval == -1) 
			return -1
		if (retval > 126)
			return repairValue
		retval
	}

	override def read(b: Array[Byte], off: Int, len: Int) = {
		  val retval = in.read(b, off, len)
		  for (off2 <- off until off+len) {
		    val byteVal = b(off2)
		    if (byteVal < 0 || byteVal == 127)
		    	b(off2) = repairValue
		  }
		  retval
	}
}