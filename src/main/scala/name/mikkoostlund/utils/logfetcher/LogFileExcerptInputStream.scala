package name.mikkoostlund.utils.logfetcher

import java.io.InputStream
import java.io.BufferedReader
import java.io.InputStreamReader
import java.nio.ByteBuffer
import scala.io.Codec
import java.util.GregorianCalendar
import java.io.File
import java.util.Calendar
import scala.collection.mutable.Buffer
import java.io.FileInputStream
import java.io.IOException
import java.io.Closeable
import java.io.EOFException
import scala.math.min
import java.io.BufferedInputStream
import java.util.zip.ZipOutputStream
import java.util.zip.ZipEntry
import java.io.FileNotFoundException

class LogFileExcerptInputStream(val logFile: LogFile, excerptStrategy: ExcerptStrategy) extends InputStream {
	require(logFile != null)
	require(excerptStrategy != null)

	override
	def available() = {
		if (isClosed)
			throw new IOException("available called on closed LogFileExcerptInputStream")
		buffer.remaining()
	}

	private
	def isClosed = myState == State.EXCERPT_STREAM_CLOSED

	override
	def close = {
		if (reader != null) {
			reader.close;
			reader = null
		}
		myState = State.EXCERPT_STREAM_CLOSED
		reader = null
		buffer = null
	}

	override
	def read: Int = {
		if (isClosed)
			throw new IOException("read called on closed LogFileExcerptInputStream")

		if (buffer.remaining == 0) {
			try {
			  fill()
			}
			catch {
			  	case ioe: EOFException => return -1
			}
		}

		val byte = buffer.get()
		if (byte < 0)
			byte + 256
		else
			byte
  	}

	private
	def internalRead(accBytes: Int, b: Array[Byte], off: Int, len: Int): Int = {
		if (len == 0)
			return accBytes

		if (buffer.remaining == 0) {
			try {
				fill()
			}
			catch {
			  	case _: EOFException => {
					if (accBytes == 0) return -1
					else return accBytes
				}
			  	case ioe: IOException => {
					if (accBytes == 0) throw new IOException(ioe)
					else return accBytes
			  	}
			}
		}

		val nBytes = min(buffer.remaining, len)
		buffer.get(b, off, nBytes)

		internalRead(accBytes + nBytes, b, off + nBytes, len - nBytes)
	}

	override
	def read(b: Array[Byte], off: Int, len: Int): Int = {
		if (isClosed)
			throw new IOException("read called on closed LogFileExcerptInputStream")

		if (b == null)
		  throw new NullPointerException

		if (off < 0 || len < 0 || len > b.length - off)
			throw new IndexOutOfBoundsException("off = "+ off +" (must be off >=0 ), len = "+ len +" (must be 0 <= len <= b.length - off = "+ (b.length - off))

		return internalRead(0, b, off, len)
	}

	def state = myState

	def state_=(aState: State) = {
		require(State.values.indexOf(aState) > State.values.indexOf(myState))
		myState = aState
	}

	def stateIs(aState: State) = state == aState

	private
	def fill(): Unit = {
		var currentLine: String = null			
		do {
			if (reader == null) {
				reader = nextReader match {
				  	case Some(someReader) => someReader
				  	case None => throw new EOFException
				}
			}

			currentLine = reader.readLine()
			if (currentLine == null) {
				reader.close()
				reader = null
			}
			else if (!excerptStrategy.acceptLine(this, currentLine)) {
				currentLine = null
			}
		} while (currentLine == null);
		buffer = java.nio.ByteBuffer wrap { Codec.toUTF8(currentLine + EOL) }
	}

	private[this]
	def wrapWithBufferedInputStreamReader(createInputStream: => InputStream) = {
		val is: InputStream = createInputStream
		var resource: Closeable = is;
		val bufferedReader = {
			try {
				val isr = new InputStreamReader(is)
				resource = isr
				new BufferedReader(isr)
			}
			catch {
				case e: Throwable => resource.close(); throw e
			}
		}
		bufferedReader
	}

	private[this]
	val parts = excerptStrategy.getLogFileParts(this).iterator
	
	var currentPart:LogFilePart = null

	private[this]
	def nextReader: Option[BufferedReader] = {
		if (parts.hasNext) {
			currentPart = parts.next
			Some(wrapWithBufferedInputStreamReader { currentPart.createInputStream })
		}
		else
			None
	}

	private[this]
	var	reader: BufferedReader = nextReader match {
		case Some(theNext) => theNext
		case None          => null
	}

	private[this]
	var	buffer: ByteBuffer = ByteBuffer.allocate(0)

	private val EOL = System.getProperty( "line.separator" );

	sealed trait State extends State.Value
	object State extends Enum[State] {
		case object EXCERPT_NOT_YET_STARTED extends State
		case object EXCERPT_STARTED extends State
		case object EXCERPT_FINISHED extends State
		case object EXCERPT_STREAM_CLOSED extends State
	}

	private var myState: State = State.EXCERPT_NOT_YET_STARTED
}

trait LogFile extends LogFilePartsSelector with LineTimeStampExtractor {
	def name: String

	@throws(classOf[FileNotFoundException])
	def writeToZipOutputStream(out: ZipOutputStream, excerptStrategy: ExcerptStrategy)  = {

   		def newBufferedInputStream(createInputStream: => InputStream): BufferedInputStream = {
    	  	val inputStream = createInputStream
    		try {
    			new BufferedInputStream(inputStream)
    		}
    		catch {
    			case t: Throwable => inputStream.close(); throw t
    		}
		}

   	  	val in = newBufferedInputStream { new LogFileExcerptInputStream(this, excerptStrategy) }
    	try {
   			out.putNextEntry(new ZipEntry(name));

   			val BUFFER_SIZE = 2048
   			val data = Array.ofDim[Byte](BUFFER_SIZE);

   			var count = 0
   			while({count = in.read(data, 0, BUFFER_SIZE); count} != -1) {
   				out.write(data, 0, count);
   			}
   		}
   		finally {
   			in.close();
   		}
	}
}

trait LogFilePartsSelector {
	def parts: Seq[LogFilePart]
}

trait LogFilePart {
	def timePeriod: Option[TimePeriod]
	def createInputStream(): InputStream
	def name: String
}

trait LineTimeStampExtractor {
	def timeStampOfLine(line: String, logFilePart: LogFilePart, prevTimeStamp: Option[Calendar]): Option[Calendar]
}

trait YYYY_MM_DD_SuffixedFiles_LogFilePartsSelector extends LogFilePartsSelector { logFile: FileLogFile =>
	val priv_separatorInFilenameTimeStamp_YYYY_MM: String
	val priv_separatorInFilenameTimeStamp_MM_DD: String



	override
	def parts: Seq[LogFilePart] = {
		val these = Buffer.empty[LogFilePart]

		filesInDir.getOrElse(Array.empty[File]).foreach { file =>
			val filenamePattern = ("^" + logFile.name +
					"(\\.(20[0-9][0-9])"+ priv_separatorInFilenameTimeStamp_YYYY_MM +
					"(0[1-9]|1[0-2])"+         priv_separatorInFilenameTimeStamp_MM_DD +
					"([0-2][0-9]|3[0-1]))?$").r

			file.getName match {
			  	case filenamePattern(ext, year, month, day) => {
			  		these += new LogFilePart {
			  			override lazy val timePeriod = {
			  				if (ext==null)
			  					None
			  				else
			  					Some(TimePeriod.oneDay(year.toInt, month.toInt - 1, day.toInt))
			  			}
			  			override def createInputStream() = new FileInputStream(file)
			  			override def name = file.getName
			  		}
				}
			  	case _ => ()
			}
		}

		these.sortWith{ (f1, f2) =>
			f1.timePeriod match {
			  	case None => true 
				case Some(timePeriod1) => f2.timePeriod match {
				  	case None => false
				  	case Some (timePeriod2) => {
				  		timePeriod1.startTime.compareTo(timePeriod2.startTime) > 0
				  	}
				}
			}
		}
	}
}

trait YYYY_MM_DD_HH_MM_SS_NN_LineTimeStampExtractor extends LineTimeStampExtractor {
	val priv_separatorInLineTimeStamp_YYYY_MM: String
	val priv_separatorInLineTimeStamp_MM_DD: String
	val priv_separatorInLineTimeStamp_DD_HH: String
	val priv_separatorInLineTimeStamp_HH_MM: String
	val priv_separatorInLineTimeStamp_MM_SS: String
	val priv_separatorInLineTimeStamp_SS_NN: String

	override
	def timeStampOfLine(line: String, logFilePart: LogFilePart, prevTimeStamp: Option[Calendar]): Option[Calendar] = {
		val LogLine = ("^(20[0-9][0-9])"+ priv_separatorInLineTimeStamp_YYYY_MM +
		    "(0[1-9]|1[0-2])"+                 priv_separatorInLineTimeStamp_MM_DD +
		    "(0[1-9]|[12][0-9]|3[01])"+           priv_separatorInLineTimeStamp_DD_HH +
		    "([01][0-9]|2[0-3])"+                    priv_separatorInLineTimeStamp_HH_MM +
		    "([0-5][0-9])"+                             priv_separatorInLineTimeStamp_MM_SS +     
		    "([0-5][0-9])"+                                priv_separatorInLineTimeStamp_SS_NN +
		    "([0-9]{3}) .*").r

		line match {
			case LogLine(year, month, day, hour, minute, second, millisecond) => {
				val date = new GregorianCalendar(year.toInt, month.toInt-1, day.toInt, hour.toInt, minute.toInt, second.toInt)
				date.set(Calendar.MILLISECOND, millisecond.toInt)
				Some(date)
			}
			case _  => None
		}
	}
}

abstract class FileLogFile(val file: File) extends LogFile {
	require(file != null)

	lazy val dir = file.getParentFile()
	
	lazy val filesInDir = Option(dir.listFiles())

	override
	def name = file.getName()
}

trait ExcerptStrategy {
	def getLogFileParts(inputStream: LogFileExcerptInputStream):Seq[LogFilePart]
	def acceptLine(inputStream: LogFileExcerptInputStream, line: String): Boolean
}

object IncludeAllExcerptStrategy extends ExcerptStrategy {
	override def getLogFileParts(inputStream: LogFileExcerptInputStream):Seq[LogFilePart] = inputStream.logFile.parts
	override def acceptLine(inputStream: LogFileExcerptInputStream, line: String) = true
}


class TimePeriodExcerptStrategy(timePeriod: TimePeriod) extends ExcerptStrategy {
	private[this]
	var prevTimeStamp: Option[Calendar] = None

	private[this]
	var currentPart: LogFilePart = null

	override def acceptLine(inputStream: LogFileExcerptInputStream, line: String) = {
		import inputStream.State._
		import inputStream.logFile.timeStampOfLine

		if (currentPart != inputStream.currentPart) {
			currentPart  = inputStream.currentPart
			prevTimeStamp = None
		}

		inputStream.state match {
		  	case EXCERPT_NOT_YET_STARTED => {
		  		val maybeTimeStamp = timeStampOfLine(line, currentPart, prevTimeStamp)
		  		maybeTimeStamp match {
					case Some(timeStamp) => {
		  	  			prevTimeStamp = maybeTimeStamp
		  	  			if (timePeriod includes timeStamp) {
		  	  				inputStream.state = EXCERPT_STARTED
		  	  				true
	 					}
		  	  			else
		  	  				false
		  			}
		  	  		case None => false
		  		}
		  	}

		  	case EXCERPT_STARTED => {
		  		val maybeTimeStamp = timeStampOfLine(line, inputStream.currentPart, prevTimeStamp)
		  		maybeTimeStamp match {
		  			case Some(timeStamp) => {
		  				prevTimeStamp = maybeTimeStamp
		  	  			if (timePeriod includes timeStamp) {
		  	  				true
		  	  			}
		  	  			else {
		  	  				inputStream.state = EXCERPT_FINISHED
		  	  				false
		  	  			}
		  	  		}
		  	  		case None => true
		  		}
		  	}

		  	case EXCERPT_FINISHED => false
		}
	}

	private def getPartsCovering(timePeriod: TimePeriod, parts: Seq[LogFilePart]):Seq[LogFilePart] = {
		var list:List[LogFilePart] = Nil
		val iter = parts.iterator
		var finished = false
		while (!finished && iter.hasNext) {
			val part = iter.next()
			part.timePeriod match {
			  	case None => list = part :: list
			  	case Some(partTimePeriod) => {
			  		if (partTimePeriod overlaps timePeriod)
			  			list = part :: list
			  		finished = partTimePeriod.startTime before timePeriod.startTime
			  	}
			}
		}
		list
	}

	override def getLogFileParts(inputStream: LogFileExcerptInputStream):Seq[LogFilePart] = {
		val parts = inputStream.logFile.parts
		getPartsCovering(timePeriod, parts)
	}
}

trait Enum[A] {
	trait Value { self: A =>
		_values :+= this
	}
	private var _values = List.empty[A]
	def values = _values
}
