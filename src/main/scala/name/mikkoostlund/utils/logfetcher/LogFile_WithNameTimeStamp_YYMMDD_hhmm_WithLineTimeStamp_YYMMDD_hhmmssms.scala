package name.mikkoostlund.utils.logfetcher

import java.io.File
import java.util.Calendar
import java.util.GregorianCalendar
import java.io.InputStream
import scala.io.Source
import java.io.FileInputStream
import java.io.Closeable
import java.io.BufferedInputStream

class LogFile_WithNameTimeStamp_YYMMDD_hhmm_WithLineTimeStamp_YYMMDD_hhmmssms(baseFile: File) extends FileLogFile(baseFile) {

	private val YY = "([0-9]{2})"
	private val MM = "(0[1-9]|1[0-2])"
	private val DD = "(0[1-9]|[12][0-9]|3[01])"
	private val hh = "(0[1-9]|[12][0-9]|3[01])"
	private val mm, ss = "([0-5][0-9])"
	private val ms = "([0-9]{3})"
	private val ext = "log" 
	private val FileName = ("^" + baseFile.getName + YY + MM + DD + "_" + hh + mm + "\\." + ext + "$").r

	private object extractor extends FileTimesExtractor (line => timeStampOfLine(line, null, null))

	def parts(): Seq[LogFilePart] = { 
		
		val allPartsUnsorted = filesInDir.getOrElse(Array.empty[File]).filter(file => FileName.findFirstIn(file.getName).isDefined)
		val allParts = allPartsUnsorted.sortWith { _.getName > _.getName }

		val list = allParts.foldLeft(List.empty[LogFilePart]) { (list, file) => 

		  val fileTimePeriod = list match {
		    case Nil => extractor.determineTimePeriod(toInputStream(file))

		    case head :: tail => {
		    	val laterFile = head
		    	val startTime = extractor.nextTimeStamp(Source.fromInputStream(toInputStream(file)).getLines).get
		    	val endTime = laterFile.timePeriod.get.startTime
		    	Some(new TimePeriod(startTime, endTime))
		    }
		  }
		  val newLogFilePart = new LogFilePart {
		    override val timePeriod: Option[TimePeriod] = fileTimePeriod
		    override def createInputStream = toInputStream(file)
		  	override def name = file.getName
		  }

		  newLogFilePart :: list
		}
		list.reverse
	}

	def toInputStream(file: File): InputStream = {
		val fis = new FileInputStream(file)
		var resource: Closeable = fis
		try {
			val bis = new BufferedInputStream(fis)
			resource = bis
			new ASCIIRepairFilterInputStream(bis, '$')
		}
		catch {
		  	case throwable => resource.close; throw throwable
		}
	}
	
	val LineTimeStamp = ("^" + YY + MM + DD +" "+ hh +":"+ mm +":"+ ss +"\\."+ ms +" .*$").r

	def timeStampOfLine(line: String, logFilePart: LogFilePart, maybePrevLineTimeStamp: Option[Calendar]): Option[Calendar] = {
		line match {
		  case LineTimeStamp(year, month, day, hour, minute, second, millisecond) => {
		    val ts = new GregorianCalendar(2000 + year.toInt, month.toInt - 1, day.toInt, hour.toInt, minute.toInt, second.toInt)
		    ts.set(Calendar.MILLISECOND, millisecond.toInt)
		    Some(ts)
		  }
		  case _ => None
		}
	}
}