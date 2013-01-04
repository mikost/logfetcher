package name.mikkoostlund.utils.logfetcher.syslog

import scala.collection.Seq
import java.io.File
import java.util.Calendar
import scala.collection.mutable.Buffer
import java.util.Calendar.YEAR
import name.mikkoostlund.utils.logfetcher.LogFilePart
import name.mikkoostlund.utils.logfetcher.FileLogFile
import name.mikkoostlund.utils.logfetcher.TimePeriod
import scala.Array.fallbackCanBuildFrom


class SysLogTypeLogFile(baseFile: File) extends FileLogFile(baseFile) {

	def parts(): Seq[LogFilePart] = { 
		val these = Buffer.empty[LogFilePart]
		val FileNamePattern = ("^" + baseFile.getName + """(\.([0-9]+))?$""").r
		val allPartsUnsorted = filesInDir.getOrElse(Array.empty[File]).filter(file => FileNamePattern.findFirstIn(file.getName).isDefined)
		val allParts = allPartsUnsorted.sortWith { (file1, file2) => 
		  		val FileNamePattern(extension1, number1) = file1.getName
		  		lazy val FileNamePattern(extension2, number2) = file2.getName

		  		if (extension1 == null)
					true
				else if (extension2 == null)
					false
				else
					number1.toInt < number2.toInt // Sort 'file1' before 'file2' if its number 
					                              //  (in the file extension) is lower.
		};
		allParts map { new SysLogTypeLogFilePart(_) }
	}

	def timeStampOfLine(line: String, logFilePart: LogFilePart, maybePrevLineTimeStamp: Option[Calendar]): Option[Calendar] = {
		val maybePrevTimeStamp = findOutPrevTimeStampToUse(maybePrevLineTimeStamp, logFilePart.timePeriod)
		if (maybePrevTimeStamp.isDefined) {
			val maybeLineTimeStamp = logFilePart.asInstanceOf[SysLogTypeLogFilePart].extractTimeStamp(line)
			if (maybeLineTimeStamp.isDefined) {
				val prevTimeStamp = maybePrevTimeStamp.get
				val lineTimeStamp = maybeLineTimeStamp.get

				val interpretation = logFilePart.asInstanceOf[SysLogTypeLogFilePart].interpretTimeStamp(prevTimeStamp, lineTimeStamp)

				if (interpretation.isLater) {
					if (interpretation.isNewYear) {
						lineTimeStamp.set(YEAR, lineTimeStamp.get(YEAR) + 1)
					}
					maybeLineTimeStamp
					
				}
				else {
					maybePrevTimeStamp
				}
			}
			else
				None
		}
		else
			None
	}

	private def findOutPrevTimeStampToUse(maybePrevLineTimeStamp: Option[Calendar], maybeTimePeriodOfLogFilePart: Option[TimePeriod]): Option[Calendar] = {
		if (maybePrevLineTimeStamp.isDefined)
			maybePrevLineTimeStamp
		else if (maybeTimePeriodOfLogFilePart.isDefined)
			Some(maybeTimePeriodOfLogFilePart.get.startTime)
		else
			None
	}
}