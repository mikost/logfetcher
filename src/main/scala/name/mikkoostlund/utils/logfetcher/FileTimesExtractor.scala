package name.mikkoostlund.utils.logfetcher
import scala.io.Source
import scala.annotation.tailrec
import java.util.Calendar
import java.io.InputStream
import scala.annotation.tailrec

class FileTimesExtractor(extractTimeStamp: (String) => Option[Calendar]) {

	def determineTimePeriod(in: InputStream): Option[TimePeriod] = {
		val lines = Source.fromInputStream(in).getLines

		nextTimeStamp(lines) map {
		  timeStamp => 
		    val firstTimeStamp = timeStamp
		    val lastTimeStamp = lastTimeStampAfter(firstTimeStamp, lines)
		    TimePeriod(firstTimeStamp, lastTimeStamp)
		}
	}

	@tailrec
	final def nextTimeStamp(lines: Iterator[String]): Option[Calendar] = {
		if (!lines.hasNext)
			None
		else {
			val timeStamp = extractTimeStamp(lines.next())
			if (timeStamp.isDefined)
				timeStamp
			else
				nextTimeStamp(lines)
		}
	}

	@tailrec
	final def lastTimeStampAfter(foundTimeStamp: Calendar, lines: Iterator[String]): Calendar = {
		require(foundTimeStamp != null)

		nextTimeStamp(lines) match {
			case Some(nextTimeStamp) => {
				lastTimeStampAfter(nextTimeStamp, lines)
			}
			case None => foundTimeStamp
		}
	}
}