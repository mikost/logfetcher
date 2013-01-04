package name.mikkoostlund.utils.logfetcher
import scala.io.Source
import scala.annotation.tailrec
import java.util.Calendar
import java.io.InputStream
import scala.annotation.tailrec

class FileTimesExtractor(extractTimeStamp: (String) => Option[Calendar]) {

	def determineTimePeriod(in: InputStream): Option[TimePeriod] = {
		val lines = Source.fromInputStream(in).getLines
		val maybeFirstTimeStamp = findNextTimeStamp(lines)
		if (maybeFirstTimeStamp.isEmpty)
			None
		else {
			val firstTimeStamp = maybeFirstTimeStamp.get
			val lastTimeStamp = findLastTimeStamp(firstTimeStamp, lines)
			Some(TimePeriod(firstTimeStamp, lastTimeStamp))
		}
	}

	@tailrec
	final def findNextTimeStamp(lines: Iterator[String]): Option[Calendar] = {
		if (!lines.hasNext)
			None
		else {
			val timeStamp = extractTimeStamp(lines.next())
			if (timeStamp.isDefined)
				timeStamp
			else
				findNextTimeStamp(lines)
		}
	}

	@tailrec
	final def findLastTimeStamp(foundTimeStamp: Calendar, lines: Iterator[String]): Calendar = {
		require(foundTimeStamp != null)

		findNextTimeStamp(lines) match {
			case Some(nextTimeStamp) => {
				findLastTimeStamp(nextTimeStamp, lines)
			}
			case None => foundTimeStamp
		}
	}
}