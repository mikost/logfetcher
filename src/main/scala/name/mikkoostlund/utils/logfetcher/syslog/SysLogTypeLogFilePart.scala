package name.mikkoostlund.utils.logfetcher.syslog
import java.io.FileInputStream
import java.io.InputStream
import java.io.File
import java.io.BufferedInputStream
import scala.io.Source
import scala.annotation.tailrec
import java.util.Calendar
import java.util.GregorianCalendar
import java.util.Date
import java.util.Calendar.{YEAR, MONTH, DAY_OF_MONTH, HOUR_OF_DAY, MINUTE, SECOND, JANUARY, DECEMBER}
import name.mikkoostlund.utils.logfetcher.ASCIIRepairFilterInputStream
import name.mikkoostlund.utils.logfetcher.LogFilePart
import name.mikkoostlund.utils.logfetcher.TimePeriod
import scala.annotation.tailrec

class SysLogTypeLogFilePart(file: File) extends LogFilePart {
	case class InterpretationResult(isNewYear: Boolean, isLater: Boolean)

	override lazy val timePeriod: Option[TimePeriod] = determineTimePeriod

	override def createInputStream: InputStream = new ASCIIRepairFilterInputStream(new FileInputStream(file), '$')

	override def name = file.getName

	def extractTimeStamp(line: String): Option[Calendar] = {
		val month = "(Jan|Feb|Mar|Apr|May|Jun|Jul|Aug|Sep|Oct|Nov|Dec)"
		val day = "( ([1-9])|[12][0-9]|3[01])"
		val hour = "([01][0-9]|2[0-3])"
		val minute, second = "([0-5][0-9])"
		val TimeStampedLine = ("^" + month +" "+ day +" "+ hour +":"+ minute + ":" + second + " .*").r

		line match {
			case TimeStampedLine(monthStr, day1to31Str, day1To9Str, hourStr, minuteStr, secondStr) => {
				val month = monthToInt(monthStr)
				val dayStr = if (day1To9Str != null) day1To9Str else day1to31Str
				val day = dayStr.toInt
				val hour = hourStr.toInt
				val minute = minuteStr.toInt
				val second = secondStr.toInt
				val ANY_LEAP_YEAR = 2008
				Some(new GregorianCalendar(ANY_LEAP_YEAR, month, day, hour, minute, second))
			}
			case _ => None
		}
	}

	def interpretTimeStamp(previousTimeStamp: Calendar, currentTimeStamp: Calendar):InterpretationResult = {
		currentTimeStamp.set(YEAR, previousTimeStamp.get(YEAR))

		if (! (currentTimeStamp before previousTimeStamp)) {
			if (isLikelyTrueTimeIncrease(from = previousTimeStamp, to = currentTimeStamp))
				InterpretationResult(isNewYear=false, isLater = true)
			else
				InterpretationResult(isNewYear=false, isLater = false)
		}
		else if (guessThat(currentTimeStamp) isNextYearOf previousTimeStamp) {
			InterpretationResult(isNewYear=true, isLater = true)
		}
		else {
			InterpretationResult(isNewYear=false, isLater = false)
		}
	}

	private def determineTimePeriod: Option[TimePeriod] = {
		val fileTimeStamp = file.lastModified()
		if (fileTimeStamp == 0)
			return None

		val in = new ASCIIRepairFilterInputStream(new BufferedInputStream(new FileInputStream(file)), '$')
		val lines = Source.fromInputStream(in).getLines
		val maybeFirstTimeStamp = findNextTimeStamp(lines)
		if (maybeFirstTimeStamp.isEmpty)
			None
		else {
			val firstTimeStamp = maybeFirstTimeStamp.get
			val (years, lastTimeStamp) = findLastTimeStamp(firstTimeStamp, lines)
			val yearOfLastTimeStamp = toYear(fileTimeStamp)
			lastTimeStamp.set(Calendar.YEAR, yearOfLastTimeStamp)
			firstTimeStamp.set(Calendar.YEAR, yearOfLastTimeStamp - years)
			Some(TimePeriod(firstTimeStamp, lastTimeStamp))
		}

	}

	@tailrec
	private def findNextTimeStamp(lines: Iterator[String]): Option[Calendar] = {
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

	private def findLastTimeStamp(firstTimeStamp: Calendar, lines: Iterator[String]): (Int, Calendar) = {
		require(firstTimeStamp != null)

		@tailrec
		def tailRecursive(yearCount: Int, previousTimeStamp: Calendar): (Int, Calendar) = {
			findNextTimeStamp(lines) match {
			  	case Some(currentTimeStamp) => {
			  		val result = interpretTimeStamp(previousTimeStamp, currentTimeStamp)
			  		val updatedPreviousTimeStamp = if (result.isLater) currentTimeStamp else previousTimeStamp
			  		val updatedYearCount =
			  			if (result.isNewYear)
			  				yearCount + 1
			  			else
			  				yearCount

			  		tailRecursive(updatedYearCount, updatedPreviousTimeStamp)
			  	}
			  	case None => (yearCount, previousTimeStamp)
			}
		}
		tailRecursive(yearCount = 0, previousTimeStamp = firstTimeStamp)
	}

	private def toYear(millisSince1970: Long): Int = {
		val cal = new GregorianCalendar()
		cal.setTime(new Date(millisSince1970))
		cal.get(Calendar.YEAR)
	}

	private def monthToInt(monthStr: String) = {
		monthStr match {
		  	case "Jan" => 0
		  	case "Feb" => 1
		  	case "Mar" => 2
		  	case "Apr" => 3
		  	case "May" => 4
		  	case "Jun" => 5
		  	case "Jul" => 6
		  	case "Aug" => 7
		  	case "Sep" => 8
		  	case "Oct" => 9
		  	case "Nov" => 10
		  	case "Dec" => 11
		}
	}

	private def isLikelyTrueTimeIncrease(from: Calendar, to: Calendar): Boolean = {
		/* Regard as "unlikely" a time increase that is
		 *     from "Jan  1 (HH1):(MM1):(SS1)"
		 *       to "Dec 31 (HH2):(MM2):(SS2)",
		 *     where HH2 >= HH1 and MM2 >= MM1 and SS2 >= SS1.
		 * It, likely, represents a time DECREASE, by less than 24 hours
		 *     from "Jan 1 of some year Y"
		 *       to "Dec 31 of year Y-1".
		 * It has been observed that such "falses" steps back in time appear in
		 * the log file, maybe due to some race-condition between logging processes.
		 * The purpose of this method is to catch (return false for) the rare
		 * occasions when such a "false" step goes back to Dec 31 from Jan 1,
		 * (which is seen as a long step forward from Jan 1 to Dec 31, since the
		 * time stamps don't have a year field).
		 * 
		 * The time interval used here for judging, 24 hours, is quite arbitrary.
		 * This algorithm will make the wrong judgement if the two time stamps
		 * actually DO represent the unlikely time increase from Jan 1 to Dec 31
		 * of the same year.
		 */
	
		val unlikely = (
			     to.get(MONTH) == DECEMBER 
			&& from.get(MONTH) == JANUARY
			&&   to.get(DAY_OF_MONTH) == 31
			&& from.get(DAY_OF_MONTH) == 1
			&&   to.get(HOUR_OF_DAY) >= from.get(HOUR_OF_DAY)
			&&   to.get(MINUTE)      >= from.get(MINUTE)
			&&   to.get(SECOND)      >= from.get(SECOND)
		)
	
		return !unlikely
	}

	private def guessThat(timeStamp: Calendar) = new CalendarGuess(timeStamp)

	private class CalendarGuess(timeStamp: Calendar) {
		def isNextYearOf(precedingTimeStamp: Calendar) =  {
			/* The parameter 'preceedingTimeStamp' should be a timeStamp that precedes
			 * (i.e. appears at a lower line number than) 'timeStamp' in a log file,
			 * but whose time fields (i.e. MONTH, DAY_OF_MONTH, HOUR, MINUTE, SECOND)
			 * correspond to a later time than 'timeStamp'. This can happen due to
			 * two reasons:
			 *    1) The 'timeStamp' has been incorrectly inserted into the log file,
			 *       e.g. due to some race-condition between logging processes.
			 *    2) The 'timeStamp' belongs to the next year compared to
			 *       'precedingTimeStamp'.
			 * 
			 * This method "guesses" that reason 2 applies, if the time of 'timeStamp'
			 * is more than a day before the time of 'precedingTimeStamp'.
			 */

			val timeDiff = timeStamp.getTimeInMillis - precedingTimeStamp.getTimeInMillis 
			timeDiff < -24 * 60 * 60 * 1000 // the number of milliseconds in a day
		}
	}
}