package name.mikkoostlund.utils.logfetcher
import java.util.Calendar
import java.text.DateFormat
import java.util.GregorianCalendar

class TimePeriod (val startTime: Calendar, val endTime: Calendar) {
	require(startTime.before(endTime) || startTime.equals(endTime), 
			{
				val df = DateFormat.getInstance;
				import df.format;
				"startTime<=endTime; found startTime="+ format(startTime.getTime) + ", endTime="+ format(endTime.getTime)
			})

	def overlaps(that: TimePeriod): Boolean = {
		if (that.startTime.compareTo(this.startTime) < 0) {
			that.endTime.compareTo(this.startTime) >= 0
		}
		else if (that.startTime.compareTo(this.endTime) <= 0) {
			true
		}
		else
			false
	}
	
	def includes(date: Calendar): Boolean = {
		this.startTime.compareTo(date) <= 0 && date.compareTo(this.endTime) <= 0
	}

	override def toString = ""+ startTime.getTime +" "+ startTime.get(Calendar.MILLISECOND) +" ms -->"+ endTime.getTime +" "+ endTime.get(Calendar.MILLISECOND) +" ms"
}

object TimePeriod {
	def apply(startTime: Calendar, endTime: Calendar): TimePeriod = new TimePeriod(startTime, endTime)

	def oneDay(year: Int, month: Int, day: Int): TimePeriod = {
		val startOfDay = new GregorianCalendar(year, month, day)
		val endOfDay = new GregorianCalendar(year, month, day)
		endOfDay.add(Calendar.DAY_OF_MONTH, 1)
		endOfDay.add(Calendar.MILLISECOND, -1)
		new TimePeriod(startOfDay, endOfDay)
	}
}