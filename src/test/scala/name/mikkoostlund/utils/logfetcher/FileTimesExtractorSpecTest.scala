package name.mikkoostlund.utils.logfetcher

import org.specs2.Specification
import org.specs2.execute.Result
import java.util.Calendar
import java.util.GregorianCalendar
import java.io.ByteArrayInputStream 
import org.junit.runner.RunWith
import org.junit.runner.RunWith
import org.specs2.runner.JUnitRunner

@RunWith(classOf[JUnitRunner])
class FileTimesExtractorSpecTest extends Specification { def is =
  "A FileTimesExtractor must" ^
    "correctly determine the TimePeriod corresponding" +
    "to the timestamps in a log file part"                      ! Context().testDetermineTimePeriod

  case class Context() {

    def testDetermineTimePeriod:Result = {
      val extractor = new FileTimesExtractor ( `YYMMDD hh:mm:ss.mmm` )
      val tp = extractor.determineTimePeriod(TestLogFile1.inputStream).get
      val extractedStartTime = tp.startTime.getTime
      val extractedEndTime = tp.endTime.getTime

      extractedStartTime must beEqualTo (TestLogFile1.expectedStartTime)
      extractedEndTime must beEqualTo (TestLogFile1.expectedEndTime)
    }

    val YY = "([0-9]{2})"
    val MM = "(0[1-9]|1[0-2])"
    val DD = "(0[1-9]|[12][0-9]|3[01])"
    val hh = "(0[1-9]|[12][0-9]|3[01])"
    val mm, ss = "([0-5][0-9])"
    val ms = "([0-9]{3})"
    val ext = "log" 

    val LineTimeStamp = ("^" + YY + MM + DD +" "+ hh +":"+ mm +":"+ ss +"\\."+ ms +" .*$").r

    def `YYMMDD hh:mm:ss.mmm` = (line: String) => line match {
      case LineTimeStamp(year, month, day, hour, minute, second, millisecond) => {
        val ts = new GregorianCalendar(2000 + year.toInt, month.toInt - 1, day.toInt, hour.toInt, minute.toInt, second.toInt)
        ts.set(Calendar.MILLISECOND, millisecond.toInt)
        Some(ts)
      }
      case _ => None
    }

    object TestLogFile1 {
      lazy val inputStream = logLines.openStream()
      lazy val expectedStartTime = getTime(2013, 1, 21, 14, 39, 1, 123)
      lazy val expectedEndTime   = getTime(2013, 1, 21, 14, 40, 2, 456)

      private[this] val logLines = this.getClass().getResource("test_log_file.txt")
      
      private[this] def getTime(year: Int, month: Int, day: Int, hour: Int, minute: Int, second: Int, millis: Int) = {
        val expectedStartTimeCal = new GregorianCalendar(year, month-1, day, hour, minute, second)
        expectedStartTimeCal.set(Calendar.MILLISECOND, millis)
        expectedStartTimeCal.getTime
      }
    }
  }
}
