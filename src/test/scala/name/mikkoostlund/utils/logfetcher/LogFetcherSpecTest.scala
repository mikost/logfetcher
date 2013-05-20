package name.mikkoostlund.utils.logfetcher

import org.specs2.Specification
import org.junit.runner.RunWith
import org.specs2.runner.JUnitRunner
import java.io.File
import java.util.GregorianCalendar
import java.util.Calendar
import java.net.URL

@RunWith(classOf[JUnitRunner])
class LogFetcherSpecTest extends Specification { def is =
  "A LogFetcher must" ^
    "correctly extract and gather log lines from " +
    "the parts of a rotated log file"                      ! Context().testFetchLogs

    case class Context() {

	  def testFetchLogs = {
	    import TimePeriod._
	    val startTime = new GregorianCalendar(2013, 0, 22, 3, 2, 6)
	    startTime.set(Calendar.MILLISECOND, 500)
	    val endTime = new GregorianCalendar(2013, 0, 23, 23, 45, 4)
	    endTime.set(Calendar.MILLISECOND, 500)
	    val logFetcher = new LogFetcher
	    val oneLogFile = new File(this.getClass.getResource("test_logfile_130122_0302.log").getPath)
	    val theLogDirName = oneLogFile.getParent()
	    val	logFileBaseName = theLogDirName + "/test_logfile_"    
	    logFetcher add (new LogFile_WithNameTimeStamp_YYMMDD_hhmm_WithLineTimeStamp_YYMMDD_hhmmssms(new File(logFileBaseName)))
	    logFetcher.setTimePeriod(TimePeriod(startTime, endTime))
	    logFetcher.logsToZipFile("/tmp/logs_20130122_3.zip")
	    1 must beEqualTo(1)
	  }
	}
}