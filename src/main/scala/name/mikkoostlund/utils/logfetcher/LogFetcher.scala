package name.mikkoostlund.utils.logfetcher
import scala.collection.mutable.Set
import java.util.zip.ZipOutputStream
import java.io.BufferedOutputStream
import java.io.FileOutputStream
import java.io.Closeable
import java.io.FileNotFoundException


class LogFetcher() {
	private val logFiles = Set.empty[LogFile]
	private var someTimePeriod: Option[TimePeriod] = None
	
	def add(logFile: LogFile) {
		logFiles += logFile
	}

	def setTimePeriod(timePeriod: TimePeriod) {
		require(timePeriod != null)
		this.someTimePeriod = Some(timePeriod)
	}

	def clearTimePeriod = this.someTimePeriod = None

	def logsToZipFile(zipFileName: String): Unit = {
		val out = createZippedBufferedFileOutputStream(zipFileName)
		try {
			val excerptStrategy: ExcerptStrategy = {
				someTimePeriod match {
					case Some(timePeriod) => new TimePeriodExcerptStrategy(timePeriod)
					case None             => IncludeAllExcerptStrategy
				}
			}
    		
			logFiles.foreach { logfile => logfile.writeToZipOutputStream(out, excerptStrategy) }

		}
		finally {
			out.close()
		}
	}
	
	private def createZippedBufferedFileOutputStream(zipFileName: String) = {
		val fos = new FileOutputStream(zipFileName)
		var resource: Closeable = fos
		try {
			val bos = new BufferedOutputStream(fos)
			resource = bos
			new ZipOutputStream(bos)
		}
		catch {
		  	case t: Throwable => resource.close(); throw t
		}
	}
}