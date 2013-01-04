package name.mikkoostlund.utils.logfetcher
import java.io.File

class StandardLogFileBuilder() {
	protected[logfetcher] var basefile: File = null

  	protected[logfetcher] var separatorInFilenameTimeStamp_YYYY_MM = "-"
  	def with_separatorInFilenameTimeStamp_YYYY_MM(sep: String) = { separatorInFilenameTimeStamp_YYYY_MM = sep; this }
  	  
	protected[logfetcher] var separatorInFilenameTimeStamp_MM_DD = "-"
  	def with_separatorInFilenameTimeStamp_MM_DD(sep: String) = { separatorInFilenameTimeStamp_MM_DD = sep; this }
  	
	protected[logfetcher] var separatorInLineTimeStamp_YYYY_MM = ""
  	def with_separatorInLineTimeStamp_YYYY_MM(sep: String) = { separatorInLineTimeStamp_YYYY_MM = sep; this }

	protected[logfetcher] var separatorInLineTimeStamp_MM_DD = ""
  	def with_separatorInLineTimeStamp_MM_DD(sep: String) = { separatorInLineTimeStamp_MM_DD = sep; this }

	protected[logfetcher] var separatorInLineTimeStamp_DD_HH = " "
  	def with_separatorInLineTimeStamp_DD_HH(sep: String) = { separatorInLineTimeStamp_DD_HH = sep; this }

	protected[logfetcher] var separatorInLineTimeStamp_HH_MM = ":"
  	def with_separatorInLineTimeStamp_HH_MM(sep: String) = { separatorInLineTimeStamp_HH_MM = sep; this }

	protected[logfetcher] var separatorInLineTimeStamp_MM_SS = ":"
  	def with_separatorInLineTimeStamp_MM_SS(sep: String) = { separatorInLineTimeStamp_MM_SS = sep; this }

	protected[logfetcher] var separatorInLineTimeStamp_SS_NN = "."
  	def with_separatorInLineTimeStamp_SS_NN(sep: String) = { separatorInLineTimeStamp_SS_NN = sep; this }


	def build(basefile: File): StandardLogFile = { this.basefile = basefile; new StandardLogFileImpl(this) }
}

protected class StandardLogFileImpl(builder: StandardLogFileBuilder) extends StandardLogFile(builder.basefile) {

	override val priv_separatorInFilenameTimeStamp_YYYY_MM = builder.separatorInFilenameTimeStamp_YYYY_MM
	override val priv_separatorInFilenameTimeStamp_MM_DD = builder.separatorInFilenameTimeStamp_MM_DD

	override val priv_separatorInLineTimeStamp_YYYY_MM = builder.separatorInLineTimeStamp_YYYY_MM
	override val priv_separatorInLineTimeStamp_MM_DD = builder.separatorInLineTimeStamp_MM_DD
	override val priv_separatorInLineTimeStamp_DD_HH = builder.separatorInLineTimeStamp_DD_HH
	override val priv_separatorInLineTimeStamp_HH_MM = builder.separatorInLineTimeStamp_HH_MM
	override val priv_separatorInLineTimeStamp_MM_SS = builder.separatorInLineTimeStamp_MM_SS
	override val priv_separatorInLineTimeStamp_SS_NN = builder.separatorInLineTimeStamp_SS_NN
}

abstract class StandardLogFile(val basefile: File) extends FileLogFile(basefile)
													with YYYY_MM_DD_SuffixedFiles_LogFilePartsSelector
													with YYYY_MM_DD_HH_MM_SS_NN_LineTimeStampExtractor