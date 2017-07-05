package pub.ayada.scala.utils

object DateTimeUtils {

    private val dtFmt = new java.text.SimpleDateFormat("yyyyMMdd hh:mm:ss")

    def getFmtDtm(jobID: String): String = {
        val sb: StringBuilder = new StringBuilder()
        sb.append("[")
            .append(dtFmt.format(new java.util.Date()))
            .append("][")
            .append(jobID)
            .append("] ")

        sb.toString
    }
}
