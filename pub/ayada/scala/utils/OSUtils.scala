package pub.ayada.scala.utils
 

object OSUtils {  
  def getOSVars(envVarName:String): String = sys.env(envVarName) 
  def getSysTimeStamp(format:String):String = new java.text.SimpleDateFormat(format).format(new java.util.Date())
  
  
  def main (args: Array[String]): Unit = {
    
    println("Current TimeStamp:" + getSysTimeStamp("yyyy/MM/dd hh:mm:ss"))
  }
}
