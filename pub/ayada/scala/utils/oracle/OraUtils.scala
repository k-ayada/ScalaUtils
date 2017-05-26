package pub.ayada.scala.utils.oracle


object OraUtils {
  
  def getJDBC_URL_SID(Server:String, Port:String, SID:String):String = {
    
    "jdbc:oracle:thin:@"+Server+ ":"+ Port + ":"+SID;
  }
  def getJDBC_URL_SRVC(Server:String, Port:String, Service:String):String = {
    
    "jdbc:oracle:thin:@//"+Server+ ":"+ Port + "/"+Service;
  }

  def testConnection( JDBCDriverClass: String
                    , JDBCUrl: String
                    , UserID: String
                    , Password: String): Boolean = {
    {if (OraUtils.getJDBCConnection(JDBCDriverClass, JDBCUrl, UserID, Password) == null) true else false}
  }

  def getJDBCConnection(JDBCDriverClass: String
                       , JDBCUrl: String
                       , UserID: String
                       , Password: String) : java.sql.Connection = {
    val JDBCConn: java.sql.Connection = null
    Class.forName(JDBCDriverClass);
    try {
      val JDBCConn = java.sql.DriverManager.getConnection(JDBCUrl, UserID, Password);
    } catch {
      case e: Exception => ()
    }
    JDBCConn
  }
  def executeQuery( JDBCConn: java.sql.Connection
                  , readStatement: String): java.sql.ResultSet = {

    val st: java.sql.Statement = JDBCConn.createStatement( java.sql.ResultSet.TYPE_SCROLL_INSENSITIVE
                                                         , java.sql.ResultSet.CONCUR_READ_ONLY)
    st.executeQuery(readStatement)
  }
  
  def executeQuery( JDBCConn: java.sql.Connection
                    , readStatement: String
                    , Commit:Boolean
                    , Values: java.util.ArrayList[Any]): java.sql.ResultSet = {     
    val st: java.sql.PreparedStatement = JDBCConn.prepareStatement(readStatement)    
    for ( i <- 0 until  Values.size) {
        st.setObject(i, Values.get(i))
    }      
    val res:java.sql.ResultSet = st.executeQuery     
    st.close    
    res
  }  
  def executeUpdate(JDBCConn: java.sql.Connection
                   , UpdateStatement: String
                   , Commit:Boolean): Int = {
    val st: java.sql.Statement = JDBCConn.createStatement( java.sql.ResultSet.TYPE_SCROLL_INSENSITIVE
                                                         , java.sql.ResultSet.CONCUR_READ_ONLY)
    val res:Int = st.executeUpdate(UpdateStatement)    
    st.close
    if (Commit) JDBCConn.commit    
    res    
  }
  
   def executeUpdate( JDBCConn: java.sql.Connection
                    , UpdateStatement: String
                    , Commit:Boolean
                    , Values: java.util.ArrayList[Any]): Int = {     
    val st: java.sql.PreparedStatement = JDBCConn.prepareStatement(UpdateStatement)
    
    for ( i <- 0 until  Values.size) {
        st.setObject(i, Values.get(i))
    }      
    
    val res:Int = st.executeUpdate    
    st.close
    if (Commit) JDBCConn.commit    
    res
  }
  def executeBatchUpdate( JDBCConn: java.sql.Connection
                   , UpdateStatement: String
                   , Commit:Boolean
                   , Values: java.util.ArrayList[java.util.ArrayList[Any]]): Array[Int] = {
    val st: java.sql.PreparedStatement = JDBCConn.prepareStatement(UpdateStatement)
    
    for ( i <- 0 until  Values.size) {      
        for ( j <- 0 until  Values.get(i).size) {
            st.setObject(i, Values.get(i).get(j))  
        }
        st.addBatch        
    }
    val res:Array[Int] = st.executeBatch    
    st.close
    if (Commit) JDBCConn.commit    
    res
  } 
   
  def executeDelete(JDBCConn: java.sql.Connection,
                    DeleteStatement: String, 
                    Commit:Boolean): Int = {
    val st: java.sql.Statement = JDBCConn.createStatement( java.sql.ResultSet.TYPE_SCROLL_INSENSITIVE
                                                         , java.sql.ResultSet.CONCUR_READ_ONLY)
    val res:Int = st.executeUpdate(DeleteStatement) 
    st.close
    if (Commit) JDBCConn.commit    
    res
  }
  
   def executeDelete( JDBCConn: java.sql.Connection
                    , DeleteStatement: String
                    , Commit:Boolean
                    , Values: java.util.ArrayList[Any]): Int = {
    val st: java.sql.PreparedStatement = JDBCConn.prepareStatement(DeleteStatement)
    
    for ( i <- 0 until  Values.size) {
        st.setObject(i, Values.get(i))
    }      
    
    val res:Int = st.executeUpdate   
    
    st.close
    if (Commit) JDBCConn.commit    
    res
  }
   
  def executeBatchDelete( JDBCConn: java.sql.Connection
                   , DeleteStatement: String
                   , Commit:Boolean
                   , Values: java.util.ArrayList[java.util.ArrayList[Any]]): Array[Int] = {
    val st: java.sql.PreparedStatement = JDBCConn.prepareStatement(DeleteStatement)
    
    for ( i <- 0 until  Values.size) {      
        for ( j <- 0 until  Values.get(i).size) {
            st.setObject(i, Values.get(i).get(j))  
        }
        st.addBatch        
    }
    val res:Array[Int] = st.executeBatch    
    st.close
    if (Commit) JDBCConn.commit    
    res
  }  
  
  def executeInsert( JDBCConn: java.sql.Connection
                   , InsertStatement: String
                   , Commit:Boolean): Int = {
    val st: java.sql.Statement = JDBCConn.createStatement( java.sql.ResultSet.TYPE_SCROLL_INSENSITIVE
                                                         , java.sql.ResultSet.CONCUR_READ_ONLY)
    val res:Int = st.executeUpdate(InsertStatement)    
    if (Commit) JDBCConn.commit    
    res
  }
  
  def executeInsert( JDBCConn: java.sql.Connection
                   , InsertStatement: String
                   , Commit:Boolean
                   , Values: java.util.ArrayList[Any]): Int = {
    val st: java.sql.PreparedStatement = JDBCConn.prepareStatement(InsertStatement)
    
    for ( i <- 0 until  Values.size) {
        st.setObject(i, Values.get(i))
    }      
    
    val res:Int = st.executeUpdate    
    if (Commit) JDBCConn.commit    
    res
  }
  def executeBatchInsert( JDBCConn: java.sql.Connection
                   , InsertStatement: String
                   , Commit:Boolean
                   , Values: java.util.ArrayList[java.util.ArrayList[Any]]): Array[Int] = {
    val st: java.sql.PreparedStatement = JDBCConn.prepareStatement(InsertStatement)
    
    for ( i <- 0 until  Values.size) {      
        for ( j <- 0 until  Values.get(i).size) {
            st.setObject(i, Values.get(i).get(j))  
        }
        st.addBatch        
    }
    val res:Array[Int] = st.executeBatch    
    if (Commit) JDBCConn.commit    
    res
  }
  
}
