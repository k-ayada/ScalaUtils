package scala.pub.ayada.scala.utils

import java.text.DecimalFormat
import java.util.ArrayList
import java.util.Arrays
import java.util.Formatter
import java.util.StringTokenizer
import StringUtils._
import scala.util.control.Breaks._


object StringUtils {

  /**
   * Splits the input string by the length specified and returns the Array of size SplitCount. 
   * If string length > len*SplitCount, the additional characters will be ignored.
   */
  def splitByLength(string: String, len: Int, SplitCount: Int): Array[String] = {
    if (string == null || len <= 0)  
      return null
      
    if (string.length < len) {
      val arr: Array[String] = Array.ofDim[String](1)
      arr(0) = string
      return arr;
    }
    
    //decide number of elements 
    var chunks: Int = string.length / len + {if ((string.length % len > 0)) 1 else 0}    
    chunks = if (chunks > SplitCount) SplitCount else chunks
    
    return getSplits(string,len,chunks)
  }
  
  /**
   * Splits the input string by the length specified and returns the Array
   */
  def splitByLength(string: String, len: Int): Array[String] = {
    // if the input is null, return null
    if (string == null || len <= 0)
      return null

    //if the input string is shorter return return 1 element array 
    if (string.length < len) {
      val arr: Array[String] = Array.ofDim[String](1)
      arr(0) = string
      return arr;
    }
    
    val chunks: Int = string.length / len + (if ((string.length % len > 0)) 1 else 0)
    return getSplits(string,len,chunks)
  }
  
  private def getSplits(string: String, len: Int, chunks: Int): Array[String] = {
    val arr: Array[String] = Array.ofDim[String](chunks)
    var i: Int = 0
    var j: Int = 0
    var l: Int = string.length
    while (i < l && j < chunks - 1 &&  (i + len) <= l) {
        arr(j) = string.substring(i, i + len)
        i += len 
        j += 1
    }
    arr(j) = string.substring(i)
    return arr
  }

  def splitByLength(string: String,
                    len: Int,
                    splitAtNewLine: Boolean,
                    Justify: Char): ArrayList[String] = {
    if (string == null || len <= 0) 
      return null.asInstanceOf[ArrayList[String]]
    
    var fmt: String = if (Justify == 'l') "%-" + len + "s" else "%" + len + "s"

    if (string.length <= len) {
      val splits: Array[String] = string.split("\n")
      val arr: ArrayList[String] = new ArrayList[String](splits.length)
      for (i <- 0 until splits.length) arr.add(String.format(fmt, splits(i)))
      return arr
    }
    
    val chunks: Int = string.length / len + (if ((string.length % len > 0)) 1  else 0)
    val arr: ArrayList[String] = new ArrayList[String](chunks)
    val carr: Array[Char] = string.toCharArray()
    var sb: StringBuilder = new StringBuilder()
    for (i <- 0 until carr.length) {
      if (sb.length == len) {
        arr.add(sb.toString)
        sb = new StringBuilder()
        sb.append(carr(i))
      } else if (splitAtNewLine && carr(i) == '\n') {
        arr.add(String.format(fmt, sb.toString.trim()))
        sb = new StringBuilder()
      } else sb.append(carr(i))
    }
    arr.add(String.format(fmt, sb.toString.trim()))
    return arr
  }

  def join[T](stringArr: Array[T]): String = join(stringArr, "")

  def join[T](stringArr: Array[T], delm: String): String = {
    if (stringArr.length == 0) 
      return ""
    if (stringArr.length == 1) 
      return String.valueOf(stringArr(0))
      
    val sb: StringBuilder = new StringBuilder()
    for (str <- stringArr) sb.append(String.valueOf(str)).append(delm)
    
    sb.setLength(sb.length - (delm.length))
    sb.toString
  }

  def repeat(str: String, count: Int): String = {
    val sb: StringBuilder = new StringBuilder(str.length * count)
    for (i <- 0 until count) {
      sb.append(str)
    }
    sb.toString
  }

  def repeat(c: Char, count: Int): String = {
    val sb: StringBuilder = new StringBuilder(count)
    for (i <- 0 until count) {
      sb.append(c)
    }
    sb.toString
  }

  def Trim(argInStr: String): String = {
    val RegEx: StringBuilder = new StringBuilder(argInStr)
    var i: Int = 0
// Replace tab with space
    i = RegEx.indexOf("\t", i) 
    while (i >= 0) {RegEx.replace(i, i + 1, "") ; i = RegEx.indexOf("\t", i)  }
// Replace LineFeed with null
    i = RegEx.indexOf("\r", i)
    while (i >= 0) {RegEx.replace(i, i + 1, "") ; i = RegEx.indexOf("\r", i)  }
// Replace CarriageReturn with space
    i = RegEx.indexOf("\n", i)
    while (i >= 0) {RegEx.replace(i, i + 1, " "); i = RegEx.indexOf("\n", i)  }
// Replace double spaces with space
    i = RegEx.indexOf("  ", i)
    while (i >= 0) {RegEx.replace(i, i + 2, " ") ; i = RegEx.indexOf("  ", i) }
    RegEx.toString.trim()
  }

  def clearStringBuilder(inStr: StringBuilder): StringBuilder =
    inStr.delete(0, inStr.length)

  def RemoveAllWhitSpaceChars(arglnStr: String): String = {
    val in: Array[Char] = arglnStr.toCharArray()
    val out: Array[Char] = Array.ofDim[Char](arglnStr.length)
    val j: Int = -1
    for (i <- 0 until in.length if !java.lang.Character.isWhitespace(in(i))) {
      out(j) = in(i)
    }
      
    new String(out).trim()
  }

  def RemoveWhitSpaceChars(argInStr: String,
                           EnclosureChar: Char,
                           EscapeChar: Char): String = {
    val in: Array[Char] = argInStr.toCharArray()
    val out: Array[Char] = Array.ofDim[Char](argInStr.length)
    var j: Int = -1
    var inEnclosure: Boolean = false
    var ignore: Boolean = false
    for (i <- 0 until in.length) {
// Found Enclosure char
      if (in(i) == EnclosureChar) {
        inEnclosure = !inEnclosure
      } else if (in(i) == EscapeChar && inEnclosure && in(i + 1) == EnclosureChar) {
        ignore = true
      } else if ((java.lang.Character.isWhitespace(in(i)) && !inEnclosure)) {
        ignore = true
      }
      if (!ignore) {
        j += 1
        out(j) = in(i)
      }
      ignore = false
    }
    new String(out).trim()
  }

  def paddL(inStr: String, totLen: Int, paddChar: Char): String = {
    if (inStr.length >= totLen) inStr
    val miss: Int = totLen - inStr.length
    val sb: StringBuilder = new StringBuilder(totLen)
    for (i <- 0 until miss) {
      sb.append(paddChar)
    }
    sb.append(inStr)
    sb.toString
  }

  def paddR(inStr: String, totLen: Int, paddChar: Char): String = {
    if (inStr.length >= totLen) inStr
    val sb: StringBuilder = new StringBuilder(totLen)
    sb.append(inStr)
    for (i <- sb.length until totLen) {
      sb.append(paddChar)
    }
    sb.toString
  }

  def asFormattedStr(format: String, eol: String, objects: AnyRef*): String = {
    val f: Formatter = new Formatter()
    val s: String = f.format(format + "%s", objects, eol).toString
    f.close()
    s
  }

  def asFormattedStr(f: Formatter,
                     format: String,
                     eol: String,
                     objects: AnyRef*): String = {
    val s: String = f.format(format + "%s", objects, eol).toString
    s
  }

  def asFormattedStr(format: String, objects: AnyRef*): String = {
    val f: Formatter = new Formatter()
    try {
      val s: String = f.format(format, objects).toString
      s
    } catch {
      case e: Exception =>
        throw new Exception("Failed to format the values " + Arrays.toString(objects.asInstanceOf[Array[Object]]) + " using format:" +  format, e)

    } finally f.close()
  }

  def asFormattedStr(f: Formatter, format: String, objects: AnyRef*): String = {
    val s: String = f.format(format, objects).toString
    s
  }

  /**
    * Encodes the text into safe XML by replacing < > and & with XML tokens
    *
    * @param text  the text
    * @return the encoded text
    */
  def xmlEncode(text: String): String = {
    if (text == null) {
      return ""
    }
// must replace amp first, so we don't replace &lt; to amp later
    var res = text.replaceAll("&", "&amp;")
    res = res.replaceAll("\"", "&quot;")
    res = res.replaceAll("<", "&lt;")
    res = res.replaceAll(">", "&gt;")
    
    return res
  }

  def humanReadable(number: Long): String = {
    val absNumber: Long = Math.abs(number)
    var result: Double = number
    var suffix: String = ""
    if (absNumber < 1024L) {
      String.valueOf(number)
    }
    if (absNumber < 1048576L) {
      result = number / 1024.0D
      suffix = "k"
    } else if (absNumber < 1073741824L) {
      result = number / 1048576.0D
      suffix = "m"
    } else {
      result = number / 1073741824.0D
      suffix = "g"
    }
    new DecimalFormat("0.0").format(result) + suffix
  }

}

class StringUtils {

  

  def splitByLengthWithWordWrap(text: String, len: Int): ArrayList[String] = {
    val st: StringTokenizer = new StringTokenizer(text)
    var SpaceLeft: Int = len
    val SpaceWidth: Int = 1
    
    var sb: StringBuilder = new StringBuilder(len)
    val wrapList: ArrayList[String] = new ArrayList[String]()
    
    while (st.hasMoreTokens()) {
      var nxtWord: String = st.nextToken()
      while (nxtWord.length > len) {
        sb = new StringBuilder(len)
        val arr: Array[String] = splitByLength(nxtWord, len, 2)
        var i: Int = 0
        for ( i <- 0 until arr.length - 1) {
          wrapList.add(arr(i))
        }
        nxtWord = arr(i)
        SpaceLeft = len - nxtWord.length
      }
      if ((nxtWord.length + SpaceWidth) > SpaceLeft) {
        wrapList.add(sb.toString)
        sb = new StringBuilder(len)
        sb.append(nxtWord + " ")
        SpaceLeft = len - (nxtWord.length + SpaceWidth)
      } else {
        sb.append(nxtWord + " ")
        SpaceLeft -= (nxtWord.length + SpaceWidth)
      }
    }
    wrapList.add(sb.toString)
    return wrapList;
  }
  def getLongMemoryValueInBytes(str : String) : Long = {
        if (null == str || str.isEmpty) {
            0L
        } else {
            val txt = str.trim.toUpperCase
            if (txt.contains("GB")) {
                { txt.substring(0, txt.indexOf("GB")) }.toLong * 10737412742L
            } else if (txt.contains("MB")) {
                { txt.substring(0, txt.indexOf("MB")) }.toLong * 1048576L
            } else if (txt.contains("KB")) {
                { txt.substring(0, txt.indexOf("KB")) }.toLong * 1024L
            } else { txt.substring(0, txt.indexOf("KB")) }.toLong
        }
    }
}
