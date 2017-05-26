package scala.pub.ayada.scala.utils

object ArrayUtils {

    /**
     *  Returns new byte array by copying the data from input
     */
    def handleArraySize(src: Array[Byte], length: Int): Array[Byte] = {
        val dest: Array[Byte] = Array.ofDim[Byte](length)

        if (length <= src.length)
            System.arraycopy(src, 0, dest, 0, length)
        else System.arraycopy(src, 0, dest, 0, src.length)

        return dest
    }

    /**
     *  Returns new char array by copying the data from input
     * @param src
     * @param length
     * @return
     */
    def handleArraySize(src: Array[Char], length: Int): Array[Char] = {
        if (length == src.length)
            return src
        val dest: Array[Char] = Array.ofDim[Char](length)

        if (length <= src.length)
            System.arraycopy(src, 0, dest, 0, length)
        else System.arraycopy(src, 0, dest, 0, src.length)

        return dest
    }

    /**
     *  Returns new StringBuilder array by copying the data from input
     * @param src
     * @param length
     * @return
     */
    def handleArraySize(src: Array[StringBuilder],
                        length: Int): Array[StringBuilder] = {
        if (length == src.length)
            return src

        val dest: Array[StringBuilder] = Array.ofDim[StringBuilder](length)
        if (length <= src.length)
            System.arraycopy(src, 0, dest, 0, length)
        else System.arraycopy(src, 0, dest, 0, src.length)

        return dest
    }
    /**
     * Returns new byte array by copying the data from input from the input start position + length specified
     * @param src
     * @param startPos
     * @param length
     * @return
     * @throws Exception
     */
    def ArrayCopy(src: Array[Byte], startPos: Int, length: Int): Array[Byte] = {
        if (src == null)
            return null

        val dest: Array[Byte] = Array.ofDim[Byte](length - startPos + 1)
        try
            System.arraycopy(src, startPos, dest, 0, length)
        catch {
            case e: ArrayIndexOutOfBoundsException => {
                ShowParentExceptio(e)
                throw new Exception(
                    "Failed to copy the Data from Source Array to Destination Array\n" +
                        "Source Start Position : " + startPos +
                        "  length : " + length + "\n" +
                        disp(src))
            }
        }
        return dest
    }

    private def disp(BufArr: Array[Byte]): String = {
        if (BufArr == null)
            return "(null)"

        val sb: StringBuilder = new StringBuilder()

        for (i <- 0 until BufArr.length)
            sb.append((BufArr(i) & 0xFF).toChar)

        return sb.toString
    }

    private def ShowParentExceptio(e: Exception): Unit = {
        System.err.println(e.getMessage)
        e.printStackTrace(System.err)
    }

}
