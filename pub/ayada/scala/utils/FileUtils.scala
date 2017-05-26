package pub.ayada.scala.utils

import java.util.Properties
import java.io.{ File, FileInputStream }
import scala.io.Source
import scala.collection.JavaConverters._

object FileUtils {
    def getPropsFromBashEnvFile(envFilePath: String): Properties = {
        val props = new Properties
        val lines = Source.fromFile(envFilePath)
            .getLines // read all the lines in the file
            .toList // convert to list[String]
            .filter(!_.isEmpty) // drop the empty lines
            .map(line => line.trim() // trim the line
                .replace("export", "") // remove the export command
                )
            .map(kvp => kvp.split("=")) // split the var=val by '='
            .map { case Array(k, v) => props.setProperty(k, v) } // set the property
        props
    }

    def getPropsMapFromBashEnvFile(envFilePath: String): Map[String, String] = {
        val props = getPropsFromBashEnvFile(envFilePath)
        props.stringPropertyNames().asScala.map { k =>
            (k, props.getProperty(k).trim())
        }.toMap

    }

    def getProps(propsFilePath: String): Properties = {
        val props = new Properties
        props.load(new FileInputStream(new File(propsFilePath)))
        props
    }
    def getPropsMap(propsFilePath: String): Map[String, String] = {
        val props = getProps(propsFilePath)
        props.stringPropertyNames().asScala.map { k =>
            (k, props.getProperty(k).trim())
        }.toMap
    }

    def main(args: Array[String]): Unit = {
        println(FileUtils.getPropsFromBashEnvFile("H:\\hadoop\\bin\\event_bpm\\psurf.env"))

    }
}
