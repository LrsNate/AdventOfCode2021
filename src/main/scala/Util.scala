package fr.nate

import scala.io.Source

object Util {
  def getLines(prefix: String): List[String] = {
    val url = getClass.getClassLoader.getResource(f"${prefix}_input.txt")
    val source = Source.fromURL(url)
    return source.getLines.toList
  }
}
