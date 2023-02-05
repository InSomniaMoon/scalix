package fr.leroyer.athimon
package scalixObject

import org.json4s.*
import org.json4s.native.JsonMethods.*

import java.io.{File, PrintWriter}
import scala.io.Source

object CacheObject {

  implicit val formats: Formats = DefaultFormats

  val path = File(".").getCanonicalPath + "/src/main"
  // Using memoization

  def cacheMemoization[S, T](f: S => T): S => T =
    var cache = Map[S, T]()
    (s: S) =>
      cache.get(s) match
        case None => val t = f(s); cache += (s, t); t
        case Some(t) => println("got it"); t


  // file cache reader
  def secondaryCacheFactoryReader(cache: "actor" | "actor-credits" | "director" | "collaborations", id: String): String = {
    val filename = s"$path/data/$cache$id.json"
    try {
      val file = Source.fromFile(filename)
      val content = file.mkString
      file.close()
      return content
    } catch
      case _: Exception => return null

    val out = Source.fromFile(filename)
    val content = out.mkString
    out.close()
    content
  }

  def secondaryCacheFactoryWriter(cache: "actor" | "actor-credits" | "director" | "collaborations", content: String, id: String): Unit = {
    val filename = s"$path/data/$cache$id.json"
    if (!File(filename).exists()) {
      val file = new File(filename)
      file.createNewFile()
    }
    val out = new PrintWriter(filename)
    out.print(content)
    out.close()
  }




  def cacheReaderFactory(cache: "actor" | "actor-credits" | "director" | "collaborations", id: String): JValue = {
    // Look in file cache
    val fileCache = secondaryCacheFactoryReader(cache, id.split(" ").reduce(_ + _))
    if (fileCache != null) {
     return parse(fileCache)
    }
    println(s"$id NOT IN FILE CACHE")
    JNothing
  }

}
