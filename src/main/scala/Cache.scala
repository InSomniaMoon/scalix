package fr.leroyer.athimon

import scala.io.Source
import org.json4s.*
import org.json4s.Formats
import org.json4s.native.JsonMethods.*

import java.io.{File, PrintWriter}

object Cache {

  implicit val formats: Formats = DefaultFormats

  var actorPCache: Map[(String, String), Int] = Map()
  var actorCreditsPCache: Map[Int, Set[(Int, String)]] = Map()
  var directorPCache: Map[Int, (Int, String)] = Map()
  val path = File(".").getCanonicalPath + "/src/main"

  case class MovieLight(id: Int, title: String)
  
  // file cache reader
  def secondaryCacheFactoryReader(cache: "actor" | "actor-credits" | "director", id: String): String = {
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

  def secondaryCacheFactoryWriter(cache: "actor" | "actor-credits" | "director", content: String, id: String): Unit = {
    val filename = s"$path/data/$cache$id.json"

    if (!File(filename).exists()) {
      val file = new File(filename)

      file.createNewFile()
    }
    val out = new PrintWriter(filename)
    out.print(content)
    out.close()
  }


  def cacheReaderFactory(cache: "actor" | "actor-credits" | "director", id: String): JValue = {
    // look in memory cache
    cache match {
      case "actor" =>
        val name = id.split(" ").head
        val surname = id.split(" ").tail.head
        actorPCache.get(name, surname) match {
          case Some(value) =>
            return parse("""{"id":%d}""".format(value))
          case _ =>
        }
      case "director" =>
        directorPCache.get(id.toInt) match {
          case Some(_) =>
            val (mid, name) = directorPCache(id.toInt)
            return parse("""{"id":%d,"name":%s}""".format(mid, name))
          case _ =>
        }

      case "actor-credits" => actorCreditsPCache.get(id.toInt) match {
        case Some(_) =>
          val sCredits = "[" + actorCreditsPCache(id.toInt).map(x => """{"id":%d,"title":%s},""".format(x._1, x._2)).reduce(_ + _) + "]"
          return parse(sCredits)
        case _ =>
      }
    }
    println(s"$id NOT IN MEMORY CACHE")

    // Look in file cache
    val fileCache = secondaryCacheFactoryReader(cache, id.split(" ").reduce(_ + _))
    if (fileCache != null) {
      val parsedCache = parse(fileCache)
      cache match {
        case "actor" =>
          val actorId = (parsedCache \ "id").extract[Int]
          val name = id.split(" ").head
          val surname = id.split(" ").tail.head
          actorPCache += ((name, surname) -> actorId)

        case "actor-credits" =>

          actorCreditsPCache += (id.toInt -> parsedCache.asInstanceOf[JArray].arr.map(x => (compact(render(x \ "id")).toInt, compact(render(x \ "title")))).toSet)

        case "director" =>
          val id = (parsedCache \ "id").extract[Int]
          val name = (parsedCache \ "name").extract[String]
          directorPCache += (id -> (id, name))
      }
      return parsedCache
    }
    println(s"$id NOT IN FILE CACHE")
    JNothing
  }

}
