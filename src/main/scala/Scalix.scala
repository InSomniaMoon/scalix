package fr.leroyer.athimon

import scala.io.Source
import org.json4s._
import org.json4s.native.JsonMethods._

trait Config(val api_key: String)

object Scalix extends App, Config("65c251744206a64af3ad031e4d5a4a48") {

  def getData(uri: String, query: String = "") = {
    val url = s"https://api.themoviedb.org/3$uri?api_key=$api_key&language=fr-FR$query"
    val source = Source.fromURL(url)
    println(url)
    val contents  = source.mkString
    parse(contents)
  }

  def findActorId(name: String, surname: String): Option[Int] = {
    val data = getData("/search/person", s"&query=$name+$surname")
    if (data.getClass != JNothing.getClass) {
      return None
    }

    val results = (data \ "results")

    Option(compact(render(results(0) \ "id")).toInt)
  }

  val id = findActorId("Brad", "Pitt")
  println(id)
  val id2 = findActorId("Brad", "Pittt")
  println(id2)
}
