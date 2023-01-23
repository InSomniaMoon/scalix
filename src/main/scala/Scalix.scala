//package fr.leroyer.athimon

import scala.io.Source
import org.json4s._
import org.json4s.native.JsonMethods._

trait Config(val api_key: String)

object Scalix extends App, Config("65c251744206a64af3ad031e4d5a4a48") {
  implicit val formats: Formats = DefaultFormats
  case class MovieLight(id: Int, title:String);


  def getData(uri: String, query: String = "") = {
    val url = s"https://api.themoviedb.org/3$uri?api_key=$api_key&language=fr-FR$query"
    val source = Source.fromURL(url)
    println(url)
    val contents  = source.mkString
    parse(contents)
  }

  def findActorId(name: String, surname: String): Option[Int] = {
    val data = getData("/search/person", s"&query=$name+$surname")
    if (data.getClass == JNothing.getClass) {
      return None
    }
    val totalResults = compact(render(data\"total_results")).toInt;
    if(totalResults > 0) {
      val results = (data \ "results")

      Option(compact(render(results(0) \ "id")).toInt)
    }else{
      return None
    }

  }

  def findActorMovies(actorId : Int): Set[(Int, String)] = {
    val data = getData(s"/person/$actorId/movie_credits", "")
    if (data.getClass == JNothing.getClass) {
      return Set((0, "No movie for this actor"));
    }
    val movies = (data \ "cast").extract[List[MovieLight]];
    var results = Set[(Int, String)]();
    movies.map(e => results += (e.id,e.title));
    return results;

  }


  val id = findActorId("Brad", "Pitt")
  println(id)
  val id2 = findActorId("Brad", "Pittt")
  println(id2)
  val moviesBradPitt = findActorMovies(287);
  println(moviesBradPitt);
}
