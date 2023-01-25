package fr.leroyer.athimon

import scala.io.Source
import org.json4s._
import org.json4s.native.JsonMethods._

trait Config(val api_key: String)

object Scalix extends App, Config("65c251744206a64af3ad031e4d5a4a48") {
  implicit val formats: Formats = DefaultFormats
  case class MovieLight(id: Int, title:String);

  /**
   * utils function
   * @param uri the uri of the service to call
   * @param query the optional query parameter to add
   * @return the response of the service
   */
  def getData(uri: String, query: String = ""): JValue = {
    val url = s"https://api.themoviedb.org/3$uri?api_key=$api_key&language=fr-FR$query"
    val source = Source.fromURL(url)
    val contents  = source.mkString
    parse(contents)
  }

  def findActorId(name: String, surname: String): Option[Int] = {
    val data = getData("/search/person", s"&query=$name+$surname")
    if (data.getClass == JNothing.getClass) {
      return None
    }

    val results = data \ "results"
    if (results.children.isEmpty) {
      return None
    }
    Option(compact(render(results(0) \ "id")).toInt)
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

  def findMovieDirector(movieId: Int): Option[(Int, String)] = {
    val data = getData(s"/movie/$movieId/credits")

    val results = (data \ "crew").find( _ \ "job" == JString("Director"))
    results match
      case Some(result) => Option((compact(render(result \ "id")).toInt, compact(render(result \ "name"))))
      case None => None
  }

  def collaboration(actor1: FullName, actor2: FullName): Set[(String, String)] = {
    val id1 = findActorId(actor1.firstName,actor1.lastName);
    val id2 = findActorId(actor2.firstName,actor2.lastName);
    val data = getData("/discover/movie",s"&with_cast=${id1.head},${id2.head}");
    if (data.getClass == JNothing.getClass) {
      return Set(("No movies for those two actors", "No movie for those actors"));
    }
    val totalResults = compact(render(data\"total_results")).toInt;
    if(totalResults > 0) {
      val movies = (data \ "results").extract[List[MovieLight]];
      var results = Set[(String, String)]();
      movies.map((movie) => {
        var director = findMovieDirector(movie.id);
        results += (director.head(1),movie.title)
      });
      results;
    }else{
      Set(("No movies for those two actors", "No movie for those actors"));
    }


  }

  val id = findActorId("Brad", "Pitt")
  println(id)
  val id2 = findActorId("Brad", "Pittt")
  println(id2)
  val moviesBradPitt = findActorMovies(287);
  println(moviesBradPitt);
  val dir = findMovieDirector(550)
  println(dir)

  val bradPitt = new FullName("Brad","Pitt");
  val claireForlani = new FullName("Claire","Forlani");
  val collaborationPittForlani = collaboration(bradPitt,claireForlani);
  println(collaborationPittForlani);

  val mattDamon = new FullName("Matt", "Damon");
  val collaborationPittDamon = collaboration(bradPitt,mattDamon);
  println(collaborationPittDamon);

  val pierreNiney = new FullName("Pierre", "Niney");
  val collaborationPittNiney = collaboration(bradPitt,pierreNiney);
  println(collaborationPittNiney);


}
