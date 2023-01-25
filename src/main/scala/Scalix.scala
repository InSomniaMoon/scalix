package fr.leroyer.athimon

import scala.io.Source
import org.json4s._
import org.json4s.native.JsonMethods._
  import java.io.PrintWriter

trait Config(val api_key: String)

object Scalix extends App, Config("65c251744206a64af3ad031e4d5a4a48") {
  implicit val formats: Formats = DefaultFormats
  case class MovieLight(id: Int, title:String)

  var actorPCache : Map[(String, String), Int] = Map()
  var directorPCache : Map[Int,(Int, String)] = Map()

  def secondaryCacheFactoryWriter(cache:"actor"|"director", content: String ) = {
    val filename=s"data/$cache.json"
    val out = new PrintWriter(filename)
    out.print(content)
  }

  //  Finction = cherche dans P -> cherche dans secondary sinon call

//  def cacheFactory<T>(type: "actor"| "director", object: T) = {
//    type match {
//      case "actor" => actorPCache += (id, name) -> 0
//      case "director" => directorPCache += id -> (0, name)
//    }
//  }

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
    if (actorPCache.contains((name, surname))) {
      secondaryCacheFactoryWriter("actor", actorPCache((name, surname)).toString)
      return actorPCache.get((name, surname))
    }



    val data = getData("/search/person", s"&query=$name+$surname")
    if (data.getClass == JNothing.getClass) {
      return None
    }

    val results = data \ "results"
    if (results.children.isEmpty) {
      return None
    }

    val actorId = compact(render(results(0) \ "id")).toInt
    actorPCache += ((name, surname) -> actorId)
    Some(actorId)
  }


  def findActorMovies(actorId : Int): Set[(Int, String)] = {
    val data = getData(s"/person/$actorId/movie_credits")
    if (data.getClass == JNothing.getClass) {
      return Set((0, "No movie for this actor"))
    }
    val movies = (data \ "cast").extract[List[MovieLight]]
    movies.map(e =>  (e.id,e.title)).toSet
  }

  def findMovieDirector(movieId: Int): Option[(Int, String)] = {
    if(directorPCache.contains(movieId)){
      return directorPCache.get(movieId);
    }

    val data = getData(s"/movie/$movieId/credits")
    if (data.getClass == JNothing.getClass) {
      return None
    }
    (data \ "crew").find(_ \ "job" == JString("Director")) match
      case Some(result) => {
        val directorId = compact(render(result \ "id")).toInt
        val directorName = compact(render(result \ "name"))
        directorPCache += (movieId  ->(directorId, directorName))
        Some((directorId, directorName))
      }
      case None => None
  }

  def collaboration(actor1: FullName, actor2: FullName): Set[(String, String)] = {
    val id1 = findActorId(actor1.firstName,actor1.lastName)
    val id2 = findActorId(actor2.firstName,actor2.lastName)
    if (id1.isEmpty || id2.isEmpty) {
      return Set(("No actor found", "No actor found"))
    }
    val data = getData("/discover/movie",s"&with_cast=$id1,$id2")
    if (data.getClass == JNothing.getClass) {
      return Set(("No movies for those two actors", "No movie for those actors"))
    }
    val totalResults = compact(render(data\"total_results")).toInt

    if(totalResults > 0) {
      return (data \ "results").extract[List[MovieLight]].map(movie =>
       (findMovieDirector(movie.id).head(1),movie.title)
      ).toSet
    }

    Set(("No movies for those two actors", "No movie for those actors"))
  }

  val id = findActorId("Brad", "Pitt")
  println(id)
  val id2 = findActorId("Brad", "Pittt")
  println(id2)
  val moviesBradPitt = findActorMovies(287)
  println(moviesBradPitt)
  val dir = findMovieDirector(550)
  println(dir)

  val bradPitt = new FullName("Brad","Pitt")
  val claireForlani = new FullName("Claire","Forlani")
  val collaborationPittForlani = collaboration(bradPitt,claireForlani)
  println(collaborationPittForlani)

  val mattDamon = new FullName("Matt", "Damon")
  val collaborationPittDamon = collaboration(bradPitt,mattDamon)
  println(collaborationPittDamon)

  val pierreNiney = new FullName("Pierre", "Niney")
  val collaborationPittNiney = collaboration(bradPitt,pierreNiney)
  println(collaborationPittNiney)


}
