package fr.leroyer.athimon
package scalixObject

import scala.io.Source
import org.json4s.*
import org.json4s.native.JsonMethods.*
import fr.leroyer.athimon.scalixObject.*
import fr.leroyer.athimon.scalixObject.CacheObject.cacheMemoization

import java.awt.SecondaryLoop
import java.io.{File, PrintWriter}
import javax.lang.model.element.ModuleElement.Directive

trait Config(val api_key: String)

object ScalixObject extends App, Config("65c251744206a64af3ad031e4d5a4a48") {

  implicit val formats: Formats = DefaultFormats

  /**
   * utils function
   *
   * @param uri   the uri of the service to call
   * @param query the optional query parameter to add
   * @return the response of the service
   */
  def getData(uri: String, query: String = ""): JValue = {
    val url = s"https://api.themoviedb.org/3$uri?api_key=$api_key&language=fr-FR$query"
    val source = Source.fromURL(url)
    parse(source.mkString)
  }

  /**
   * get the actor id from the name and the first name
   *
   * @param name    the name of the actor
   * @param surname the first name of the actor
   * @return the id of the actor
   */
  def findActorId(actor: Actor): Actor = {
    // check if actor is in cache
    val cache = CacheObject.cacheReaderFactory("actor", s"${actor.name} ${actor.surname}")
    if (cache.getClass != JNothing.getClass) {
      val id = compact(render(cache \ "id")).toInt
      actor.id = id
      return actor
    }

    val data = getData("/search/person", s"&query=${actor.name}+${actor.surname}")
    if (data.getClass == JNothing.getClass) {
      return actor
    }
    val results = data \ "results"
    if (results.children.isEmpty) {
      return actor
    }
    val actorId = compact(render(results(0) \ "id")).toInt
    CacheObject.secondaryCacheFactoryWriter("actor", s"{\"id\":$actorId}", s"${actor.name}${actor.surname}")
    actor.id = actorId
    actor
  }


  /**
   * get the find the movies where the actor played
   *
   * @param actorId the id of the actor
   * @return a set of id + title of the movies
   */
  def findActorMovies(actorId: Int): Set[Movie] = {
    // check if actor is in cache
    val cache = CacheObject.cacheReaderFactory("actor-credits", actorId.toString)
    if (cache.getClass != JNothing.getClass) {
      return cache.children.map(c => Movie(compact(render(c \ "id")).toInt, compact(render(c \ "title")))).toSet
    }
    val data = getData(s"/person/$actorId/movie_credits")
    if (data.getClass == JNothing.getClass) {
      return Set(Movie(0, "No movies"))
    }
    val movies = (data \ "cast").extract[List[Movie]]
    CacheObject.secondaryCacheFactoryWriter("actor-credits", "[" + movies.map(movie => s"{\"id\":${movie.id}, \"title\": \"${movie.title}\"},").reduce((a, b) => a + b) + "]", actorId.toString)
    movies.map(e => Movie(e.id, e.title)).toSet
  }

  /**
   * find the director of a movie
   *
   * @param movieId the id of the movie
   * @return a pair of id + name of the director
   */
  def findMovieDirector(movieId: Int): Option[Director] = {
    // check if movieId is in cache
    val cache = CacheObject.cacheReaderFactory("director", movieId.toString)
    if (cache.getClass != JNothing.getClass) {
      val id = compact(render(cache \ "id")).toInt
      val name = compact(render(cache \ "name"))
      return Some(Director(id, name))
    }

    val data = getData(s"/movie/$movieId/credits")
    if (data.getClass == JNothing.getClass) {
      return None
    }
    (data \ "crew").find(_ \ "job" == JString("Director")) match
      case Some(result) =>
        val directorId = compact(render(result \ "id")).toInt
        val directorName = compact(render(result \ "name"))
        CacheObject.secondaryCacheFactoryWriter("director", s"{\"id\":$directorId,\"name\":$directorName}", movieId.toString)
        Some(Director(directorId, directorName))
      case None => None
  }

  /**
   * find the movies where the two actors played together
   *
   * @param actor1 the id of the first actor
   * @param actor2 the id of the second actor
   * @return a set of the director + title of the movies
   */
  def collaboration(actor1: Actor, actor2: Actor): Set[(Director, Movie)] = {
    val id1 = findActorId(actor1)
    val id2 = findActorId(actor2)

    if (id1.id == -1 || id2.id == -1) {
      return Set((null, null))
    }
    val cache = CacheObject.cacheReaderFactory("collaborations", s"${id1.id},${id2.id}")
    if (cache.getClass != JNothing.getClass) {
      return cache.extract[List[Collaboration]].map(collab => (collab.director, collab.movie)).toSet
    }
    val data = getData("/discover/movie", s"&with_cast=$id1,$id2")
    if (data.getClass == JNothing.getClass) {
      return Set((null, null))
    }

    if (compact(render(data \ "total_results")).toInt > 0) {
      val res = (data \ "results").extract[List[Movie]].map(movie =>
        (findMovieDirector(movie.id).get, movie)
      ).toSet
      CacheObject.secondaryCacheFactoryWriter("collaborations", "[" + res.map(collab => """{"director": {"id": %s, "name": %s}, "movie": {"id": %s, "title": "%s"}},""".format(collab._1.id, collab._1.name, collab._2.id, collab._2.title)).reduce((a, b) => a + b) + "]", s"${id1.id},${id2.id}")

      return res
    }
    Set((null, null))

  }


  val cacheMemoActorId = cacheMemoization[Actor, Actor](findActorId)
  val BradPitt = new Actor("Brad", "Pitt")

  var bradPittId = cacheMemoActorId(BradPitt)
  println(bradPittId)
  bradPittId = cacheMemoActorId(BradPitt)
  println(bradPittId)

  val cacheMemoMovieActor = cacheMemoization[Int, Set[Movie]](findActorMovies)
  val moviesBradPitt = cacheMemoMovieActor(bradPittId.id)
  println(moviesBradPitt)

  val cacheMemoDirectors = cacheMemoization[Int, Option[Director]](findMovieDirector)
  val moviesof550 = cacheMemoDirectors(550)
  println(moviesof550)

  val claireForlani = new Actor("Claire", "Forlani")
  val claireForlaniId = cacheMemoActorId(claireForlani)

  val cacheMemoCollaboration = cacheMemoization[(Actor, Actor), Set[(Director, Movie)]](collaboration)
  val collabClaireBrad = cacheMemoCollaboration((claireForlani, BradPitt))

  //4.1 On créé une liste de paires d'acteurs et ensuite on cherche leurs collaborations.
  // Pour finir on fait en sorte de compter et de sortir ceux qui ont le plus collaborations communes
  val acteurs = List(
    Actor("Orlando", "Bloom"),
    Actor("Monica", "Bellucci"),
    Actor("Johnny", "Depp"),
    Actor("Keira", "Knightley"),
    Actor("Tom", "Cruise"),
    Actor("Penelope", "Cruz"),
    BradPitt,
    claireForlani).map(actor => cacheMemoActorId(actor))
  val paires = (for (a <- acteurs) yield for (b <- acteurs) yield (a, b)).reduce((a, b) => a ++ b).filter(p => p._1 != p._2)
  println(paires)
  // faire un hashmap (actor, actor -> int) avec la paire et son nombre de collab, et filtrer le max
  val collabs = paires.map(pair => (pair, cacheMemoCollaboration(pair)))
    .filter(collab => collab._2.nonEmpty)
    .map(collab => (collab._1, collab._2.size))
    .maxBy(_._2)
  println(collabs)
}
