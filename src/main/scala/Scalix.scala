package fr.leroyer.athimon

import scala.io.Source
import org.json4s.*
import org.json4s.native.JsonMethods.*

import java.io.{File, PrintWriter}

trait Config(val api_key: String)

object Scalix extends App, Config("65c251744206a64af3ad031e4d5a4a48") {
  implicit val formats: Formats = DefaultFormats

  case class MovieLight(id: Int, title: String)

  var actorPCache: Map[(String, String), Int] = Map()
  var actorCreditsPCache: Map[Int, Set[(Int, String)]] = Map()
  var directorPCache: Map[Int, (Int, String)] = Map()
  val path = File(".").getCanonicalPath + "/src/main"

  // file cache writer : Usage de id en tant que string pour cotenter l'ajoute par non ou id
  // TODO : refacto le writer pour écrire dans les deux caches à la fois !

  def secondaryCacheFactoryWriter(cache: "actor" | "actor-credits" | "director", content: String, id: String): Unit = {
    val filename = s"$path/data/$cache\u0024$id.json"

    if (!File(filename).exists()) {
      val file = new File(filename)

      file.createNewFile()
    }
    val out = new PrintWriter(filename)
    out.print(content)
    out.close()
  }


  // file cache reader
  def secondaryCacheFactoryReader(cache: "actor" | "actor-credits" | "director", id: String): String = {
    val filename = s"$path/data/$cache\u0024$id.json"
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
   *         Terminated
   */
  def findActorId(name: String, surname: String): Option[Int] = {
    // check if actor is in cache
    val cache = cacheReaderFactory("actor", s"$name $surname")
    if (cache.getClass != JNothing.getClass) {
      val id = compact(render(cache \ "id")).toInt
      return Some(id)
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
    secondaryCacheFactoryWriter("actor", s"{\"id\":$actorId}", s"$name$surname")

    Some(actorId)
  }


  /**
   * get the find the movies where the actor played
   *
   * @param actorId the id of the actor
   * @return a set of id + title of the movies
   */
  def findActorMovies(actorId: Int): Set[(Int, String)] = {
    // check if actor is in cache
    val cache = cacheReaderFactory("actor-credits", actorId.toString)
    if (cache.getClass != JNothing.getClass) {
      return cache.children.map(c => (compact(render(c \ "id")).toInt, compact(render(c \ "title")))).toSet
    }
    val data = getData(s"/person/$actorId/movie_credits")
    if (data.getClass == JNothing.getClass) {
      return Set((0, "No movie for this actor"))
    }
    val movies = (data \ "cast").extract[List[MovieLight]]
    secondaryCacheFactoryWriter("actor-credits", "[" + movies.map(movie => s"{\"id\":${movie.id}, \"title\": \"${movie.title}\"},").reduce((a, b) => a + b) + "]", actorId.toString)
    movies.map(e => (e.id, e.title)).toSet
  }

  /**
   * find the director of a movie
   *
   * @param movieId the id of the movie
   * @return a pair of id + name of the director
   *         terminated
   */
  def findMovieDirector(movieId: Int): Option[(Int, String)] = {
    // check if movieId is in cache
    val cache = cacheReaderFactory("director", movieId.toString)
    if (cache.getClass != JNothing.getClass) {
      val id = compact(render(cache \ "id")).toInt
      val name = compact(render(cache \ "name"))
      return Some((id, name))
    }

    val data = getData(s"/movie/$movieId/credits")
    if (data.getClass == JNothing.getClass) {
      return None
    }
    (data \ "crew").find(_ \ "job" == JString("Director")) match
      case Some(result) =>
        val directorId = compact(render(result \ "id")).toInt
        val directorName = compact(render(result \ "name"))
        secondaryCacheFactoryWriter("director", s"{\"id\":$directorId,\"name\":$directorName}", movieId.toString)
        Some((directorId, directorName))
      case None => None
  }

  /**
   * find the movies where the two actors played together
   *
   * @param actor1 the id of the first actor
   * @param actor2 the id of the second actor
   * @return a set of the director + title of the movies
   *         terminated
   */
  def collaboration(actor1: FullName, actor2: FullName): Set[(String, String)] = {
    val id1 = findActorId(actor1.firstName, actor1.lastName)
    val id2 = findActorId(actor2.firstName, actor2.lastName)
    if (id1.isEmpty || id2.isEmpty) {
      return Set(("No actor found", "No actor found"))
    }
    val data = getData("/discover/movie", s"&with_cast=$id1,$id2")
    if (data.getClass == JNothing.getClass) {
      return Set(("No movies for those two actors", "No movie for those actors"))
    }
    val totalResults = compact(render(data \ "total_results")).toInt

    if (totalResults > 0) {
      return (data \ "results").extract[List[MovieLight]].map(movie =>
        (findMovieDirector(movie.id).head(1), movie.title)
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

  val moviesBradPitt2 = findActorMovies(287)
  println(moviesBradPitt2)

  println("\n looking for director 550")
  val dir = findMovieDirector(550)
  println("director found : ")
  println(dir)

  val bradPitt = new FullName("Brad", "Pitt")
  val claireForlani = new FullName("Claire", "Forlani")
  val collaborationPittForlani = collaboration(bradPitt, claireForlani)
  println(collaborationPittForlani)

  val mattDamon = new FullName("Matt", "Damon")
  val collaborationPittDamon = collaboration(bradPitt, mattDamon)
  println(collaborationPittDamon)

  val pierreNiney = new FullName("Pierre", "Niney")
  val collaborationPittNiney = collaboration(bradPitt, pierreNiney)
  println(collaborationPittNiney)
}
