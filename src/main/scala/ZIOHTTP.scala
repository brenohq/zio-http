import zhttp.http._
import zhttp.service.Server
import zio._

object ZIOHTTP extends ZIOAppDefault {

  val port = 9000

  val app: Http[Any, Nothing, Request, Response] = Http.collect[Request] {
    case Method.GET -> !! / "owls" => Response.text("Hello owls!")
  }

  val zApp: UHttpApp = Http.collectZIO[Request] {
    case Method.POST -> !! / "owls" =>
      Random.nextIntBetween(3, 5).map(n => Response.text("Hello " * n + ", owls!"))
  }

  val combined = app ++ zApp

  // default middleware
  val wrapped = combined @@ Middleware.debug
  // request goes to middleware, then goes to combined

  // custom middleware
  val logging = combined @@ VerboseMiddleware.log

  val httpProgram = for {
    _ <- Console.printLine(s"Starting server at port: $port")
    + <- Server.start(port, logging)
  } yield ()

  override def run: ZIO[Any, Throwable, Unit] = httpProgram
}

// request => contramap => http application => response => map => final response
//         |---------------------middleware---------------------|

object VerboseMiddleware {
  def log[R, E >: Exception]: Middleware[R, E, Request, Response, Request, Response] = new Middleware[R, E, Request, Response, Request, Response] {

    override def apply[R1 <: R, E1 >: E](http: Http[R1, E1, Request, Response]): Http[R1, E1, Request, Response] = {
      http
        .contramapZIO[R1, E1, Request] { request =>
          for {
            _ <- Console.printLine(s"${request.method} ${request.path} ${request.version}")
            _ <- ZIO.foreach(request.headers.toList) { header =>
              Console.printLine(s"> ${header._1} - ${header._2}")
            }
          } yield request
        }
        .mapZIO[R1, E1, Response](r =>
          for {
            _ <- Console.printLine(s"< ${r.status}")
            _ <- ZIO.foreach(r.headers.toList) { header => Console.printLine(s"< ${header._1} - ${header._2}") }
          } yield r
        )
    }
  }
}
