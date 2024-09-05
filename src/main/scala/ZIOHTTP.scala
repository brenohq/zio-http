import zhttp.http.Method.{GET, POST}
import zhttp.http._
import zhttp.http.middleware.Cors.CorsConfig
import zhttp.service.Server
import zio._

object ZIOHTTP extends ZIOAppDefault {

  val port = 9000

  val app: Http[Any, Nothing, Request, Response] = Http.collect[Request] {
    case Method.GET -> !! / "owls" => Response.text("Hello owls!")
  } @@ Middleware.csrfGenerate()

  val zApp: UHttpApp = Http.collectZIO[Request] {
    case Method.POST -> !! / "owls" =>
      Random.nextIntBetween(3, 5).map(n => Response.text("Hello " * n + ", owls!"))
  } @@ Middleware.csrfValidate()

  val authApp = Http.collect[Request] {
    case Method.GET -> !! / "secret" / "owls" => Response.text("The password is 123!")
  } @@ Middleware.basicAuth("brenohq", "123")

  val combined =  app ++ zApp

  // default middleware
  val wrapped = combined @@ Middleware.debug
  // request goes to middleware, then goes to combined

  // custom middleware
  val logging = combined @@ VerboseMiddleware.log

  // CORS
  val corsConfig = CorsConfig(
    anyOrigin = false,
    anyMethod = false,
    allowedOrigins = s => s.equals("localhost"),
    allowedMethods = Some(Set(GET, POST))
  )

  val corsEnabledHttp = combined @@ Middleware.cors(corsConfig) @@ VerboseMiddleware.log

  // CSRF
  // Middleware.csrfGenerate() and Middleware.csrfValidate()

  // Authentication

  val httpProgram = for {
    _ <- Console.printLine(s"Starting server at port: $port")
    + <- Server.start(port, authApp)
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
            _ <- ZIO.foreachDiscard(request.headers.toList) { header => Console.printLine(s"> ${header._1} - ${header._2}") }
          } yield request
        }
        .mapZIO[R1, E1, Response](r =>
          for {
            _ <- Console.printLine(s"< ${r.status}")
            _ <- ZIO.foreachDiscard(r.headers.toList)(header => Console.printLine(s"< ${header._1} - ${header._2}"))
          } yield r
        )
    }
  }
}
