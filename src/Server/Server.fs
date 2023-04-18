open Fable.Remoting.Giraffe
open Fable.Remoting.Server
open Saturn
open Shared
open Microsoft.AspNetCore.Http
open Microsoft.Extensions.Configuration

let webApp =
    Remoting.createApi ()
    |> Remoting.withRouteBuilder Route.builder
    //|> Remoting.fromValue Api.dojoApi
    |> Remoting.fromContext (fun (ctx:HttpContext) -> 
        //Could have logger here: let logger = ctx.GetService<ILogger<IMusicStore>>()
        let config = ctx.GetService<IConfiguration>()
        Api.dojoApi config)
    |> Remoting.buildHttpHandler

let app = application {
    url "http://0.0.0.0:8085"
    use_router webApp
    use_static "public"
    use_gzip
}

run app