module IpGeoLocations

open HigherKindedTypes

open System.Threading.Tasks
open FSharp.Control.Tasks.ContextInsensitive


/// --| Functional Core
module Core =
  type IpTree =
    { v4Tree : obj
      v6Tree : obj
    }

  module v4 =
    type IpV4 = IpV4 of string

    let lookup ipTree ip = "BE"

  module v6 =
    type IpV6 = IpV6 of string

    let lookup ipTree ip = "FR"

  type Ip = IpV4 of v4.IpV4 | IpV6 of v6.IpV6

  let parse value = IpV4 (v4.IpV4 value)

  module MaxMind =
    type MD5CheckSum = MD5CheckSum of string


/// --| HttpHandler Computation Expressions
module Giraffe =
  open Giraffe
  open Microsoft.AspNetCore.Http

  let getBody _ (ctx : HttpContext) = ctx.Request.Body

  // let decodeBodyWith codec next ctx =
  //   task {
  //     let reader = new StreamReader(getBody next ctx)
  //     let! body = reader.ReadToEndAsync()
      
  //     return Codec.decode codec body
  //   }

  // let setBodyWithCodec codec value =
  //   setHttpHeader "Content-Type" "application/json"
  //   >=> setBodyFromString (Codec.encode codec value)

  //https://thomashoneyman.com/guides/real-world-halogen/push-effects-to-the-edges/

  type HktHttpHandler<'m when 'm :> IMonad<'m>>() =
    let hktBuilder = HKTBuilder<'m>()

    member _.Bind(x : HKT<'m, 'x>, f) : HKT<'m, _> =
      hktBuilder.Bind(x, f)

    member _.Return x : HKT<'m, _> = hktBuilder.Return x

    member _.ReturnFrom x : HKT<'m, _> = x

    member _.Bind(x : HttpHandler, f : 'a -> HKT<'m, HttpHandler>) : HKT<'m, HttpHandler> =
      hktBuilder.Bind(HigherKindedType (fun _ -> unbox x), f)

    member _.ReturnFrom x : HKT<'m, _> =
      HigherKindedType (fun _ -> unbox x)

  let httpHandler<'m when 'm :> IMonad<'m>> = HktHttpHandler<'m>()


/// --| Imperative Shell

type Has<'m, 't when 'm :> IMonad<'m>> =
  abstract member ask : unit -> HKT<'m, 't>

let inline ask<'m, 't when 'm :> Has<'m, 't>> =
  HigherKindedType.bind<'m, _> (fun m -> m.ask ())

open Giraffe
open Core

let lookupHandler ip : HKT<'m, HttpHandler> =
  httpHandler {
    let! ipTree = ask<_, IpTree>
    let location =
      match parse ip with
      | IpV4 ipV4 -> v4.lookup ipTree.v4Tree ipV4
      | IpV6 ipV6 -> v6.lookup ipTree.v6Tree ipV6

    do! setStatusCode 200
    return! setBodyFromString location
  }

/// --| Application

open Core
open HigherKindedType

type Env =
  { ipTree  : IpTree
  }
  
type App(env : Env) =
  static member Assign (_ : HKT<App, 't>, _: Task<'t>) = ()

  interface IMonad<App> with
    member _.Return x = 
      Task.FromResult x |> pack

    member this.Bind (f : 'x -> HKT<App, _>) (Unpack (hx : App -> Task<'x>)) =
      task {
        let! x = hx this
        let (Unpack fx) = f x
        return! fx this
      }
      |> pack

  interface Has<App, IpTree> with
    member _.ask () = env.ipTree |> Task.FromResult |> pack


(*
let adminInterface = pipeline {
  use_open_api

  env "Development" (pipeline {
    use_open_api_ui
  })
}

application<App> {
  use_nlog

  use_esf (esf {
    with_akms
    with_consul

    with_sql_healthchecks
    with_health_check<XXHealthCheck>
    with_readiness_check<XXReadinessCheck>

    with_service_client<XXApi>
  })

  plug adminInterface

  use_router (router {
    getf "/ip-locations/%s" ..
    get "/ip-locations" ..
  })
}
*)
