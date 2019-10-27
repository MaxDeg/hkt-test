[<AutoOpen>]
module Prelude

open System

open HigherKindedTypes
open Giraffe.ComputationExpression.Configuration

open Microsoft.Extensions.Configuration

type ServiceMetadata =
  { serviceName       : string
    serviceInstanceId : string 
  }

[<EntryPoint>]
let main _ =
  let f = getNameFromId' 1

  HigherKindedType.runHKT f AppAsyncM
  |> Async.RunSynchronously
  |> printfn "%A"

  let config = configuration {
    require_ini_file "appsettings.ini"

    // configure<ServiceMetadata> "service:metadata"
  }

  printfn "%A" <| config.GetValue<string>("service:metadata:serviceName", "oups")

  0
