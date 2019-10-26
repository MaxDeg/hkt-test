[<AutoOpen>]
module Prelude

open System


type HKT<'m, 't when 'm :> IMonad<'m>> =
  HigherKindedType of payload: ('m -> obj)

and IMonad<'m when 'm :> IMonad<'m>> =
  abstract member Return : 'x -> HKT<'m, 'x>
  abstract member Bind: ('x -> HKT<'m, 'fx>) -> HKT<'m, 'x> -> HKT<'m, 'fx>

module HigherKindedType =  
  let inline private assoc<'m, 'a, 'ma when 'm : (static member Assign : HKT<'m, 'a> * 'ma -> unit)> = ()
  
  let inline pack (t : 'mt) : HKT<'m, 't> =
    do assoc<'m, 't, 'mt>
    HigherKindedType (fun _ -> unbox t)

  let inline unpack (HigherKindedType t : HKT<'m, 't>) : ('m -> 'mt) =
    do assoc<'m, 't, 'mt>
    t >> unbox

  let inline (|Unpack|) x = unpack x

  let inline runHKT (Unpack hkt) m = hkt m

  let inline bind<'m, 'x when 'm :> IMonad<'m>>
    (f : 'm -> HKT<'m, 'x>) : HKT<'m, 'x> = 
      (fun (m : 'm) ->
        let (HigherKindedType fn) = f m
        fn m)
      |> HigherKindedType


type HKTBuilder<'m when 'm :> IMonad<'m>>() =
  member _.ReturnFrom(x: HKT<'m, _>) : HKT<'m, _> = x

  member _.Return(x : 'x) : HKT<'m, 'x> =
    (fun (m : 'm) ->
      let (HigherKindedType fn) = m.Return x
      fn m)
    |> HigherKindedType

  member _.Bind(h : HKT<'m, 'x>, f : 'x -> HKT<'m, 'fx>) : HKT<'m, 'fx> = 
    (fun (m : 'm) -> 
      let (HigherKindedType fn) = m.Bind f h
      fn m)
    |> HigherKindedType


let hkt<'m when 'm :> IMonad<'m>> = HKTBuilder<'m>()




type NonEmptyString = NonEmptyString of string
type IngredientId = IngredientId of int

type Ingredient =
  { id    : IngredientId
    name  : NonEmptyString
  }

type IAcquirerIngredient<'m when 'm :> IMonad<'m>> =
  abstract member GetIngredientById : IngredientId -> HKT<'m, Ingredient option>

type ILogger<'m when 'm :> IMonad<'m>> =
  abstract member Log : string -> string -> HKT<'m, unit>

let inline getIngredientById<'m when 'm :> IAcquirerIngredient<'m>> id =
  HigherKindedType.bind<'m, _> (fun m -> m.GetIngredientById id)

let inline logInfo<'m when 'm :> ILogger<'m>> msg =
  HigherKindedType.bind<'m, _> (fun m -> m.Log "Info" msg)

let getNameFromId<'m when 'm :> IAcquirerIngredient<'m>> id =
  hkt<'m> {
    let ingId = IngredientId id
    let! ingredient = getIngredientById<'m> ingId
    
    match ingredient with
    | Some i -> return Some i.name
    | None  -> return None
  }


let getNameFromId' id =
  hkt {
    let ingId = IngredientId id
    do! logInfo "Logging from getNameFromId"
    let! ingredient = getIngredientById ingId
    
    match ingredient with
    | Some i -> return Some i.name
    | None  -> return None
  }

open HigherKindedType

type AppAsyncM = AppAsyncM with
  static member Assign (_ : HKT<AppAsyncM, 't>, _: Async<'t>) = ()

  interface IMonad<AppAsyncM> with
    member _.Return x = 
      async.Return(x) |> pack

    member this.Bind f (Unpack hx) =
      async {
        let! x = hx this
        let (Unpack fx) = f x
        return! fx this
      }
      |> pack

  interface IAcquirerIngredient<AppAsyncM> with
    member _.GetIngredientById (IngredientId id) =
      { id = IngredientId id
        name = NonEmptyString (sprintf "%i" id)
      }
      |> Some
      |> async.Return
      |> pack

  interface ILogger<AppAsyncM> with
    member _.Log level msg =
      printfn "[%s] %s" level msg
      |> async.Return
      |> pack

[<EntryPoint>]
let main _ =
  let f = getNameFromId' 1

  runHKT f AppAsyncM
  |> Async.RunSynchronously
  |> printfn "%A"

  0
