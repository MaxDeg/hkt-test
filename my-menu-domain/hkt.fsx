
// ----------------------------------------------------------------------------
// -| HigherKindedType
// ----------------------------------------------------------------------------
type HKT<'m, 't> = HigherKindedType of payload: obj

module HigherKindedType =
  let inline private assoc<'m, 'a, 'ma when 'm : (static member Assign : HKT<'m, 'a> * 'ma -> unit)> = ()
  
  let inline pack (t : 'mt) : HKT<'m, 't> =
    do assoc<'m, 't, 'mt>
    HigherKindedType t

  let inline unpack (HigherKindedType t : HKT<'m, 't>) : 'mt =
    do assoc<'m, 't, 'mt>
    unbox t

  let inline (|Unpack|) t = unpack t

type IMonad<'m> =
  abstract member Return : 'x -> HKT<'m, 'x>
  abstract member ReturnFrom : HKT<'m, 'x> -> HKT<'m, 'x>
  abstract member Bind: ('x -> HKT<'m, 'fx>) -> HKT<'m, 'x> -> HKT<'m, 'fx>

type HKTMonadBuilder<'m when 'm :> IMonad<'m>>(monad : 'm) =
  member __.Return(x) = monad.Return x
  member __.ReturnFrom(x: HKT<'m, _>) : HKT<'m, _> = monad.ReturnFrom x
  member __.Bind(m : HKT<'m, 'x>, f : 'x -> HKT<'m, 'fx>) : HKT<'m, 'fx> = monad.Bind f m

let monad m = HKTMonadBuilder(m)


// ----------------------------------------------------------------------------
// -| HigherKindedType Examples
// ----------------------------------------------------------------------------

module Domain =
  type NonEmptyString = NonEmptyString of string
  type IngredientId = IngredientId of int

  type Ingredient =
    { id    : IngredientId
      name  : NonEmptyString
    }

  type ISearchIngredientRepository<'m> =
    abstract member search : IngredientId -> HKT<'m, Ingredient>

  type IAddIngredientRepository<'m> =
    abstract member add : Ingredient -> HKT<'m, IngredientId>

  let getNameFromId (sir : 'm when 'm :> ISearchIngredientRepository<_>) id : HKT<'m, NonEmptyString> =
    monad sir {
      let ingId = IngredientId id
      let! ingredient = sir.search ingId
      return ingredient.name
    }

  let create (air : 'm when 'm :> IAddIngredientRepository<_>) ingredient : HKT<'m, IngredientId> =
    monad air { return! air.add ingredient }

module Infrastructure =
  open Domain

  module Test =
    type AppM = AppM with
      static member Assign (_ : HKT<AppM, 't>, _: 't) = ()

      interface IMonad<AppM> with
        member __.Return x = HigherKindedType.pack x
        member __.ReturnFrom x = x
        member __.Bind f x = f (HigherKindedType.unpack x)
      
      interface ISearchIngredientRepository<AppM> with
        member __.search id =
          HigherKindedType.pack
            { id    = id
              name  = sprintf "Name of %A" id |> NonEmptyString
            }

      interface IAddIngredientRepository<AppM> with
        member __.add _ = HigherKindedType.pack (IngredientId 1)

  module Option =
    type AppOptionM = AppOptionM with
      static member Assign(_ : HKT<AppOptionM, 't>, _: 't option) = ()

      interface IMonad<AppOptionM> with
        member __.Return x = HigherKindedType.pack (Some x)
        member __.ReturnFrom x = x
        member __.Bind f (HigherKindedType.Unpack x) = 
          match x with
          | Some x  -> f x
          | None    -> HigherKindedType.pack None

      interface ISearchIngredientRepository<AppOptionM> with
        member __.search id =
          { id    = id
            name  = sprintf "Name of %A" id |> NonEmptyString
          }
          |> Some
          |> HigherKindedType.pack

      interface IAddIngredientRepository<AppOptionM> with
        member __.add _ = HigherKindedType.pack (Some (IngredientId 1))

  module Async =
    type AppAsyncM = AppAsyncM with
      static member Assign (_ : HKT<AppAsyncM, 't>, _: Async<'t>) = ()

      interface IMonad<AppAsyncM> with
        member __.Return x = HigherKindedType.pack (async.Return x)
        member __.ReturnFrom x = x
        member __.Bind f x =
          async {
            let! x = HigherKindedType.unpack x
            let fx = f x
            return! HigherKindedType.unpack fx
          }
          |> HigherKindedType.pack

      interface ISearchIngredientRepository<AppAsyncM> with
        member __.search id = 
          { id    = id
            name  = sprintf "Name of %A" id |> NonEmptyString
          }
          |> async.Return
          |> HigherKindedType.pack

      interface IAddIngredientRepository<AppAsyncM> with
        member __.add _ = HigherKindedType.pack (async.Return (IngredientId 1))

open Domain

let ingredient =
  { id    = IngredientId 1
    name  = NonEmptyString "Name of 1"
  }

module Tests =
  open Infrastructure.Test
  let id = create AppM ingredient |> HigherKindedType.unpack

module Option =
  open Infrastructure.Option
  let idOption = create AppOptionM ingredient |> HigherKindedType.unpack

module Async =
  open Infrastructure.Async
  let idAsync = create AppAsyncM ingredient |> HigherKindedType.unpack

// ----------------------------------------------------------------------------
// -| Examples
// ----------------------------------------------------------------------------

// type AppData =
//   {
//   }
