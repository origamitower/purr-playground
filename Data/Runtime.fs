module Purr.Data.Runtime

open Purr.Data.Ast
open System.Collections.Generic


type PValue() = class end


type Environment(parent: Environment option) =
  let mutable bindings : Map<string, PValue> = Map.empty

  member __.Parent = parent

  member __.Lookup (name:string) = 
    Map.tryFind name bindings

  member __.Define (name:string, value: PValue) =
    bindings <- Map.add name value bindings


type PSymbol(description: string) =
  inherit PValue()

  member __.Description = description


type PClosure(environment: Environment, parameters: Name list, body: Expr) =
  inherit PValue()

  member __.Environment = environment
  member __.Parameters = parameters
  member __.Body = body


type PLabel =
  | LName of string
  | LSymbol of PSymbol


type PRecord(parent: PRecord option, messageList: (PLabel * PClosure) list) =
  inherit PValue()

  let messages = 
    let dict = new Dictionary<PLabel, PClosure>()
    for (label, closure) in messageList do
      dict.Add(label, closure)
    dict
  
  member __.Parent = parent
  member __.Messages = messages

  member __.GetMethod(label: PLabel, arguments: PValue list) =
    try
      messages.Item(label)
    with
    | :? KeyNotFoundException -> failwithf "No message %A" (label)


type PNumber(value: double) =
  inherit PValue()

  member __.Value = value


type PBoolean(value: bool) =
  inherit PValue()

  member __.Value = value


type PText(value: string) =
  inherit PValue()

  member __.Value = value


type PNothing() =
  inherit PValue()


[<RequireQualifiedAccess>]
module Primitives =
  let extendEnv (bindings:(Name * PValue) list) (env:Environment) =
    let newEnv = Environment(Some env)
    
    for (label, value) in bindings do
      newEnv.Define(label, value)
    
    newEnv


  let emptyEnv = 
    Environment(None)


  let typeName (value:PValue) =
    match value with
    | :? PSymbol -> "Symbol"
    | :? PClosure -> "Closure"
    | :? PRecord -> "Record"
    | :? PNumber -> "Number"
    | :? PBoolean -> "Boolean"
    | :? PText -> "Text"
    | :? PNothing -> "Nothing"
    | _ -> failwithf "Unexpected %A" value


  let toMaybeRecord (value:PValue) =
    match value with
    | :? PNothing -> None
    | :? PRecord as r -> Some r
    | _ -> failwithf "Expected a Record or Nothing, but got %s" (typeName value)


  let toRecord (value:PValue) =
    match value with
    | :? PRecord as r -> r
    | _ -> failwithf "Expected a Record or Nothing, but got %s" (typeName value)
