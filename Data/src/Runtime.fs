module Purr.Data.Runtime

open Purr.Data.Ast
open System.Collections.Generic


type PurrLabel =
  | LName of string
  | LSymbol of obj


let INVOKE = LName "invoke"


type Evaluator = Environment -> Expr -> PurrObject


and ICallable =
  abstract member Invoke : Evaluator * PurrObject * PurrObject list -> PurrObject


and IMessageable =
  abstract member Send : Evaluator * PurrLabel * PurrObject list -> PurrObject


and PurrObject(typeName: string, parent: PurrObject option, messages: (PurrLabel * ICallable) list) =
  let messages =
    let dict = new Dictionary<PurrLabel, ICallable>()
    for (label, callable) in messages do
      dict.Add(label, callable)
    dict

  member __.TypeName = typeName
  member __.Parent = parent
  member __.Messages = messages

  interface IMessageable with
    member self.Send(eval, label, args) =
      try
        let method = messages.Item(label)
        in method.Invoke(eval, self, args)
      with
      | :? KeyNotFoundException -> failwithf "%s does not understand %A" typeName label

  interface ICallable with
    member self.Invoke(eval, context, args) =
      let messageable = self :> IMessageable
      in messageable.Send(eval, INVOKE, args)


and PurrClosure(environment: Environment, parameters: string list, body: Expr) =
  member __.Environment = environment
  member __.Parameters = parameters
  member __.Body = body

  interface ICallable with
    member self.Invoke(eval, context, args) =
      let newEnv = environment.Extend(List.zip parameters args)
      in eval newEnv body


and PrimitiveProcedure(fn: Evaluator * PurrObject * PurrObject list -> PurrObject) =
  member __.Fn = fn

  interface ICallable with
    member self.Invoke(eval, context, args) =
      fn(eval, context, args)


and Environment(parent: Environment option) =
  let mutable bindings : Map<string, PurrObject> = Map.empty

  member __.Parent = parent

  member __.Lookup (name:string) = 
    Map.tryFind name bindings

  member __.Define (name:string, value: PurrObject) =
    bindings <- Map.add name value bindings

  member self.Extend (bindings: (string * PurrObject) list) =
    let newEnv = Environment(Some self)
    for (name, value) in bindings do
      newEnv.Define(name, value)
    newEnv




[<RequireQualifiedAccess>]
module Primitives =
  let INVOKE = INVOKE
  let DESCRIBE = LName "describe"
  let ADD = LName "+"
  let SUBTRACT = LName "-"
  let MULTIPLY = LName "*"
  let DIVIDE = LName "/"
  let AND = LName "and"
  let OR = LName "or"
  let NOT = LName "not"


  