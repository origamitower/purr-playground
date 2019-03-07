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


and PurrObjectState = {
  typeName: string
  state: Map<string, obj>
  hashFunction: (PurrObject -> int) option
  equalityFunction: (PurrObject -> PurrObject -> bool) option
  parent: PurrObject option
  messages: (PurrLabel * ICallable) list
}

and PurrObject(state: PurrObjectState) =
  let messages =
    let dict = new Dictionary<PurrLabel, ICallable>()
    for (label, callable) in state.messages do
      dict.Add(label, callable)
    dict

  member __.TypeName = state.typeName
  member __.State = state.state
  member __.Parent = state.parent
  member __.Messages = messages

  member __.AddMethod(label, callable) =
    messages.Add(label, callable)

  override self.GetHashCode() =
    match state.hashFunction with
    | Some fn -> fn self
    | None -> LanguagePrimitives.PhysicalHash self

  override self.Equals(that) =
    match that, state.equalityFunction with
    | :? PurrObject as that, None -> LanguagePrimitives.PhysicalEquality self that
    | :? PurrObject as that, Some equals -> equals self that
    | _ -> false

  interface IMessageable with
    member self.Send(eval, label, args) =
      try
        let method = messages.Item(label)
        in method.Invoke(eval, self, args)
      with
      | :? KeyNotFoundException -> failwithf "%s does not understand %A" state.typeName label

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

  let proc fn = PrimitiveProcedure(fn)

  let private primHash (x:PurrObject) = 
    hash (x.TypeName, x.State, x.Parent)

  let private primEq (a:PurrObject) (b:PurrObject) =
    (a.TypeName, a.State, a.Parent) = (b.TypeName, b.State, b.Parent)

  let private defMessages (obj:PurrObject) messages =
    for (name, callable) in messages do
      obj.AddMethod(name, callable)

  let record parent messages = PurrObject({
    typeName = "Record"
    state = Map.empty
    hashFunction = None
    equalityFunction = None
    parent = parent
    messages = messages
  })

  let primitive typeName parent value messages = PurrObject({
    typeName = typeName
    state = Map.ofList ["value", box value]
    hashFunction = Some primHash
    equalityFunction = Some primEq
    parent = Some parent
    messages = messages
  })

  let special typeName parent state messages = PurrObject({
    typeName = typeName
    state = state
    hashFunction = None
    equalityFunction = None
    parent = Some parent
    messages = messages
  })

  let Root = record None []


  let Nothing = record (Some Root) []

  let NumberProto = record (Some Root) []

  let TextProto = record (Some Root) []

  let BoolProto = record (Some Root) []

  let SymbolProto = record (Some Root) []


  let text (s:string) = primitive "Text" TextProto s []

  let number (n:double) = primitive "Number" NumberProto n []

  let boolean (b:bool) = primitive "Boolean" BoolProto b []

  let symbol (s:string) = 
    special "Symbol" SymbolProto
            (Map.ofList ["description", box s])
            []

  // -- Common messages --
  defMessages Root
    [
      DESCRIBE, proc (fun (eval, self, args) -> text "Root")
    ]

  defMessages Nothing 
    [
      DESCRIBE, proc (fun (eval, self, args) -> text "Nothing")
    ]

  defMessages NumberProto
    [
      DESCRIBE, proc (fun (eval, self, args) -> 
        let value = self.State.Item("value") :?> double
        in text (sprintf "%s" (value.ToString("N")))
      )
    ]

  defMessages TextProto
    [
      DESCRIBE, proc (fun (eval, self, args) ->
        let value = self.State.Item("value") :?> string
        in text (sprintf "%A" value)
      )
    ]

  defMessages BoolProto
    [
      DESCRIBE, proc (fun (eval, self, args) ->
        let value = self.State.Item("value") :?> bool
        in text (sprintf "%b" value)
      )
    ]

  defMessages SymbolProto
    [
      DESCRIBE, proc (fun (eval, self, args) ->
        let desc = self.State.Item("description") :?> string
        in text (sprintf "Symbol(%A)" desc)
      )
    ]

