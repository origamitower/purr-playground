module Purr.Data.Runtime

open Purr.Data.Ast
open System.Collections.Generic


type PurrLabel =
  | LName of string
  | LSymbol of obj


[<CustomEquality; NoComparison>]
type Boxed<'T> =
  { value: 'T }

  override self.Equals(that) =
    match that with
    | :? Boxed<'T> as that -> LanguagePrimitives.PhysicalEquality self that
    | _ -> false

  override self.GetHashCode() = LanguagePrimitives.PhysicalHash self


let INVOKE = LName "invoke"


type Evaluator = Environment -> Expr -> PurrObject


and ICallable =
  abstract member Invoke : Evaluator * PurrObject * PurrObject list -> PurrObject


and IMessageable =
  abstract member Send : Evaluator * PurrLabel * PurrObject list -> PurrObject


and PurrType =
  | TText of string
  | TNumber of double
  | TBoolean of bool
  | TSymbol of description: string
  | TProcedure of Boxed<(Evaluator * PurrObject * PurrObject list -> PurrObject)>
  | TRecord
  with
    member self.Name =
      match self with
      | TText _ -> "Text"
      | TNumber _ -> "Number"
      | TBoolean _ -> "Boolean"
      | TSymbol _ -> "Symbol"
      | TProcedure _ -> "Procedure"
      | TRecord -> "Record"

and PurrObjectState = {
  typeData: PurrType
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

  member __.Data = state.typeData
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
      | :? KeyNotFoundException -> failwithf "%s does not understand %A" state.typeData.Name label

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


and PurrThunk(expr: Expr) =
  let mutable value : PurrObject option = None

  member __.Expr = expr

  member self.Force(eval, environment) =
    match value with
    | Some value -> value
    | None ->
        let result = eval environment expr
        value <- Some result
        result 
          


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

  let textData state =
    match state with
    | TText s -> s
    | _ -> failwithf "Expected a Text, got %s" state.Name

  let numberData state =
    match state with
    | TNumber n -> n
    | _ -> failwithf "Expected a Number, got %s" state.Name

  let boolData state =
    match state with
    | TBoolean b -> b
    | _ -> failwithf "Expected a Boolean, got %s" state.Name

  let symbolData state =
    match state with
    | TSymbol s -> s
    | _ -> failwithf "Expected a Symbol, got %s" state.Name

  let procedureData state =
    match state with
    | TProcedure p -> p
    | _ -> failwithf "Expected a Procedure, got %s" state.Name

  let proc fn = PrimitiveProcedure(fn)

  let private primHash (x:PurrObject) = 
    hash (x.Data, x.Parent)

  let private primEq (a:PurrObject) (b:PurrObject) =
    (a.Data, a.Parent) = (b.Data, b.Parent)

  let private defMessages (obj:PurrObject) messages =
    for (name, callable) in messages do
      obj.AddMethod(name, callable)

  let record parent messages = PurrObject({
    typeData = TRecord
    hashFunction = None
    equalityFunction = None
    parent = parent
    messages = messages
  })

  let primitive typeData parent messages = PurrObject({
    typeData = typeData
    hashFunction = Some primHash
    equalityFunction = Some primEq
    parent = Some parent
    messages = messages
  })

  let special typeData parent messages = PurrObject({
    typeData = typeData
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


  let text (s:string) = primitive (TText s) TextProto []

  let number (n:double) = primitive (TNumber n) NumberProto []

  let boolean (b:bool) = primitive (TBoolean b) BoolProto []

  let symbol (s:string) = 
    special (TSymbol s) SymbolProto []

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
        let value = numberData self.Data
        in text (sprintf "%s" (value.ToString("N")))
      )
    ]

  defMessages TextProto
    [
      DESCRIBE, proc (fun (eval, self, args) ->
        let value = textData self.Data
        in text (sprintf "%A" value)
      )
    ]

  defMessages BoolProto
    [
      DESCRIBE, proc (fun (eval, self, args) ->
        let value = boolData self.Data
        in text (sprintf "%b" value)
      )
    ]

  defMessages SymbolProto
    [
      DESCRIBE, proc (fun (eval, self, args) ->
        let desc = symbolData self.Data
        in text (sprintf "Symbol(%A)" desc)
      )
    ]

