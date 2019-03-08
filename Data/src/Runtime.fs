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


type Evaluator = IEnvironment -> Expr -> PurrObject


and LookupValue =
  | Object of PurrObject
  | Thunk of PurrThunk


and ICallable =
  abstract member Invoke : Evaluator * PurrObject * PurrObject list -> PurrObject


and IMessageable =
  abstract member Send : Evaluator * PurrLabel * PurrObject list -> PurrObject


and IEnvironment =
  abstract member Lookup : string -> LookupValue option


and PurrType =
  | TText of string
  | TNumber of double
  | TBoolean of bool
  | TSymbol of description: string
  | TProcedure of Boxed<(Evaluator * PurrObject * PurrObject list -> PurrObject)>
  | TLambda of Boxed<PurrClosure>
  | TRecord
  with
    member self.Name =
      match self with
      | TText _ -> "Text"
      | TNumber _ -> "Number"
      | TBoolean _ -> "Boolean"
      | TSymbol _ -> "Symbol"
      | TProcedure _ -> "Procedure"
      | TLambda _ -> "Lambda"
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


and PurrClosure(environment: IEnvironment, parameters: string list, body: Expr) =
  member __.Environment = environment
  member __.Parameters = parameters
  member __.Body = body

  interface ICallable with
    member self.Invoke(eval, context, args) =
      let args = List.map Object args
      let newEnv = Environment.Extend(environment, List.zip parameters args)
      in eval newEnv body


and PrimitiveProcedure(fn: Evaluator * PurrObject * PurrObject list -> PurrObject) =
  member __.Fn = fn

  interface ICallable with
    member self.Invoke(eval, context, args) =
      fn(eval, context, args)


and PurrThunk(expr: Expr) =
  let mutable value : PurrObject option = None

  member __.Expr = expr

  member self.Force(eval: Evaluator, environment) =
    match value with
    | Some value -> value
    | None ->
        let result = eval environment expr
        value <- Some result
        result 
          


and Environment(parent: IEnvironment option) =
  let mutable bindings : Map<string, LookupValue> = Map.empty

  member __.Parent = parent

  member __.Define (name:string, value: LookupValue) =
    bindings <- Map.add name value bindings

  static member Extend (env: IEnvironment, bindings: (string * LookupValue) list) =
    let newEnv = Environment(Some env)
    for (name, value) in bindings do
      newEnv.Define(name, value)
    newEnv

  interface IEnvironment with
    member self.Lookup name =
      match Map.tryFind name bindings with
      | Some v -> Some v
      | None ->
          match parent with
          | Some parent -> parent.Lookup name
          | None -> None


[<RequireQualifiedAccess>]
module Primitives =
  let INVOKE = INVOKE
  let THEN_ELSE = LName "if"
  let DESCRIBE = LName "describe"
  let ADD = LName "+"
  let SUBTRACT = LName "-"
  let MULTIPLY = LName "*"
  let DIVIDE = LName "/"
  let AND = LName "and"
  let OR = LName "or"
  let NOT = LName "not"

  let extendEnv bindings env = Environment.Extend(env, bindings)

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
    | TProcedure p -> p.value
    | _ -> failwithf "Expected a Procedure, got %s" state.Name

  let lambdaData state =
    match state with
    | TLambda p -> p.value
    | _ -> failwithf "Expected a Lambda, got %s" state.Name


  let intoBox x = { value = x }

  let send (obj:IMessageable) eval label args =
    obj.Send(eval, label, args)

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

  let ProcedureProto = record (Some Root) []

  let LambdaProto = record (Some Root) []


  let text (s:string) = primitive (TText s) TextProto []

  let number (n:double) = primitive (TNumber n) NumberProto []

  let boolean (b:bool) = primitive (TBoolean b) BoolProto []

  let symbol (s:string) = 
    special (TSymbol s) SymbolProto []

  let lambda env parameters body =
    special (PurrClosure(env, parameters, body) |> intoBox |> TLambda) LambdaProto []

  let procedure fn =
    special (TProcedure (intoBox fn)) ProcedureProto []

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

  defMessages ProcedureProto
    [
      INVOKE, proc (fun (eval, self, args) ->
        let fn = procedureData self.Data
        in fn(eval, self, args)
      )
    ]

  defMessages LambdaProto
    [
      INVOKE, proc (fun (eval, self, args) ->
        let fn = lambdaData self.Data :> ICallable
        in fn.Invoke(eval, self, args)
      )
    ]

  // More operations
  let handleNothingAsNone (obj:PurrObject) =
    if obj = Nothing then None else Some obj

  let closure env parameters body =
    PurrClosure(env, parameters, body)