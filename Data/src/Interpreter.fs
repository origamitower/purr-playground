module Purr.Data.Interpreter

open Purr.Data.Ast
open Purr.Data.Runtime


let rec evaluateDefinitions (env:IEnvironment) (program:Program) =
  let rec go (env:Environment) (declarations:Program) =
    match declarations with
    | Define (name, expr) :: rest ->
        env.Define(name, Thunk (PurrThunk expr))
        go env rest
    | [] -> env
  in go (Environment (Some env)) program


and evaluate (env:IEnvironment) (expr:Expr) : PurrObject =
  match expr with
  | Lambda (parameters, body) ->
      Primitives.lambda env parameters body

  | Let (name, value, body) ->
      let value = evaluate env value
      let newEnv = Primitives.extendEnv [name, Object value] env
      in evaluate newEnv body

  | If (test, consequent, alternate) ->
      let value = evaluate env test 
      let consequent = Primitives.lambda env [] consequent
      let alternate = Primitives.lambda env [] alternate
      in Primitives.send value evaluate Primitives.THEN_ELSE [consequent; alternate]

  | Load (name) ->
      match env.Lookup name with
      | Some (Object v) -> v
      | Some (Thunk fn) -> fn.Force(evaluate, env)
      | None -> failwithf "Undefined variable %s" name

  | Number value -> Primitives.number value
  | Text value -> Primitives.text value
  | Symbol desc -> Primitives.symbol desc
  | Boolean value -> Primitives.boolean value
  | Nothing -> Primitives.Nothing

  | Record (parent, messages) ->
      let parent = Primitives.handleNothingAsNone (evaluate env parent)
      let messages = messages |> List.map (evaluateMessage env)
      in Primitives.record parent messages

  | Send (object, label, args) ->
      let object = evaluate env object :> IMessageable
      let label = evaluateLabel env label
      let args = evaluateList env args
      in object.Send(evaluate, label, args)


and evaluateMessage (env:IEnvironment) (label, parameters, body) =
  let label = evaluateLabel env label
  in (label, Primitives.closure env parameters body :> ICallable)


and evaluateLabel (env:IEnvironment) (label:Label) =
  match label with
  | Static name -> LName name
  | Dynamic expr -> LSymbol (box <| evaluate env expr)


and evaluateList (env:IEnvironment) (items:Expr list) =
  items |> List.map (evaluate env)

