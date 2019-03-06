module Purr.Data.Interpreter

open Purr.Data.Ast
open Purr.Data.Runtime


let rec evaluateLabel (env:Environment) (label:Label) =
  match label with
  | Static name -> LName(name)
  | Dynamic expr ->
      match evaluate env expr with
      | :? PSymbol as s -> LSymbol(s)
      | :? PText as t -> LName(t.Value)
      | x -> failwithf "Expected a Symbol or Text, got %s" (Primitives.typeName x)


and evaluateMessage (env:Environment) (label, parameters, body) =
  let label = evaluateLabel env label
  in (label, PClosure(env, parameters, body))


and evaluateList (env:Environment) (items:Expr list) =
  items |> List.map (evaluate env)


and evaluateClosure (arguments:PValue list) (closure:PClosure) =
  let newEnv = Primitives.extendEnv (List.zip closure.Parameters arguments) closure.Environment
  in evaluate newEnv closure.Body


and evaluate (env:Environment) (expr:Expr) : PValue =
  match expr with
  | Lambda (parameters, body) ->
      upcast PClosure(env, parameters, body)

  | Let (name, value, body) ->
      let value = evaluate env value
      let newEnv = Primitives.extendEnv [name, value] env
      in evaluate newEnv body

  | If (test, consequent, alternate) ->
      if Primitives.toBoolean (evaluate env test) then
        evaluate env consequent
      else
        evaluate env alternate

  | Load (name) ->
      match env.Lookup name with
      | Some value -> value
      | None -> failwithf "Undefined variable %s" name

  | Number (value) -> upcast PNumber(value)
  | Text (value) -> upcast PText(value)
  | Symbol (description) -> upcast PSymbol(description)
  | Boolean (value) -> upcast PBoolean(value)
  | Nothing -> upcast PNothing()
  
  | Record (parent, messages) ->
      let parent = Primitives.toMaybeRecord (evaluate env parent)
      let messages = messages |> List.map (evaluateMessage env)
      in upcast PRecord(parent, messages)

  | Send (object, label, arguments) ->
      let object = Primitives.toRecord (evaluate env object)
      let label = evaluateLabel env label
      let arguments = evaluateList env arguments
      let method = object.GetMethod(label, arguments)
      in evaluateClosure arguments method
