module Purr.Data.Ast

type Name = string

and Declaration =
  | Define of Name * Expr

and Expr =
  | Lambda of Name list * body: Expr
  | Let of Name * value: Expr * body: Expr
  | Load of Name
  | Number of double
  | Text of string
  | Symbol of description: string
  | Boolean of bool
  | Nothing
  | Record of parent: Expr * messages: Message list
  | Send of object: Expr * message: Label * arguments: Expr list

and Message = Label * Name list * Expr

and Label =
  | Static of Name
  | Dynamic of Expr
