module Purr.Data.Ast

type Name = string

and Program = Declaration list

and Declaration =
  | Define of Name * Expr

and Expr =
  | Let of Name * value: Expr * body: Expr
  | If of test: Expr * consequent: Expr * alternate: Expr
  | Lambda of parameters: string list * body: Expr
  | Record of parent: Expr * messages: Message list
  | Send of object: Expr * message: Label * arguments: Expr list
  | Symbol of description: string
  | Load of Name
  | Nothing
  | Number of double
  | Text of string
  | Boolean of bool

and Message = Label * Name list * Expr

and Label =
  | Static of Name
  | Dynamic of Expr
