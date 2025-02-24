type expr =
  | Int of int
  | Var of string
  | BinOp of string * expr * expr
  | Bool of bool
  | If of expr * expr * expr  (* If condition then expr else expr *)
  | FunCall of string * expr list

and stmt =
  | IfStmt of expr * expr * expr
  | Assign of string * expr
  | Expr of expr
  | Print of expr
  | FunDef of string * string list * stmt list
