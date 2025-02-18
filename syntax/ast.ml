(* expr - Base type for all expression nodes. *)
type expr =
  (* variant for numeric literals like "1.0". *)
  | Number of float
  (* variant for referencing a variable, like "a". *)
  | Variable of string

  (* variant for a binary operator. *)
  | Binary of char * expr * expr

  (* variant for function calls. *)
  | Call of string * expr array [@@deriving show, eq]

(* proto - This type represents the "prototype" for a function, which captures
* its name, and its argument names (thus implicitly the number of arguments the
* function takes). *)
type proto = Prototype of string * string array [@@deriving show, eq]

(* func - This type represents a function definition itself. *)
type func = Function of proto * expr [@@deriving show, eq]

type toplevel =
  | Expr of expr
  | Func of func
