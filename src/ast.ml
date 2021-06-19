type binary_op =
  | Add
  | Subtract
  | Divide
  | Multiply
  | Mod

let string_to_opcode opcode =
  match opcode with
  | "+" -> Some Add
  | "-" -> Some Subtract
  | "/" -> Some Divide
  | "*" -> Some Multiply
  | "%" -> Some Mod
  | _ -> None
;;

type t =
  | Binary of (binary_op * t * t)
  | Atom of Value.t
  | Variable of string
  | Let of (string * t * t)
  | Print of t

let rec check ?(scope : string list = []) (expr : t) =
  match expr with
  | Binary (_, lhs, rhs) -> check ~scope lhs && check ~scope rhs
  | Atom _ -> true
  | Variable name ->
    (match Stack_scope.stack_location scope name with
    | Some _ -> true
    | None -> false)
  | Let (name, assignment_expr, in_expr) ->
    check ~scope assignment_expr
    && check ~scope:(Stack_scope.push_scope scope name) in_expr
  | Print expr -> check expr ~scope
;;
