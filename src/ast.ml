open Core

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
  | Variable of (string * Type.t option)
  | Let of (string * t * t)
  | If of (t * t * t)
  | Print of t

let rec type_ (expr : t) : Type.t =
  match expr with
  | Binary (_, lhs, rhs) ->
    let t1 = type_ lhs in
    let t2 = type_ rhs in
    if phys_equal t1 t2 then t1 else raise_s [%message "Invalid Binary Op"]
  | If (_, lhs, rhs) ->
    let t1 = type_ lhs in
    let t2 = type_ rhs in
    if phys_equal t1 t2 then t1 else raise_s [%message "Invalid Binary Op"]
  | Atom v -> Value.value_to_type v
  | Variable (name, t) ->
    (match t with
    | Some t -> t
    | None -> raise_s [%message "Variable " name " is not in scope"])
  | Let (_, _, in_expr) -> type_ in_expr
  | Print _ -> UnitType
;;

(* TODO: Actually do type checking here *)
let rec typecheck ?(scope = Hashtbl.create (module String)) (expr : t) : t =
  match expr with
  | Binary (op, lhs, rhs) -> Binary (op, typecheck ~scope lhs, typecheck ~scope rhs)
  | Atom v -> Atom v
  | If (cnd, lhs, rhs) ->
    let cnd = typecheck ~scope cnd in
    let cnd_type = type_ cnd in
    if phys_equal cnd_type BoolType
       || phys_equal cnd_type IntType
       || phys_equal cnd_type FloatType
    then If (cnd, typecheck ~scope lhs, typecheck ~scope rhs)
    else raise_s [%message "Conditional must be int bool or float"]
  | Variable (name, _) ->
    (match Hashtbl.find scope name with
    | Some v -> Variable (name, Some v)
    | None -> raise_s [%message "The variable name " name " is not in scope"])
  | Let (name, assignment_expr, in_expr) ->
    let assignment_expr = typecheck ~scope assignment_expr in
    let assignment_expr_type = type_ assignment_expr in
    let scope = Hashtbl.copy scope in
    Hashtbl.set scope ~key:name ~data:assignment_expr_type;
    Let (name, assignment_expr, typecheck ~scope in_expr)
  | Print k -> Print k
;;
