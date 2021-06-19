open Core

type scope_entry = string
type t = scope_entry list

let push_scope scope ident = ident :: scope

let pop_scope = function
  | [] -> raise_s (Sexp.of_string "empty scope")
  | _ :: xs -> xs
;;

let rec stack_location ?(count = 0) scope ident =
  match scope with
  | [] -> None
  | x :: xs ->
    if String.( = ) x ident
    then Some count
    else stack_location ~count:(count + 1) xs ident
;;
