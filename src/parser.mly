%token <float> NUMBER
%token <string> STRING
%token <string> IDENT
%token END OPEN CLOSE TRUE FALSE

%type <Ast.t> main
%start main
%{ open Core %}

%%

main:
  | expr END { $1 }
;;

atom:
  | NUMBER { Ast.Atom (Value.Number $1) }
  | STRING { Ast.Atom (Value.String $1) }
  | TRUE { Ast.Atom (Value.Bool true ) }
  | FALSE { Ast.Atom (Value.Bool false) }
  | IDENT { Ast.Variable $1 }
;;

exprs:
  | { [] }
  | expr exprs {
    $1::$2
  }
;;

expr:
  | atom { $1 }
  | OPEN IDENT exprs CLOSE {
    match Ast.string_to_opcode $2 with
    | Some code -> (
      match $3 with
      | [lhs; rhs ] -> Ast.Binary (code, lhs, rhs)
      | _ -> raise_s (Sexp.of_string "Invalid application of special form")
    )
    | _ ->
    match $2 with
    | "let" -> (
      match $3 with
      | [ Ast.Atom (Value.String name); assign_to_expr; in_expr ] -> Ast.Let (name, assign_to_expr, in_expr)
      | _ -> raise_s (Sexp.of_string "Invalid form for let")
    )
    | _ -> raise_s (Sexp.of_string "Unknown application")
  }
;;
