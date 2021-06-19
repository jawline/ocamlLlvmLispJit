{
  open Parser
  exception Error of string
}

rule token = parse
| [' ' '\t']
    { token lexbuf }
| "true" { TRUE }
| "false" { FALSE }
| ['a'-'z' 'A'-'Z' '+' '-' '*' '/' '%']+ as ident { IDENT ident }
| ['0'-'9']+ ('.' ['0'-'9']+ )? as i
    { NUMBER (float_of_string i) }
| '\'' { read_string (Buffer.create 128) lexbuf }
| '(' { OPEN }
| ')' { CLOSE }
| eof { END }
| _ { raise (Error (Printf.sprintf "At offset %d: unexpected character.\n" (Lexing.lexeme_start lexbuf))) }

and read_string buf = parse
  | '\''       { STRING (Buffer.contents buf) }
  | '\\' 'n'  { Buffer.add_char buf '\n'; read_string buf lexbuf }
  | [^ '\'' '\\']+
    { Buffer.add_string buf (Lexing.lexeme lexbuf);
      read_string buf lexbuf
    }
  | _ { raise (Error ("Illegal string character: " ^ Lexing.lexeme lexbuf)) }
  | eof { raise (Error ("String is not terminated")) }
