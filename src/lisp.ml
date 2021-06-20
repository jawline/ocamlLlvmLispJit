open Core

type t = Ast.t

let parse (json : string) : t =
  let linebuf = Lexing.from_string json in
  Ast.typecheck (Parser.main Lexer.token linebuf)
;;

let jit_engine ~module_ () =
  (match Llvm_executionengine.initialize () with
  | true -> ()
  | false -> raise_s [%message "failed to initialize"]);
  Llvm_executionengine.create module_
;;

let optimizer ~module_ () =
  let the_fpm = Llvm.PassManager.create_function module_ in
  (* Promote allocas to registers. *)
  Llvm_scalar_opts.add_memory_to_register_promotion the_fpm;
  (* Do simple "peephole" optimizations and bit-twiddling optzn. *)
  Llvm_scalar_opts.add_instruction_combination the_fpm;
  (* reassociate expressions. *)
  Llvm_scalar_opts.add_reassociation the_fpm;
  (* Eliminate Common SubExpressions. *)
  Llvm_scalar_opts.add_gvn the_fpm;
  (* Simplify the control flow graph (deleting unreachable blocks, etc). *)
  Llvm_scalar_opts.add_cfg_simplification the_fpm;
  ignore (Llvm.PassManager.initialize the_fpm : bool);
  the_fpm
;;

let run_program ast : Value.t =
  (* Create the LLVM context and codegen apperatus *)
  let context = Llvm.global_context () in
  let module_ = Llvm.create_module context "mod" in
  let builder = Llvm.builder context in
  (* Create the JIT *)
  let the_execution_engine = jit_engine ~module_ () in
  let the_fpm = optimizer ~module_ () in
  (* Generate our program *)
  let program = Codegen.create_program ~context ~module_ ~builder ast the_fpm in
  Llvm_executionengine.add_module module_ the_execution_engine;
  (* Dump the program to stdout for debugging *)
  Llvm.dump_value program;
  (* JIT the function, execute it then return the value *)
  let result =
    match Ast.type_ ast with
    | FloatType ->
      let fp =
        Llvm_executionengine.get_function_address
          "main"
          (Foreign.funptr Ctypes.(void @-> returning double))
          the_execution_engine
      in
      let result = fp () in
      Value.Float result
    | IntType ->
      let fp =
        Llvm_executionengine.get_function_address
          "main"
          (Foreign.funptr Ctypes.(void @-> returning int))
          the_execution_engine
      in
      let result = fp () in
      Value.Int result
    | BoolType ->
      let fp =
        Llvm_executionengine.get_function_address
          "main"
          (Foreign.funptr Ctypes.(void @-> returning bool))
          the_execution_engine
      in
      let result = fp () in
      Value.Bool result
    | UnitType ->
      let fp =
        Llvm_executionengine.get_function_address
          "main"
          (Foreign.funptr Ctypes.(void @-> returning void))
          the_execution_engine
      in
      fp ();
      Value.Unit
  in
  Llvm_executionengine.remove_module module_ the_execution_engine;
  result
;;

let%test "double atom test" =
  let r = run_program (parse "51.3") in
  match r with
  | Value.Float (51.3) -> true
  | _ -> raise_s [%message "Broken " (Value.show r)]
;;

let%test "let test float" =
  let r = run_program (parse "(let a 4.0 (let b 3.0 (+ a b)))") in
  match r with
  | Value.Float 7.0 -> true
  | _ -> raise_s [%message "Broken " (Value.show r)]
;;

let%test "let test int" =
  let r = run_program (parse "(let a 6 (let b 69 (+ a b)))") in
  match r with
  | Value.Int 75 -> true
  | _ -> raise_s [%message "Broken " (Value.show r)]
;;

let%test "trivial if" =
  let r = run_program (parse "(if 0 5 10)") in
  match r with
  | Value.Int 10 -> true
  | _ -> raise_s [%message "Broken " (Value.show r)]
;;

let%test "trivial if float" =
  let r = run_program (parse "(if 0.0 5.0 10.0)") in
  match r with
  | Value.Float 10.0 -> true
  | _ -> raise_s [%message "Broken " (Value.show r)]
;;

let%test "let test if" =
  let r = run_program (parse "(let a 6 (let b 69 (if a a b)))") in
  match r with
  | Value.Int 6 -> true
  | _ -> raise_s [%message "Broken " (Value.show r)]
;;

let%test "parse example" =
  ignore (Ast.typecheck (parse "(let a 6 (+ a a))") : Ast.t);
  true
;;

let%test "parse broken example" =
  try
    ignore (Ast.typecheck (parse "(let a 6 (+ a b))") : Ast.t);
    false
  with
  | _ -> true
;;
