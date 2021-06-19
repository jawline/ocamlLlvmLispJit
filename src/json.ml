open Core

type t = Ast.t

let parse (json : string) : t =
  let linebuf = Lexing.from_string json in
  Parser.main Lexer.token linebuf
;;

let jit_engine () =
  (match Llvm_executionengine.initialize () with
  | true -> ()
  | false -> raise_s [%message "failed to initialize"]);
  Llvm_executionengine.create Codegen.the_module
;;

let optimizer () =
  let the_fpm = Llvm.PassManager.create_function Codegen.the_module in
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

let run_program () : float =
  (* Create the JIT *)
  let the_execution_engine = jit_engine () in
  let the_fpm = optimizer () in
  (* Generate our program *)
  let program = Codegen.create_program the_fpm in
  Llvm_executionengine.add_module Codegen.the_module the_execution_engine;
  printf "Dumping program\n";
  Llvm.dump_value program;
  printf "Done\n";
  (* JIT the function, returning a function pointer. *)
  let fp =
    Llvm_executionengine.get_function_address
      "main"
      (Foreign.funptr Ctypes.(void @-> returning double))
      the_execution_engine
  in
  (* Execute the JITed method and collect the result *)
  let result_of_program = fp () in
  Llvm_executionengine.remove_module Codegen.the_module the_execution_engine;
  result_of_program
;;

let%test "program_test" = Float.( = ) (run_program ()) 5.0
let%test "parse_example" = Ast.check (parse "(let 'a' 6 (+ a a))")
let%test "parse_broken_example" = not (Ast.check (parse "(let 'a' 6 (+ a b))"))
