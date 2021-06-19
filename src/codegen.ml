open Core

let context = Llvm.global_context ()
let the_module = Llvm.create_module context "mymod"
let builder = Llvm.builder context
let named_values : (string, Llvm.llvalue) Hashtbl.t = Hashtbl.create (module String)
let double_type = Llvm.double_type context

let create_program the_fpm =
  let function_type = Llvm.function_type double_type (Array.create ~len:0 double_type) in
  let code_expr = Llvm.const_float double_type 5.0 in
  let fn = Llvm.declare_function "main" function_type the_module in
  (* Create a new basic block to start insertion into. *)
  let bb = Llvm.append_block context "entry" fn in
  Llvm.position_at_end bb builder;
  (* Insert our code into the bb *)
  let (_ : Llvm.llvalue) = Llvm.build_ret code_expr builder in
  (* Validate the generated code, checking for consistency. *)
  (match Llvm_analysis.verify_function fn with
  | true -> ()
  | false ->
    printf "invalid function generated\n%s\n" (Llvm.string_of_llvalue fn);
    Llvm_analysis.assert_valid_function fn);
  (* Optimize the function. *)
  let (_ : bool) = Llvm.PassManager.run_function fn the_fpm in
  fn
;;
