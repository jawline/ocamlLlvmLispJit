open Core

let to_llvm_type ~context (t : Type.t) =
  match t with
  | FloatType -> Llvm.double_type context
  | IntType -> Llvm.i64_type context
  | BoolType -> Llvm.i1_type context
  | UnitType -> Llvm.void_type context
;;

let to_llvm_atom ~context (t : Value.t) =
  let value_type = Value.value_to_type t in
  match t with
  | Int n -> Llvm.const_int (to_llvm_type ~context value_type) n
  | Float f -> Llvm.const_float (to_llvm_type ~context value_type) f
  | Bool b -> Llvm.const_int (to_llvm_type ~context value_type) (if b then 1 else 0)
  | Unit -> Llvm.const_null (to_llvm_type ~context value_type)
;;

let rec to_codegen
    ~context
    ~module_
    ~builder
    ?(scope = Hashtbl.create (module String))
    (t : Ast.t)
  =
  let llvm_alloca scope name expr =
    let function_ = Llvm.block_parent (Llvm.insertion_block builder) in
    let builder =
      Llvm.builder_at context (Llvm.instr_begin (Llvm.entry_block function_))
    in
    let value_type = Ast.type_ expr in
    let allocated_value =
      Llvm.build_alloca (to_llvm_type ~context value_type) name builder
    in
    ignore
      (Llvm.build_store
         (to_codegen ~context ~module_ ~builder ~scope expr)
         allocated_value
         builder
        : Llvm.llvalue);
    Hashtbl.set scope ~key:name ~data:allocated_value;
    ()
  in
  let r = to_codegen ~context ~module_ ~builder ~scope in
  let to_llvm_binop (op : Ast.binary_op) lhs rhs =
    match op with
    | Add ->
      (match Ast.type_ lhs with
      | IntType -> Llvm.build_add (r lhs) (r rhs) "add_tmp" builder
      | FloatType -> Llvm.build_fadd (r lhs) (r rhs) "add_tmp" builder
      | _ ->
        raise_s [%message (Type.show (Ast.type_ lhs)) " cannot be used in binary add"])
    | Subtract ->
      (match Ast.type_ lhs with
      | IntType -> Llvm.build_sub (r lhs) (r rhs) "sub_tmp" builder
      | FloatType -> Llvm.build_fsub (r lhs) (r rhs) "sub_tmp" builder
      | _ ->
        raise_s
          [%message (Type.show (Ast.type_ lhs)) " cannot be used in binary subtract"])
    | Multiply ->
      (match Ast.type_ lhs with
      | IntType -> Llvm.build_mul (r lhs) (r rhs) "mul_tmp" builder
      | FloatType -> Llvm.build_fmul (r lhs) (r rhs) "mul_tmp" builder
      | _ ->
        raise_s
          [%message (Type.show (Ast.type_ lhs)) " cannot be used in binary multiply"])
    | Divide ->
      (match Ast.type_ lhs with
      | IntType -> Llvm.build_sdiv (r lhs) (r rhs) "div_tmp" builder
      | FloatType -> Llvm.build_fdiv (r lhs) (r rhs) "div_tmp" builder
      | _ ->
        raise_s [%message (Type.show (Ast.type_ lhs)) " cannot be used in binary divide"])
    | Mod ->
      (match Ast.type_ lhs with
      | IntType -> Llvm.build_srem (r lhs) (r rhs) "rem_tmp" builder
      | FloatType -> Llvm.build_frem (r lhs) (r rhs) "rem_tmp" builder
      | _ ->
        raise_s
          [%message (Type.show (Ast.type_ lhs)) " cannot be used in binary remainder"])
  in
  let build_if cnd =
    match Ast.type_ cnd with
    | BoolType ->
      Llvm.build_icmp
        Llvm.Icmp.Ne
        (Llvm.const_int (to_llvm_type ~context BoolType) 0)
        (r cnd)
        "tmp_cnd_compare"
        builder
    | IntType ->
      Llvm.build_icmp
        Llvm.Icmp.Ne
        (Llvm.const_int (to_llvm_type ~context IntType) 0)
        (r cnd)
        "tmp_cnd_compare"
        builder
    | FloatType ->
      Llvm.build_fcmp
        Llvm.Fcmp.Une
        (Llvm.const_float (to_llvm_type ~context FloatType) 0.0)
        (r cnd)
        "tmp_cnd_compare"
        builder
    | _ -> raise_s [%message "Type incompatible with conditional"]
  in
  match t with
  | Binary (op, lhs, rhs) -> to_llvm_binop op lhs rhs
  | Atom const_val -> to_llvm_atom ~context const_val
  | Variable (name, _) ->
    (match Hashtbl.find scope name with
    | Some v -> Llvm.build_load v name builder
    | None -> raise_s [%message "No variable " name])
  | If (cnd, lhs, rhs) ->
    let cnd_gen = build_if cnd in
    Llvm.build_select cnd_gen (r lhs) (r rhs) "tmp_cnd_result" builder
  | Let (name, value, inexpr) ->
    let scope = Hashtbl.copy scope in
    llvm_alloca scope name value;
    to_codegen inexpr ~context ~module_ ~builder ~scope
  | Print _expr -> raise_s [%message "Unfinished"]
;;

let create_program ~context ~module_ ~builder (t : Ast.t) the_fpm =
  let function_type =
    Llvm.function_type
      (to_llvm_type ~context (Ast.type_ t))
      (Array.create ~len:0 (to_llvm_type ~context UnitType))
  in
  let fn = Llvm.declare_function "main" function_type module_ in
  (* Create a new basic block to start insertion into. *)
  let bb = Llvm.append_block context "entry" fn in
  Llvm.position_at_end bb builder;
  (* Generate the code into the basic block *)
  let code_expr = to_codegen ~context ~module_ ~builder t in
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
