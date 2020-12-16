open Llvm;;
(* open Llvm_analysis *)
open Syntax;;
exception Error of string

let context = Llvm.global_context ()
let the_module = Llvm.create_module context "my cool jit"
let builder = Llvm.builder context
let named_values:(string, Llvm.llvalue) Hashtbl.t = Hashtbl.create 10
let double_type = Llvm.double_type context

let rec codegen_expr = function
  | Ast.Number n -> Llvm.const_float double_type n 
  | Ast.Variable name ->  (
    try Hashtbl.find named_values name with
      | Not_found -> raise (Error "unknown variable name")
  )
  | Ast.Binary (op, lhs, rhs) ->
    let lhs_val = codegen_expr lhs in
    let rhs_val = codegen_expr rhs in
    begin
      match op with
      | '+' -> Llvm.build_fadd lhs_val rhs_val "addtmp" builder
      | '-' -> Llvm.build_fsub lhs_val rhs_val "subtmp" builder
      | '*' -> Llvm.build_fmul lhs_val rhs_val "multmp" builder
      | '<' ->
          (* Convert bool 0/1 to double 0.0 or 1.0 *)
          let i = Llvm.build_fcmp Llvm.Fcmp.Ult lhs_val rhs_val "cmptmp" builder in
          Llvm.build_uitofp i double_type "booltmp" builder
      | _ -> raise (Error "invalid binary operator")
    end
    | Call (callee, args) ->
      (* Look up the name in the module table. *)
      let callee =
        match Llvm.lookup_function callee the_module with
        | Some callee -> callee
        | None -> raise (Error "unknown function referenced")
      in
      let params = Llvm.params callee in

      (* If argument mismatch error. *)
      if Array.length params == Array.length args then () else
        raise (Error "incorrect # arguments passed");
      let args = Array.map codegen_expr args in
      Llvm.build_call callee args "calltmp" builder;;

let codegen_proto = function
  | Ast.Prototype (name, args) ->
    (* Make the function type: double(double,double) etc. *)
    let doubles = Array.make (Array.length args) double_type in
    let ft = function_type double_type doubles in
    let f = match lookup_function name the_module with
      | None -> declare_function name ft the_module
      (* If 'f' conflicted, there was already something named 'name'. If it has a body, don't allow redefinition or reextern. *)
      | Some f ->
        (* If 'f' already has a body, reject this. *)
        if Array.length (basic_blocks f) == 0 then () else
          raise (Error "redefinition of function");
        (* If 'f' took a different number of arguments, reject. *)
        if Array.length (params f) == Array.length args then () else
          raise (Error "redefinition of function with different # args");
        f in
          (* Set names for all arguments. *)
          Array.iteri (fun i a ->
            let n = args.(i) in
            set_value_name n a;
            Hashtbl.add named_values n a;
          ) (params f);
          f

let codegen_func = function
| Ast.Function (proto, body) ->
  Hashtbl.clear named_values;
  let the_function = codegen_proto proto in

  (* Create a new basic block to start insertion into. *)
  let bb = append_block context "entry" the_function in
  position_at_end bb builder;

  try
    let ret_val = codegen_expr body in

    (* Finish off the function. *)
    let _ = build_ret ret_val builder in

    (* Validate the generated code, checking for consistency. *)
    (* TODO: このコードは意図的にコメントアウトしているが、多分大事 *)
    (* Llvm_analysis.assert_valid_function the_function; *)

    the_function
  with e ->
    delete_function the_function;
    raise e
