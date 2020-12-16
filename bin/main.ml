open Syntax;;
open Codegen;;

let main () = Ast.Binary ('+', (Ast.Number 12000.0), (Ast.Number 20.0)) |> codegen_expr |>  Llvm.dump_value ;;
main () |> print_newline;;

Ast.Function ((Ast.Prototype ("test", [|"y"|])), Ast.Number 12000.0) |> codegen_func |> Llvm.dump_value;;
