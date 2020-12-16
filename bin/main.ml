open Syntax;;
open Codegen;;

let main () =
  while true do
    print_string "ready> "; flush stdout;
    let toplevels = Parser.toplevel Lexer.token (Lexing.from_channel stdin) in
    toplevels |> codegen_toplevel |> Llvm.dump_value |> print_newline
  done;;

main ()
