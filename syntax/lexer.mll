{
open Parser
}

let digit = ['0'-'9']
let alpha = ['a'-'z' 'A'-'Z']
let id = alpha+ (alpha | digit | '_')*
let num = digit digit*
let ws = ['\t' ' ' '\n']

rule token = parse
  | num as n  { NUMBER (int_of_string n) }
  | ";;" { SEMISEMI }
