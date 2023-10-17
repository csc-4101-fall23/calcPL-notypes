open Ast

(** [parse s] parses [s] into an AST. *)
let parse (s : string) : expr =
  let lexbuf = Lexing.from_string s in
  let ast = Parser.prog Lexer.read lexbuf in
  ast

let string_of_val (e: expr) : string =
  match e with
  | Int i -> string_of_int i
  | Binop _ -> failwith "precondition violated"


let is_value : expr -> bool = function
  | Int _ -> true
  | Binop _ -> false

let rec step : expr -> expr = function
  | Int _ -> failwith "Does not setp"
  | Binop (bop, e1, e2) when is_value e1 && is_value e2 -> step_bop bop e1 e2
  | Binop (bop, e1, e2) when is_value e1 -> Binop (bop, e1, step e2)
  | Binop (bop, e1, e2) -> Binop(bop, step e1, e2)

and step_bop bop e1 e2 = match bop, e1, e2 with
  | Add, Int a, Int b -> Int (a + b)
  | Mult, Int a, Int b -> Int (a * b) 
  | _ -> failwith "precondition violated"


let rec eval (e: expr) : expr = 
  if is_value e then e else
    e |> step |> eval

  
(** [interp s] interprets [s] by lexing and parsing,
    evaluating, and converting the result to string. *)
let interp (s : string) : string =
    s |> parse |> eval |> string_of_val
  
  