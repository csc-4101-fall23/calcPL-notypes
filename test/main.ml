open OUnit2
open Interp
open Ast
open Main

(** [make_i n i s] makes an OUnit test named [n] that expects
    [s] to evalute to [i]. *)
let make_i n i s =
  n >:: (fun _ -> assert_equal (string_of_int i) (interp s))


let tests = [
  make_i "int" 2 "2";
  (* make_i "add" 22 "11+11"; *)
  (* make_i "mul1" 22 "2*11"; *)
  (* make_i "mul of mult" 44 "2*2*11"; *)
  (* make_i "add of add" 33 "11+11+11"; *)
  (* make_i "mult on right of add" 24 "2+2*11"; *)
  (* make_i "mult on left of add" 15 "2*2+11"; *)
  (* make_i "nested add" 22 "(10 + 1) + (5 + 6)"; *)
]

let _ = run_test_tt_main ("suite" >::: tests)
