(* open Hardcaml.Bits *)
(* open Hardcaml.Signal *)

let vhdl_of_adder_circuit () =
  let open Hardcaml.Signal in
  let adder a b = a +: b in
  let c = output "c" (adder (input "a" 8) (input "b" 8)) in
  let circuit = Hardcaml.Circuit.create_exn ~name:"my_adder" [ c ] in
    Hardcaml.Rtl.print Vhdl circuit

let run () =
  (* let x = of_string "11001" in
     Format.printf "%a@." pp x;
     print_int (width x);
     print_newline () *)
  vhdl_of_adder_circuit ()

