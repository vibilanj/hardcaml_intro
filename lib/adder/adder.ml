open Hardcaml.Signal

type add_result_t = { sum : Hardcaml.Signal.t; carry : Hardcaml.Signal.t }

let half_adder x y : add_result_t = { sum = x ^: y; carry = x &: y }

let full_adder x y c : add_result_t =
  { sum = x ^: y ^: c; carry = x &: y |: (x &: c) |: (y &: c) }

let full_adder_using_half_adder x y c : add_result_t =
  let add_result_1 = half_adder x y in
  let add_result_2 = half_adder add_result_1.sum c in
  { sum = add_result_2.sum; carry = add_result_1.carry |: add_result_2.carry }

let full_adder_circuit =
  let out = full_adder (input "x" 1) (input "y" 1) (input "c" 1) in
  let sum = output "sum" out.sum in
  let carry = output "carry" out.carry in
  Hardcaml.Circuit.create_exn ~name:"full_adder" [ sum; carry ]

let vhdl_of_full_adder () = Hardcaml.Rtl.print Vhdl full_adder_circuit
