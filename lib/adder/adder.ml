open Base
open Hardcaml.Signal

module Add_result = struct
  type t = { sum : Hardcaml.Signal.t; carry : Hardcaml.Signal.t }
end

let half_adder x y : Add_result.t = { sum = x ^: y; carry = x &: y }

let full_adder x y c : Add_result.t =
  { sum = x ^: y ^: c; carry = x &: y |: (x &: c) |: (y &: c) }

let full_adder_using_half_adder x y c : Add_result.t =
  let add_result_1 = half_adder x y in
  let add_result_2 = half_adder add_result_1.sum c in
  { sum = add_result_2.sum; carry = add_result_1.carry |: add_result_2.carry }

let full_adder_circuit =
  let out = full_adder (input "x" 1) (input "y" 1) (input "c" 1) in
  let sum = output "sum" out.sum in
  let carry = output "carry" out.carry in
  Hardcaml.Circuit.create_exn ~name:"full_adder" [ sum; carry ]

let vhdl_of_full_adder () = Hardcaml.Rtl.print Vhdl full_adder_circuit

let ripple_carry_adder x y ~carry_in_bit =
  let c, s =
    List.fold2_exn (bits_lsb x) (bits_lsb y) ~init:(carry_in_bit, [])
      ~f:(fun (carry_in, sum_bits) x y ->
        let { Add_result.carry; sum } = full_adder x y carry_in in
        (carry, sum :: sum_bits))
  in
  concat_msb (c :: s)

let ripple_carry_adder_circuit =
  let out =
    ripple_carry_adder (input "x" 8) (input "y" 8) ~carry_in_bit:(input "c" 1)
  in
  let sum = output "sum" out in
  Hardcaml.Circuit.create_exn ~name:"full_adder" [ sum ]

let vhdl_of_ripple_carry_adder () =
  Hardcaml.Rtl.print Vhdl ripple_carry_adder_circuit
