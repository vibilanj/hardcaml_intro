open Base
open Hardcaml
open Hardcaml.Signal
(* open Hardcaml_waveterm *)

module I = struct
  type 'a t = { clock : 'a; clear : 'a; (* reset *) button : 'a }
  [@@deriving hardcaml]
end

module O = struct
  type 'a t = { state : 'a; [@bits 2] main : 'a; [@bits 3] side : 'a [@bits 3] }
  [@@deriving hardcaml]
end

module States = struct
  type t =
    | GreenMainRedSide
    | YellowMainRedSide
    | RedMainGreenSide
    | RedMainYellowSide
  [@@deriving sexp_of, compare, enumerate]
end

let create (i : _ I.t) =
  let r_sync = Reg_spec.create ~clock:i.clock ~clear:i.clear () in
  let sm = Always.State_machine.create (module States) ~enable:vdd r_sync in
  let main_result = Always.Variable.wire ~default:(of_string "010") in
  let side_result = Always.Variable.wire ~default:(of_string "100") in
  let wait_period = Always.Variable.reg ~width:2 ~enable:vdd r_sync in
  Always.(
    compile
      [
        sm.switch
          [
            ( GreenMainRedSide,
              [
                main_result <-- Signal.of_string "010";
                side_result <-- Signal.of_string "100";
                when_ (i.button ==: vdd)
                  [ wait_period <--. 2; sm.set_next YellowMainRedSide ]
                (* when_ is like if_ without an else case *);
              ] );
            ( YellowMainRedSide,
              [
                main_result <-- Signal.of_string "110";
                side_result <-- Signal.of_string "100";
                sm.set_next RedMainGreenSide;
              ] );
            ( RedMainGreenSide,
              [
                main_result <-- Signal.of_string "100";
                side_result <-- Signal.of_string "010";
                if_ (wait_period.value ==:. 0)
                   [ sm.set_next RedMainYellowSide ]
                   [ wait_period <-- wait_period.value -:. 1 ];
              ] );
            ( RedMainYellowSide,
              [
                main_result <-- Signal.of_string "100";
                side_result <-- Signal.of_string "110";
                sm.set_next GreenMainRedSide;
              ] );
          ];
      ]);
  { O.state = sm.current; main = main_result.value; side = side_result.value }

let traffic_testbench (sim : (_ I.t, _ O.t) Cyclesim.t) =
  let inputs, outputs = (Cyclesim.inputs sim, Cyclesim.outputs sim) in
  let print_state_and_outputs () =
    let state = List.nth_exn States.all (Bits.to_int !(outputs.state)) in
    let main = Bits.to_string !(outputs.main) in
    let side = Bits.to_string !(outputs.side) in
    Stdio.print_s [%message (state : States.t) (main : string) (side : string)]
  in

  (* Cycle 0 *)
  Cyclesim.reset sim;
  inputs.clear := Bits.vdd;
  Cyclesim.cycle sim;
  print_state_and_outputs ();

  (* Cycle 1 *)
  inputs.clear := Bits.gnd;
  Cyclesim.cycle sim;
  print_state_and_outputs ();

  (* Cycle 2 *)
  Cyclesim.cycle sim;
  print_state_and_outputs ();

  (* Cycle 3 *)
  inputs.button := Bits.vdd;
  Cyclesim.cycle sim;
  print_state_and_outputs ();

  (* Cycle 4 *)
  inputs.button := Bits.gnd;
  Cyclesim.cycle sim;
  print_state_and_outputs ();

  (* Cycle 5 *)
  Cyclesim.cycle sim;
  print_state_and_outputs ();

  (* Cycle 6 *)
  Cyclesim.cycle sim;
  print_state_and_outputs ();

  (* Cycle 7 *)
  Cyclesim.cycle sim;
  print_state_and_outputs ();

  (* Cycle 8 *)
  Cyclesim.cycle sim;
  print_state_and_outputs ();

  (* Cycle 9 *)
  Cyclesim.cycle sim;
  print_state_and_outputs ()

let print_simulation () =
  let module Sim = Cyclesim.With_interface (I) (O) in
  let sim = Sim.create create in
  traffic_testbench sim
