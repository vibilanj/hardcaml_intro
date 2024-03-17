open Base
open Hardcaml
open Hardcaml.Signal
open Hardcaml_waveterm

module I = struct
  type 'a t = { clock : 'a; clear : 'a; start : 'a; n : 'a [@bits 8] }
  [@@deriving hardcaml]
end

module O = struct
  type 'a t = {
    done_ : 'a; [@rtlname "done"]
    result : 'a; [@bits 32]
    state : 'a; [@bits 2]
  }
  [@@deriving hardcaml]
end

module States = struct
  type t = S_wait | S_counting | S_write_result
  [@@deriving sexp_of, compare, enumerate]
end

let create (i : _ I.t) =
  let r_sync = Reg_spec.create ~clock:i.clock ~clear:i.clear () in
  let sm = Always.State_machine.create (module States) ~enable:vdd r_sync in
  let done_ = Always.Variable.wire ~default:gnd in
  let result = Always.Variable.wire ~default:(zero 32) in
  let f0 = Always.Variable.reg ~width:32 ~enable:vdd r_sync in
  let f1 = Always.Variable.reg ~width:32 ~enable:vdd r_sync in
  let remaining = Always.Variable.reg ~width:8 ~enable:vdd r_sync in
  Always.(
    compile
      [
        sm.switch
          [
            ( S_wait,
              [
                f0 <--. 1;
                f1 <--. 1;
                remaining <-- i.n -:. 1;
                when_ i.start
                  [
                    if_ (i.n ==:. 1)
                      [ sm.set_next S_write_result ]
                      [ sm.set_next S_counting ];
                  ];
              ] );
            ( S_counting,
              [
                if_ (remaining.value ==:. 0)
                  [ sm.set_next S_write_result ]
                  [
                    remaining <-- remaining.value -:. 1;
                    f0 <-- f1.value;
                    f1 <-- f0.value +: f1.value;
                  ];
              ] );
            ( S_write_result,
              [ done_ <--. 1; result <-- f1.value; sm.set_next S_wait ] );
          ];
      ]);
  { O.done_ = done_.value; result = result.value; state = sm.current }

let fibonacci_testbench (sim : (_ I.t, _ O.t) Cyclesim.t) =
  let inputs, outputs = (Cyclesim.inputs sim, Cyclesim.outputs sim) in
  let print_state_and_outputs () =
    let state = List.nth_exn States.all (Bits.to_int !(outputs.state)) in
    let done_ = Bits.is_vdd !(outputs.done_) in
    let result = Bits.to_int !(outputs.result) in
    Stdio.print_s [%message (state : States.t) (done_ : bool) (result : int)]
  in

  Cyclesim.reset sim;
  inputs.clear := Bits.vdd;
  Cyclesim.cycle sim;
  inputs.clear := Bits.gnd;

  (* Cycle 0 *)
  print_state_and_outputs ();

  (* Cycle 1 *)
  inputs.start := Bits.vdd;
  inputs.n := Bits.of_int ~width:8 4;
  Cyclesim.cycle sim;
  print_state_and_outputs ();
  inputs.start := Bits.gnd;

  (* Cycle 2 - Start counting the fibonacci number. *)
  Cyclesim.cycle sim;
  print_state_and_outputs ();

  (* Cycle 3 *)
  Cyclesim.cycle sim;
  print_state_and_outputs ();

  (* Cycle 4 *)
  Cyclesim.cycle sim;
  print_state_and_outputs ();

  (* Cycle 5 *)
  Cyclesim.cycle sim;
  print_state_and_outputs ();

  (* Cycle 6 *)
  Cyclesim.cycle sim;
  print_state_and_outputs ();

  Cyclesim.cycle sim

let waves = 
  let module Sim = Cyclesim.With_interface(I)(O) in 
  let sim = Sim.create create in 
  let waves, sim = Waveform.create sim in 
  fibonacci_testbench sim;
  waves