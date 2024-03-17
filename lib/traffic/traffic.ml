
(* module States = struct
     type t =
       | RedMainGreenSide
       | RedMainYellowSide
       | GreenMainRedSide
       | YellowMainRedSide
     [@@deriving sexp_of, compare, enumerate]
   end *)

(* let clock = input "clock" 1
   let clear = input "clear" 1
   let r_sync = Reg_spec.create ~clock ~clear ()
   let start = input "start" 1

   let outputs =
     let sm = Always.State_machine.create (module States) ~enable:vdd r_sync in
     let done_ = Always.Variable.wire ~default:gnd in
     Always.(
       compile
         [
           sm.switch
             [
               (RedMainGreenSide, [ when_ start [ sm.set_next RedMainGreenSide ] ]);
               (RedMainYellowSide, [ sm.set_next GreenMainRedSide ]);
               (GreenMainRedSide, [ sm.set_next YellowMainRedSide ]);
               (YellowMainRedSide, [ sm.set_next RedMainGreenSide ]);
             ];
         ]);
     [ output "done" done_.value; output "state" sm.current ] *)