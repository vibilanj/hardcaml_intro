let testbench x_val y_val c_val =
  let open Hardcaml in
  let simulator = Cyclesim.create Adder.full_adder_circuit in
  (* let inputs, outputs = Cyclesim.inputs simulator, Cyclesim.outputs simulator in  *)
  let sum = Cyclesim.out_port simulator "sum" in
  let carry = Cyclesim.out_port simulator "carry" in
  Cyclesim.reset simulator;
  Cyclesim.in_port simulator "x" := x_val;
  Cyclesim.in_port simulator "y" := y_val;
  Cyclesim.in_port simulator "c" := c_val;
  Cyclesim.cycle simulator;
  Format.printf "Sum: %a, Carry: %a@." Hardcaml.Bits.pp !sum Hardcaml.Bits.pp
    !carry

let%expect_test "should compute sum and carry properly for full_adder" =
  let open Hardcaml.Bits in
  testbench gnd gnd gnd;
  [%expect {| Sum: 0, Carry: 0 |}];
  testbench gnd vdd gnd;
  [%expect {| Sum: 1, Carry: 0 |}];
  testbench vdd vdd gnd;
  [%expect {| Sum: 0, Carry: 1 |}];
  testbench vdd gnd vdd;
  [%expect {| Sum: 0, Carry: 1 |}];
  testbench vdd vdd vdd;
  [%expect {| Sum: 1, Carry: 1 |}]

let%expect_test "should produce correct vhdl for full_adder" =
  Adder.vhdl_of_full_adder ();
  [%expect
    {|
    library ieee;
    use ieee.std_logic_1164.all;
    use ieee.numeric_std.all;

    entity full_adder is
        port (
            c : in std_logic;
            y : in std_logic;
            x : in std_logic;
            sum : out std_logic;
            carry : out std_logic
        );
    end entity;

    architecture rtl of full_adder is

        -- conversion functions
        function hc_uns(a : std_logic)        return unsigned         is variable b : unsigned(0 downto 0); begin b(0) := a; return b; end;
        function hc_uns(a : std_logic_vector) return unsigned         is begin return unsigned(a); end;
        function hc_sgn(a : std_logic)        return signed           is variable b : signed(0 downto 0); begin b(0) := a; return b; end;
        function hc_sgn(a : std_logic_vector) return signed           is begin return signed(a); end;
        function hc_sl (a : std_logic_vector) return std_logic        is begin return a(a'right); end;
        function hc_sl (a : unsigned)         return std_logic        is begin return a(a'right); end;
        function hc_sl (a : signed)           return std_logic        is begin return a(a'right); end;
        function hc_sl (a : boolean)          return std_logic        is begin if a then return '1'; else return '0'; end if; end;
        function hc_slv(a : std_logic_vector) return std_logic_vector is begin return a; end;
        function hc_slv(a : unsigned)         return std_logic_vector is begin return std_logic_vector(a); end;
        function hc_slv(a : signed)           return std_logic_vector is begin return std_logic_vector(a); end;
        signal hc_9 : std_logic;
        signal hc_7 : std_logic;
        signal hc_6 : std_logic;
        signal hc_8 : std_logic;
        signal hc_10 : std_logic;
        signal hc_11 : std_logic;
        signal hc_12 : std_logic;

    begin

        hc_9 <= hc_sl(hc_uns(y) and hc_uns(c));
        hc_7 <= hc_sl(hc_uns(x) and hc_uns(c));
        hc_6 <= hc_sl(hc_uns(x) and hc_uns(y));
        hc_8 <= hc_sl(hc_uns(hc_6) or hc_uns(hc_7));
        hc_10 <= hc_sl(hc_uns(hc_8) or hc_uns(hc_9));
        hc_11 <= hc_sl(hc_uns(y) xor hc_uns(c));
        hc_12 <= hc_sl(hc_uns(x) xor hc_uns(hc_11));
        sum <= hc_12;
        carry <= hc_10;

    end architecture;
    |}]

let%expect_test "should compute sum properly for ripple_carry_adder" =
  let open Hardcaml in
  let open Hardcaml.Bits in
  let simulator = Cyclesim.create Adder.ripple_carry_adder_circuit in
  let sum = Cyclesim.out_port simulator "sum" in
  Cyclesim.reset simulator;
  Cyclesim.in_port simulator "x" := of_int ~width:8 42;
  Cyclesim.in_port simulator "y" := of_int ~width:8 37;
  Cyclesim.in_port simulator "c" := of_int ~width:1 0;
  Cyclesim.cycle simulator;
  print_int (to_int !sum);
  [%expect {| 79 |}]
