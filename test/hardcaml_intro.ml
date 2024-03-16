let%expect_test "should produce correct vhdl" =
  Combinational.vhdl_of_adder_circuit ();
  [%expect {|
    library ieee;
    use ieee.std_logic_1164.all;
    use ieee.numeric_std.all;

    entity my_adder is
        port (
            b : in std_logic_vector(7 downto 0);
            a : in std_logic_vector(7 downto 0);
            c : out std_logic_vector(7 downto 0)
        );
    end entity;

    architecture rtl of my_adder is

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
        signal hc_4 : std_logic_vector(7 downto 0);

    begin

        hc_4 <= hc_slv(hc_uns(a) + hc_uns(b));
        c <= hc_4;

    end architecture;
    |}]
