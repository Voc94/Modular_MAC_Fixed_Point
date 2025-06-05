-- File: tb_mac_unit.vhd
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.fixed_pkg.all;         -- for sfixed, to_sfixed, to_slv, resize, scalb
use work.fixed_float_types.all; -- in case mac_unit uses any float routines

entity tb_mac_unit is
end tb_mac_unit;

architecture sim of tb_mac_unit is

    --------------------------------------------------------------------
    -- 1) DUT generics & port signals
    --------------------------------------------------------------------
    constant INT_BITS       : integer := 7;   -- 7 integer bits (excl. sign)
    constant FRAC_BITS      : integer := 8;   -- 8 fractional bits
    constant INPUT_BITWIDTH : integer := 15;  -- each input is 16 bits (15 downto 0)

    signal clk        : std_logic := '0';
    signal rstn       : std_logic := '0';
    signal en         : std_logic := '0';
    signal add_path   : std_logic := '0';  -- '0' = multiply+add, '1' = shift+add

    signal img_val    : std_logic_vector(INPUT_BITWIDTH downto 0) := (others => '0');
    signal weight_val : std_logic_vector(INPUT_BITWIDTH downto 0) := (others => '0');
    signal bias       : std_logic_vector(INPUT_BITWIDTH downto 0) := (others => '0');

    signal done       : std_logic;
    signal out_val    : std_logic_vector(2*INPUT_BITWIDTH+1 downto 0);

begin

    --------------------------------------------------------------------
    -- 2) Instantiate the DUT (mac_unit)
    --------------------------------------------------------------------
    DUT: entity work.mac_unit
        generic map (
            INT_BITS        => INT_BITS,
            FRAC_BITS       => FRAC_BITS,
            INPUT_BITWIDTH  => INPUT_BITWIDTH
        )
        port map (
            clk         => clk,
            rstn        => rstn,
            en          => en,
            add         => add_path,
            img_val     => img_val,
            weight_val  => weight_val,
            bias        => bias,
            done        => done,
            out_val     => out_val
        );

    --------------------------------------------------------------------
    -- 3) 10 ns clock generation
    --------------------------------------------------------------------
    clk_proc: process
    begin
        while true loop
            clk <= '0'; wait for 5 ns;
            clk <= '1'; wait for 5 ns;
        end loop;
    end process;

    --------------------------------------------------------------------
    -- 4) Single stimulus: reset → drive values → wait for done → stop
    --------------------------------------------------------------------
    stim_proc: process
        -- Change #1: Declare 16-bit-wide sfixed variables (7 downto -8)
        variable img_sfx  : sfixed(INT_BITS downto -FRAC_BITS);
        variable wgt_sfx  : sfixed(INT_BITS downto -FRAC_BITS);
        variable bias_sfx : sfixed(INT_BITS downto -FRAC_BITS);
    begin
        -- a) Hold reset low for 20 ns
        rstn <= '0';
        wait for 20 ns;

        -- b) Release reset and let things settle
        rstn <= '1';
        wait for 20 ns;

        ----------------------------------------------------------------
        -- c) Drive the inputs: for example 1.2, 2.5, 3.75 (in Q7.8)
        ----------------------------------------------------------------
        img_sfx  := to_sfixed(1.2,  INT_BITS, -FRAC_BITS);   -- 16-bit: 1.2 × 2^8 = 307 → 0x0133
        wgt_sfx  := to_sfixed(2.5,  INT_BITS, -FRAC_BITS);   -- 2.5 × 2^8 = 640 → 0x0280
        bias_sfx := to_sfixed(3.75, INT_BITS, -FRAC_BITS);   -- 3.75 × 2^8 = 960 → 0x03C0

        img_val    <= to_slv(img_sfx);
        weight_val <= to_slv(wgt_sfx);
        bias       <= to_slv(bias_sfx);
        add_path   <= '0';  -- use multiply+add path

        -- d) Pulse "en" for one rising edge of clk
        en <= '1';
        wait until rising_edge(clk);
        en <= '0';

        -- e) Wait until the DUT asserts done = '1'
        wait until rising_edge(clk) and done = '1';

        ----------------------------------------------------------------
        -- f) Report raw integer codes (pop-up + Transcript)
        ----------------------------------------------------------------
        report "=== RAW VALUES ==="
            & " img_val    = " & integer'image(to_integer(signed(img_val)))
            & "  weight_val = " & integer'image(to_integer(signed(weight_val)))
            & "  bias       = " & integer'image(to_integer(signed(bias)))
            & "  out_val    = " & integer'image(to_integer(signed(out_val)))
        severity warning;  -- use 'warning' so Vivado pops up a dialog automatically

        -- g) Freeze simulation so you can inspect waveforms
        wait;
    end process;

end architecture sim;
