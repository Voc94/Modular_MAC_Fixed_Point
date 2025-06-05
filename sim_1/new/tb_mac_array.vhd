-- File: tb_mac_array.vhd
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

-- We need the same fixed-point packages that mac_unit uses:
use work.fixed_pkg.all;
use work.fixed_float_types.all;

entity tb_mac_array is
end tb_mac_array;

architecture sim of tb_mac_array is

    --------------------------------------------------------------------
    -- 1) Match the generics of mac_array
    --------------------------------------------------------------------
    constant INT_BITS       : integer := 7;    -- as in mac_array
    constant FRAC_BITS      : integer := 8;    -- as in mac_array
    constant INPUT_BITWIDTH : integer := 15;   -- each MAC sees a 16-bit input
    constant NUM_MACS       : integer := 4;    -- we will instantiate 4 MACs

    -- Clock, reset, global enable
    signal clk      : std_logic := '0';
    signal rstn     : std_logic := '0';
    signal en_all   : std_logic := '0';

    --------------------------------------------------------------------
    -- 2) Buses for all MACs' inputs (concatenate 4×16 bits = 64 bits total)
    --------------------------------------------------------------------
    signal img_vals    : std_logic_vector((INPUT_BITWIDTH+1)*NUM_MACS - 1 downto 0)
                        := (others => '0');
    signal weight_vals : std_logic_vector((INPUT_BITWIDTH+1)*NUM_MACS - 1 downto 0)
                        := (others => '0');
    signal biases      : std_logic_vector((INPUT_BITWIDTH+1)*NUM_MACS - 1 downto 0)
                        := (others => '0');

    -- One "add" bit per MAC
    signal adds        : std_logic_vector(NUM_MACS - 1 downto 0) := (others => '0');

    --------------------------------------------------------------------
    -- 3) Outputs: each MAC drives a done flag and a 32-bit result → total 4×32 = 128 bits
    --------------------------------------------------------------------
    signal done_all  : std_logic_vector(NUM_MACS - 1 downto 0);
    signal out_vals  : std_logic_vector((2*(INPUT_BITWIDTH+1))*NUM_MACS - 1 downto 0);

begin

    --------------------------------------------------------------------
    -- 4) Instantiate mac_array
    --------------------------------------------------------------------
    MAC_ARRAY_INST : entity work.mac_array
        generic map (
            INT_BITS        => INT_BITS,
            FRAC_BITS       => FRAC_BITS,
            INPUT_BITWIDTH  => INPUT_BITWIDTH,
            NUM_MACS        => NUM_MACS
        )
        port map (
            clk         => clk,
            rstn        => rstn,
            img_vals    => img_vals,
            weight_vals => weight_vals,
            biases      => biases,
            adds        => adds,
            en_all      => en_all,
            done_all    => done_all,
            out_vals    => out_vals
        );

    --------------------------------------------------------------------
    -- 5) Clock generation: 10 ns period (100 MHz)
    --------------------------------------------------------------------
    clk_proc: process
    begin
        while true loop
            clk <= '0';
            wait for 5 ns;
            clk <= '1';
            wait for 5 ns;
        end loop;
    end process clk_proc;

    --------------------------------------------------------------------
    -- 6) Stimulus: apply reset, then drive four MACs in parallel, wait for done
    --------------------------------------------------------------------
    stim_proc: process
        -- Helper variables to hold each fixed-point literal before concatenation:
        variable img_sfxs    : array (0 to NUM_MACS-1) of sfixed(INT_BITS downto -FRAC_BITS);
        variable wgt_sfxs    : array (0 to NUM_MACS-1) of sfixed(INT_BITS downto -FRAC_BITS);
        variable bias_sfxs   : array (0 to NUM_MACS-1) of sfixed(INT_BITS downto -FRAC_BITS);
        variable tmp_img_bus : std_logic_vector((INPUT_BITWIDTH+1)*NUM_MACS - 1 downto 0);
        variable tmp_wgt_bus : std_logic_vector((INPUT_BITWIDTH+1)*NUM_MACS - 1 downto 0);
        variable tmp_bias_bus: std_logic_vector((INPUT_BITWIDTH+1)*NUM_MACS - 1 downto 0);
    begin
        -- a) Initial reset
        rstn <= '0';
        en_all <= '0';
        wait for 20 ns;

        -- b) Release reset
        rstn <= '1';
        wait for 20 ns;

        ----------------------------------------------------------------
        -- c) Prepare four different fixed-point values (Q7.8) for each MAC:
        ----------------------------------------------------------------
        -- Example values (you can choose any real numbers you like):
        --   MAC 0:   img = 1.0, weight = 2.0, bias =  0.5
        --   MAC 1:   img = 1.5, weight = 0.5, bias =  1.0
        --   MAC 2:   img = 2.0, weight = 1.0, bias = -0.5
        --   MAC 3:   img = 3.75, weight = 2.25, bias = 0.25

        img_sfxs(0)  := to_sfixed(1.0,    INT_BITS, -FRAC_BITS);
        wgt_sfxs(0)  := to_sfixed(2.0,    INT_BITS, -FRAC_BITS);
        bias_sfxs(0) := to_sfixed(0.5,    INT_BITS, -FRAC_BITS);

        img_sfxs(1)  := to_sfixed(1.5,    INT_BITS, -FRAC_BITS);
        wgt_sfxs(1)  := to_sfixed(0.5,    INT_BITS, -FRAC_BITS);
        bias_sfxs(1) := to_sfixed(1.0,    INT_BITS, -FRAC_BITS);

        img_sfxs(2)  := to_sfixed(2.0,    INT_BITS, -FRAC_BITS);
        wgt_sfxs(2)  := to_sfixed(1.0,    INT_BITS, -FRAC_BITS);
        bias_sfxs(2) := to_sfixed(-0.5,   INT_BITS, -FRAC_BITS);

        img_sfxs(3)  := to_sfixed(3.75,   INT_BITS, -FRAC_BITS);
        wgt_sfxs(3)  := to_sfixed(2.25,   INT_BITS, -FRAC_BITS);
        bias_sfxs(3) := to_sfixed(0.25,   INT_BITS, -FRAC_BITS);

        -- d) Pack each 16-bit sfixed into a 64-bit bus (4×16 = 64)
        for i in 0 to NUM_MACS-1 loop
            tmp_img_bus(((i+1)*(INPUT_BITWIDTH+1)-1) downto (i*(INPUT_BITWIDTH+1)))  :=
              to_slv(img_sfxs(i));
            tmp_wgt_bus(((i+1)*(INPUT_BITWIDTH+1)-1) downto (i*(INPUT_BITWIDTH+1)))  :=
              to_slv(wgt_sfxs(i));
            tmp_bias_bus(((i+1)*(INPUT_BITWIDTH+1)-1) downto (i*(INPUT_BITWIDTH+1))) :=
              to_slv(bias_sfxs(i));
        end loop;

        -- Drive the signals
        img_vals    <= tmp_img_bus;
        weight_vals <= tmp_wgt_bus;
        biases      <= tmp_bias_bus;

        -- Let's do "multiply+add" (add='0') for all four MACs initially:
        adds <= (others => '0');

        -- e) Pulse global enable for one clock cycle
        en_all <= '1';
        wait until rising_edge(clk);
        en_all <= '0';

        -- f) Now wait until all four done_all bits go high
        wait until done_all = (others => '1');

        ----------------------------------------------------------------
        -- g) At this point, each MAC has computed: (img*weight)+bias.
        --    We can optionally "report" the raw 32-bit codes or freeze.
        ----------------------------------------------------------------
        report "=== RAW RESULTS BUS ===" &
               " out_vals = " & to_hstring(out_vals)
               severity note;

        -- h) Freeze the simulation so you can inspect waveforms
        wait;
    end process stim_proc;

end architecture sim;
