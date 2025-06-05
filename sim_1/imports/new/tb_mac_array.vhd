--------------------------------------------------------------------------------
-- File: tb_mac_array.vhd   (revised so that we wait an extra clock after done_all)
--------------------------------------------------------------------------------
library ieee;
  use ieee.std_logic_1164.all;
  use ieee.numeric_std.all;
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
    constant INPUT_BITWIDTH : integer := 15;   -- each MAC sees a 16-bit input (15 downto 0)
    constant NUM_MACS       : integer := 4;    -- we will instantiate 4 MACs

    --------------------------------------------------------------------
    -- 2) "Sub-signals" for each MAC's 16-bit inputs and 1-bit add flag
    --------------------------------------------------------------------
    signal img0,    img1,    img2,    img3    : std_logic_vector(INPUT_BITWIDTH downto 0) := (others => '0');
    signal weight0, weight1, weight2, weight3 : std_logic_vector(INPUT_BITWIDTH downto 0) := (others => '0');
    signal bias0,   bias1,   bias2,   bias3   : std_logic_vector(INPUT_BITWIDTH downto 0) := (others => '0');

    signal add0,    add1,    add2,    add3    : std_logic := '0';

    --------------------------------------------------------------------
    -- 3) We still need the 64-bit buses that mac_array expects:
    --------------------------------------------------------------------
    signal img_vals    : std_logic_vector((INPUT_BITWIDTH+1)*NUM_MACS - 1 downto 0) := (others => '0');
    signal weight_vals : std_logic_vector((INPUT_BITWIDTH+1)*NUM_MACS - 1 downto 0) := (others => '0');
    signal biases      : std_logic_vector((INPUT_BITWIDTH+1)*NUM_MACS - 1 downto 0) := (others => '0');

    -- Pack all four addX bits into a 4-bit "adds"
    signal adds        : std_logic_vector(NUM_MACS - 1 downto 0) := (others => '0');

    --------------------------------------------------------------------
    -- 4) Output from mac_array: one 4-bit "done_all" and 128-bit "out_vals"
    --------------------------------------------------------------------
    signal done_all  : std_logic_vector(NUM_MACS - 1 downto 0);
    signal out_vals  : std_logic_vector((2*(INPUT_BITWIDTH+1))*NUM_MACS - 1 downto 0);

    --------------------------------------------------------------------
    -- 5) Now introduce four separate 32-bit signals for each MAC's result:
    --------------------------------------------------------------------
    signal out0, out1, out2, out3 : std_logic_vector(31 downto 0);

    --------------------------------------------------------------------
    -- 6) "All 1s" constant to compare done_all against
    --------------------------------------------------------------------
    constant ALL_DONE : std_logic_vector(NUM_MACS - 1 downto 0) := (others => '1');

    --------------------------------------------------------------------
    -- 7) Clock, reset, and global enable
    --------------------------------------------------------------------
    signal clk    : std_logic := '0';
    signal rstn   : std_logic := '0';
    signal en_all : std_logic := '0';

begin

    --------------------------------------------------------------------
    -- 8) Concatenate the four 16-bit imgX into a single 64-bit img_vals
    --------------------------------------------------------------------
    img_vals    <= img3 & img2 & img1 & img0;
    weight_vals <= weight3 & weight2 & weight1 & weight0;
    biases      <= bias3 & bias2 & bias1 & bias0;
    adds        <= add3 & add2 & add1 & add0;
    -- Note bit-ordering: 
    --   img_vals(15 downto 0)   = img0
    --   img_vals(31 downto 16)  = img1
    --   img_vals(47 downto 32)  = img2
    --   img_vals(63 downto 48)  = img3
    --   adds(0) = add0, ..., adds(3) = add3

    --------------------------------------------------------------------
    -- 9) Split the 128-bit "out_vals" into four 32-bit slices:
    --------------------------------------------------------------------
    out0 <= out_vals(31   downto 0);
    out1 <= out_vals(63   downto 32);
    out2 <= out_vals(95   downto 64);
    out3 <= out_vals(127  downto 96);

    --------------------------------------------------------------------
    -- 10) Instantiate mac_array (unchanged)
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
    -- 11) Clock generator (10 ns period)
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
    -- 12) Stimulus: drive each imgX, weightX, biasX, addX separately
    --------------------------------------------------------------------
    stim_proc: process
        -- A small array type of sfixed so we can build up four Q7.8 values
        type sfixed_arr_t is array (0 to NUM_MACS-1) of sfixed(INT_BITS downto -FRAC_BITS);

        variable img_sfxs  : sfixed_arr_t;
        variable wgt_sfxs  : sfixed_arr_t;
        variable bias_sfxs : sfixed_arr_t;

        -- For building a human-readable message:
        variable result_msg : string(1 to 200);
        variable idx        : integer;
        variable ival       : integer;
        variable slv32      : std_logic_vector(31 downto 0);
    begin
        ----------------------------------------------------------------
        -- (a) Assert reset (active-low) for 20 ns
        ----------------------------------------------------------------
        rstn   <= '0';
        en_all <= '0';

        -- Initialize all sub-signals to zero
        img0    <= (others => '0');
        img1    <= (others => '0');
        img2    <= (others => '0');
        img3    <= (others => '0');
        weight0 <= (others => '0');
        weight1 <= (others => '0');
        weight2 <= (others => '0');
        weight3 <= (others => '0');
        bias0   <= (others => '0');
        bias1   <= (others => '0');
        bias2   <= (others => '0');
        bias3   <= (others => '0');
        add0    <= '0';
        add1    <= '0';
        add2    <= '0';
        add3    <= '0';

        wait for 20 ns;

        ----------------------------------------------------------------
        -- (b) De-assert reset, wait 20 ns
        ----------------------------------------------------------------
        rstn <= '1';
        wait for 20 ns;

        ----------------------------------------------------------------
        -- (c) Build four Q7.8 "img, wgt, bias" values for each MAC:
        ----------------------------------------------------------------
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

        ----------------------------------------------------------------
        -- (d) Drive each sub-signal from its sfixed value
        ----------------------------------------------------------------
        img0    <= to_slv(img_sfxs(0));
        weight0 <= to_slv(wgt_sfxs(0));
        bias0   <= to_slv(bias_sfxs(0));
        add0    <= '0';  -- multiply+add path

        img1    <= to_slv(img_sfxs(1));
        weight1 <= to_slv(wgt_sfxs(1));
        bias1   <= to_slv(bias_sfxs(1));
        add1    <= '0';

        img2    <= to_slv(img_sfxs(2));
        weight2 <= to_slv(wgt_sfxs(2));
        bias2   <= to_slv(bias_sfxs(2));
        add2    <= '0';

        img3    <= to_slv(img_sfxs(3));
        weight3 <= to_slv(wgt_sfxs(3));
        bias3   <= to_slv(bias_sfxs(3));
        add3    <= '0';

        ----------------------------------------------------------------
        -- (e) Pulse the global enable for exactly one clock edge
        ----------------------------------------------------------------
        en_all <= '1';
        wait until rising_edge(clk);
        en_all <= '0';

        ----------------------------------------------------------------
        -- (f) Wait until all four done_all bits are '1'
        ----------------------------------------------------------------
        wait until done_all = ALL_DONE;

        -- *** ADD THIS EXTRA WAIT HERE *** : step into the bias-add cycle
        wait until rising_edge(clk);

        ----------------------------------------------------------------
        -- (g) Extract each 32-bit slice from out_vals → out0..out3
        --     Convert each to integer (raw Q15.16 count) and report.
        ----------------------------------------------------------------
        result_msg := (others => ' ');
        for idx in 0 to NUM_MACS-1 loop
            -- pick out the 32-bit chunk:
            slv32 := out_vals((idx+1)*32 - 1 downto idx*32);

            -- convert signed(32) → integer
            ival := to_integer(signed(slv32));

            -- append "MACn=ival" to result_msg
            result_msg := result_msg & "MAC" & integer'image(idx) &
                          "=" & integer'image(ival) & "   ";
        end loop;

        report "=== RAW RESULTS (Q15.16) === " & result_msg severity note;

        ----------------------------------------------------------------
        -- (h) Freeze here so you can inspect the waveform in Vivado
        ----------------------------------------------------------------
        wait;
    end process stim_proc;

end architecture sim;
