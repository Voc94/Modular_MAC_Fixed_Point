-- File: mac_unit.vhd
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.fixed_float_types.all;
use work.fixed_pkg.all;

entity mac_unit is
    generic (
        INT_BITS        : integer := 7;     -- number of integer bits (excluding sign)
        FRAC_BITS       : integer := 8;     -- number of fractional bits
        INPUT_BITWIDTH  : integer := 15     -- "15" means each input is 16 bits wide (15 downto 0)
    );
    port (
        clk         : in  std_logic;
        rstn        : in  std_logic;
        en          : in  std_logic;
        add         : in  std_logic;
        img_val     : in  std_logic_vector(INPUT_BITWIDTH downto 0);  -- 16 bits
        weight_val  : in  std_logic_vector(INPUT_BITWIDTH downto 0);  -- 16 bits
        bias        : in  std_logic_vector(INPUT_BITWIDTH downto 0);  -- 16 bits
        done        : out std_logic;
        out_val     : out std_logic_vector(2*INPUT_BITWIDTH+1 downto 0) -- 32 bits (31 downto 0)
    );
end mac_unit;

architecture rtl of mac_unit is

    -------------------------------------------------------------------
    -- 1) Fixed-point subtypes
    -------------------------------------------------------------------
    constant L : integer := INT_BITS;          -- e.g. 7
    constant R : integer := -FRAC_BITS;        -- e.g. -8

    -- T_SFIX = sfixed(7 downto -8) → 16 bits total
    subtype T_SFIX is sfixed(L downto R);

    -- T_ACC = sfixed(2*7+1 downto -2*8) = sfixed(15 downto -16) → 32 bits total
    subtype T_ACC  is sfixed(2*L+1 downto 2*R);

    -------------------------------------------------------------------
    -- 2) State machine
    -------------------------------------------------------------------
    type state_type is (STATE_IDLE, STATE_MULT, STATE_ACCM);
    signal state : state_type := STATE_IDLE;

    -------------------------------------------------------------------
    -- 3) "sfixed" registers to hold converted inputs and accumulator
    -------------------------------------------------------------------
    signal img_fx   : T_SFIX := (others => '0');
    signal w_fx     : T_SFIX := (others => '0');
    signal bias_fx  : T_SFIX := (others => '0');
    signal acc_fx   : T_ACC  := (others => '0');

    -------------------------------------------------------------------
    -- 4) Internal done flag (registered)
    -------------------------------------------------------------------
    signal done_int : std_logic := '0';

begin

    -------------------------------------------------------------------
    -- Main synchronous process: reset / multiply / add-accumulate / output
    -------------------------------------------------------------------
    process(clk, rstn)
    begin
        if rstn = '0' then
            -- Asynchronous reset: clear everything
            state    <= STATE_IDLE;
            img_fx   <= (others => '0');
            w_fx     <= (others => '0');
            bias_fx  <= (others => '0');
            acc_fx   <= (others => '0');
            done_int <= '0';

        elsif rising_edge(clk) then
            -- By default, clear done flag each cycle
            done_int <= '0';

            case state is
                ------------------------------------------------------------------
                when STATE_IDLE =>
                    if en = '1' then
                        -- Convert the 16-bit std_logic_vector inputs into sfixed(7 downto -8)
                        img_fx  <= to_sfixed(img_val,  L, R);
                        w_fx    <= to_sfixed(weight_val, L, R);
                        bias_fx <= to_sfixed(bias,      L, R);
                        state   <= STATE_MULT;
                    end if;

                ------------------------------------------------------------------
                when STATE_MULT =>
                    if add = '0' then
                        -- Multiply: img_fx * w_fx → raw width is (14 downto -16) or larger
                        -- Resize result back into T_ACC = sfixed(15 downto -16)
                        acc_fx <= resize(img_fx * w_fx, T_ACC'high, T_ACC'low);
                    else
                        -- Shift-left (×2^8) if add='1'
                        acc_fx <= resize(scalb(img_fx, FRAC_BITS), T_ACC'high, T_ACC'low);
                    end if;
                    state <= STATE_ACCM;

                ------------------------------------------------------------------
                when STATE_ACCM =>
                    -- Add bias: (acc_fx + bias_fx). Each must be resized to T_ACC first.
                    acc_fx <= resize(
                                 acc_fx 
                                 + resize(bias_fx, T_ACC'high, T_ACC'low), 
                                 T_ACC'high, 
                                 T_ACC'low
                              );
                    done_int <= '1';
                    state    <= STATE_IDLE;

                when others =>
                    state <= STATE_IDLE;
            end case;
        end if;
    end process;

    -------------------------------------------------------------------
    -- 5) Drive outputs
    -------------------------------------------------------------------
    done    <= done_int;
    -- Convert T_ACC (32-bit sfixed) → 32-bit std_logic_vector
    out_val <= to_slv(acc_fx);

end rtl;
