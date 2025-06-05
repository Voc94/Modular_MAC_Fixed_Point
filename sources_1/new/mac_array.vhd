--------------------------------------------------------------------------------
-- File: mac_array.vhd
-- A wrapper that instantiates N identical mac_unit's in parallel.
--------------------------------------------------------------------------------
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity mac_array is
  generic
    (
      INT_BITS        : integer := 7;    -- same generic as mac_unit
      FRAC_BITS       : integer := 8;    -- 
      INPUT_BITWIDTH  : integer := 15;   -- each individual MAC sees 16-bit inputs
      NUM_MACS        : integer := 4     -- number of parallel MAC units
    );
  port
    (
      clk        : in  std_logic;
      rstn       : in  std_logic;
      
      -- We will drive all MACs in one big "bus":
      -- Each MAC needs its own img, weight, bias, and add flag.
      img_vals    : in  std_logic_vector((INPUT_BITWIDTH+1)*NUM_MACS - 1 downto 0);
      weight_vals : in  std_logic_vector((INPUT_BITWIDTH+1)*NUM_MACS - 1 downto 0);
      biases      : in  std_logic_vector((INPUT_BITWIDTH+1)*NUM_MACS - 1 downto 0);
      adds        : in  std_logic_vector(NUM_MACS - 1 downto 0);

      en_all      : in  std_logic;   -- one "global" enable for all MACs
      done_all    : out std_logic_vector(NUM_MACS - 1 downto 0);
      
      -- Each MAC produces a 32-bit out; we pack them all into one big vector:
      out_vals    : out std_logic_vector((2*(INPUT_BITWIDTH+1))*NUM_MACS - 1 downto 0)
    );
end entity mac_array;

architecture rtl of mac_array is

  --------------------------------------------------------------------------
  -- Component declaration for mac_unit (we assume mac_unit.vhd is compiled 
  -- into WORK already).  If you prefer not to use a component block, you can
  -- also just write:
  --   DUT: entity work.mac_unit
  -- But for clarity we show a component here.
  --------------------------------------------------------------------------
  component mac_unit
    generic
      (
        INT_BITS        : integer := 7;
        FRAC_BITS       : integer := 8;
        INPUT_BITWIDTH  : integer := 15
      );
    port
      (
        clk         : in  std_logic;
        rstn        : in  std_logic;
        en          : in  std_logic;
        add         : in  std_logic;
        img_val     : in  std_logic_vector(INPUT_BITWIDTH downto 0);
        weight_val  : in  std_logic_vector(INPUT_BITWIDTH downto 0);
        bias        : in  std_logic_vector(INPUT_BITWIDTH downto 0);
        done        : out std_logic;
        out_val     : out std_logic_vector(2*INPUT_BITWIDTH+1 downto 0)
      );
  end component;

  -- A small function to extract a slice from a big vector:
  function slice_vec(
    big_vec : std_logic_vector;
    idx     : integer;      -- which MAC index (0 to NUM_MACS-1)
    width   : integer       -- how many bits per MAC port
  ) return std_logic_vector is
    variable start_bit : integer := idx * width;
    variable slice_w   : integer := width - 1;
    variable tmp       : std_logic_vector(width - 1 downto 0);
  begin
    tmp := big_vec(start_bit + slice_w downto start_bit);
    return tmp;
  end function slice_vec;

begin

  --------------------------------------------------------------------------
  -- Generate loop: create NUM_MACS copies of mac_unit, each seeing its own 
  -- img_val, weight_val, bias, add, and producing its own done and out_val.
  --------------------------------------------------------------------------
  gen_macs : for i in 0 to NUM_MACS - 1 generate

    signal img_i    : std_logic_vector(INPUT_BITWIDTH downto 0);
    signal wgt_i    : std_logic_vector(INPUT_BITWIDTH downto 0);
    signal bias_i   : std_logic_vector(INPUT_BITWIDTH downto 0);
    signal add_i    : std_logic;
    signal done_i   : std_logic;
    signal out_i    : std_logic_vector(2*INPUT_BITWIDTH+1 downto 0);

  begin
    -- Extract the i-th slice for img, weight, bias:
    img_i    <= slice_vec(img_vals,    i, INPUT_BITWIDTH+1);
    wgt_i    <= slice_vec(weight_vals, i, INPUT_BITWIDTH+1);
    bias_i   <= slice_vec(biases,      i, INPUT_BITWIDTH+1);
    add_i    <= adds(i);

    MAC_INST : mac_unit
      generic map (
        INT_BITS       => INT_BITS,
        FRAC_BITS      => FRAC_BITS,
        INPUT_BITWIDTH => INPUT_BITWIDTH
      )
      port map (
        clk         => clk,
        rstn        => rstn,
        en          => en_all,
        add         => add_i,
        img_val     => img_i,
        weight_val  => wgt_i,
        bias        => bias_i,
        done        => done_i,
        out_val     => out_i
      );

    -- Connect each instance's done_i and out_i back to the top-level bus:
    done_all(i)  <= done_i;
    out_vals((i+1)*(2*(INPUT_BITWIDTH+1)) - 1 downto i*(2*(INPUT_BITWIDTH+1))) <= out_i;
  end generate gen_macs;

end architecture rtl;
