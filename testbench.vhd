library ieee;
use ieee.std_logic_1164.all;

entity testbench is
end entity testbench;

architecture bhv of testbench is
  constant clkHalfPeriod : time := 10 ns;
  constant clkFullPeriod : time := clkHalfPeriod * 2;

  component soc is
    port (
      clk : in std_logic;
      nres : in std_logic;

      gpio_a : inout std_logic_vector(15 downto 0));
  end component soc;

  signal simulation_done : boolean := false;
  signal clk_s : std_logic;
  signal nres_s : std_logic;
  signal gpio_a_s : std_logic_vector(15 downto 0);
begin  -- architecture bhv

  clkgen : process is
  begin
    while not simulation_done loop
      clk_s <= '1';
      wait for clkHalfPeriod;
      clk_s <= '0';
      wait for clkHalfPeriod;
    end loop;
    wait;
  end process;

  nresgen : process is
  begin
    nres_s <= '0';
    wait for clkFullPeriod * 2;
    nres_s <= '1';
    wait;
  end process;

  process is
  begin
    simulation_done <= false;
    wait for clkFullPeriod * 512;
    simulation_done <= true;
    wait;
  end process;

  dut : soc
    port map (
      clk => clk_s,
      nres => nres_s,

      gpio_a => gpio_a_s);
end architecture bhv;
