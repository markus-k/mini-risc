library ieee;
use ieee.std_logic_1164.all;

entity top is
  port (
    clk_50 : in    std_logic;
    nres   : in    std_logic;
    led    : out   std_logic_vector(7 downto 0);
    gpio_a : inout std_logic_vector(15 downto 0));
end entity top;

architecture rtl of top is
  component soc is
    port (
      clk : in std_logic;
      nres : in std_logic;

      gpio_a : inout std_logic_vector(15 downto 0));
  end component soc;
  for all : soc use entity work.soc(rtl);
begin
  soc_1 : soc
    port map (
      clk => clk_50,
      nres => nres,

      gpio_a => gpio_a);

  led <= gpio_a(7 downto 0);
end architecture rtl;
