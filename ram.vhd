library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity ram is
  generic (
    ram_size : natural := 64);

  port (
    clk  : in  std_logic;
    nres : in  std_logic;

    di   : in  std_logic_vector(15 downto 0);
    do   : out std_logic_vector(15 downto 0);
    we   : in  std_logic;
    addr : in  std_logic_vector(15 downto 0));
end entity ram;

architecture rtl of ram is
  type ram_type is array (0 to ram_size-1) of std_logic_vector(15 downto 0);
  signal ram_data : ram_type := (others => (others => '0'));
begin
  process (clk) is
    variable addr_v : std_logic_vector(14 downto 0);
  begin
    addr_v := (others => '0');

    if clk'event and clk = '1' then
      addr_v := addr(addr'high downto 1); -- *2 for array access

      if we = '1' then
        ram_data(to_integer(unsigned(addr_v))) <= di;
      end if;

      do <= ram_data(to_integer(unsigned(addr_v)));
    end if;
  end process;
end architecture rtl;
