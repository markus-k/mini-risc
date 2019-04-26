library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use std.textio.all;

entity rom is
  port (
    clk  : in  std_logic;
    nres : in  std_logic;

    do   : out std_logic_vector(15 downto 0);
    addr : in  std_logic_vector(15 downto 0));
end entity rom;

architecture rtl of rom is
  constant rom_size : natural := 2**16;
  type rom_type is array (0 to rom_size-1) of std_logic_vector(15 downto 0);

  impure function rom_init(filename : string) return rom_type is
    file rom_file : text open read_mode is filename;
    variable rom_line : line;
    variable rom_value : bit_vector(15 downto 0);
    variable temp : rom_type;
  begin
    temp := (others => (others => '0'));

    for rom_index in 0 to rom_size-1 loop
      if not endfile(rom_file) then
        readline(rom_file, rom_line);
        read(rom_line, rom_value);
        temp(rom_index) := to_stdlogicvector(rom_value);
      end if;
    end loop;

    return temp;
  end function;

  constant rom_data : rom_type := rom_init(filename => "asm/test.out");
begin
  process (clk) is
  begin
    if clk'event and clk = '1' then
      do <= rom_data(to_integer(unsigned(addr)));
    end if;
  end process;
end architecture rtl;
