library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity rom is
  port (
    clk  : in  std_logic;
    nres : in  std_logic;

    do   : out std_logic_vector(15 downto 0);
    addr : in  std_logic_vector(15 downto 0));
end entity rom;

architecture rtl of rom is
  type rom_type is array (0 to 2**16-1) of std_logic_vector(15 downto 0);
  signal rom_data : rom_type := (
    ( X"1" & X"0" & X"05" ), -- movil r0, 0x05
    ( X"2" & X"0" & X"01" ), -- movih r0, 0x01
    ( X"1" & X"1" & X"06" ), -- movih r1, 0x06
    ( X"1" & X"2" & X"00" ), -- movil r2, 0x00
    ( X"2" & X"2" & X"00" ), -- movih r2, 0x00

    ( X"1" & X"3" & X"01" ), -- movil r3, 0x01
    ( X"2" & X"3" & X"00" ), -- movih r3, 0x00

    ( X"1" & X"4" & X"09" ), -- movil r4, 0x09
    ( X"2" & X"4" & X"00" ), -- movih r4, 0x00

    ( X"4" & X"1" & X"1" & X"3" ), -- sub r1, r1, r3
    ( X"3" & X"2" & X"2" & X"0" ), -- add r2, r2, r0
    ( X"A" & X"0" & X"1" & X"5" ), -- cmp r1, r5 (0)
    ( X"B" & X"4" & X"2" & X"0" ), -- jmpne r4

    ( X"9" & X"fff" ), -- jmp .
    others => (others => '0') -- nop
    );
begin
  process (clk) is
  begin
    if clk'event and clk = '1' then
      do <= rom_data(to_integer(unsigned(addr)));
    end if;
  end process;
end architecture rtl;