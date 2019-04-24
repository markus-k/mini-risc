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
  constant rom_data : rom_type := (
    ( X"1" & X"d" & X"ff" ), -- movil sp, 0xff
    ( X"2" & X"d" & X"00" ), -- movih sp, 0x00

    ( X"1" & X"0" & X"05" ), -- movil r0, 0x05
    ( X"2" & X"0" & X"01" ), -- movih r0, 0x01
    ( X"1" & X"1" & X"06" ), -- movih r1, 0x06
    ( X"1" & X"2" & X"00" ), -- movil r2, 0x00
    ( X"2" & X"2" & X"00" ), -- movih r2, 0x00

    ( X"1" & X"3" & X"01" ), -- movil r3, 0x01
    ( X"2" & X"3" & X"00" ), -- movih r3, 0x00

    ( X"1" & X"4" & X"0b" ), -- movil r4, 0x0b
    ( X"2" & X"4" & X"00" ), -- movih r4, 0x00

    ( X"4" & X"1" & X"1" & X"3" ), -- sub r1, r1, r3
    ( X"3" & X"2" & X"2" & X"0" ), -- add r2, r2, r0
    ( X"A" & X"0" & X"1" & X"5" ), -- cmp r1, r5 (0)
    ( X"B" & X"4" & X"2" & X"0" ), -- jmpne r4

    ( X"1" & X"5" & X"02" ), -- movil r5, 0x02

    ( X"D" & X"5" & X"2" & X"0" ), -- str r5, r2
    ( X"E" & X"2" & X"00"), -- push r2

    ( X"8" & X"2" & X"2" & "0" & "011" ), -- sht r2, r2, l, 3
    ( X"8" & X"2" & X"2" & "1" & "010" ), -- sht r2, r2, r, 2

    ( X"C" & X"1" & X"5" & X"0" ), -- ldr r2, r5
    ( X"F" & X"2" & X"00"), -- pop r2

    ( X"1" & X"0" & X"00" ), -- movil r0, 0x00
    ( X"2" & X"0" & X"80" ), -- movih r0, 0x80
    ( X"1" & X"1" & X"01" ), -- movih r1, 0x01
    ( X"2" & X"1" & X"00" ), -- movil r1, 0x00
    ( X"D" & X"0" & X"1" & X"0" ), -- str r0, r1
    ( X"1" & X"0" & X"02" ), -- movil r0, 0x00
    ( X"D" & X"0" & X"1" & X"0" ), -- str r0, r1

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
