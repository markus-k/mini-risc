library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

-- GPIO peripheral with 16 pins
-- register map:
-- base: 0x8000, length: 16
-- | offset | name           | description                       |
-- |--------|----------------|-----------------------------------|
-- | 0x00   | GPIO_OE        | Output enable, 1 = out, 0 = in    |
-- | 0x02   | GPIO_SET       | Set bit register                  |
-- | 0x04   | GPIO_CLR       | Clear bit register                |
-- | 0x06   | GPIO_IN        | Inputs                            |
-- | 0x08   | GPIO_OUT       | Outputs                           |
--

entity periph_gpio is
  port (
    clk  : in std_logic;
    nres : in std_logic;

    -- gpio pins
    pins : inout std_logic_vector(15 downto 0);

    -- io bus
    io_di : in std_logic_vector(15 downto 0);
    io_do : out std_logic_vector(15 downto 0);
    io_we : in std_logic;
    io_addr : in std_logic_vector(15 downto 0));
end entity periph_gpio;

architecture rtl of periph_gpio is
  signal io_di_s : std_logic_vector(15 downto 0);
  signal io_do_s : std_logic_vector(15 downto 0);
  signal io_we_s : std_logic;
  signal io_addr_s : std_logic_vector(15 downto 0);

  signal io_do_ns : std_logic_vector(15 downto 0);
  signal io_do_cs : std_logic_vector(15 downto 0);

  signal gpio_pins_s : std_logic_vector(15 downto 0);

  signal gpio_oe_ns : std_logic_vector(15 downto 0);
  signal gpio_oe_cs : std_logic_vector(15 downto 0);
  signal gpio_in_ns : std_logic_vector(15 downto 0);
  signal gpio_in_cs : std_logic_vector(15 downto 0);
  signal gpio_out_ns : std_logic_vector(15 downto 0);
  signal gpio_out_cs : std_logic_vector(15 downto 0);
begin
  gpio: process (gpio_oe_cs, gpio_out_cs, gpio_in_cs, gpio_pins_s) is
    variable gpio_in_v : std_logic_vector(15 downto 0);
  begin
    gpio_in_v := gpio_in_cs;

    for i in gpio_pins_s'range loop
      if gpio_oe_cs(i) = '1' then
        gpio_pins_s(i) <= gpio_out_cs(i);
      else
        gpio_pins_s(i) <= 'Z';
      end if;

      gpio_in_v(i) := gpio_pins_s(i);
    end loop;

    gpio_in_ns <= gpio_in_v;
  end process;

  memio: process (io_addr_s, io_di_s, io_we_s, gpio_oe_cs, gpio_in_cs, gpio_out_cs) is
    variable gpio_oe_v : std_logic_vector(15 downto 0);
    variable gpio_in_v : std_logic_vector(15 downto 0);
    variable gpio_out_v : std_logic_vector(15 downto 0);
  begin
    gpio_oe_v := gpio_oe_cs;
    gpio_in_v := gpio_in_cs;
    gpio_out_v := gpio_out_cs;

    if io_addr_s(15 downto 4) = X"800" then
      if io_we_s = '1' then
        -- write
        case io_addr_s(3 downto 0) is
          when X"0" =>
            gpio_oe_v := io_di_s;
          when X"2" =>
            -- atomic set
            gpio_out_v := gpio_out_v or io_di_s;
          when X"4" =>
            -- atomic clear
            gpio_out_v := gpio_out_v and not io_di_s;
          when X"6" =>
            -- read only
            null;
          when X"8" =>
            gpio_out_v := io_di_s;
          when others =>
            null;
        end case;
      else -- read
        case io_addr_s(3 downto 0) is
          when X"0" =>
            null;
          when X"2" =>
            -- write only
            null;
          when X"4" =>
            null;
          when X"6" =>
            null;
          when X"8" =>
            null;
          when others =>
            null;
        end case;
      end if;
    end if;

    gpio_oe_ns <= gpio_oe_v;
    gpio_in_ns <= gpio_in_v;
    gpio_out_ns <= gpio_out_v;
  end process;

  reg: process (clk) is
  begin
    if clk'event and clk = '1' then
      if nres = '0' then
        io_do_cs <= (others => '0');

        gpio_oe_cs <= (others => '0');
        gpio_in_cs <= (others => '0');
        gpio_out_cs <= (others => '0');
      else
        io_do_cs <= io_do_ns;

        gpio_oe_cs <= gpio_oe_ns;
        gpio_in_cs <= gpio_in_ns;
        gpio_out_cs <= gpio_out_ns;
      end if;
    end if;
  end process;

  pins <= gpio_pins_s;

  io_di_s <= io_di;
  io_do <= io_do_s;
  io_we_s <= io_we;
  io_addr_s <= io_addr;
end architecture rtl;
