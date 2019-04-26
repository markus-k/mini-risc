library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity soc is
  port (
    clk  : in std_logic;
    nres : in std_logic;

    gpio_a : inout std_logic_vector(15 downto 0));
end entity soc;

architecture rtl of soc is
  component core is
    port (
      clk : in std_logic;
      nres : in std_logic;

      -- rom bus
      rom_do : in std_logic_vector(15 downto 0);
      rom_addr : out std_logic_vector(15 downto 0);

      -- ram bus
      ram_di : out std_logic_vector(15 downto 0);
      ram_do : in std_logic_vector(15 downto 0);
      ram_we : out std_logic;
      ram_addr : out std_logic_vector(15 downto 0);

      -- io bus
      io_di : out std_logic_vector(15 downto 0);
      io_do : in std_logic_vector(15 downto 0);
      io_we : out std_logic;
      io_addr : out std_logic_vector(15 downto 0));
  end component core;
  for all : core use entity work.core(rtl);

  component rom is
      port (
        clk  : in  std_logic;
        nres : in  std_logic;

        do   : out std_logic_vector(15 downto 0);
        addr : in  std_logic_vector(15 downto 0));
  end component rom;
  for all : rom use entity work.rom(rtl);

  component ram is
    generic (
      ram_size : natural := 64);

    port (
      clk  : in  std_logic;
      nres : in  std_logic;

      di   : in  std_logic_vector(15 downto 0);
      do   : out std_logic_vector(15 downto 0);
      we   : in  std_logic;
      addr : in  std_logic_vector(15 downto 0));
  end component ram;
  for all : ram use entity work.ram(rtl);

  component periph_gpio is
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
  end component periph_gpio;
  for all : periph_gpio use entity work.periph_gpio(rtl);

  signal rom_do_s : std_logic_vector(15 downto 0);
  signal rom_addr_s : std_logic_vector(15 downto 0);

  signal ram_di_s : std_logic_vector(15 downto 0);
  signal ram_do_s : std_logic_vector(15 downto 0);
  signal ram_we_s : std_logic;
  signal ram_addr_s : std_logic_vector(15 downto 0);

  signal gpio_pins_s : std_logic_vector(15 downto 0);

  signal io_di_s : std_logic_vector(15 downto 0);
  signal io_do_s : std_logic_vector(15 downto 0);
  signal io_we_s : std_logic;
  signal io_addr_s : std_logic_vector(15 downto 0);
begin
  progrom : rom
    port map(
      clk => clk,
      nres => nres,

      do => rom_do_s,
      addr => rom_addr_s);

  appram : ram
    generic map(
      ram_size => 256)
    port map(
      clk => clk,
      nres => nres,

      di => ram_di_s,
      do => ram_do_s,
      we => ram_we_s,
      addr => ram_addr_s);

  gpio_1 : periph_gpio
    port map (
      clk => clk,
      nres => nres,

      pins => gpio_pins_s,

      io_di => io_di_s,
      io_do => io_do_s,
      io_we => io_we_s,
      io_addr => io_addr_s);

  core_1 : core
    port map (
      clk      => clk,
      nres     => nres,

      rom_do   => rom_do_s,
      rom_addr => rom_addr_s,

      ram_di   => ram_di_s,
      ram_do   => ram_do_s,
      ram_we   => ram_we_s,
      ram_addr => ram_addr_s,

      io_di    => io_di_s,
      io_do    => io_do_s,
      io_we    => io_we_s,
      io_addr  => io_addr_s);

  gpio_a <= gpio_pins_s;
end architecture rtl;
