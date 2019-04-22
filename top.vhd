library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

-- registers:
-- 16 bit registers 0-f:
-- 0-c: general purpose
-- e: flags
-- f: pc
--
-- flags register (ARM style):
-- -------------------------------------------------------------------
-- | 15                                                            0 |
-- -------------------------------------------------------------------
-- |                                                 | V | C | Z | N |
-- -------------------------------------------------------------------
--
-- N = negative flags, i.e. bit 15 is set
-- Z = zero flag
-- C = carry flag
-- V = signed overflow
--
-- instruction with 3 operands:
-- -------------------------------------------------------------------
-- | 15                                                            0 |
-- -------------------------------------------------------------------
-- | 4 bit opcode | 4 bit dest reg | 4 bit src1 reg | 4 bit src2 reg |
-- -------------------------------------------------------------------
--
-- instruction with 8 bit immediate
-- -------------------------------------------------------------------
-- | 4 bit opcode | 4 bit dest reg | 8 bit immediate                 |
-- -------------------------------------------------------------------
--
-- instruction with 12 bit immediate
-- -------------------------------------------------------------------
-- | 4 bit opcode | 12 bit immediate                                 |
-- -------------------------------------------------------------------
--
-- instruction with flags
-- -------------------------------------------------------------------
-- | 4 bit opcode | 4 bit dest reg | 4 bit cond flags |              |
-- -------------------------------------------------------------------
--
-- opcodes:
--
-- 0 mov dst = src1
-- 1 movil dst[7:0]  = imm8
-- 2 movih dst[15:8] = imm8
--
-- 3 add dst = src1 + src2
-- 4 sub dst = src1 - src2
--
-- 5
-- 6
-- 7
-- 8
--
-- 9 jmpri pc += imm12
-- a cmp src1, src2
-- b jmpc if (cond met) { pc = dst }
-- c
-- d
-- e
-- f
--
-- condition codes (ARM-style):
-- 0 always
-- 1 eq (Z=1)
-- 2 ne (Z=0)
-- 3 hs (C=1)
-- 4 lo (C=0)
-- 5 mi (N=1)
-- 6 po (N=0)
-- 7 vs (V=1)
-- 8 vc (V=0)
-- 9 hi (C=1) && (Z=0)
-- a ls (C=0) || (Z=1)
-- b ge (N=V)
-- c lt (N!=V)
-- d gt (Z=0) && (N=V)
-- e le (Z=1) || (N!=V)
-- f reserved

entity top is
  port (
    clk : in std_logic;
    nres : in std_logic);
end entity top;

architecture rtl of top is
  constant bit_width : natural := 16;
  constant width_msb : natural := bit_width - 1;

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

  signal rom_do_s : std_logic_vector(15 downto 0);
  signal rom_addr_s : std_logic_vector(15 downto 0);

  signal ram_di_s : std_logic_vector(15 downto 0);
  signal ram_do_s : std_logic_vector(15 downto 0);
  signal ram_we_s : std_logic;
  signal ram_addr_s : std_logic_vector(15 downto 0);

  type instr_fsm_type is (FETCH, DECODE, EXECUTE);

  constant num_regs : natural := 16;
  constant reg_pc_idx : natural := num_regs - 1;
  constant reg_flags_idx : natural := num_regs - 2;
  type reg_bank_type is array (0 to num_regs - 1) of std_logic_vector(width_msb downto 0);
  signal reg_bank_ns : reg_bank_type;
  signal reg_bank_cs : reg_bank_type := (others => (others => '0'));

  signal instr_fetch_ns : std_logic_vector(width_msb downto 0);
  signal instr_fetch_cs : std_logic_vector(width_msb downto 0) := (others => '0');
  signal instr_fsm_ns : instr_fsm_type;
  signal instr_fsm_cs : instr_fsm_type := FETCH;

  signal cond_flags_s : std_logic_vector(width_msb downto 0);
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

  cond_flags: process (reg_bank_cs) is
    variable cond_flags_v : std_logic_vector(width_msb downto 0);
    variable flags_v : std_logic_vector(3 downto 0);
  begin
    flags_v := reg_bank_cs(reg_flags_idx)(3 downto 0);

    cond_flags_v := (others => '0');
    cond_flags_v(0) := '1'; -- always
    cond_flags_v(1) := flags_v(1); -- eq
    cond_flags_v(2) := not flags_v(1); -- ne
    cond_flags_v(3) := flags_v(2); -- hs
    cond_flags_v(4) := not flags_v(2); -- lo
    cond_flags_v(5) := flags_v(0); -- mi (negative)
    cond_flags_v(6) := not flags_v(0); -- positive
    cond_flags_v(7) := flags_v(3); -- vs
    cond_flags_v(8) := not flags_v(3); -- vc
    cond_flags_v(9) := flags_v(2) and not flags_v(0); -- hi
    cond_flags_v(10) := not flags_v(2) or flags_v(0); -- ls
    cond_flags_v(11) := (flags_v(1) and flags_v(3)) or (not flags_v(1) and not flags_v(3)); -- ge
    cond_flags_v(12) := (flags_v(1) and not flags_v(3)) or (not flags_v(1) and flags_v(3));
    cond_flags_v(13) := not flags_v(0) and ((flags_v(1) and flags_v(3)) or (not flags_v(1) and not flags_v(3)));
    cond_flags_v(14) := flags_v(0) or ((flags_v(1) and not flags_v(3)) or (not flags_v(1) and flags_v(3))); --
    cond_flags_v(15) := '0'; -- reserved

    cond_flags_s <= cond_flags_v;
  end process;

  process (instr_fetch_cs, reg_bank_cs, instr_fsm_cs, cond_flags_s) is
    variable opcode_v : std_logic_vector(3 downto 0);
    variable dst_reg_v : integer range 0 to 15;
    variable src1_reg_v : integer range 0 to 15;
    variable src2_reg_v : integer range 0 to 15;
    variable imm8_v : std_logic_vector(7 downto 0);
    variable imm12_v : std_logic_vector(11 downto 0);
    variable sub_v : std_logic_vector(width_msb+1 downto 0);
    variable flags_v : std_logic_vector(3 downto 0);

    variable reg_bank_v : reg_bank_type;
  begin
    opcode_v := instr_fetch_cs(15 downto 12);
    dst_reg_v := to_integer(unsigned(instr_fetch_cs(11 downto 8)));
    src1_reg_v := to_integer(unsigned(instr_fetch_cs(7 downto 4)));
    src2_reg_v := to_integer(unsigned(instr_fetch_cs(3 downto 0)));
    imm8_v := instr_fetch_cs(7 downto 0); -- 8 bit immediate
    imm12_v := instr_fetch_cs(11 downto 0); -- 8 bit immediate

    flags_v := reg_bank_v(reg_flags_idx)(3 downto 0);

    sub_v := (others => '0');

    reg_bank_v := reg_bank_cs;

    if instr_fsm_cs = EXECUTE then
      -- increment pc
      reg_bank_v(reg_pc_idx) := std_logic_vector(unsigned(reg_bank_v(reg_pc_idx)) + 1);

      case opcode_v is
        when X"0" =>
          -- load src1 into dst reg
          reg_bank_v(dst_reg_v) := reg_bank_v(src1_reg_v);
        when X"1" =>
          -- load imm8 into dst reg lower byte
          reg_bank_v(dst_reg_v)(7 downto 0) := imm8_v;
        when X"2" =>
          -- load imm8 into dst reg upper byte
          reg_bank_v(dst_reg_v)(15 downto 8) := imm8_v;
        when X"3" =>
          -- dst = src1 + src2
          reg_bank_v(dst_reg_v) := std_logic_vector(to_unsigned(
            to_integer(unsigned(reg_bank_v(src1_reg_v))) + to_integer(unsigned(reg_bank_v(src2_reg_v))),
            16));
        when X"4" =>
          -- dst = src1 - src2
          reg_bank_v(dst_reg_v) := std_logic_vector(to_unsigned(
            to_integer(unsigned(reg_bank_v(src1_reg_v))) - to_integer(unsigned(reg_bank_v(src2_reg_v))),
            16));
        when X"9" =>
          -- load 12 bit relative immediate pc
          reg_bank_v(reg_pc_idx) := std_logic_vector(to_unsigned(
            to_integer(unsigned(reg_bank_v(reg_pc_idx))) + to_integer(signed(imm12_v)),
            16));
        when X"A" =>
          -- compare src1 and src2 and set flags
          sub_v := std_logic_vector(to_signed(
            to_integer(signed(reg_bank_v(src1_reg_v))) - to_integer(signed(reg_bank_v(src2_reg_v))),
            17));

          -- update flags
          flags_v := "0000";
          flags_v(0) := sub_v(15);
          if sub_v = "00000000000000000" then
            flags_v(1) := '1';
          end if;
          flags_v(2) := sub_v(16);
          flags_v(3) := '0'; -- TODO
        when X"B" =>
          -- load pc from dst reg if src1 != 0
          if cond_flags_s(src1_reg_v) = '1' then
            reg_bank_v(reg_pc_idx) := reg_bank_v(dst_reg_v);
          end if;
        when others =>
          null;
      end case;

      reg_bank_v(reg_flags_idx)(3 downto 0) := flags_v;
    end if;

    reg_bank_ns <= reg_bank_v;
  end process;

  process (reg_bank_cs, instr_fsm_cs, instr_fetch_cs) is
    variable instr_fsm_v : instr_fsm_type;
    variable instr_fetch_v : std_logic_vector(width_msb downto 0) := (others => '0');
  begin
    instr_fsm_v := instr_fsm_cs;
    instr_fetch_v := instr_fetch_cs;

    -- ram addr is always pc for now
    rom_addr_s <= reg_bank_cs(reg_pc_idx);

    case instr_fsm_v is
      when FETCH =>
        instr_fsm_v := DECODE;
      when DECODE =>
        instr_fetch_v := rom_do_s;
        instr_fsm_v := EXECUTE;
      when EXECUTE =>
        instr_fsm_v := FETCH;
    end case;

    instr_fsm_ns <= instr_fsm_v;
    instr_fetch_ns <= instr_fetch_v;
  end process;

  reg: process (clk) is
  begin
    if clk'event and clk = '1' then
      if nres = '0' then
        reg_bank_cs <= (others => (others => '0'));
        instr_fetch_cs <= (others => '0');
        instr_fsm_cs <= FETCH;
      else
        reg_bank_cs <= reg_bank_ns;
        instr_fetch_cs <= instr_fetch_ns;
        instr_fsm_cs <= instr_fsm_ns;
      end if;
    end if;
  end process;
end architecture rtl;
