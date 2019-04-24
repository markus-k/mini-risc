library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

-- registers:
-- 16 bit registers 0-f:
-- 0-c: general purpose
-- d: stack pointer sp
-- e: flags
-- f: program counter pc
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
-- instruction with 2 operands and 4 bit immediate:
-- -------------------------------------------------------------------
-- | 15                                                            0 |
-- -------------------------------------------------------------------
-- | 4 bit opcode | 4 bit dest reg | 4 bit src1 reg | 4 bit immd.    |
-- -------------------------------------------------------------------
--
-- instruction with 12 bit immediate
-- -------------------------------------------------------------------
-- | 4 bit opcode | 12 bit immediate                                 |
-- -------------------------------------------------------------------
--
-- instruction with cond flags
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
-- 5 and dst = src1 and src2
-- 6 or dst = src1 or src2
-- 7 xor dst = src1 xor src2
-- 8 sht if (imm4[3]) { dst = src1 >> imm4[2:0] } else { dst = src1 << imm4[2:0] }
--
-- 9 jmpri pc += imm12
-- a cmp src1, src2
-- b jmpc if (cond met) { pc = dst }
--
-- c ldr dst = ram[src1]
-- d str ram[src1] = dst
-- e push ram[sp] = dst; sp--
-- f pop dst = ram[sp]; sp++
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
--
-- memory map
-- 0x0000 - 0x7fff: ram
-- 0x8000 - 0xffff: io

entity top is
  port (
    clk : in std_logic;
    nres : in std_logic);
end entity top;

architecture rtl of top is
  constant bit_width : natural := 16;
  constant width_msb : natural := bit_width - 1;

  attribute keep : string;

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

  -- rom address bus
  signal rom_do_s : std_logic_vector(15 downto 0);
  signal rom_addr_s : std_logic_vector(15 downto 0);

  -- ram address bus
  signal ram_di_s : std_logic_vector(15 downto 0);
  signal ram_do_s : std_logic_vector(15 downto 0);
  signal ram_we_s : std_logic;
  signal ram_addr_s : std_logic_vector(15 downto 0);

  -- io address bus
  signal io_di_s : std_logic_vector(15 downto 0);
  signal io_do_s : std_logic_vector(15 downto 0);
  signal io_we_s : std_logic;
  signal io_addr_s : std_logic_vector(15 downto 0);

  type instr_fsm_type is (FETCH, DECODE, EXECUTE, MEMACCESS);

  constant num_regs : natural := 16;
  constant reg_pc_idx : natural := num_regs - 1;
  constant reg_flags_idx : natural := num_regs - 2;
  constant reg_sp_idx : natural := num_regs - 3;
  type reg_bank_type is array (0 to num_regs - 1) of std_logic_vector(width_msb downto 0);
  signal reg_bank_ns : reg_bank_type;
  signal reg_bank_cs : reg_bank_type := (others => (others => '0'));
  attribute keep of reg_bank_cs : signal is "true";

  signal instr_fetch_ns : std_logic_vector(width_msb downto 0);
  signal instr_fetch_cs : std_logic_vector(width_msb downto 0) := (others => '0');
  signal instr_fsm_ns : instr_fsm_type;
  signal instr_fsm_cs : instr_fsm_type := FETCH;

  -- memory bus
  signal mem_do_s : std_logic_vector(15 downto 0);
  signal mem_di_ns : std_logic_vector(15 downto 0);
  signal mem_di_cs : std_logic_vector(15 downto 0) := (others => '0');
  signal mem_we_ns : std_logic;
  signal mem_we_cs : std_logic := '0';
  signal mem_addr_ns : std_logic_vector(15 downto 0);
  signal mem_addr_cs : std_logic_vector(15 downto 0) := (others => '0');

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

  memmap: process (mem_di_cs, ram_do_s, io_do_s, mem_we_cs, mem_addr_cs) is
    variable mem_do_v : std_logic_vector(15 downto 0);

    variable ram_di_v : std_logic_vector(15 downto 0);
    variable ram_we_v : std_logic;
    variable ram_addr_v : std_logic_vector(15 downto 0);

    variable io_di_v : std_logic_vector(15 downto 0);
    variable io_we_v : std_logic;
    variable io_addr_v : std_logic_vector(15 downto 0);
  begin
    mem_do_v := (others => '0');

    ram_di_v := (others => '0');
    ram_we_v := '0';
    ram_addr_v := (others => '0');

    io_di_v := (others => '0');
    io_we_v := '0';
    io_addr_v := (others => '0');

    if mem_addr_cs(15) = '0' then
      mem_do_v := ram_do_s;
      ram_di_v := mem_di_cs;
      ram_we_v := mem_we_cs;
      ram_addr_v := mem_addr_cs;
    else
      mem_do_v := io_do_s;
      io_di_v := mem_di_cs;
      io_we_v := mem_we_cs;
      io_addr_v := mem_addr_cs;
    end if;

    mem_do_s <= mem_do_v;

    ram_di_s <= ram_di_v;
    ram_we_s <= ram_we_v;
    ram_addr_s <= ram_addr_v;

    io_di_s <= io_di_v;
    io_we_s <= io_we_v;
    io_addr_s <= io_addr_v;
  end process;

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

  process (instr_fetch_cs, reg_bank_cs, instr_fsm_cs, cond_flags_s, mem_di_cs, mem_do_s, mem_we_cs, mem_addr_cs) is
    variable opcode_v : std_logic_vector(3 downto 0);
    variable dst_reg_v : integer range 0 to 15;
    variable src1_reg_v : integer range 0 to 15;
    variable src2_reg_v : integer range 0 to 15;
    variable imm4_v : std_logic_vector(3 downto 0);
    variable imm8_v : std_logic_vector(7 downto 0);
    variable imm12_v : std_logic_vector(11 downto 0);
    variable sub_v : std_logic_vector(width_msb+1 downto 0);
    variable flags_v : std_logic_vector(3 downto 0);

    variable reg_bank_v : reg_bank_type;
    variable mem_di_v : std_logic_vector(15 downto 0);
    variable mem_we_v : std_logic;
    variable mem_addr_v : std_logic_vector(15 downto 0);
  begin
    opcode_v := instr_fetch_cs(15 downto 12);
    dst_reg_v := to_integer(unsigned(instr_fetch_cs(11 downto 8)));
    src1_reg_v := to_integer(unsigned(instr_fetch_cs(7 downto 4)));
    src2_reg_v := to_integer(unsigned(instr_fetch_cs(3 downto 0)));
    imm4_v := instr_fetch_cs(3 downto 0); -- 4 bit immediate
    imm8_v := instr_fetch_cs(7 downto 0); -- 8 bit immediate
    imm12_v := instr_fetch_cs(11 downto 0); -- 8 bit immediate

    sub_v := (others => '0');

    reg_bank_v := reg_bank_cs;
    mem_di_v := mem_di_cs;
    mem_we_v := mem_we_cs;
    mem_addr_v := mem_addr_cs;

    flags_v := reg_bank_v(reg_flags_idx)(3 downto 0);

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
        when X"5" =>
          -- dst = src1 and src2
          reg_bank_v(dst_reg_v) := reg_bank_v(src1_reg_v) and reg_bank_v(src2_reg_v);
        when X"6" =>
          -- dst = src1 or src2
          reg_bank_v(dst_reg_v) := reg_bank_v(src1_reg_v) or reg_bank_v(src2_reg_v);
        when X"7" =>
          -- dst = src1 xor src2
          reg_bank_v(dst_reg_v) := reg_bank_v(src1_reg_v) xor reg_bank_v(src2_reg_v);
        when X"8" =>
          -- shift left or right by immediate
          if imm8_v(3) = '1' then
            reg_bank_v(dst_reg_v) := to_stdlogicvector(to_bitvector(reg_bank_v(src1_reg_v)) srl to_integer(unsigned(imm4_v(2 downto 0))));
          else
            reg_bank_v(dst_reg_v) := to_stdlogicvector(to_bitvector(reg_bank_v(src1_reg_v)) sll to_integer(unsigned(imm4_v(2 downto 0))));
          end if;
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
        when X"C" =>
          -- load from memory
          mem_addr_v := reg_bank_v(src1_reg_v);
          mem_we_v := '0';
        when X"D" =>
          -- store in memory
          mem_addr_v := reg_bank_v(dst_reg_v);
          mem_di_v := reg_bank_v(src1_reg_v);
          mem_we_v := '1';
        when X"E" =>
          -- push to stack, sp post decrement
          mem_addr_v := reg_bank_v(reg_sp_idx);
          mem_di_v := reg_bank_v(dst_reg_v);
          mem_we_v := '1';
        when X"F" =>
          -- pop from stack, pre increment
          reg_bank_v(reg_sp_idx) := std_logic_vector(to_unsigned(
            to_integer(unsigned(reg_bank_v(reg_sp_idx))) + 2,
            16));
          mem_addr_v := reg_bank_v(reg_sp_idx);
          mem_we_v := '0';
        when others =>
          null;
      end case;
    elsif instr_fsm_cs = MEMACCESS then
      case opcode_v is
        when X"C" =>
          -- load
          reg_bank_v(dst_reg_v) := mem_do_s;
        when X"D" =>
          -- store
          mem_we_v := '0';
        when X"E" =>
          -- push
          mem_we_v := '0';
          reg_bank_v(reg_sp_idx) := std_logic_vector(to_unsigned(
            to_integer(unsigned(reg_bank_v(reg_sp_idx))) - 2,
            16));
        when X"F" =>
          -- pop
          reg_bank_v(dst_reg_v) := ram_do_s;
        when others =>
          null; -- nothing to be done for non-memory instructions
      end case;
    end if;

    reg_bank_v(reg_flags_idx)(3 downto 0) := flags_v;

    reg_bank_ns <= reg_bank_v;
    mem_di_ns <= mem_di_v;
    mem_we_ns <= mem_we_v;
    mem_addr_ns <= mem_addr_v;
  end process;

  process (reg_bank_cs, instr_fsm_cs, instr_fetch_cs, rom_do_s) is
    variable instr_fsm_v : instr_fsm_type;
    variable instr_fetch_v : std_logic_vector(width_msb downto 0) := (others => '0');
  begin
    instr_fsm_v := instr_fsm_cs;
    instr_fetch_v := instr_fetch_cs;

    -- rom addr is always pc for now
    rom_addr_s <= reg_bank_cs(reg_pc_idx);

    case instr_fsm_v is
      when FETCH =>
        instr_fsm_v := DECODE;
      when DECODE =>
        instr_fetch_v := rom_do_s;
        instr_fsm_v := EXECUTE;
      when EXECUTE =>
        instr_fsm_v := MEMACCESS;
      when MEMACCESS =>
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

        mem_di_cs <= (others => '0');
        mem_we_cs <= '0';
        mem_addr_cs <= (others => '0');
      else
        reg_bank_cs <= reg_bank_ns;
        instr_fetch_cs <= instr_fetch_ns;
        instr_fsm_cs <= instr_fsm_ns;

        mem_di_cs <= mem_di_ns;
        mem_we_cs <= mem_we_ns;
        mem_addr_cs <= mem_addr_ns;
      end if;
    end if;
  end process;
end architecture rtl;
