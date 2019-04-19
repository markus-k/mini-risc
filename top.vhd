library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

-- registers:
-- 16 bit registers 0-f:
-- 0-c: general purpose
-- f: pc
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
-- 9 jmpi pc = imm12
-- a jmp pc = dst
-- b jmpnz if src1 != 0 { pc = dst }
-- c
-- d
-- e
-- f

entity top is
  port (
    clk : in std_logic;
    nres : in std_logic);
end entity top;

architecture rtl of top is
  constant bit_width : natural := 16;
  constant width_msb : natural := bit_width - 1;

  component mem is
      port (
        clk  : in  std_logic;
        nres : in  std_logic;

        di   : in  std_logic_vector(15 downto 0);
        do   : out std_logic_vector(15 downto 0);
        we   : in  std_logic;
        addr : in  std_logic_vector(15 downto 0));
  end component mem;

  type instr_fsm_type is (FETCH, DECODE, EXECUTE);

  signal ram_di_s : std_logic_vector(15 downto 0);
  signal ram_do_s : std_logic_vector(15 downto 0);
  signal ram_we_s : std_logic;
  signal ram_addr_s : std_logic_vector(15 downto 0);

  constant num_regs : natural := 16;
  constant reg_pc_idx : natural := num_regs - 1;
  type reg_bank_type is array (0 to num_regs - 1) of std_logic_vector(width_msb downto 0);
  signal reg_bank_ns : reg_bank_type;
  signal reg_bank_cs : reg_bank_type := (others => (others => '0'));

  signal instr_fetch_ns : std_logic_vector(width_msb downto 0);
  signal instr_fetch_cs : std_logic_vector(width_msb downto 0) := (others => '0');
  signal instr_fsm_ns : instr_fsm_type;
  signal instr_fsm_cs : instr_fsm_type := FETCH;
begin
  ram : mem
    port map(
      clk => clk,
      nres => nres,

      di => ram_di_s,
      do => ram_do_s,
      we => ram_we_s,
      addr => ram_addr_s);

  process (instr_fetch_cs, reg_bank_cs, instr_fsm_cs) is
    variable opcode_v : std_logic_vector(3 downto 0);
    variable dst_reg_v : integer range 0 to 15;
    variable src1_reg_v : integer range 0 to 15;
    variable src2_reg_v : integer range 0 to 15;
    variable imm8_v : std_logic_vector(7 downto 0);
    variable imm12_v : std_logic_vector(11 downto 0);

    variable reg_bank_v : reg_bank_type;
  begin
    opcode_v := instr_fetch_cs(15 downto 12);
    dst_reg_v := to_integer(unsigned(instr_fetch_cs(11 downto 8)));
    src1_reg_v := to_integer(unsigned(instr_fetch_cs(7 downto 4)));
    src2_reg_v := to_integer(unsigned(instr_fetch_cs(3 downto 0)));
    imm8_v := instr_fetch_cs(7 downto 0); -- 8 bit immediate
    imm12_v := instr_fetch_cs(11 downto 0); -- 8 bit immediate

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
          -- load 12 bit immediate pc
          reg_bank_v(reg_pc_idx) := "0000" & imm12_v;
        when X"A" =>
          -- load pc from dst reg
          reg_bank_v(reg_pc_idx) := reg_bank_v(dst_reg_v);
        when X"B" =>
          -- load pc from dst reg if src1 != 0
          if not (reg_bank_v(src1_reg_v) = X"0000") then
            reg_bank_v(reg_pc_idx) := reg_bank_v(dst_reg_v);
          end if;
        when others =>
          null;
      end case;
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
    ram_addr_s <= reg_bank_cs(reg_pc_idx);

    case instr_fsm_v is
      when FETCH =>
        instr_fsm_v := DECODE;
      when DECODE =>
        instr_fetch_v := ram_do_s;
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
