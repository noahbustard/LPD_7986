library IEEE;
use IEEE.STD_LOGIC_1164.all;
use IEEE.NUMERIC_STD.all;

------------------------------------------------------------
-- TOP-LEVEL ENTITY: 7986 microprocessor
------------------------------------------------------------
entity ldp7986 is
    port (
        -- 16-bit instruction input
        instruction : in  std_logic_vector(15 downto 0);

        -- control signals from assignment
        exe         : in  std_logic;  -- execute (active when 0 → falling edge)
        upd         : in  std_logic;  -- update (active when 0 → falling edge)

        -- 8-bit output bus
        Y           : out std_logic_vector(7 downto 0);

        -- 7-seg outputs
        a0, a1, a2, a3, a4, a5, a6 : out std_logic;
        b0, b1, b2, b3, b4, b5, b6 : out std_logic
    );
end entity ldp7986;

architecture struct of ldp7986 is

    --------------------------------------------------------
    -- Component declarations
    --------------------------------------------------------
    component ir16 is
        port (
            instr_in  : in  std_logic_vector(15 downto 0);
            exe       : in  std_logic;
            opcode    : out std_logic_vector(5 downto 0);
            instr_out : out std_logic_vector(15 downto 0)
        );
    end component;

    component reg8 is
        port (
            ck     : in  std_logic;
            enable : in  std_logic;
            d      : in  std_logic_vector(7 downto 0);
            q      : out std_logic_vector(7 downto 0)
        );
    end component;

    component dec3to4 is
        port (
            reg_sel : in  std_logic_vector(2 downto 0);
            we      : in  std_logic;
            en_AL   : out std_logic;
            en_BL   : out std_logic;
            en_CL   : out std_logic;
            en_DL   : out std_logic
        );
    end component;

    component muxA5 is
        port (
            sel     : in  std_logic_vector(2 downto 0);
            AL_in   : in  std_logic_vector(7 downto 0);
            BL_in   : in  std_logic_vector(7 downto 0);
            CL_in   : in  std_logic_vector(7 downto 0);
            DL_in   : in  std_logic_vector(7 downto 0);
            imm_in  : in  std_logic_vector(7 downto 0);
            Y       : out std_logic_vector(7 downto 0)
        );
    end component;

    component muxB4 is
        port (
            sel     : in  std_logic_vector(1 downto 0);
            AL_in   : in  std_logic_vector(7 downto 0);
            BL_in   : in  std_logic_vector(7 downto 0);
            CL_in   : in  std_logic_vector(7 downto 0);
            DL_in   : in  std_logic_vector(7 downto 0);
            Y       : out std_logic_vector(7 downto 0)
        );
    end component;

    component alu8 is
        port (
            A      : in  std_logic_vector(7 downto 0);
            B      : in  std_logic_vector(7 downto 0);
            alu_op : in  std_logic_vector(3 downto 0);
            Y      : out std_logic_vector(7 downto 0)
        );
    end component;

    component hex7seg is
        port (
            D  : in  std_logic_vector(3 downto 0);
            s0 : out std_logic;
            s1 : out std_logic;
            s2 : out std_logic;
            s3 : out std_logic;
            s4 : out std_logic;
            s5 : out std_logic;
            s6 : out std_logic
        );
    end component;

    --------------------------------------------------------
    -- Opcode constants
    --------------------------------------------------------
    constant OPC_ADD      : std_logic_vector(5 downto 0) := "000000"; -- 0000 0000 ...
    constant OPC_XOR      : std_logic_vector(5 downto 0) := "001100"; -- 0011 0000 ...
    constant OPC_MOV_RR   : std_logic_vector(5 downto 0) := "100010"; -- 1000 1000 ...
    constant OPC_MOV_IMM  : std_logic_vector(5 downto 0) := "101100"; -- 1011 0reg ...
    constant OPC_OR_IMM   : std_logic_vector(5 downto 0) := "000011"; -- 0000 1100 ...
    constant OPC_AND_IMM  : std_logic_vector(5 downto 0) := "001001"; -- 0010 0100 ...
    constant OPC_SHIFT    : std_logic_vector(5 downto 0) := "110100"; -- SHL/SHR 1101 0000 ...
    constant OPC_NEG      : std_logic_vector(5 downto 0) := "111101"; -- 1111 0110 ...
    constant OPC_OUT      : std_logic_vector(5 downto 0) := "111001"; -- 1110 0110 ...

    --------------------------------------------------------
    -- Internal signals
    --------------------------------------------------------
    signal opcode     : std_logic_vector(5 downto 0);
    signal instr_word : std_logic_vector(15 downto 0);
    signal imm_data   : std_logic_vector(7 downto 0);

    -- Register file contents
    signal AL_reg, BL_reg, CL_reg, DL_reg : std_logic_vector(7 downto 0);

    -- MUX outputs
    signal muxA_out, muxB_out : std_logic_vector(7 downto 0);

    -- ALU outputs and control
    signal alu_result : std_logic_vector(7 downto 0);
    signal alu_op     : std_logic_vector(3 downto 0);

    -- Register enables
    signal en_AL, en_BL, en_CL, en_DL : std_logic;
    signal dest_sel                   : std_logic_vector(2 downto 0);
    signal reg_we                     : std_logic;

    -- Display data
    signal disp_data : std_logic_vector(7 downto 0);

    -- MUX select signals
    signal selA : std_logic_vector(2 downto 0);
    signal selB : std_logic_vector(1 downto 0);

begin

    --------------------------------------------------------
    -- IR
    --------------------------------------------------------
    IR0 : ir16
        port map (
            instr_in  => instruction,
            exe       => exe,
            opcode    => opcode,
            instr_out => instr_word
        );

    imm_data <= instr_word(7 downto 0);

    --------------------------------------------------------
    -- Register file: AL, BL, CL, DL
    --------------------------------------------------------
    REG_AL : reg8
        port map (
            ck     => upd,
            enable => en_AL,
            d      => alu_result,
            q      => AL_reg
        );

    REG_BL : reg8
        port map (
            ck     => upd,
            enable => en_BL,
            d      => alu_result,
            q      => BL_reg
        );

    REG_CL : reg8
        port map (
            ck     => upd,
            enable => en_CL,
            d      => alu_result,
            q      => CL_reg
        );

    REG_DL : reg8
        port map (
            ck     => upd,
            enable => en_DL,
            d      => alu_result,
            q      => DL_reg
        );

    --------------------------------------------------------
    -- Decoder
    --------------------------------------------------------
    DEC0 : dec3to4
        port map (
            reg_sel => dest_sel,
            we      => reg_we,
            en_AL   => en_AL,
            en_BL   => en_BL,
            en_CL   => en_CL,
            en_DL   => en_DL
        );

    --------------------------------------------------------
    -- MUX A
    --------------------------------------------------------
    MUXA0 : muxA5
        port map (
            sel    => selA,
            AL_in  => AL_reg,
            BL_in  => BL_reg,
            CL_in  => CL_reg,
            DL_in  => DL_reg,
            imm_in => imm_data,
            Y      => muxA_out
        );

    --------------------------------------------------------
    -- MUX B
    --------------------------------------------------------
    MUXB0 : muxB4
        port map (
            sel   => selB,
            AL_in => AL_reg,
            BL_in => BL_reg,
            CL_in => CL_reg,
            DL_in => DL_reg,
            Y     => muxB_out
        );

    --------------------------------------------------------
    -- ALU
    --------------------------------------------------------
    ALU0 : alu8
        port map (
            A      => muxA_out,
            B      => muxB_out,
            alu_op => alu_op,
            Y      => alu_result
        );

    --------------------------------------------------------
    -- Y bus
    --------------------------------------------------------
    Y <= disp_data;

    --------------------------------------------------------
    -- 7-seg display
    --------------------------------------------------------
    HEX_HI : hex7seg
        port map (
            D  => disp_data(7 downto 4),
            s0 => a0,
            s1 => a1,
            s2 => a2,
            s3 => a3,
            s4 => a4,
            s5 => a5,
            s6 => a6
        );

    HEX_LO : hex7seg
        port map (
            D  => disp_data(3 downto 0),
            s0 => b0,
            s1 => b1,
            s2 => b2,
            s3 => b3,
            s4 => b4,
            s5 => b5,
            s6 => b6
        );

    --------------------------------------------------------
    -- CONTROL UNIT
    --------------------------------------------------------
    control_proc : process(opcode, instr_word, imm_data,
                       AL_reg, BL_reg, CL_reg, DL_reg)
        variable dest_code : std_logic_vector(2 downto 0);
        variable src_code  : std_logic_vector(2 downto 0);
    begin
        -- Safe defaults (NOP)
        selA      <= "000";      -- A = AL
        selB      <= "00";       -- B = AL
        alu_op    <= "0010";     -- MOV A (pass-through)
        reg_we    <= '0';        -- no register write
        dest_sel  <= "000";      -- default dest = AL
        disp_data <= AL_reg;     -- normally show AL

        case opcode is

            ------------------------------------------------
            -- ADD R1,R2 : R1 ← R1 + R2
            -- pattern: 0000 0000 11 reg2 reg1
            ------------------------------------------------
            when OPC_ADD =>
                dest_code := imm_data(2 downto 0); -- reg1 (dest)
                src_code  := imm_data(5 downto 3); -- reg2 (source)
                reg_we    <= '1';

                -- A = dest (R1)
                case dest_code is
                    when "000" => selA <= "000"; dest_sel <= "000"; -- AL
                    when "011" => selA <= "001"; dest_sel <= "011"; -- BL
                    when "001" => selA <= "010"; dest_sel <= "001"; -- CL
                    when "010" => selA <= "011"; dest_sel <= "010"; -- DL
                    when others =>
                        selA     <= "000";
                        dest_sel <= "000";
                end case;

                -- B = source (R2)
                case src_code is
                    when "000" => selB <= "00"; -- AL
                    when "011" => selB <= "01"; -- BL
                    when "001" => selB <= "10"; -- CL
                    when "010" => selB <= "11"; -- DL
                    when others => selB <= "00";
                end case;

                alu_op <= "0000"; -- ADD

            ------------------------------------------------
            -- XOR R1,R2 : R1 ← R1 xor R2
            -- pattern: 0011 0000 11 reg2 reg1
            ------------------------------------------------
            when OPC_XOR =>
                dest_code := imm_data(2 downto 0);
                src_code  := imm_data(5 downto 3);
                reg_we    <= '1';

                -- A = dest
                case dest_code is
                    when "000" => selA <= "000"; dest_sel <= "000";
                    when "011" => selA <= "001"; dest_sel <= "011";
                    when "001" => selA <= "010"; dest_sel <= "001";
                    when "010" => selA <= "011"; dest_sel <= "010";
                    when others =>
                        selA     <= "000";
                        dest_sel <= "000";
                end case;

                -- B = source
                case src_code is
                    when "000" => selB <= "00";
                    when "011" => selB <= "01";
                    when "001" => selB <= "10";
                    when "010" => selB <= "11";
                    when others => selB <= "00";
                end case;

                alu_op <= "0001"; -- XOR

            ------------------------------------------------
            -- MOV R1,R2 : R1 ← R2
            -- pattern: 1000 1000 11 reg2 reg1
            ------------------------------------------------
            when OPC_MOV_RR =>
                dest_code := imm_data(2 downto 0); -- R1 (dest)
                src_code  := imm_data(5 downto 3); -- R2 (source)
                reg_we    <= '1';

                -- A = source (R2)
                case src_code is
                    when "000" => selA <= "000"; -- AL
                    when "011" => selA <= "001"; -- BL
                    when "001" => selA <= "010"; -- CL
                    when "010" => selA <= "011"; -- DL
                    when others => selA <= "000";
                end case;

                -- Destination
                case dest_code is
                    when "000" => dest_sel <= "000"; -- AL
                    when "011" => dest_sel <= "011"; -- BL
                    when "001" => dest_sel <= "001"; -- CL
                    when "010" => dest_sel <= "010"; -- DL
                    when others => dest_sel <= "000";
                end case;

                alu_op <= "0010"; -- MOV A

            ------------------------------------------------
            -- MOV R,imm : R ← imm_data
            -- pattern: 1011 0 reg immdata
            -- dest reg is bits 10..8
            ------------------------------------------------
            when OPC_MOV_IMM =>
                dest_code := instr_word(10 downto 8);
                reg_we    <= '1';

                selA <= "100"; -- A = imm_data (muxA "others" branch)

                case dest_code is
                    when "000" => dest_sel <= "000"; -- AL
                    when "011" => dest_sel <= "011"; -- BL
                    when "001" => dest_sel <= "001"; -- CL
                    when "010" => dest_sel <= "010"; -- DL
                    when others => dest_sel <= "000";
                end case;

                alu_op <= "0010"; -- MOV A

            ------------------------------------------------
            -- OR AL,imm : AL ← AL OR imm
            -- pattern: 0000 1100 immdata
            ------------------------------------------------
            when OPC_OR_IMM =>
                reg_we   <= '1';
                dest_sel <= "000"; -- AL
                selA     <= "100"; -- A = imm
                selB     <= "00";  -- B = AL
                alu_op   <= "0011"; -- OR

            ------------------------------------------------
            -- AND AL,imm : AL ← AL AND imm
            -- pattern: 0010 0100 immdata
            ------------------------------------------------
            when OPC_AND_IMM =>
                reg_we   <= '1';
                dest_sel <= "000"; -- AL
                selA     <= "100"; -- A = imm
                selB     <= "00";  -- B = AL
                alu_op   <= "0100"; -- AND

            ------------------------------------------------
            -- SHL / SHR R,1 :
            -- SHL: 1101 0000 1110 0 reg
            -- SHR: 1101 0000 1110 1 reg
            -- both share opcode = "110100"
            -- bit 3 = 0 → SHL, bit 3 = 1 → SHR
            ------------------------------------------------
            when OPC_SHIFT =>
                dest_code := instr_word(2 downto 0); -- reg
                reg_we    <= '1';

                -- A = dest reg
                case dest_code is
                    when "000" => selA <= "000"; dest_sel <= "000";
                    when "011" => selA <= "001"; dest_sel <= "011";
                    when "001" => selA <= "010"; dest_sel <= "001";
                    when "010" => selA <= "011"; dest_sel <= "010";
                    when others =>
                        selA     <= "000";
                        dest_sel <= "000";
                end case;

                if instr_word(3) = '0' then
                    alu_op <= "0101"; -- SHL
                else
                    alu_op <= "0110"; -- SHR
                end if;

            ------------------------------------------------
            -- NEG R : R ← -R
            -- pattern: 1111 0110 1101 1 reg
            ------------------------------------------------
            when OPC_NEG =>
                dest_code := instr_word(2 downto 0);
                reg_we    <= '1';

                case dest_code is
                    when "000" => selA <= "000"; dest_sel <= "000";
                    when "011" => selA <= "001"; dest_sel <= "011";
                    when "001" => selA <= "010"; dest_sel <= "001";
                    when "010" => selA <= "011"; dest_sel <= "010";
                    when others =>
                        selA     <= "000";
                        dest_sel <= "000";
                end case;

                alu_op <= "0111"; -- NEG

            ------------------------------------------------
            -- OUT R
            ------------------------------------------------
            when OPC_OUT =>
                reg_we <= '0';

                dest_code := instr_word(2 downto 0);

                case dest_code is
                    when "000" => disp_data <= AL_reg;
                    when "011" => disp_data <= BL_reg;
                    when "001" => disp_data <= CL_reg;
                    when "010" => disp_data <= DL_reg;
                    when others => disp_data <= AL_reg;
                end case;

            ------------------------------------------------
            -- anything else → NOP (defaults)
            ------------------------------------------------
            when others =>
                null;
        end case;
    end process control_proc;

end architecture struct;

------------------------------------------------------------
-- Instruction Register (IR)
------------------------------------------------------------
library IEEE;
use IEEE.STD_LOGIC_1164.all;
use IEEE.NUMERIC_STD.all;

entity ir16 is
    port (
        instr_in  : in  std_logic_vector(15 downto 0);
        exe       : in  std_logic;  -- active-low clock (falling_edge)
        opcode    : out std_logic_vector(5 downto 0);
        instr_out : out std_logic_vector(15 downto 0)
    );
end entity ir16;

architecture beh of ir16 is
    signal ir_reg : std_logic_vector(15 downto 0);
begin
    -- Load instruction on falling edge of exe (1 -> 0)
    process(exe)
    begin
        if falling_edge(exe) then
            ir_reg <= instr_in;
        end if;
    end process;

    instr_out <= ir_reg;
    opcode    <= ir_reg(15 downto 10);
end architecture beh;

------------------------------------------------------------
-- 8-bit Register (reg8) – AL, BL, CL, DL
------------------------------------------------------------
library IEEE;
use IEEE.STD_LOGIC_1164.all;
use IEEE.NUMERIC_STD.all;

entity reg8 is
    port (
        ck     : in  std_logic;                    -- we use UPD (active-low clock)
        enable : in  std_logic;                    -- load enable, active high
        d      : in  std_logic_vector(7 downto 0); -- data in
        q      : out std_logic_vector(7 downto 0)  -- data out
    );
end entity reg8;

architecture beh of reg8 is
    signal q_int : std_logic_vector(7 downto 0);
begin
    process(ck)
    begin
        if falling_edge(ck) then
            if enable = '1' then
                q_int <= d;
            end if;
        end if;
    end process;

    q <= q_int;
end architecture beh;

------------------------------------------------------------
-- 3-to-4 Decoder for register enables
------------------------------------------------------------
library IEEE;
use IEEE.STD_LOGIC_1164.all;

entity dec3to4 is
    port (
        reg_sel : in  std_logic_vector(2 downto 0);
        we      : in  std_logic;
        en_AL   : out std_logic;
        en_BL   : out std_logic;
        en_CL   : out std_logic;
        en_DL   : out std_logic
    );
end entity dec3to4;

architecture beh of dec3to4 is
begin
    process(reg_sel, we)
    begin
        en_AL <= '0';
        en_BL <= '0';
        en_CL <= '0';
        en_DL <= '0';

        if we = '1' then
            case reg_sel is
                when "000" => en_AL <= '1'; -- AL
                when "011" => en_BL <= '1'; -- BL
                when "001" => en_CL <= '1'; -- CL
                when "010" => en_DL <= '1'; -- DL
                when others =>
                    null;
            end case;
        end if;
    end process;
end architecture beh;

------------------------------------------------------------
-- MUX A
------------------------------------------------------------
library IEEE;
use IEEE.STD_LOGIC_1164.all;

entity muxA5 is
    port (
        sel     : in  std_logic_vector(2 downto 0);
        AL_in   : in  std_logic_vector(7 downto 0);
        BL_in   : in  std_logic_vector(7 downto 0);
        CL_in   : in  std_logic_vector(7 downto 0);
        DL_in   : in  std_logic_vector(7 downto 0);
        imm_in  : in  std_logic_vector(7 downto 0);
        Y       : out std_logic_vector(7 downto 0)
    );
end entity muxA5;

architecture beh of muxA5 is
begin
    process(sel, AL_in, BL_in, CL_in, DL_in, imm_in)
    begin
        case sel is
            when "000" => Y <= AL_in;
            when "001" => Y <= BL_in;
            when "010" => Y <= CL_in;
            when "011" => Y <= DL_in;
            when others => Y <= imm_in;
        end case;
    end process;
end architecture beh;

------------------------------------------------------------
-- MUX B
------------------------------------------------------------
library IEEE;
use IEEE.STD_LOGIC_1164.all;

entity muxB4 is
    port (
        sel     : in  std_logic_vector(1 downto 0);
        AL_in   : in  std_logic_vector(7 downto 0);
        BL_in   : in  std_logic_vector(7 downto 0);
        CL_in   : in  std_logic_vector(7 downto 0);
        DL_in   : in  std_logic_vector(7 downto 0);
        Y       : out std_logic_vector(7 downto 0)
    );
end entity muxB4;

architecture beh of muxB4 is
begin
    process(sel, AL_in, BL_in, CL_in, DL_in)
    begin
        case sel is
            when "00" => Y <= AL_in;
            when "01" => Y <= BL_in;
            when "10" => Y <= CL_in;
            when others => Y <= DL_in;
        end case;
    end process;
end architecture beh;

------------------------------------------------------------
-- 8-bit ALU
------------------------------------------------------------
library IEEE;
use IEEE.STD_LOGIC_1164.all;
use IEEE.NUMERIC_STD.all;

entity alu8 is
    port (
        A      : in  std_logic_vector(7 downto 0);
        B      : in  std_logic_vector(7 downto 0);
        alu_op : in  std_logic_vector(3 downto 0);
        Y      : out std_logic_vector(7 downto 0)
    );
end entity alu8;

architecture beh of alu8 is
begin
    process(A, B, alu_op)
        variable a_u : unsigned(7 downto 0);
        variable b_u : unsigned(7 downto 0);
        variable y_u : unsigned(7 downto 0);
    begin
        a_u := unsigned(A);
        b_u := unsigned(B);

        case alu_op is
            when "0000" =>  -- ADD
                y_u := a_u + b_u;
            when "0001" =>  -- XOR
                y_u := a_u xor b_u;
            when "0010" =>  -- MOV A
                y_u := a_u;
            when "0011" =>  -- OR
                y_u := a_u or b_u;
            when "0100" =>  -- AND
                y_u := a_u and b_u;
            when "0101" =>  -- SHL A by 1
                y_u := shift_left(a_u, 1);
            when "0110" =>  -- SHR A by 1
                y_u := shift_right(a_u, 1);
            when "0111" =>  -- NEG A (two's complement)
                y_u := (not a_u) + 1;
            when others =>
                y_u := (others => '0');
        end case;

        Y <= std_logic_vector(y_u);
    end process;
end architecture beh;

------------------------------------------------------------
-- HEX to 7-Segment Decoder
------------------------------------------------------------
library IEEE;
use IEEE.STD_LOGIC_1164.all;

entity hex7seg is
    port (
        D  : in  std_logic_vector(3 downto 0);  -- hex digit
        s0 : out std_logic;  -- segment a
        s1 : out std_logic;  -- segment b
        s2 : out std_logic;  -- segment c
        s3 : out std_logic;  -- segment d
        s4 : out std_logic;  -- segment e
        s5 : out std_logic;  -- segment f
        s6 : out std_logic   -- segment g
    );
end entity hex7seg;

architecture beh of hex7seg is
begin
    process(D)
    begin
        -- default: all OFF (active-low)
        s0 <= '1'; s1 <= '1'; s2 <= '1';
        s3 <= '1'; s4 <= '1'; s5 <= '1'; s6 <= '1';

        case D is
            -- 0
            when "0000" =>
                s0 <= '0'; s1 <= '0'; s2 <= '0';
                s3 <= '0'; s4 <= '0'; s5 <= '0';
                s6 <= '1';
            -- 1
            when "0001" =>
                s0 <= '1'; s1 <= '0'; s2 <= '0';
                s3 <= '1'; s4 <= '1'; s5 <= '1';
                s6 <= '1';
            -- 2
            when "0010" =>
                s0 <= '0'; s1 <= '0'; s2 <= '1';
                s3 <= '0'; s4 <= '0'; s5 <= '1';
                s6 <= '0';
            -- 3
            when "0011" =>
                s0 <= '0'; s1 <= '0'; s2 <= '0';
                s3 <= '0'; s4 <= '1'; s5 <= '1';
                s6 <= '0';
            -- 4
            when "0100" =>
                s0 <= '1'; s1 <= '0'; s2 <= '0';
                s3 <= '1'; s4 <= '1'; s5 <= '0';
                s6 <= '0';
            -- 5
            when "0101" =>
                s0 <= '0'; s1 <= '1'; s2 <= '0';
                s3 <= '0'; s4 <= '1'; s5 <= '0';
                s6 <= '0';
            -- 6
            when "0110" =>
                s0 <= '0'; s1 <= '1'; s2 <= '0';
                s3 <= '0'; s4 <= '0'; s5 <= '0';
                s6 <= '0';
            -- 7
            when "0111" =>
                s0 <= '0'; s1 <= '0'; s2 <= '0';
                s3 <= '1'; s4 <= '1'; s5 <= '1';
                s6 <= '1';
            -- 8
            when "1000" =>
                s0 <= '0'; s1 <= '0'; s2 <= '0';
                s3 <= '0'; s4 <= '0'; s5 <= '0';
                s6 <= '0';
            -- 9
            when "1001" =>
                s0 <= '0'; s1 <= '0'; s2 <= '0';
                s3 <= '0'; s4 <= '1'; s5 <= '0';
                s6 <= '0';
            -- A
            when "1010" =>
                s0 <= '0'; s1 <= '0'; s2 <= '0';
                s3 <= '1'; s4 <= '0'; s5 <= '0';
                s6 <= '0';
            -- b
            when "1011" =>
                s0 <= '1'; s1 <= '1'; s2 <= '0';
                s3 <= '0'; s4 <= '0'; s5 <= '0';
                s6 <= '0';
            -- C
            when "1100" =>
                s0 <= '0'; s1 <= '1'; s2 <= '1';
                s3 <= '0'; s4 <= '0'; s5 <= '0';
                s6 <= '1';
            -- d
            when "1101" =>
                s0 <= '1'; s1 <= '0'; s2 <= '0';
                s3 <= '0'; s4 <= '0'; s5 <= '1';
                s6 <= '0';
            -- E
            when "1110" =>
                s0 <= '0'; s1 <= '1'; s2 <= '1';
                s3 <= '0'; s4 <= '0'; s5 <= '0';
                s6 <= '0';
            -- F
            when "1111" =>
                s0 <= '0'; s1 <= '1'; s2 <= '1';
                s3 <= '1'; s4 <= '0'; s5 <= '0';
                s6 <= '0';
            when others =>
                null;
        end case;
    end process;
end architecture beh;
