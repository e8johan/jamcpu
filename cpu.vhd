---------------------------------------------------------
-- JAM CPU core
-- Simple 32bit RISC CPU
--
-- Copyright © 2002:
-- Anders Lindström, Johan E. Thelin, Michael Nordseth
---------------------------------------------------------

-- This is free software; you can redistribute it and/or
-- modify it under the terms of the GNU Library General Public
-- License as published by the Free Software Foundation; either
-- version 2 of the License, or (at your option) any later version.

library IEEE;
library work;
use IEEE.std_logic_1164.all;
use IEEE.std_logic_unsigned."+";
use IEEE.std_logic_unsigned.conv_integer;

entity CPU is
	port(
		--clock and reset
		clk, reset  : in std_logic;
		
		--in and out ports
		inport      : in std_logic_vector(31 downto 0);
		outport     : out std_logic_vector(31 downto 0);
		
		--IO interface
		synctrap    : in std_logic;
		put         : out std_logic;
		curpsw11_31 : out std_logic_vector(31 downto 11);
		curpsw0_7   : out std_logic_vector(7 downto 0);
		newpsw11_31 : in std_logic_vector(31 downto 11); 
		newpsw0_7   : in std_logic_vector(7 downto 0);
		
		--IM
		im_cs    : out std_logic_vector(7 downto 0);    
		im_oe    : out std_logic;                       
		im_wri   : out std_logic;  
		im_adr   : out std_logic_vector( 16 downto 0 ); 
		im_dat   : inout std_logic_vector( 63 downto 0 );
		
		--DM
		dm_cs    : out std_logic_vector(7 downto 0);   
		dm_oe    : out std_logic;                      
		dm_wri   : out std_logic;                      
		dm_adr   : out std_logic_vector( 16 downto 0 );
		dm_dat   : inout std_logic_vector( 63 downto 0 ));
end;


architecture rev1 of CPU is
	--The location of the traphandler
	constant trap_vector : std_logic_vector(31 downto 0) := (others => '0');
	
	--PC is reset to this value on reset
	constant start_vector : std_logic_vector(31 downto 0) := (others => '0');

	---------------------------------------------------
	-- Pipelineregisters declarative part
	--
	-- foo_reg   - signals out from pipelineregister
	-- foo_in    - signals to be clocked into the reg
	-- foo_reset - output zeros next tick
	-- foo_hold  - hold signal out next tick
	---------------------------------------------------
	-- IF/ID
	type ifid_struct is record
		iword     : std_logic_vector(31 downto 0);  --instruction word
		pc        : std_logic_vector(31 downto 0);  --pc for this instruction
		
		cm_hw_int : std_logic;                      --hw interupt
	end record;
	
	signal ifid_reg   : ifid_struct;
	signal ifid_in    : ifid_struct;
	signal ifid_reset : std_logic;
	signal ifid_hold  : std_logic;


	-- ID/EX
	type idex_struct is record
		-- data
		pc            : std_logic_vector(31 downto 0);
		areg          : std_logic_vector(31 downto 0);
		breg          : std_logic_vector(31 downto 0);
		imm           : std_logic_vector(31 downto 0);
		
		-- reg index
		src1          : std_logic_vector(4 downto 0);
		src2          : std_logic_vector(4 downto 0);
		dest          : std_logic_vector(4 downto 0);
		
		-- control signals to EX stage
		cex_asel      : std_logic;
		cex_bsel      : std_logic;
		cex_ressel    : std_logic;
		cex_regsel    : std_logic_vector(1 downto 0);
		cex_aluop     : std_logic_vector(2 downto 0);
		cex_domult    : std_logic;
		cex_multop    : std_logic;
		cex_invin2    : std_logic;
		cex_valid_reg : std_logic;
		cex_valid_res : std_logic;
		cex_psw_enable: std_logic;
		cex_put       : std_logic;
		
		-- control signals to MEM stage
		cm_valid_mem  : std_logic;
		cm_valid_reg  : std_logic;
		cm_write      : std_logic;
		cm_read       : std_logic;
		cm_hw_int     : std_logic;
		cm_bad_op     : std_logic;
		cm_sw_int     : std_logic;
		cm_jump       : std_logic;
		
		-- control signals to WB stage
		cwb_sel       : std_logic;
		cwb_enable    : std_logic;
		cwb_valid     : std_logic;
		cwb_branch    : std_logic;		
	end record;
	
	signal idex_reg	  : idex_struct;
	signal idex_in    : idex_struct;
	signal idex_reset : std_logic;
	signal idex_hold  : std_logic;
	
	
	-- EX/MEM
	type exmem_struct is record
		pc           : std_logic_vector(31 downto 0);
		res          : std_logic_vector(31 downto 0);
		reg          : std_logic_vector(31 downto 0);
		dest         : std_logic_vector(4 downto 0);
		
        cm_write     : std_logic;
		cm_read      : std_logic;
		cm_valid_mem : std_logic;
		cm_valid_reg : std_logic;
		cm_hw_int    : std_logic;
		cm_bad_op    : std_logic;
		cm_sw_int    : std_logic;
		cm_jump      : std_logic;
		cm_ovf       : std_logic;
		
		cwb_sel      : std_logic;
		cwb_enable   : std_logic;
		cwb_valid    : std_logic;
		cwb_branch   : std_logic;
	end record;
	
	signal exmem_reg   : exmem_struct;
	signal exmem_in    : exmem_struct;
	signal exmem_reset : std_logic;
	signal exmem_hold  : std_logic;


	-- MEM/WB
	type memwb_struct is record
		mem        : std_logic_vector(31 downto 0);
		reg        : std_logic_vector(31 downto 0);
		dest       : std_logic_vector(4 downto 0);
		
		cwb_sel    : std_logic;
		cwb_enable : std_logic;
		cwb_valid  : std_logic;
		cwb_branch : std_logic;
	end record;
	
	signal memwb_reg   : memwb_struct;
	signal memwb_in    : memwb_struct;
	signal memwb_reset : std_logic;
	signal memwb_hold  : std_logic;



	---------------------------------------------------
	-- IF declarative part
	---------------------------------------------------
	-- memory access unit (IM)
	component MAU
		port(
			-- Signalas from/to CPU/CONTROLL
			CLK      : in std_logic;
			CTRL_R   : in std_logic;                       -- Read Mem
			CTRL_W   : in std_logic;                       -- Write Mem
			RESET    : in std_logic;
			STALL    : out std_logic;                      -- Used to stall one cycle on write
			IN_ADDR  : in std_logic_vector(31 downto 0);   -- Adress Buss from CPU
			IN_DATA  : in std_logic_vector(31 downto 0);   -- Data Buss from CPU
			OUT_DATA : out std_logic_vector(31 downto 0);  -- Data Buss to CPU
	
			-- Signals from/to MEM
			DM_CS    : out std_logic_vector(7 downto 0);   -- Chip Select (active low)
			DM_OE    : out std_logic;                      -- Output Enable (active low)
			DM_WRITE : out std_logic;                      -- Write Data, Mem (data -> HighZ) (active low)
			DM_ADDR  : out std_logic_vector(16 downto 0);  -- Address Buss to mem
			DM_DATA  : inout std_logic_vector(63 downto 0) -- Mem Data Buss (Bidirectional)
	);
	end component;
	
	for if_mau: MAU use entity work.MAU;
	
	signal if_pc      : std_logic_vector(31 downto 0);  -- current PC register
	signal if_pc_word : std_logic_vector(31 downto 0);
	
	signal if_int     : std_logic;
	signal if_iword   : std_logic_vector(31 downto 0);
	
	signal if_zero    : std_logic;
	signal if_one     : std_logic;
	signal if_zero32  : std_logic_vector(31 downto 0);
	

	---------------------------------------------------
	-- ID declarative part
	---------------------------------------------------
	-- Register file
	component regs
		port (
			ra, rb, rw       : in std_logic_vector(4 downto 0);
			value_w          : in std_logic_vector(31 downto 0);
			clk              : in std_logic;
			value_a, value_b : out std_logic_vector(31 downto 0) );
	end component;
	
	-- The format handler
	component imm_ext
    	port (
			mode : in std_logic_vector(1 downto 0);
        	op5  : in std_logic;
        	imm  : in std_logic_vector(15 downto 0);
        	r    : out std_logic_vector(31 downto 0) );
	end component;

	-- Control unit
	component control
		port (
			op           : in std_logic_vector (5 downto 0);   -- The op code

			ex_asel      : out std_logic;                      -- (ra,psw)
			ex_bsel      : out std_logic;                      -- (rb,imm)
			ex_ressel    : out std_logic;                      -- (res,sign)
			ex_regsel    : out std_logic_vector (1 downto 0);  -- (psw,pc,b,in)
			ex_aluop     : out std_logic_vector (2 downto 0);  -- (nop,add,sub,op,and,xor,shz,shs)
			ex_domult    : out std_logic;                      -- (off,on)
			ex_multop    : out std_logic;                      -- (low,high)
			ex_invin2    : out std_logic;                      -- (off,on)
			ex_valid_reg : out std_logic;                      -- valid data
			ex_valid_res : out std_logic;                      -- valid data
			ex_psw_enable: out std_logic;                      -- write to psw?
			ex_put       : out std_logic;                      -- write to outport

			m_read       : out std_logic;                      -- DM read
			m_write      : out std_logic;                      -- DM write
			m_valid_reg  : out std_logic;                      -- valid data
			m_valid_mem  : out std_logic;                      -- valid data

			wb_sel       : out std_logic;                      -- (mem,reg)
			wb_enable    : out std_logic;                      -- (off,on)

			id_bsel      : out std_logic;                      -- '1' if rd is used as r2
			id_beq       : out std_logic;                      -- '1' if the op is a branch
			id_bne       : out std_logic;                      -- '1' if the op is a branch n.eq.

			illegal_op   : out std_logic;                      -- '1' when illegal op
	
			trap         : out std_logic;                      -- trap
			jump         : out std_logic);                     -- jump instr.
	end component;

	for id_regs: regs use entity work.regs(rev1);
	for id_ix: imm_ext use entity work.imm_ext(rev1);
	for id_ctrl: control use entity work.control(rev1);
	
	signal id_rb        : std_logic_vector(4 downto 0);
	signal id_branchadr : std_logic_vector(31 downto 0);
	signal id_a         : std_logic_vector(31 downto 0);
	signal id_b         : std_logic_vector(31 downto 0);
	signal id_stalling  : std_logic;
	signal cid_branch   : std_logic;  -- branch?
	signal cid_cmp      : std_logic;  -- High when a == b
	signal cid_bsel     : std_logic;
	signal cid_beq      : std_logic;
	signal cid_bne      : std_logic;

	-- Valid signals from control
	signal cex_valid_reg : std_logic;
	signal cex_valid_res : std_logic;
	signal cm_valid_mem  : std_logic;
	signal cm_valid_reg  : std_logic;
	
	
	---------------------------------------------------
	-- EX declarative part
	---------------------------------------------------
	-- integer unit (ALU + mult)
	component IU
		port(
			a_in, b_in : in std_logic_vector(31 downto 0);  -- IU input
			do_mult    : in std_logic;                      -- Do multicycle mult?
			mult_op    : in std_logic;                      -- Return [0=LSW] or [1=MSW) of mult result
			alu_op     : in std_logic_vector(2 downto 0);   -- ALU op
			inv_in2    : in std_logic;                      -- Invert operator in2 in ALU
			clk        : in std_logic;                      -- Global clock
			reset      : in std_logic;                      -- Global reset
			ovf        : out std_logic;                     -- Overflow
			result     : out std_logic_vector(31 downto 0); -- IU result
			mc         : out std_logic);                    -- Goes high during the last cycle of a multicycle op
	end component;

	for ex_iu: IU use entity work.IU(rev1);
	
	signal ex_ain          : std_logic_vector(31 downto 0);
	signal ex_bin          : std_logic_vector(31 downto 0);
	signal ex_breg         : std_logic_vector(31 downto 0);
	signal ex_psw          : std_logic_vector(31 downto 0);
	signal ex_result       : std_logic_vector(31 downto 0);
	signal ex_wb_data_buf  : std_logic_vector(31 downto 0);
	signal ex_wb_dest_buf  : std_logic_vector(4 downto 0);
	signal ex_wb_valid_buf : std_logic;
	signal ex_stalling     : std_logic;
	signal ex_lw_hazard    : std_logic;
	signal ex_mc_finished  : std_logic;
	signal ex_do_mult      : std_logic;
	
	
	---------------------------------------------------
	-- MEM declarative part
	---------------------------------------------------
	-- memory access unit (DM)
	for mem_mau: MAU use entity work.MAU;
		
	signal mem_stalling   : std_logic;
	signal mem_clear      : std_logic;
	signal mem_res        : std_logic_vector(31 downto 0);
	signal mem_data       : std_logic_vector(31 downto 0);
	signal mem_adr_word   : std_logic_vector(31 downto 0);
	signal mem_jump_trap  : std_logic;
	
	
	---------------------------------------------------
	-- WB declarative part
	---------------------------------------------------
	signal wb_rw    : std_logic_vector(4 downto 0);
	signal wb_value : std_logic_vector (31 downto 0);


begin
	---------------------------------------------------
	-- Pipeline registers
	---------------------------------------------------
	--IF/ID
	process(clk)
	begin
		if clk'event and clk='1' then
			if ( (ifid_reset = '1') or (reset = '1') ) then
				ifid_reg <= ((others => '0'), (others => '0'), '0');
			elsif ifid_hold /= '1' then
				ifid_reg <= ifid_in;
			end if;
		end if;
	end process;
	
	-- Stall/nop logic
	ifid_hold <=
		'1' when ( (id_stalling = '1') or (ex_stalling = '1') or (mem_stalling = '1') ) else
		'0';
	ifid_reset <= mem_clear;


	--ID/EX
	process(clk)
	begin
		if clk'event and clk='1' then
			if ( (idex_reset = '1') or (reset = '1') ) then
				idex_reg <= ((others => '0'),(others => '0'),(others => '0'),(others => '0'),(others => '0'),(others => '0'),(others => '0'),'0','0','0',(others => '0'),(others => '0'),'0','0','0','0','0','0','0','0','0','0','0','0','0','0','0','0','0','0','0');
			elsif idex_hold /= '1' then
				idex_reg <= idex_in;
			end if;
		end if;
	end process;
	
	-- Stall/nop logic	
	idex_hold <=
		'1' when ( (ex_stalling = '1') or (mem_stalling = '1') ) else
		'0';
	idex_reset <=
		'1' when ( ( (id_stalling = '1') and (idex_hold = '0') ) or (mem_clear = '1') ) else
		'0';
	

	--EX/MEM
	process(clk)
	begin
		if clk'event and clk='1' then
			if ( (exmem_reset = '1') or (reset = '1') ) then
				exmem_reg <= ((others => '0'),(others => '0'),(others => '0'),(others => '0'),'0','0','0','0','0','0','0','0','0','0','0','0','0');
			elsif exmem_hold /= '1' then
				exmem_reg <= exmem_in;
			end if;
		end if;
	end process;
	
	-- Stall/nop logic
	exmem_hold <=
		'1' when  (mem_stalling = '1') else
		'0';	
	exmem_reset <= 
		'1' when mem_clear = '1' else
		'1' when ( (ex_stalling = '1') and (mem_stalling = '0') ) else
		'0';


	--MEM/WB latch
	process(clk)
	begin
		if clk'event and clk='1' then
			if ( (memwb_reset = '1') or (reset = '1') ) then
				memwb_reg <= ((others => '0'),(others => '0'),(others => '0'),'0','0','0','0');
			elsif memwb_hold /= '1' then
				memwb_reg <= memwb_in;
			end if;
		end if;
	end process;
	
	-- Stall/nop logic
	memwb_hold <= 
		'1' when (mem_stalling = '1') else
		'0';
	memwb_reset <= '0';


	
	---------------------------------------------------
	-- IF Stage
	---------------------------------------------------
	-- IM is read only
	if_zero <= '0';
	if_one <= '1';
	if_zero32 <= (others => '0');
	
	if_pc_word <= "00" & if_pc(31 downto 2);
	
	-- [PORTMAP] IM
	if_mau : MAU port map(
		CLK => clk,
		RESET => reset,
		CTRL_W => if_zero,
		CTRL_R => if_one,
		--STALL =>
		IN_ADDR => if_pc_word,
		IN_DATA => if_zero32,
		OUT_DATA => if_iword,
		DM_CS => im_cs,
		DM_OE => im_oe,
		DM_WRITE => im_wri,
		DM_ADDR => im_adr,
		DM_DATA => im_dat
		);
		
	ifid_in.pc <= if_pc; 
	--ifid_in.iword <= rom_image(conv_integer(if_pc(31 downto 2)));  --fetch instruction from ROM (in simcpu nowdays)
	
	ifid_in.iword <= 
		if_iword when if_int = '0' else
		"10100100000000000000000000000000";  --inject trap
	
	-- HW interupt injector
	ifid_in.cm_hw_int <= if_int;
	
	-- handle PC
	process(clk)
	begin
		if clk'event and clk='1' then
			if reset = '1' then
				if_pc <= start_vector;
			--Update PC
			elsif ifid_hold = '0' then
				if exmem_reg.cm_jump = '1' then
					if_pc <= exmem_reg.res;
				elsif mem_jump_trap = '1' then
					if_pc <= trap_vector;
				elsif cid_branch = '1' then
					if_pc <= id_branchadr;
				else
					if_pc <= if_pc + "100"; -- pc+=4
				end if;
			end if;
		end if;
	end process;

	-- Interupt injector
	process(clk)
	begin
		if clk'event and clk='1' then
			if ( (reset = '1') or (exmem_reg.cm_hw_int = '1') ) then
				if_int <= '0';
			elsif ( (synctrap='1') or (if_int = '1') ) then
				if_int <= '1';
			end if;
		end if;
	end process;
			


	---------------------------------------------------
	-- ID Stage
	---------------------------------------------------
	-- Some stupid instructions use dest for regb, this handles them!
	-- We can let control handle this or do it ourself.
	id_rb <= 
		--ifid_reg.iword(25 downto 21) when cid_bsel = '0' else
		ifid_reg.iword(25 downto 21) when (ifid_reg.iword(31 downto 26) = "100111") else  --SW
		ifid_reg.iword(25 downto 21) when (ifid_reg.iword(31 downto 26) = "010111") else  --BNE
		ifid_reg.iword(25 downto 21) when (ifid_reg.iword(31 downto 26) = "010011") else  --BEQ
		ifid_reg.iword(15 downto 11);
	
	-- Destination field in the iword
	idex_in.dest <= ifid_reg.iword(25 downto 21);
	
	-- [PORTMAP] The register bank
	id_regs : regs port map(
		ra => ifid_reg.iword(20 downto 16),
		rb => id_rb,
		rw => wb_rw,
		value_w => wb_value,
		clk => clk,
		value_a => idex_in.areg,
		value_b => idex_in.breg);
	
	-- Send sourceregs. to idex reg (used for forwarding logic)
	idex_in.src1 <= ifid_reg.iword(20 downto 16);
	idex_in.src2 <= id_rb;
	
	-- [PORTMAP] The immediate extender unit, does magic to the imm field
	id_ix : imm_ext port map(
		mode => ifid_reg.iword(27 downto 26),
		op5 => ifid_reg.iword(31),
		imm => ifid_reg.iword(15 downto 0),
		r => idex_in.imm);
	
	-- Forwarding logic for branch compare
	id_a <=
		--exmem_in.res when ( (idex_in.src1 = idex_reg.dest) and (idex_reg.cex_valid_res = '1') ) else  -- critical path
		--exmem_in.reg when ( (idex_in.src1 = idex_reg.dest) and (idex_reg.cex_valid_reg = '1') ) else  -- we stall instead
		exmem_reg.res when ( (idex_in.src1 = exmem_reg.dest) and (exmem_reg.cm_valid_mem = '1') ) else
		exmem_reg.reg when ( (idex_in.src1 = exmem_reg.dest) and (exmem_reg.cm_valid_reg = '1') ) else
		idex_in.areg;
	id_b <=
		--exmem_in.res when ( (idex_in.src2 = idex_reg.dest) and (idex_reg.cex_valid_res = '1') ) else
		--exmem_in.reg when ( (idex_in.src2 = idex_reg.dest) and (idex_reg.cex_valid_reg = '1') ) else		
		exmem_reg.res when ( (idex_in.src2 = exmem_reg.dest) and (exmem_reg.cm_valid_mem = '1') ) else		
		exmem_reg.reg when ( (idex_in.src2 = exmem_reg.dest) and (exmem_reg.cm_valid_reg = '1') ) else
		idex_in.breg;

	--Stall when we need data from EX in branch logic
	id_stalling <=
		'0' when ( (cid_bne = '0') and (cid_beq = '0') ) else
		'1' when ( (idex_in.src1 = idex_reg.dest) and ( (idex_reg.cex_valid_res = '1') or (idex_reg.cex_valid_reg = '1') ) ) else
		'1' when ( (idex_in.src2 = idex_reg.dest) and ( (idex_reg.cex_valid_res = '1') or (idex_reg.cex_valid_reg = '1') ) ) else
		'0';

	-- The branch logic
	cid_cmp <= 
		'1' when (id_a = id_b) else
		'0';
	cid_branch <= 
		'1' when ( (cid_cmp = '0') and (cid_bne = '1') ) else
		'1' when ( (cid_cmp = '1') and (cid_beq = '1') ) else
		'0';
	
	-- Save branch flag for branch delay slot detection by trap handler
	idex_in.cwb_branch <= cid_branch;
	
	-- Jumpdestination calc.
	id_branchadr <= idex_in.imm + ifid_reg.pc;
		
	-- [PORTMAP] The control unit
	id_ctrl : control port map(
		op => ifid_reg.iword(31 downto 26),
		ex_asel => idex_in.cex_asel,
		ex_bsel => idex_in.cex_bsel,
		ex_ressel => idex_in.cex_ressel,
		ex_regsel => idex_in.cex_regsel,
		ex_aluop => idex_in.cex_aluop,
		ex_domult => idex_in.cex_domult,
		ex_multop => idex_in.cex_multop,
		ex_invin2 => idex_in.cex_invin2,
		ex_valid_res => cex_valid_res,
		ex_valid_reg => cex_valid_reg,
		ex_psw_enable => idex_in.cex_psw_enable,
		ex_put => idex_in.cex_put,

		m_write => idex_in.cm_write,
		m_read => idex_in.cm_read,
		m_valid_mem => cm_valid_mem,
		m_valid_reg => cm_valid_reg,
		
		wb_sel => idex_in.cwb_sel,
		wb_enable => idex_in.cwb_enable,
		
		id_bsel => cid_bsel,
		id_beq => cid_beq,
		id_bne => cid_bne,
		
		illegal_op => idex_in.cm_bad_op,
		trap => idex_in.cm_sw_int,
		jump => idex_in.cm_jump
		);

	-- The valid data control signals
	idex_in.cex_valid_reg <=
		'0' when ifid_reg.iword(25 downto 21) = "00000" else
		cex_valid_reg;              

	idex_in.cex_valid_res <=
		'0' when ifid_reg.iword(25 downto 21) = "00000" else
		cex_valid_res;              

	idex_in.cm_valid_mem <=
		'0' when ifid_reg.iword(25 downto 21) = "00000" else
		cm_valid_mem;              

	idex_in.cm_valid_reg <=
		'0' when ifid_reg.iword(25 downto 21) = "00000" else
		cm_valid_reg;              

	idex_in.cwb_valid <= 
		'0' when idex_in.dest = "00000" else   --R0
		'0' when idex_in.cwb_enable = '0' else --No valid data
		'1';

	-- copy to next pipeline stage
	idex_in.pc <= ifid_reg.pc;
	idex_in.cm_hw_int <= ifid_reg.cm_hw_int;
	

	

	---------------------------------------------------
	-- EX Stage
	---------------------------------------------------
	-- [PORTMAP] Integer unit
	ex_iu : IU port map(
		a_in => ex_ain,
		b_in => ex_bin,
		do_mult => ex_do_mult,
		mult_op => idex_reg.cex_multop,
		alu_op => idex_reg.cex_aluop,
		inv_in2 => idex_reg.cex_invin2,
		clk => clk,
		reset => reset,
		ovf => exmem_in.cm_ovf,
		result => ex_result,
		mc => ex_mc_finished
		);
	
	-- LW hazard (we need the result from a LW)?
	ex_lw_hazard <=
		'0' when exmem_reg.cm_read = '0' else
		'1' when ((exmem_reg.dest = idex_reg.src1) or (exmem_reg.dest = idex_reg.src2)) else
		'0';
	
	-- EX stall logic
	ex_stalling <=
		--stall on mult
		'0' when ex_mc_finished = '1' else
		'1' when idex_reg.cex_domult = '1' else
		
		-- stall on LW (and we need the result)
		'1' when ex_lw_hazard = '1' else
		'0';

		
	-- don't start mult on LW stall
	ex_do_mult <=
		'0' when ex_lw_hazard = '1' else
		idex_reg.cex_domult;

	
	-- copy signals to next pipeline stage
	exmem_in.pc           <= idex_reg.pc;
	exmem_in.dest         <= idex_reg.dest;
	exmem_in.cm_read      <= idex_reg.cm_read;
	exmem_in.cm_write     <= idex_reg.cm_write;
	exmem_in.cm_valid_mem <= idex_reg.cm_valid_mem;
	exmem_in.cm_valid_reg <= idex_reg.cm_valid_reg;
	exmem_in.cm_hw_int    <= idex_reg.cm_hw_int;
	exmem_in.cm_bad_op    <= idex_reg.cm_bad_op;
	exmem_in.cm_sw_int    <= idex_reg.cm_sw_int;
	exmem_in.cm_jump      <= idex_reg.cm_jump;
	exmem_in.cwb_sel      <= idex_reg.cwb_sel;
	exmem_in.cwb_enable   <= idex_reg.cwb_enable;
	exmem_in.cwb_valid    <= idex_reg.cwb_valid;
	exmem_in.cwb_branch   <= idex_reg.cwb_branch;
	
	-- outport pins on CPU
	outport <= idex_reg.areg;
	put <= idex_reg.cex_put;
			
	-- A input to IU
	ex_ain <= 
		ex_psw when idex_reg.cex_asel = '1' else
		exmem_reg.res when ( (idex_reg.src1 = exmem_reg.dest) and (exmem_reg.cm_valid_mem = '1') ) else  -- Forwarding from mem to ex
		exmem_reg.reg when ( (idex_reg.src1 = exmem_reg.dest) and (exmem_reg.cm_valid_reg = '1') ) else  -- Forwarding from mem to ex
		wb_value when ( (idex_reg.src1 = memwb_reg.dest) and (memwb_reg.cwb_valid = '1') ) else          -- Forwarding from wb to ex
		ex_wb_data_buf when ( (idex_reg.src1 = ex_wb_dest_buf) and (ex_wb_valid_buf ='1') ) else
		idex_reg.areg;
		
	-- Forwarding logic for breg data
	ex_breg <=
		exmem_reg.res when ( (idex_reg.src2 = exmem_reg.dest) and (exmem_reg.cm_valid_mem = '1') ) else  -- Forwarding from mem to ex
		exmem_reg.reg when ( (idex_reg.src2 = exmem_reg.dest) and (exmem_reg.cm_valid_reg = '1') ) else  -- Forwarding from mem to ex
		wb_value when ( (idex_reg.src2 = memwb_reg.dest) and (memwb_reg.cwb_valid = '1') ) else          -- Forwarding from wb to ex	
		ex_wb_data_buf when ( (idex_reg.src2 = ex_wb_dest_buf) and (ex_wb_valid_buf ='1') ) else
		idex_reg.breg;
					  
	-- B input on IU
	ex_bin <= 
		idex_reg.imm when idex_reg.cex_bsel = '1' else
		ex_breg;
	
	-- Mux after IU
	exmem_in.res <= 
		(0 => ex_result(31), others => '0') when idex_reg.cex_ressel = '1' else
		ex_result;
	
	-- Data for reg
	exmem_in.reg <= 
		ex_psw when idex_reg.cex_regsel = "00" else
		idex_reg.pc when idex_reg.cex_regsel = "01" else
		ex_breg when idex_reg.cex_regsel = "10" else
		inport;
	
	-- Mirror PSW to outside world
	curpsw11_31 <= ex_psw(31 downto 11);
	curpsw0_7 <= ex_psw(7 downto 0);
	
	process(clk)
	begin
		if clk'event and clk='1' then
			if reset = '1' then
				-- reset PSW
				ex_psw <= "11111111111111111111111100000000";
				ex_wb_valid_buf <= '0';
			else
				--We must buffer the WB stage data on LW hazards
				if ex_lw_hazard = '1' then
					ex_wb_valid_buf <= memwb_reg.cwb_valid;
					ex_wb_dest_buf <= wb_rw;
					ex_wb_data_buf <= wb_value;
				else
					ex_wb_valid_buf <= '0';
				end if;
		
				--Update psw
				if exmem_reg.cm_bad_op = '1' then
					ex_psw(9) <= '1';
				elsif exmem_reg.cm_ovf = '1' then
					ex_psw(8) <= '1';
				elsif idex_reg.cex_psw_enable = '1' then
					ex_psw <= ex_result;
				end if;
				
				if ifid_reg.cm_hw_int = '1' then
					ex_psw(31 downto 11) <= newpsw11_31;
					ex_psw(7 downto 0) <= newpsw0_7;
				end if;
			end if;
		end if;
	end process;



	---------------------------------------------------
	-- MEM Stage
	---------------------------------------------------
	mem_adr_word <= "00" & exmem_reg.res(31 downto 2);
	
	--[PORTMAP] DM
	mem_mau : MAU port map(
		CLK => clk,
		RESET => reset,
		CTRL_W => exmem_reg.cm_write,
		CTRL_R => exmem_reg.cm_read,
		STALL => mem_stalling,
		IN_ADDR => mem_adr_word,
		IN_DATA => mem_data, -- To forw. mux
		OUT_DATA => mem_res,
		DM_CS => dm_cs,
		DM_OE => dm_oe,
		DM_WRITE => dm_wri,
		DM_ADDR => dm_adr,
		DM_DATA => dm_dat
		);
	
	
	-- copy signals and regs to next pipe stage
	memwb_in.cwb_enable <= exmem_reg.cwb_enable;
	memwb_in.cwb_valid  <= exmem_reg.cwb_valid;
	memwb_in.cwb_branch <= exmem_reg.cwb_branch;

	-- Forwarding from wb to mem only when SW. 
	mem_data <=	
		wb_value when ( (exmem_reg.cm_write = '1') and (exmem_reg.dest = memwb_reg.dest) and (memwb_reg.cwb_valid = '1') ) else
		exmem_reg.reg;
	
	-- Mux, selecting mem-result or IU-result.
	memwb_in.mem <= 
		mem_res when exmem_reg.cm_read = '1' else
		exmem_reg.res;
	
	-- Jump to trap handler?
	mem_jump_trap <=
		'0' when memwb_reg.cwb_branch = '1' else --this is a delay slot (ignore trap)
		'1' when exmem_reg.cm_hw_int = '1' else
		'1' when exmem_reg.cm_bad_op = '1' else
		'1' when exmem_reg.cm_sw_int = '1' else
		'1' when exmem_reg.cm_ovf = '1' else
		'0';
	
	-- Make WB write PC to R31 on trap (else copy normal values)
	memwb_in.dest <=
		"11111" when mem_jump_trap = '1' else
		exmem_reg.dest;
	memwb_in.cwb_sel <=
		'1' when mem_jump_trap = '1' else
		exmem_reg.cwb_sel;
	memwb_in.reg <=
		exmem_reg.pc when mem_jump_trap = '1' else
		exmem_reg.reg;
		
	-- Clear IF/ID, ID/EX and EX/MEM
	mem_clear <=
		'1' when ( (exmem_reg.cm_jump = '1') or (mem_jump_trap = '1') ) else
		'0';		
	


	---------------------------------------------------
	-- WB Stage
	---------------------------------------------------
	-- Write what to regs?
	wb_value <= 
		memwb_reg.mem when memwb_reg.cwb_sel = '0' else
		memwb_reg.reg;
		
	-- Write or not to write, that is the question... 
	wb_rw <= 
		"00000" when memwb_reg.cwb_enable = '0' else
		memwb_reg.dest;	
	
end;
