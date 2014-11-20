---------------------------------------------------------
-- JAM CPU
-- Control unit
--
-- License: LGPL v2+ (see the file LICENSE)
-- Copyright © 2002:
-- Anders Lindström, Johan E. Thelin, Michael Nordseth
---------------------------------------------------------

-- This is free software; you can redistribute it and/or
-- modify it under the terms of the GNU Library General Public
-- License as published by the Free Software Foundation; either
-- version 2 of the License, or (at your option) any later version.

library ieee;
use ieee.std_logic_1164.all;

entity control is
	port (
		op : in std_logic_vector (5 downto 0);         	-- The op code
		
		ex_asel : out std_logic;						-- (ra,psw)
		ex_bsel : out std_logic;						-- (rb,imm)
		ex_ressel : out std_logic;						-- (res,sign)
		ex_regsel : out std_logic_vector (1 downto 0);	-- (psw,pc,b,in)
		ex_aluop : out std_logic_vector (2 downto 0);	-- (nop,add,sub,or,and,xor,shz,shs)
		ex_domult : out std_logic;						-- (off,on)
		ex_multop : out std_logic;						-- (low,high)
		ex_invin2 : out std_logic;						-- (off,on)
    	ex_valid_reg : out std_logic;                   -- Forwarding valid
    	ex_valid_res : out std_logic;                   -- Forwarding valid
		ex_psw_enable : out std_logic;					-- PSW will be updated
		ex_put : out std_logic;							-- This is a PUT

    	m_write : out std_logic;						-- Write signal to the DM
    	m_read : out std_logic;							-- Read signal to the DM
    	m_valid_mem : out std_logic;                    -- Forwarding valid
    	m_valid_reg : out std_logic;                    -- Forwarding valid

		wb_sel : out std_logic;							-- (mem,reg)
		wb_enable : out std_logic;						-- (off,on)

		id_bsel : out std_logic;                       	-- '0' if rd is used as r2
		id_beq : out std_logic;                        	-- '1' if the op is a branch
		id_bne : out std_logic;                        	-- '1' if the op is a branch n.eq.
		
		illegal_op : out std_logic;                  	-- '1' when illegal op
		
		trap : out std_logic;
		jump : out std_logic); 
end;

architecture rev1 of control is
begin
	process(op)
	begin
		case op is
			when "000000" =>         -- ADD
				ex_asel <= '0';					-- register
				ex_bsel <= '0';					-- register
				ex_ressel <= '0';				-- result
				ex_regsel <= "10";				-- b register
				ex_aluop <= "001";				-- add
				ex_domult <= '0';				-- no mult
				ex_multop <= '0';				-- low
				ex_invin2 <= '0';				-- do not invert
				ex_valid_reg <= '0';			-- reg is not valid
				ex_valid_res <= '1';			-- res is valid
				ex_psw_enable <= '0';			-- psw is not updated
				ex_put <= '0';					-- this is not a put

				m_write <= '0';					-- no write
				m_read <= '0';					-- no read
				m_valid_mem <= '1';				-- mem is valid
				m_valid_reg <= '0';				-- reg is not valid
				
				trap <= '0';					-- no trap
				jump <= '0';					-- no jump

				wb_sel <= '0';					-- memory
				wb_enable <= '1';				-- enable

				id_bsel <= '1';					-- use b
				id_beq <= '0';					-- no branch
				id_bne <= '0';					-- no branch

				illegal_op <= '0';				-- legal op
			when "000001" =>         -- ADDI
				ex_asel <= '0';
				ex_bsel <= '1';
				ex_ressel <= '0';
				ex_regsel <= "10";
				ex_aluop <= "001";
				ex_domult <= '0';
				ex_multop <= '0';
				ex_invin2 <= '0';
				ex_valid_reg <= '0';
				ex_valid_res <= '1';
				ex_psw_enable <= '0';
				ex_put <= '0';
				
				m_write <= '0';
				m_read <= '0';
				m_valid_mem <= '1';
				m_valid_reg <= '0';

				trap <= '0';
				jump <= '0';

				wb_sel <= '0';
				wb_enable <= '1';

				id_bsel <= '1';
				id_beq <= '0';
				id_bne <= '0';

				illegal_op <= '0';
			when "000010" =>         -- ADDX
				ex_asel <= '0';
				ex_bsel <= '1';
				ex_ressel <= '0';
				ex_regsel <= "10";
				ex_aluop <= "001";
				ex_domult <= '0';
				ex_multop <= '0';
				ex_invin2 <= '0';
				ex_valid_reg <= '0';
				ex_valid_res <= '1';
				ex_psw_enable <= '0';
				ex_put <= '0';

				m_write <= '0';
				m_read <= '0';
				m_valid_mem <= '1';
				m_valid_reg <= '0';

				trap <= '0';
				jump <= '0';

				wb_sel <= '0';
				wb_enable <= '1';

				id_bsel <= '1';
				id_beq <= '0';
				id_bne <= '0';

				illegal_op <= '0';			
			when "000011" =>         -- ADDD
				ex_asel <= '0';
				ex_bsel <= '1';
				ex_ressel <= '0';
				ex_regsel <= "10";
				ex_aluop <= "001";
				ex_domult <= '0';
				ex_multop <= '0';
				ex_invin2 <= '0';
				ex_valid_reg <= '0';
				ex_valid_res <= '1';
				ex_psw_enable <= '0';
				ex_put <= '0';

				m_write <= '0';
				m_read <= '0';
				m_valid_mem <= '1';
				m_valid_reg <= '0';

				trap <= '0';
				jump <= '0';

				wb_sel <= '0';
				wb_enable <= '1';

				id_bsel <= '1';
				id_beq <= '0';
				id_bne <= '0';

				illegal_op <= '0';
			when "000100" =>         -- ADDV
				ex_asel <= '0';
				ex_bsel <= '0';
				ex_ressel <= '0';
				ex_regsel <= "10";
				ex_aluop <= "001";
				ex_domult <= '0';
				ex_multop <= '0';
				ex_invin2 <= '0';
				ex_valid_reg <= '1';
				ex_valid_res <= '0';
				ex_psw_enable <= '0';
				ex_put <= '0';

				m_write <= '0';
				m_read <= '0';
				m_valid_mem <= '1';
				m_valid_reg <= '0';

				trap <= '0';
				jump <= '0';

				wb_sel <= '0';
				wb_enable <= '1';

				id_bsel <= '1';
				id_beq <= '0';
				id_bne <= '0';

				illegal_op <= '0';
			when "000101" =>         -- ADDVI
				ex_asel <= '0';
				ex_bsel <= '1';
				ex_ressel <= '0';
				ex_regsel <= "10";
				ex_aluop <= "001";
				ex_domult <= '0';
				ex_multop <= '0';
				ex_invin2 <= '0';
				ex_valid_reg <= '0';
				ex_valid_res <= '1';
				ex_psw_enable <= '0';
				ex_put <= '0';

				m_write <= '0';
				m_read <= '0';
				m_valid_mem <= '1';
				m_valid_reg <= '0';

				trap <= '0';
				jump <= '0';

				wb_sel <= '0';
				wb_enable <= '1';

				id_bsel <= '1';
				id_beq <= '0';
				id_bne <= '0';

				illegal_op <= '0';
			when "000110" =>         -- ADDVX
				ex_asel <= '0';
				ex_bsel <= '1';
				ex_ressel <= '0';
				ex_regsel <= "10";
				ex_aluop <= "001";
				ex_domult <= '0';
				ex_multop <= '0';
				ex_invin2 <= '0';
				ex_valid_reg <= '0';
				ex_valid_res <= '1';
				ex_psw_enable <= '0';
				ex_put <= '0';

				m_write <= '0';
				m_read <= '0';
				m_valid_mem <= '1';
				m_valid_reg <= '0';

				trap <= '0';
				jump <= '0';

				wb_sel <= '0';
				wb_enable <= '1';

				id_bsel <= '1';
				id_beq <= '0';
				id_bne <= '0';

				illegal_op <= '0';
			when "000111" =>         -- ADDVD
				ex_asel <= '0';
				ex_bsel <= '1';
				ex_ressel <= '0';
				ex_regsel <= "10";
				ex_aluop <= "001";
				ex_domult <= '0';
				ex_multop <= '0';
				ex_invin2 <= '0';
				ex_valid_reg <= '0';
				ex_valid_res <= '1';
				ex_psw_enable <= '0';
				ex_put <= '0';

				m_write <= '0';
				m_read <= '0';
				m_valid_mem <= '1';
				m_valid_reg <= '0';

				trap <= '0';
				jump <= '0';

				wb_sel <= '0';
				wb_enable <= '1';

				id_bsel <= '1';
				id_beq <= '0';
				id_bne <= '0';

				illegal_op <= '0';
			when "001000" =>         -- MUL
				ex_asel <= '0';
				ex_bsel <= '0';
				ex_ressel <= '0';
				ex_regsel <= "10";
				ex_aluop <= (others=>'0');
				ex_domult <= '1';
				ex_multop <= '0';
				ex_invin2 <= '0';
				ex_valid_reg <= '0';
				ex_valid_res <= '1';
				ex_psw_enable <= '0';
				ex_put <= '0';

				m_write <= '0';
				m_read <= '0';
				m_valid_mem <= '1';
				m_valid_reg <= '0';

				trap <= '0';
				jump <= '0';

				wb_sel <= '0';
				wb_enable <= '1';

				id_bsel <= '1';
				id_beq <= '0';
				id_bne <= '0';

				illegal_op <= '0';
			when "001001" =>         -- MULI
				ex_asel <= '0';
				ex_bsel <= '1';
				ex_ressel <= '0';
				ex_regsel <= "10";
				ex_aluop <= (others=>'0');
				ex_domult <= '1';
				ex_multop <= '0';
				ex_invin2 <= '0';
				ex_valid_reg <= '0';
				ex_valid_res <= '1';
				ex_psw_enable <= '0';
				ex_put <= '0';

				m_write <= '0';
				m_read <= '0';
				m_valid_mem <= '1';
				m_valid_reg <= '0';

				trap <= '0';
				jump <= '0';

				wb_sel <= '0';
				wb_enable <= '1';

				id_bsel <= '1';
				id_beq <= '0';
				id_bne <= '0';

				illegal_op <= '0';
--			when "001010" =>         -- -
--			when "001011" =>         -- -
			when "001100" =>         -- MULH
				ex_asel <= '0';
				ex_bsel <= '0';
				ex_ressel <= '0';
				ex_regsel <= "10";
				ex_aluop <= (others=>'0');
				ex_domult <= '1';
				ex_multop <= '1';
				ex_invin2 <= '0';
				ex_valid_reg <= '0';
				ex_valid_res <= '1';
				ex_psw_enable <= '0';
				ex_put <= '0';

				m_write <= '0';
				m_read <= '0';
				m_valid_mem <= '1';
				m_valid_reg <= '0';

				trap <= '0';
				jump <= '0';

				wb_sel <= '0';
				wb_enable <= '1';

				id_bsel <= '1';
				id_beq <= '0';
				id_bne <= '0';

				illegal_op <= '0';
			when "001101" =>         -- MULHI
				ex_asel <= '0';
				ex_bsel <= '1';
				ex_ressel <= '0';
				ex_regsel <= "10";
				ex_aluop <= (others=>'0');
				ex_domult <= '1';
				ex_multop <= '1';
				ex_invin2 <= '0';
				ex_valid_reg <= '0';
				ex_valid_res <= '1';
				ex_psw_enable <= '0';
				ex_put <= '0';

				m_write <= '0';
				m_read <= '0';
				m_valid_mem <= '1';
				m_valid_reg <= '0';

				trap <= '0';
				jump <= '0';

				wb_sel <= '0';
				wb_enable <= '1';

				id_bsel <= '1';
				id_beq <= '0';
				id_bne <= '0';

				illegal_op <= '0';
--			when "001110" =>         -- -
--			when "001111" =>         -- -
			when "010000" =>         -- SHZ
				ex_asel <= '0';
				ex_bsel <= '0';
				ex_ressel <= '0';
				ex_regsel <= "10";
				ex_aluop <= "110";
				ex_domult <= '0';
				ex_multop <= '0';
				ex_invin2 <= '0';
				ex_valid_reg <= '0';
				ex_valid_res <= '1';
				ex_psw_enable <= '0';
				ex_put <= '0';

				m_write <= '0';
				m_read <= '0';
				m_valid_mem <= '1';
				m_valid_reg <= '0';

				trap <= '0';
				jump <= '0';

				wb_sel <= '0';
				wb_enable <= '1';

				id_bsel <= '1';
				id_beq <= '0';
				id_bne <= '0';

				illegal_op <= '0';
			when "010001" =>         -- SHZI
				ex_asel <= '0';
				ex_bsel <= '1';
				ex_ressel <= '0';
				ex_regsel <= "10";
				ex_aluop <= "110";
				ex_domult <= '0';
				ex_multop <= '0';
				ex_invin2 <= '0';
				ex_valid_reg <= '0';
				ex_valid_res <= '1';
				ex_psw_enable <= '0';
				ex_put <= '0';

				m_write <= '0';
				m_read <= '0';
				m_valid_mem <= '1';
				m_valid_reg <= '0';

				trap <= '0';
				jump <= '0';

				wb_sel <= '0';
				wb_enable <= '1';

				id_bsel <= '1';
				id_beq <= '0';
				id_bne <= '0';

				illegal_op <= '0';
--			when "010010" =>         -- -
			when "010011" =>         -- BEQ
				ex_asel <= '0';
				ex_bsel <= '1';
				ex_ressel <= '0';
				ex_regsel <= "10";
				ex_aluop <= (others=>'0');
				ex_domult <= '0';
				ex_multop <= '0';
				ex_invin2 <= '0';
				ex_valid_reg <= '0';
				ex_valid_res <= '0';
				ex_psw_enable <= '0';
				ex_put <= '0';

				m_write <= '0';
				m_read <= '0';
				m_valid_mem <= '0';
				m_valid_reg <= '0';

				trap <= '0';
				jump <= '0';

				wb_sel <= '0';
				wb_enable <= '0';

				id_bsel <= '0';
				id_beq <= '1';
				id_bne <= '0';

				illegal_op <= '0';
			when "010100" =>         -- SHS
				ex_asel <= '0';
				ex_bsel <= '0';
				ex_ressel <= '0';
				ex_regsel <= "10";
				ex_aluop <= "111";
				ex_domult <= '0';
				ex_multop <= '0';
				ex_invin2 <= '0';
				ex_valid_reg <= '0';
				ex_valid_res <= '1';
				ex_psw_enable <= '0';
				ex_put <= '0';

				m_write <= '0';
				m_read <= '0';
				m_valid_mem <= '1';
				m_valid_reg <= '0';

				trap <= '0';
				jump <= '0';

				wb_sel <= '0';
				wb_enable <= '1';

				id_bsel <= '1';
				id_beq <= '0';
				id_bne <= '0';

				illegal_op <= '0';
			when "010101" =>         -- SHSI
				ex_asel <= '0';
				ex_bsel <= '1';
				ex_ressel <= '0';
				ex_regsel <= "10";
				ex_aluop <= "111";
				ex_domult <= '0';
				ex_multop <= '0';
				ex_invin2 <= '0';
				ex_valid_reg <= '0';
				ex_valid_res <= '1';
				ex_psw_enable <= '0';
				ex_put <= '0';

				m_write <= '0';
				m_read <= '0';
				m_valid_mem <= '1';
				m_valid_reg <= '0';

				trap <= '0';
				jump <= '0';

				wb_sel <= '0';
				wb_enable <= '1';

				id_bsel <= '1';
				id_beq <= '0';
				id_bne <= '0';

				illegal_op <= '0';
--			when "010110" =>         -- -
			when "010111" =>         -- BNE
				ex_asel <= '0';
				ex_bsel <= '1';
				ex_ressel <= '0';
				ex_regsel <= "10";
				ex_aluop <= (others=>'0');
				ex_domult <= '0';
				ex_multop <= '0';
				ex_invin2 <= '0';
				ex_valid_reg <= '0';
				ex_valid_res <= '0';
				ex_psw_enable <= '0';
				ex_put <= '0';

				m_write <= '0';
				m_read <= '0';
				m_valid_mem <= '0';
				m_valid_reg <= '0';

				trap <= '0';
				jump <= '0';

				wb_sel <= '0';
				wb_enable <= '0';

				id_bsel <= '0';
				id_beq <= '0';
				id_bne <= '1';

				illegal_op <= '0';
			when "011000" =>         -- CMP
				ex_asel <= '0';
				ex_bsel <= '0';
				ex_ressel <= '1';
				ex_regsel <= "10";
				ex_aluop <= "010";
				ex_domult <= '0';
				ex_multop <= '0';
				ex_invin2 <= '0';
				ex_valid_reg <= '0';
				ex_valid_res <= '1';
				ex_psw_enable <= '0';
				ex_put <= '0';

				m_write <= '0';
				m_read <= '0';
				m_valid_mem <= '1';
				m_valid_reg <= '0';

				trap <= '0';
				jump <= '0';

				wb_sel <= '0';
				wb_enable <= '1';

				id_bsel <= '1';
				id_beq <= '0';
				id_bne <= '0';

				illegal_op <= '0';
			when "011001" =>         -- CMPI
				ex_asel <= '0';
				ex_bsel <= '1';
				ex_ressel <= '1';
				ex_regsel <= "10";
				ex_aluop <= "010";
				ex_domult <= '0';
				ex_multop <= '0';
				ex_invin2 <= '0';
				ex_valid_reg <= '0';
				ex_valid_res <= '1';
				ex_psw_enable <= '0';
				ex_put <= '0';

				m_write <= '0';
				m_read <= '0';
				m_valid_mem <= '1';
				m_valid_reg <= '0';

				trap <= '0';
				jump <= '0';

				wb_sel <= '0';
				wb_enable <= '1';

				id_bsel <= '1';
				id_beq <= '0';
				id_bne <= '0';

				illegal_op <= '0';
			when "011010" =>         -- CMPX
				ex_asel <= '0';
				ex_bsel <= '1';
				ex_ressel <= '1';
				ex_regsel <= "10";
				ex_aluop <= "010";
				ex_domult <= '0';
				ex_multop <= '0';
				ex_invin2 <= '0';
				ex_valid_reg <= '0';
				ex_valid_res <= '1';
				ex_psw_enable <= '0';
				ex_put <= '0';

				m_write <= '0';
				m_read <= '0';
				m_valid_mem <= '1';
				m_valid_reg <= '0';

				trap <= '0';
				jump <= '0';

				wb_sel <= '0';
				wb_enable <= '1';

				id_bsel <= '1';
				id_beq <= '0';
				id_bne <= '0';

				illegal_op <= '0';
			when "011011" =>         -- CMPD
				ex_asel <= '0';
				ex_bsel <= '1';
				ex_ressel <= '1';
				ex_regsel <= "10";
				ex_aluop <= "010";
				ex_domult <= '0';
				ex_multop <= '0';
				ex_invin2 <= '0';
				ex_valid_reg <= '0';
				ex_valid_res <= '1';
				ex_psw_enable <= '0';
				ex_put <= '0';

				m_write <= '0';
				m_read <= '0';
				m_valid_mem <= '1';
				m_valid_reg <= '0';

				trap <= '0';
				jump <= '0';

				wb_sel <= '0';
				wb_enable <= '1';

				id_bsel <= '1';
				id_beq <= '0';
				id_bne <= '0';

				illegal_op <= '0';
			when "011100" =>         -- JUMP
				ex_asel <= '0';
				ex_bsel <= '0';
				ex_ressel <= '0';
				ex_regsel <= "01";
				ex_aluop <= "001";
				ex_domult <= '0';
				ex_multop <= '0';
				ex_invin2 <= '0';
				ex_valid_reg <= '0';
				ex_valid_res <= '0';
				ex_psw_enable <= '0';
				ex_put <= '0';

				m_write <= '0';
				m_read <= '0';
				m_valid_mem <= '0';
				m_valid_reg <= '0';

				trap <= '0';
				jump <= '1';

				wb_sel <= '1';
				wb_enable <= '1';

				id_bsel <= '1';
				id_beq <= '0';
				id_bne <= '0';

				illegal_op <= '0';
--			when "011101" =>         -- -
			when "011110" =>         -- JUMPX
				ex_asel <= '0';
				ex_bsel <= '1';
				ex_ressel <= '0';
				ex_regsel <= "01";
				ex_aluop <= "001";
				ex_domult <= '0';
				ex_multop <= '0';
				ex_invin2 <= '0';
				ex_valid_reg <= '0';
				ex_valid_res <= '0';
				ex_psw_enable <= '0';
				ex_put <= '0';

				m_write <= '0';
				m_read <= '0';
				m_valid_mem <= '0';
				m_valid_reg <= '0';

				trap <= '0';
				jump <= '1';

				wb_sel <= '1';
				wb_enable <= '1';

				id_bsel <= '1';
				id_beq <= '0';
				id_bne <= '0';

				illegal_op <= '0';
			when "011111" =>         -- JUMPD
				ex_asel <= '0';
				ex_bsel <= '1';
				ex_ressel <= '0';
				ex_regsel <= "01";
				ex_aluop <= "001";
				ex_domult <= '0';
				ex_multop <= '0';
				ex_invin2 <= '0';
				ex_valid_reg <= '0';
				ex_valid_res <= '0';
				ex_psw_enable <= '0';
				ex_put <= '0';

				m_write <= '0';
				m_read <= '0';
				m_valid_mem <= '0';
				m_valid_reg <= '0';

				trap <= '0';
				jump <= '1';

				wb_sel <= '1';
				wb_enable <= '1';

				id_bsel <= '1';
				id_beq <= '0';
				id_bne <= '0';

				illegal_op <= '0';
			when "100000" =>         -- SUB
				ex_asel <= '0';
				ex_bsel <= '0';
				ex_ressel <= '0';
				ex_regsel <= "10";
				ex_aluop <= "010";
				ex_domult <= '0';
				ex_multop <= '0';
				ex_invin2 <= '0';
				ex_valid_reg <= '0';
				ex_valid_res <= '1';
				ex_psw_enable <= '0';
				ex_put <= '0';

				m_write <= '0';
				m_read <= '0';
				m_valid_mem <= '1';
				m_valid_reg <= '0';

				trap <= '0';
				jump <= '0';

				wb_sel <= '0';
				wb_enable <= '1';

				id_bsel <= '1';
				id_beq <= '0';
				id_bne <= '0';

				illegal_op <= '0';
			when "100001" =>         -- GET
				ex_asel <= '1';
				ex_bsel <= '1';
				ex_ressel <= '0';
				ex_regsel <= "11";
				ex_aluop <= "100";
				ex_domult <= '0';
				ex_multop <= '0';
				ex_invin2 <= '1';
				ex_valid_reg <= '1';
				ex_valid_res <= '0';
				ex_psw_enable <= '1';
				ex_put <= '0';

				m_write <= '0';
				m_read <= '0';
				m_valid_mem <= '0';
				m_valid_reg <= '1';

				trap <= '0';
				jump <= '0';

				wb_sel <= '1';
				wb_enable <= '1';

				id_bsel <= '1';
				id_beq <= '0';
				id_bne <= '0';

				illegal_op <= '0';
--			when "100010" =>         -- -
			when "100011" =>         -- LW
				ex_asel <= '0';
				ex_bsel <= '1';
				ex_ressel <= '0';
				ex_regsel <= "10";
				ex_aluop <= "001";
				ex_domult <= '0';
				ex_multop <= '0';
				ex_invin2 <= '0';
				ex_valid_reg <= '0';
				ex_valid_res <= '0';
				ex_psw_enable <= '0';
				ex_put <= '0';

				m_write <= '0';
				m_read <= '1';
				m_valid_mem <= '0';
				m_valid_reg <= '0';

				trap <= '0';
				jump <= '0';

				wb_sel <= '0';
				wb_enable <= '1';

				id_bsel <= '1';
				id_beq <= '0';
				id_bne <= '0';

				illegal_op <= '0';
			when "100100" =>         -- SUBV
				ex_asel <= '0';
				ex_bsel <= '0';
				ex_ressel <= '0';
				ex_regsel <= "10";
				ex_aluop <= "010";
				ex_domult <= '0';
				ex_multop <= '0';
				ex_invin2 <= '0';
				ex_valid_reg <= '0';
				ex_valid_res <= '1';
				ex_psw_enable <= '0';
				ex_put <= '0';

				m_write <= '0';
				m_read <= '0';
				m_valid_mem <= '1';
				m_valid_reg <= '0';

				trap <= '0';
				jump <= '0';

				wb_sel <= '0';
				wb_enable <= '1';

				id_bsel <= '1';
				id_beq <= '0';
				id_bne <= '0';

				illegal_op <= '0';
			when "100101" =>         -- PUT
				ex_asel <= '1';
				ex_bsel <= '1';
				ex_ressel <= '0';
				ex_regsel <= "00";
				ex_aluop <= "011";
				ex_domult <= '0';
				ex_multop <= '0';
				ex_invin2 <= '0';
				ex_valid_reg <= '0';
				ex_valid_res <= '0';
				ex_psw_enable <= '1';
				ex_put <= '1';

				m_write <= '0';
				m_read <= '0';
				m_valid_mem <= '0';
				m_valid_reg <= '0';

				trap <= '0';
				jump <= '0';

				wb_sel <= '0';
				wb_enable <= '0';

				id_bsel <= '1';
				id_beq <= '0';
				id_bne <= '0';

				illegal_op <= '0';
--			when "100110" =>         -- -
			when "100111" =>         -- SW
				ex_asel <= '0';
				ex_bsel <= '1';
				ex_ressel <= '0';
				ex_regsel <= "10";
				ex_aluop <= "001";
				ex_domult <= '0';
				ex_multop <= '0';
				ex_invin2 <= '0';
				ex_valid_reg <= '0';
				ex_valid_res <= '0';
				ex_psw_enable <= '0';
				ex_put <= '0';

				m_write <= '1';
				m_read <= '0';
				m_valid_mem <= '0';
				m_valid_reg <= '0';

				trap <= '0';
				jump <= '0';

				wb_sel <= '0';
				wb_enable <= '0';

				id_bsel <= '0';
				id_beq <= '0';
				id_bne <= '0';

				illegal_op <= '0';
--			when "101000" =>         -- -
			when "101001" =>         -- TRAP
				ex_asel <= '1';
				ex_bsel <= '1';
				ex_ressel <= '0';
				ex_regsel <= "01";
				ex_aluop <= "011";
				ex_domult <= '0';
				ex_multop <= '0';
				ex_invin2 <= '0';
				ex_valid_reg <= '0';
				ex_valid_res <= '0';
				ex_psw_enable <= '1';
				ex_put <= '0';

				m_write <= '0';
				m_read <= '0';
				m_valid_mem <= '0';
				m_valid_reg <= '0';

				trap <= '1';
				jump <= '0';

				wb_sel <= '1';
				wb_enable <= '1';

				id_bsel <= '1';
				id_beq <= '0';
				id_bne <= '0';

				illegal_op <= '0';
--			when "101010" =>         -- -
--			when "101011" =>         -- -
			when "101100" =>         -- AND
				ex_asel <= '0';
				ex_bsel <= '0';
				ex_ressel <= '0';
				ex_regsel <= "10";
				ex_aluop <= "100";
				ex_domult <= '0';
				ex_multop <= '0';
				ex_invin2 <= '0';
				ex_valid_reg <= '0';
				ex_valid_res <= '1';
				ex_psw_enable <= '0';
				ex_put <= '0';

				m_write <= '0';
				m_read <= '0';
				m_valid_mem <= '1';
				m_valid_reg <= '0';

				trap <= '0';
				jump <= '0';

				wb_sel <= '0';
				wb_enable <= '1';

				id_bsel <= '1';
				id_beq <= '0';
				id_bne <= '0';

				illegal_op <= '0';
			when "101101" =>         -- ANDI
				ex_asel <= '0';
				ex_bsel <= '1';
				ex_ressel <= '0';
				ex_regsel <= "10";
				ex_aluop <= "100";
				ex_domult <= '0';
				ex_multop <= '0';
				ex_invin2 <= '0';
				ex_valid_reg <= '0';
				ex_valid_res <= '1';
				ex_psw_enable <= '0';
				ex_put <= '0';

				m_write <= '0';
				m_read <= '0';
				m_valid_mem <= '1';
				m_valid_reg <= '0';

				trap <= '0';
				jump <= '0';

				wb_sel <= '0';
				wb_enable <= '1';

				id_bsel <= '1';
				id_beq <= '0';
				id_bne <= '0';

				illegal_op <= '0';
			when "101110" =>         -- ANDX
				ex_asel <= '0';
				ex_bsel <= '1';
				ex_ressel <= '0';
				ex_regsel <= "10";
				ex_aluop <= "100";
				ex_domult <= '0';
				ex_multop <= '0';
				ex_invin2 <= '0';
				ex_valid_reg <= '0';
				ex_valid_res <= '1';
				ex_psw_enable <= '0';
				ex_put <= '0';

				m_write <= '0';
				m_read <= '0';
				m_valid_mem <= '1';
				m_valid_reg <= '0';

				trap <= '0';
				jump <= '0';

				wb_sel <= '0';
				wb_enable <= '1';

				id_bsel <= '1';
				id_beq <= '0';
				id_bne <= '0';

				illegal_op <= '0';
--			when "101111" =>         -- -
			when "110000" =>         -- OR
				ex_asel <= '0';
				ex_bsel <= '0';
				ex_ressel <= '0';
				ex_regsel <= "10";
				ex_aluop <= "011";
				ex_domult <= '0';
				ex_multop <= '0';
				ex_invin2 <= '0';
				ex_valid_reg <= '0';
				ex_valid_res <= '1';
				ex_psw_enable <= '0';
				ex_put <= '0';

				m_write <= '0';
				m_read <= '0';
				m_valid_mem <= '1';
				m_valid_reg <= '0';

				trap <= '0';
				jump <= '0';

				wb_sel <= '0';
				wb_enable <= '1';

				id_bsel <= '1';
				id_beq <= '0';
				id_bne <= '0';

				illegal_op <= '0';
			when "110001" =>         -- ORI
				ex_asel <= '0';
				ex_bsel <= '1';
				ex_ressel <= '0';
				ex_regsel <= "10";
				ex_aluop <= "011";
				ex_domult <= '0';
				ex_multop <= '0';
				ex_invin2 <= '0';
				ex_valid_reg <= '0';
				ex_valid_res <= '1';
				ex_psw_enable <= '0';
				ex_put <= '0';

				m_write <= '0';
				m_read <= '0';
				m_valid_mem <= '1';
				m_valid_reg <= '0';

				trap <= '0';
				jump <= '0';

				wb_sel <= '0';
				wb_enable <= '1';

				id_bsel <= '1';
				id_beq <= '0';
				id_bne <= '0';

				illegal_op <= '0';
			when "110010" =>         -- ORX
				ex_asel <= '0';
				ex_bsel <= '1';
				ex_ressel <= '0';
				ex_regsel <= "10";
				ex_aluop <= "011";
				ex_domult <= '0';
				ex_multop <= '0';
				ex_invin2 <= '0';
				ex_valid_reg <= '0';
				ex_valid_res <= '1';
				ex_psw_enable <= '0';
				ex_put <= '0';

				m_write <= '0';
				m_read <= '0';
				m_valid_mem <= '1';
				m_valid_reg <= '0';

				trap <= '0';
				jump <= '0';

				wb_sel <= '0';
				wb_enable <= '1';

				id_bsel <= '1';
				id_beq <= '0';
				id_bne <= '0';

				illegal_op <= '0';
--			when "110011" =>         -- -
			when "110100" =>         -- XOR
				ex_asel <= '0';
				ex_bsel <= '0';
				ex_ressel <= '0';
				ex_regsel <= "10";
				ex_aluop <= "101";
				ex_domult <= '0';
				ex_multop <= '0';
				ex_invin2 <= '0';
				ex_valid_reg <= '0';
				ex_valid_res <= '1';
				ex_psw_enable <= '0';
				ex_put <= '0';

				m_write <= '0';
				m_read <= '0';
				m_valid_mem <= '1';
				m_valid_reg <= '0';

				trap <= '0';
				jump <= '0';

				wb_sel <= '0';
				wb_enable <= '1';

				id_bsel <= '1';
				id_beq <= '0';
				id_bne <= '0';

				illegal_op <= '0';
			when "110101" =>         -- XORI
				ex_asel <= '0';
				ex_bsel <= '1';
				ex_ressel <= '0';
				ex_regsel <= "10";
				ex_aluop <= "101";
				ex_domult <= '0';
				ex_multop <= '0';
				ex_invin2 <= '0';
				ex_valid_reg <= '0';
				ex_valid_res <= '1';
				ex_psw_enable <= '0';
				ex_put <= '0';

				m_write <= '0';
				m_read <= '0';
				m_valid_mem <= '1';
				m_valid_reg <= '0';

				trap <= '0';
				jump <= '0';

				wb_sel <= '0';
				wb_enable <= '1';

				id_bsel <= '1';
				id_beq <= '0';
				id_bne <= '0';

				illegal_op <= '0';
			when "110110" =>         -- XORX
				ex_asel <= '0';
				ex_bsel <= '1';
				ex_ressel <= '0';
				ex_regsel <= "10";
				ex_aluop <= "101";
				ex_domult <= '0';
				ex_multop <= '0';
				ex_invin2 <= '0';
				ex_valid_reg <= '0';
				ex_valid_res <= '1';
				ex_psw_enable <= '0';
				ex_put <= '0';

				m_write <= '0';
				m_read <= '0';
				m_valid_mem <= '1';
				m_valid_reg <= '0';

				trap <= '0';
				jump <= '0';

				wb_sel <= '0';
				wb_enable <= '1';

				id_bsel <= '1';
				id_beq <= '0';
				id_bne <= '0';

				illegal_op <= '0';
--			when "110111" =>         -- -
			when "111000" =>         -- SET
				ex_asel <= '1';
				ex_bsel <= '0';
				ex_ressel <= '0';
				ex_regsel <= "00";
				ex_aluop <= "011";
				ex_domult <= '0';
				ex_multop <= '0';
				ex_invin2 <= '0';
				ex_valid_reg <= '1';
				ex_valid_res <= '0';
				ex_psw_enable <= '1';
				ex_put <= '0';

				m_write <= '0';
				m_read <= '0';
				m_valid_mem <= '0';
				m_valid_reg <= '1';

				trap <= '0';
				jump <= '0';

				wb_sel <= '1';
				wb_enable <= '1';

				id_bsel <= '1';
				id_beq <= '0';
				id_bne <= '0';

				illegal_op <= '0';
			when "111001" =>         -- SETI
				ex_asel <= '1';
				ex_bsel <= '1';
				ex_ressel <= '0';
				ex_regsel <= "00";
				ex_aluop <= "011";
				ex_domult <= '0';
				ex_multop <= '0';
				ex_invin2 <= '0';
				ex_valid_reg <= '1';
				ex_valid_res <= '0';
				ex_psw_enable <= '1';
				ex_put <= '0';

				m_write <= '0';
				m_read <= '0';
				m_valid_mem <= '0';
				m_valid_reg <= '1';

				trap <= '0';
				jump <= '0';

				wb_sel <= '1';
				wb_enable <= '1';

				id_bsel <= '1';
				id_beq <= '0';
				id_bne <= '0';

				illegal_op <= '0';
			when "111010" =>         -- SETX
				ex_asel <= '1';
				ex_bsel <= '1';
				ex_ressel <= '0';
				ex_regsel <= "00";
				ex_aluop <= "011";
				ex_domult <= '0';
				ex_multop <= '0';
				ex_invin2 <= '0';
				ex_valid_reg <= '1';
				ex_valid_res <= '0';
				ex_psw_enable <= '1';
				ex_put <= '0';

				m_write <= '0';
				m_read <= '0';
				m_valid_mem <= '0';
				m_valid_reg <= '1';

				trap <= '0';
				jump <= '0';

				wb_sel <= '1';
				wb_enable <= '1';

				id_bsel <= '1';
				id_beq <= '0';
				id_bne <= '0';

				illegal_op <= '0';
--			when "111011" =>         -- -
			when "111100" =>         -- RESET
				ex_asel <= '1';
				ex_bsel <= '0';
				ex_ressel <= '0';
				ex_regsel <= "00";
				ex_aluop <= "100";
				ex_domult <= '0';
				ex_multop <= '0';
				ex_invin2 <= '1';
				ex_valid_reg <= '1';
				ex_valid_res <= '0';
				ex_psw_enable <= '1';
				ex_put <= '0';

				m_write <= '0';
				m_read <= '0';
				m_valid_mem <= '0';
				m_valid_reg <= '1';

				trap <= '0';
				jump <= '0';

				wb_sel <= '1';
				wb_enable <= '1';

				id_bsel <= '1';
				id_beq <= '0';
				id_bne <= '0';

				illegal_op <= '0';
			when "111101" =>         -- RESETI
				ex_asel <= '1';
				ex_bsel <= '1';
				ex_ressel <= '0';
				ex_regsel <= "00";
				ex_aluop <= "100";
				ex_domult <= '0';
				ex_multop <= '0';
				ex_invin2 <= '1';
				ex_valid_reg <= '1';
				ex_valid_res <= '0';
				ex_psw_enable <= '1';
				ex_put <= '0';

				m_write <= '0';
				m_read <= '0';
				m_valid_mem <= '0';
				m_valid_reg <= '1';

				trap <= '0';
				jump <= '0';

				wb_sel <= '1';
				wb_enable <= '1';

				id_bsel <= '1';
				id_beq <= '0';
				id_bne <= '0';

				illegal_op <= '0';
			when "111110" =>         -- RESETX
				ex_asel <= '1';
				ex_bsel <= '1';
				ex_ressel <= '0';
				ex_regsel <= "00";
				ex_aluop <= "100";
				ex_domult <= '0';
				ex_multop <= '0';
				ex_invin2 <= '1';
				ex_valid_reg <= '1';
				ex_valid_res <= '0';
				ex_psw_enable <= '1';
				ex_put <= '0';

				m_write <= '0';
				m_read <= '0';
				m_valid_mem <= '0';
				m_valid_reg <= '1';

				trap <= '0';
				jump <= '0';

				wb_sel <= '1';
				wb_enable <= '1';

				id_bsel <= '1';
				id_beq <= '0';
				id_bne <= '0';

				illegal_op <= '0';
--			when "111111" =>         -- -
			when others =>             -- Undefined signals, etc.
				ex_asel <= '0';
				ex_bsel <= '1';
				ex_ressel <= '0';
				ex_regsel <= "01";
				ex_aluop <= (others=>'0');
				ex_domult <= '0';
				ex_multop <= '0';
				ex_invin2 <= '0';
				ex_valid_reg <= '0';
				ex_valid_res <= '0';
				ex_psw_enable <= '0';
				ex_put <= '0';

				m_write <= '0';
				m_read <= '0';
				m_valid_mem <= '0';
				m_valid_reg <= '0';

				trap <= '1';
				jump <= '0';

				wb_sel <= '1';
				wb_enable <= '1';

				id_bsel <= '1';
				id_beq <= '0';
				id_bne <= '0';

				illegal_op <= '1';
		end case;
	end process;
end;
