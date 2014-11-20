---------------------------------------------------------
-- JAM CPU core
-- Simple ALU with shift
--
-- License: LGPL v2+ (see the file LICENSE)
-- Copyright © 2002:
-- Anders Lindström, Johan E. Thelin, Michael Nordseth
---------------------------------------------------------

-- This is free software; you can redistribute it and/or
-- modify it under the terms of the GNU Library General Public
-- License as published by the Free Software Foundation; either
-- version 2 of the License, or (at your option) any later version.

library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.std_logic_unsigned."+";


entity ALU is
	port(
		op: in std_logic_vector(2 downto 0);        -- ALU operation
		inv_in2 : in std_logic;                     -- Invert operator in2
		in1, in2: in std_logic_vector(31 downto 0); -- ALU input
		ovf: out std_logic;                         -- ALU overflow
		alu_out: out std_logic_vector(31 downto 0));-- ALU result
end;

architecture rev2 of ALU is
	--ALU OPS
	constant ALU_NOP: std_logic_vector(2 downto 0) := "000"; --pass in1
	constant ALU_ADD: std_logic_vector(2 downto 0) := "001";
	constant ALU_SUB: std_logic_vector(2 downto 0) := "010";
	constant ALU_OR:  std_logic_vector(2 downto 0) := "011";
	constant ALU_AND: std_logic_vector(2 downto 0) := "100";
	constant ALU_XOR: std_logic_vector(2 downto 0) := "101";
	constant ALU_SHZ: std_logic_vector(2 downto 0) := "110"; --shift and fill with zero
	constant ALU_SHS: std_logic_vector(2 downto 0) := "111"; --shift and fill with sign

	constant zero32: std_logic_vector(31 downto 0) := (others => '0');
begin
	process(op, in1, in2, inv_in2)
		variable shiftin: std_logic_vector(63 downto 0);
  		variable shiftcnt: std_logic_vector(4 downto 0);
		variable result: std_logic_vector(31 downto 0);
		
		variable b_in: std_logic_vector(31 downto 0);
		variable cin: std_logic;
	begin
		if (inv_in2 = '1') or (op = ALU_SUB) then
				b_in := not in2;
				cin := '1';
		else
				b_in := in2;
				cin := '0';
		end if;

		if op = ALU_NOP then
			ovf <= '0';
			result := in1;
		elsif ((op = ALU_ADD) or (op = ALU_SUB)) then
			result := in1 + b_in + cin;
			
			--overflow
			if op = ALU_ADD then
				ovf <= (in1(31) and in2(31) and not result(31)) or (result(31) and (not in1(31)) and (not in2(31)));
			else
				ovf <= (in1(31) and (not in2(31)) and not result(31)) or (result(31) and (not in1(31)) and in2(31));
			end if;
		elsif op = ALU_OR then
			result := in1 or b_in;
			ovf <= '0';
		elsif op = ALU_AND then
			result := in1 and b_in;
			ovf <= '0';
		elsif op = ALU_XOR then
			result := in1 xor b_in;
			ovf <= '0';
		else 
			--shifter
			
			if in2(31) = '1' then
				shiftcnt := (not in2(4 downto 0)) + 1; --right	
				shiftin(31 downto 0) := in1;
				if ((op = ALU_SHZ) or (in1(31) = '0')) then
					shiftin(63 downto 32) := (others => '0');
				else
					shiftin(63 downto 32) := (others => '1');
				end if;
			else
				shiftcnt := not in2(4 downto 0);       --left
				if ((op = ALU_SHZ) or (in1(31) = '0')) then
					shiftin(31 downto 0) := (others => '0');
				else
					shiftin(31 downto 0) := (others => '1');
				end if;
				shiftin(63 downto 31) := '0' & in1;
			end if;
			
			if shiftcnt(4) = '1' then
				shiftin(47 downto 0) := shiftin(63 downto 16);
			end if;
			if shiftcnt(3) = '1' then
				shiftin(39 downto 0) := shiftin(47 downto 8);
			end if;
			if shiftcnt(2) = '1' then
				shiftin(35 downto 0) := shiftin(39 downto 4);
			end if;
			if shiftcnt(1) = '1' then
				shiftin(33 downto 0) := shiftin(35 downto 2);
			end if;
			if shiftcnt(0) = '1' then
				shiftin(31 downto 0) := shiftin(32 downto 1);
			end if;
				
			result := shiftin(31 downto 0);
			ovf <= '0';
		end if;
		
		alu_out <= result;
	end process;
end;
