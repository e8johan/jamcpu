---------------------------------------------------------
-- JAM CPU
-- Integer Unit
--
-- License: LGPL v2+ (see the file LICENSE)
-- Copyright © 2002:
-- Anders Lindström, Johan E. Thelin, Michael Nordseth
---------------------------------------------------------

-- This is free software; you can redistribute it and/or
-- modify it under the terms of the GNU Library General Public
-- License as published by the Free Software Foundation; either
-- version 2 of the License, or (at your option) any later version.


--This integer unit uses a basic ALU with shift 
--and adds a multicycle multiplier.
--The multiplier uses Booth's algorithm. 

library ieee;
use ieee.std_logic_1164.all;
use IEEE.std_logic_unsigned."+";

entity IU is
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
		mc         : out std_logic);                    -- Goes high during the last cycle of multicycle ops
end;

architecture rev1 of IU is
	component ALU
		port(
			op       : in std_logic_vector(2 downto 0);    -- ALU operation
			inv_in2  : in std_logic;                       -- Invert operator in2
			in1, in2 : in std_logic_vector(31 downto 0);   -- ALU input
			ovf      : out std_logic;                      -- ALU overflow
			alu_out  : out std_logic_vector(31 downto 0)); -- ALU result
	end component;

	for alu_unit : ALU use entity work.ALU(rev2);

	signal a, b, alu_data : std_logic_vector(31 downto 0);
	signal b_reg          : std_logic_vector(31 downto 0);
	signal op             : std_logic_vector(2 downto 0);
	signal running        : std_logic;
	signal finished       : std_logic;
	signal from_latch     : std_logic_vector(64 downto 0);
begin
	alu_unit : ALU port map(op, inv_in2, a, b, ovf, alu_data);

	result <= 
		alu_data when do_mult = '0' else
		from_latch(32 downto 1) when mult_op = '0' else
		from_latch(64 downto 33);
	
	a <= 
		a_in when do_mult = '0' else
		from_latch(64 downto 33);
		
	b <= 
		b_in when do_mult = '0' else
		b_reg;

	op <= 
		alu_op when do_mult = '0' else
		"001" when from_latch(1 downto 0) = "01" else
		"010" when from_latch(1 downto 0) = "10" else
		"000";
		

	--65bit register + 32 bit buffer for b_in
	process(clk)
		constant zero32: std_logic_vector(31 downto 0) := (others => '0');
	begin
		if clk'event and clk='1' then
			if do_mult = '1' and running = '0' then
				from_latch <= zero32 & a_in & '0';
				b_reg <= b_in;
			elsif do_mult ='1' then
				from_latch <= alu_data(31) & alu_data & from_latch(32 downto 1);
			end if;
		end if;
	end process;


	process(clk)
		variable loop_nr: std_logic_vector(5 downto 0);
	begin
		if clk'event and clk='1' then
			if reset='1' then
				running <= '0';
				finished <= '0';
				mc <= '0';	
			else
				if do_mult = '1' then
					--multicycle mult
					if running = '0' then
						--setup
						running <= '1';
						loop_nr := (others => '0');
					end if;

					if loop_nr(5) = '1' then --last cycle?
						finished <= '1';
						loop_nr := (others => '0');
						mc <= '1';
					else
						mc <= '0';
						loop_nr := loop_nr + '1';
					end if;

					if finished = '1' then
						finished <= '0';
						running <= '0';
					end if;
				else
					mc <= '0';
					running <= '0';
				end if;
			end if;
		end if;
	end process;
end;
