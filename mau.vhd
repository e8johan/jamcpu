---------------------------------------------------------
-- JAM CPU
-- Memory access unit
--
-- License: LGPL v2+ (see the file LICENSE)
-- Copyright © 2002:
-- Anders Lindström, Johan E. Thelin, Michael Nordseth
---------------------------------------------------------

-- This is free software; you can redistribute it and/or
-- modify it under the terms of the GNU Library General Public
-- License as published by the Free Software Foundation; either
-- version 2 of the License, or (at your option) any later version.

-- Note: we could use the inverted CLK signal as DM_WRITE to avoid
-- two cycle write

library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_unsigned.ALL;

Entity mau is 
	port (	
	---- Signalas from/to CPU/CONTROLL ----------------------------
			CLK:		in std_logic;
			CTRL_R:		in std_logic;                       -- Read Mem
			CTRL_W:		in std_logic;                       -- Write Mem
			RESET:		in std_logic;
			STALL:		out std_logic;                      -- Used to stall one cycle on write
			IN_ADDR:	in std_logic_vector(31 downto 0);   -- Adress Buss from CPU
			IN_DATA:	in std_logic_vector(31 downto 0);   -- Data Buss from CPU
			OUT_DATA:	out std_logic_vector(31 downto 0);  -- Data Buss to CPU
	---------------------------------------------------------------		
	---- Signals from/to MEM---------------------------------------
			DM_CS:		out std_logic_vector(7 downto 0);   -- Chip Select                      Low-active.
			DM_OE:		out std_logic;                      -- Output Enable                    Low-active.
			DM_WRITE:	out std_logic;                      -- Write Data, Mem (data -> HighZ). Low-active.
			DM_ADDR:	out std_logic_vector(16 downto 0);  -- Address Buss to mem
			DM_DATA:	inout std_logic_vector(63 downto 0) -- Mem Data Buss (Bidirectional)
	---------------------------------------------------------------
	);	
end;

Architecture behav of mau is	
	signal	intern_stall:	std_logic;
    signal	GO:			std_logic;
	
begin
    DM_DATA <=
		(Others => 'Z') when intern_stall = '0' else
		"--------------------------------" & IN_DATA when IN_ADDR(0) = '0' else
		IN_DATA & "--------------------------------";
	
	DM_OE <= '0';
	DM_CS <= 
		(Others => '0') when CTRL_W = '0' else
		"11110000" when IN_ADDR(0) = '0' else
		"00001111";
	
	DM_ADDR <= IN_ADDR(17 downto 1);
	
	intern_stall <= 
		'1' when ( (CTRL_W = '1') and (GO = '0') ) else
		'0';
	
	STALL <= intern_stall;
		
	OUT_DATA <= 
		(Others => '-') when CTRL_R = '0' else
		DM_DATA(31 downto 0) when (IN_ADDR(0) = '0') else
		DM_DATA(63 downto 32);
	
	DM_WRITE <= not intern_stall;
	
	process(CLK)
	begin
		if CLK'event and CLK = '1' then
			if intern_stall = '1' then 
				GO <= '1';
			else
				GO <= '0';
			end if;
			
			if RESET = '1' then
				GO <= '0';
			end if;
		end if;
	end process; 
end;
