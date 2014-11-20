---------------------------------------------------------
-- JAM CPU
-- Register bank
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
use ieee.std_logic_unsigned.conv_integer;

entity regs is
	port (
		ra, rb, rw : in std_logic_vector (4 downto 0);
		value_w : in std_logic_vector (31 downto 0);
		clk : in std_logic;
		value_a, value_b : out std_logic_vector (31 downto 0) );
end;

architecture rev1 of regs is
	type mem_array is array (31 downto 0) of std_logic_vector (31 downto 0);
	signal mem : mem_array := (others => (others => '0'));
begin
	value_a <= 
		(others=>'0') when ra = "00000" else
		value_w when ra = rw else
		mem(conv_integer(ra));

	value_b <= 
		(others=>'0') when rb = "00000" else
		value_w when rb = rw else
		mem(conv_integer(rb));

	process (clk)
	begin
		if clk'event and clk = '1' then
			if rw /= "00000" then
				mem(conv_integer(rw)) <= value_w;
			end if;
		end if;
	end process;
end;
