---------------------------------------------------------
-- JAM CPU
-- Immediate operand extender
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

entity imm_ext is
    port (
        mode: in STD_LOGIC_VECTOR (1 downto 0);
        op5: in STD_LOGIC;
        imm: in STD_LOGIC_VECTOR (15 downto 0);
        r: out STD_LOGIC_VECTOR (31 downto 0) );
end;

architecture rev1 of imm_ext is
	signal im, ximm, disp : std_logic_vector(31 downto 0);
begin
			-- IMM (01)
	im(31 downto 16) <= (others=>(not op5) and imm(15));
	im(15 downto 0) <= imm;
		 	-- XIMM(10)
	ximm(31 downto 16) <= imm;
	ximm(15 downto 0) <= (others=>'0');
			-- DISP(11)	
	disp(31 downto 18) <= (others=>imm(15));
	disp(17 downto 2) <= imm;
	disp(1 downto 0) <= (others=>'0');
	
	r <= im when mode = "01" else
	     ximm when mode = "10" else
	     disp;
end;
