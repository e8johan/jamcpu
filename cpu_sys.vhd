---------------------------------------------------------
-- JAM CPU
-- System shell wrapper
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
library work;
use ieee.std_logic_1164.all;
use IEEE.std_logic_unsigned."+";
use IEEE.std_logic_unsigned."-";

entity cpu_sys is
	port (
		clk      : in std_logic;
		
		--reset key
		k_en     : out std_logic;
		k_in     : in std_logic;

		--IM
		im_cs    : out std_logic_vector(7 downto 0);    
		im_oe    : out std_logic;                       
		im_write : out std_logic;  
		im_adr   : out std_logic_vector( 16 downto 0 ); 
		im_data  : inout std_logic_vector( 63 downto 0 );
		
		--DM
		dm_cs    : out std_logic_vector(7 downto 0);   
		dm_oe    : out std_logic;                      
		dm_write : out std_logic;                      
		dm_adr   : out std_logic_vector( 16 downto 0 );
		dm_data  : inout std_logic_vector( 63 downto 0 ));
end;

architecture rev1 of cpu_sys is
	component cpu
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
	end component;
	
	for cpu_core : cpu use entity work.cpu(rev1);
	
	signal slow_clock  : std_logic;
	signal reset       : std_logic;

	signal inport      : std_logic_vector( 31 downto 0 );
	signal outport     : std_logic_vector( 31 downto 0 );
	
	signal synctrap    : std_logic;
	signal curpsw11_31 : std_logic_vector(31 downto 11);
	signal curpsw0_7   : std_logic_vector(7 downto 0);
	signal newpsw11_31 : std_logic_vector(31 downto 11);
	signal newpsw0_7   : std_logic_vector(7 downto 0);
	
begin
	--handle reset key on keypad
	k_en <= '1';
	reset <= k_in;

	cpu_core : cpu port map(
		--clock and reset
		clk => slow_clock, 
		reset => reset,

		--in and out ports
		inport => inport,
		outport => outport,

		--IO interface
		synctrap => synctrap,
		curpsw11_31 => curpsw11_31,
		curpsw0_7 => curpsw0_7,
		newpsw11_31 => newpsw11_31,
		newpsw0_7 => newpsw0_7,

		--IM
		im_cs => im_cs,
		im_oe => im_oe,
		im_wri => im_write,
		im_adr => im_adr,
		im_dat => im_data,

		--DM
		dm_cs => dm_cs,
		dm_oe => dm_oe,
		dm_wri => dm_write,
		dm_adr => dm_adr,
		dm_dat => dm_data);
		
	
	--fake inport
	inport <= "00000000000000000000000000101010";
	
	--slow down clock
	process(clk)
		variable clockc : std_logic_vector(1 downto 0) := "00";
	begin
		if clk'event and clk='1' then
			clockc := clockc + '1';
		end if;

		slow_clock <= clockc(1);
	end process;
	
	--STU timer
	newpsw0_7 <= curpsw0_7;
	
	process(slow_clock)
		variable cc : std_logic_vector( 25 downto 0 );
	begin
		if slow_clock'event and slow_clock='1' then
			if reset = '1' then
				cc := "00000000000000000000000001";
				synctrap <= '0';
				newpsw11_31 <= curpsw11_31;
			else
				if cc = "00000000000000000000000000" then
					cc(25 downto 10) := curpsw11_31(31 downto 16);
					if curpsw11_31(15) = '0' then
						synctrap <= '1';
						newpsw11_31 <= curpsw11_31;
						newpsw11_31(15) <= '1';
					else
						synctrap <= '0';
						newpsw11_31 <= curpsw11_31;
					end if;
				else
					synctrap <= '0';
					newpsw11_31 <= curpsw11_31;
				end if;
			
				cc := cc - '1';
			end if;
		end if;	
	end process;
	
end;
