---------------------------------------------------------
-- JAM CPU
-- Simulation wrapper
--
-- License: LGPL v2+ (see the file LICENSE)
-- Copyright © 2002:
-- Anders Lindström, Johan E. Thelin, Michael Nordseth
---------------------------------------------------------

-- This is free software; you can redistribute it and/or
-- modify it under the terms of the GNU Library General Public
-- License as published by the Free Software Foundation; either
-- version 2 of the License, or (at your option) any later version

library ieee;
library work;
use ieee.std_logic_1164.all;
use IEEE.std_logic_unsigned."+";
use IEEE.std_logic_unsigned.conv_integer;

-- Include your ROM image here
use work.p1_rom.all;

entity simcpu is
	port (
		clk, clk2: in std_logic;
		reset : in std_logic;
		synctrap : in std_logic );
end;

architecture rev1 of simcpu is
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
	
	component sram 
		port (
			ncs	     : in std_logic_vector( 3 downto 0 );		-- not chip select
	        addr     : in std_logic_vector( 16 downto 0 );
	        data     : inout std_logic_vector( 31 downto 0 );
	        nwe      : in std_logic;        					-- not write enable
	        noe      : in std_logic );        					-- not output enable
	end component;

	for U_cpu : cpu use entity work.cpu(rev1);
	for U_dm1 : sram use entity work.sram(behav);
	for U_dm2 : sram use entity work.sram(behav);
	
	signal dm_cs    : std_logic_vector (7 downto 0);
	signal dm_oe    : std_logic;
	signal dm_write : std_logic;
	signal dm_adr   : std_logic_vector( 16 downto 0 );
	signal dm_data  : std_logic_vector (63 downto 0);
	
	signal im_cs    : std_logic_vector (7 downto 0);
	signal im_oe    : std_logic;
	signal im_write : std_logic;
	signal im_adr   : std_logic_vector( 16 downto 0 );
	signal im_data  : std_logic_vector (63 downto 0);
	
	signal inport   : std_logic_vector( 31 downto 0 );
	signal outport  : std_logic_vector( 31 downto 0 );
	
--	signal synctrap    : std_logic;
	signal curpsw11_31 : std_logic_vector(31 downto 11);
	signal curpsw0_7   : std_logic_vector(7 downto 0);
	signal newpsw11_31 : std_logic_vector(31 downto 11);
	signal newpsw0_7   : std_logic_vector(7 downto 0);
begin

	-- STU and I/O
--	synctrap <= '0';
	newpsw11_31 <= curpsw11_31;
	newpsw0_7 <= curpsw0_7;
	

	-- Data memory

	U_dm1 : sram port map(
		ncs  => dm_cs(3 downto 0),
		noe  => dm_oe,
		nwe  => dm_write,
		addr => dm_adr,
		data => dm_data(31 downto 0));
	U_dm2 : sram port map(
		ncs  => dm_cs(7 downto 4),
		noe  => dm_oe,
		nwe  => dm_write,
		addr => dm_adr,
		data => dm_data(63 downto 32));

	-- CPU
	
	U_cpu : cpu port map(
		--clock and reset
		clk => clk, 
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
		
		
	-- simulate IM SRAM from rom image (32bit wide => 64bit wide)
	process
		variable address : natural;
	begin
		im_data <= (others => 'Z') ;
		--
		--
		-- process memory cycles
		--
		loop
			--
			-- wait for chip-select,
			--
			if (im_cs(0) = '0') then

				-- decode address
				address := conv_integer( im_adr(16 downto 0) & '0' );
				-- 
				if im_write = '0' then

				elsif im_write = '1' then
					-- read cycle
					if im_oe = '0' then
						im_data(31 downto 0) <=  rom_image(address);
					else
						im_data <= (others => 'Z');
					end if;

				else
					im_data <= (others => 'Z');
				end if;
			else
				--
				-- Chip not selected, disable output
				--
				im_data <= (others => 'Z');
			end if;

			wait on im_cs, im_write, im_oe, im_adr, im_data;
		end loop;
	end process;
	
	process
		variable address : natural;
	begin
		im_data <= (others => 'Z') ;
		--
		--
		-- process memory cycles
		--
		loop
			--
			-- wait for chip-select,
			--
			if (im_cs(7) = '0') then

				-- decode address
				address := conv_integer( im_adr(16 downto 0) & '0');
				-- 
				if im_write = '0' then

				elsif im_write = '1' then
					-- read cycle
					if im_oe = '0' then
						im_data(63 downto 32) <=  rom_image(address +1);
					else
						im_data <= (others => 'Z');
					end if;

				else
					im_data <= (others => 'Z');
				end if;
			else
				--
				-- Chip not selected, disable output
				--
				im_data <= (others => 'Z');
			end if;

			wait on im_cs, im_write, im_oe, im_adr, im_data;
		end loop;
	end process;
end;
