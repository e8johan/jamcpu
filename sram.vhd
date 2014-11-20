---------------------------------------------------------
-- JAM CPU
-- Very simple SRAM model for simulation
--
-- Copyright © 2002:
-- Anders Lindström, Johan E. Thelin, Michael Nordseth
---------------------------------------------------------

-- This is free software; you can redistribute it and/or
-- modify it under the terms of the GNU Library General Public
-- License as published by the Free Software Foundation; either
-- version 2 of the License, or (at your option) any later version.

Library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.std_logic_unsigned.all;
use IEEE.std_logic_arith.all;


entity sram is

  generic (rom_data_file_name: string := "sram.dat");

  port (ncs	     : in std_logic_vector( 3 downto 0 );        -- not chip select
        addr     : in std_logic_vector( 16 downto 0 );
        data     : inout std_logic_vector( 31 downto 0 );
        nwe      : in std_logic;        -- not write enable
        noe      : in std_logic        -- not output enable
       );

end sram;

architecture behav of sram is
begin

   mem: process

      constant low_address: natural := 0;
      constant high_address: natural := 131072;  -- 128K SRAM

      subtype word is std_logic_vector(31 downto 0 );

      type memory_array is
         array (natural range low_address to high_address) of word;

      variable mem: memory_array;
      variable address : natural;
      
   begin
      data <= (others => 'Z') ;
      --
      --
      -- process memory cycles
      --
      loop
         --
         -- wait for chip-select,
         --
         if (ncs(0) = '0') then

            -- decode address
            address := conv_integer( addr );
            -- 
            if nwe = '0' then
               --- write cycle
               mem( address ) := data(31 downto 0);
               data <= (others => 'Z');
            elsif nwe = '1' then
               -- read cycle
               if noe = '0' then
                  data <= mem( address );
               else
                  data <= (others => 'Z');
               end if;
            else
               data <= (others => 'Z');
            end if;
         else
            --
            -- Chip not selected, disable output
            --
            data <= (others => 'Z');
         end if;

         wait on ncs, nwe, noe, addr, data;
      end loop;
   end process;


end behav;
