-- ROM package

library ieee;
use ieee.std_logic_1164.all;

package mul2_rom is
	constant w:integer:= 32;  --width of ROM
	constant l:integer:= 17;  --lenght of ROM
 	
	subtype rom_word is std_logic_vector(w-1 downto 0);
	type rom_table is array (0 to l-1) of rom_word;

	constant rom_image:rom_table:=rom_table'(
		"00000000000000000000000000000000",
		"00000100101000000000000000000100",
		"00000100001000000000000000001000",
		"00100100010000010000000000001101",
		"10011100010000010000000000000000",
		"10000000001000010010100000000000",
		"01011100001000001111111111111101",
		"00000000000000000000000000000000",
		"00000100001000000000000000001000",
		"10001100010000010000000000000000",
		"00100100011000100000000000000101",
		"00100100100000110000000000000011",
		"10000000001000010010100000000000",
		"01011100001000001111111111111100",
		"00000000000000000000000000000000",
		"01001100000000000000000000000000",
		"00000000000000000000000000000000");
end;