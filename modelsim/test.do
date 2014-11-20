# Test macro

vsim simcpu
view wave
add wave -radix decimal sim:/simcpu/*
add wave -radix decimal sim:/simcpu/u_cpu/* 

force clk 0 0, 1 50ns -repeat 100ns
force reset 1 0, 0 60ns
force synctrap 0 0

run 100000ns;
