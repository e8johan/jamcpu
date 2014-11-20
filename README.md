JAM CPU core
=============

This is a *very* simple 32bit RISC pipeline implemented in VHDL.

Complete documentation can be found in doc/.
An ucf file for Virtex can be found in impl/ (cpu_sys should be top entity).
A simulation script for ModelSim can be found in modelsim/ (simcpu should be
top entity).
Rom images for simcpu can be found in roms/.

Use the simcpu entity for simulation (with ROM images) and the cpu_sys entity 
for implementation. Cpu_sys is a wrapper for the CPU core to adapt it to the
actual hardware. The memory system uses a 64bit wide layout with 32bit words
right now (our lab system was a bit special) so you might want to change the
MAU entity to suite your layout.


Possible (student) projects:
- Complete the 1 cycle write (easy, see mau.vhd)
- Make a 1 cycle mult (fpga vendors often have optimal code available)
- Make it faster
  - Shorten the critical path
  - Redesign with smarter stall/forwarding handling
- Fix the "branch logic stall" somehow
- Make a real MAU with memory management (with TLB) and caching.
- Extend the CPU core for memory management in MAU (offset registers...)
- Add kernel/user modes in CPU core



-------------------------------------------------------------------------------
This is free software; you can redistribute it and/or modify it under 
the terms of the GNU Lesser General Public License as published by the Free 
Software Foundation; either version 2.1 of the License, or (at your option) any 
later version. See LICENSE for more information.


(C) 2002 
Anders Lindström - e8cal1@etek.chalmers.se or cal@swipnet.se<br />
Michael Nordseth - e8mn@etek.chalmers.se<br />
Johan E. Thelin  - e8johan using gmail<br />
