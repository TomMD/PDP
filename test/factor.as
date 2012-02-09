*0000                   /FIND THE FACTORS OF N
        DCA N           /WRITE N
        TAD N           /BRING BACK N
LOOP,   TAD MONE        /SUB 1
        CMA IAC         /MAKE -F
        DCA F           /WRITE F
        TAD N           /BRING BACK N
LOOP1,  CLL             /CLEAR LINK FOR TEST
        TAD F           /SUBTRACT F
        SNL             /SKIP ON NON ZERO LINK
        JMP LOOP2       
        SZA
        JMP LOOP1
        TAD F
        CMA IAC
        HLT             /HALT WITH F IN ACCUMULATOR
LOOP2,  CLA            
        TAD F           /ADD TEMP
        CMA IAC         /MAKE +F
        JMP LOOP        /JUMP START
MONE,   7777            /MINUS ONE
N,      0000            /RESERVE FOR N
F,      0000            /RESERVE FOR F
$
