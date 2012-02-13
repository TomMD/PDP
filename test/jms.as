/ Program: jms.as
/ Desc:    This program jumps to a subroutine

*0200
Main,   cla cll
        tad A
        jms Subr
        dca C
        hlt

Subr,   0
        tad B
        jmp i Subr

*0250
A,      2
B,      3
C,      0

$