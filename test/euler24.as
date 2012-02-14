/ PROJECTEULER.NET PROBLEM 24
/
/ A PERMUTATION IS AN ORDERED ARRANGEMENT OF OBJECTS.
/ FOR EXAMPLE, 3124 IS ONE POSSIBLE PERMUTATION OF
/ THE DIGITS 1, 2, 3 AND 4. IF ALL OF THE PERMUTATIONS
/ ARE LISTED NUMERICALLY OR ALPHABETICALLY,
/ WE CALL IT LEXICOGRAPHIC ORDER.
/ THE LEXICOGRAPHIC PERMUTATIONS OF 0, 1 AND 2 ARE:
/
/     012   021   102   120   201   210
/
/ WHAT IS THE MILLIONTH LEXICOGRAPHIC PERMUTATION
/ OF THE DIGITS 0, 1, 2, 3, 4, 5, 6, 7, 8 AND 9?
/
/ PAL/8 DIGITAL EQUIPMENT CORPORATION (DEC) PDP/8
/ ASSEMBLY LANGUAGE SOLUTION BY CHUCK ROLKE, 2-NOV-2011
/
        *10
CHKPTR, 0

        *20
                                / SOLUTION IS CONSTRUCTED HERE
D0,     0
D1,     0
D2,     0
D3,     0
D4,     0
D5,     0
D6,     0
D7,     0
D8,     0
D9,     0
                                / COUNTERS
C0,     0
C1,     0
C2,     0
C3,     0
C4,     0
C5,     0
C6,     0
C7,     0
C8,     0
C9,     0
                                / SUBR POINTERS
PTR0,   S0
PTR1,   S1
PTR2,   S2
PTR3,   S3
PTR4,   S4
PTR5,   S5
PTR6,   S6
PTR7,   S7
PTR8,   S8
PTR9,   S9
                                / SEARCH COUNT

CLO,    7700                    / -1000000  LOW 12 BITS
CHI,    7777                    / -1000000 HIGH 12 BITS

PD0M1,  D0-1                    / AUTOINCREMENT PTR TO D0
KM10,   -12                     / MINUS 10.

/
/ CHK - IS A DIGIT USED YET?
/       RETURN AC==0 YES, DIGIT IN USE
/              AC!=0  NO, DIGIT IS AVAILABLE TO USE
CHK,    0
        CLA                     / AC=0
        TAD I   CHK             / GET ADDRESS OF DIGIT TO TEST FO
        ISZ     CHK
        DCA     CHKTP           / SAVE TEST DIGIT ADDRESS
        TAD I   CHKTP           / READ THE DIGIT
        CIA                     / NEGATE IT
        DCA     CHKTP           / SAVE TEST VALUE
        TAD I   CHK             / GET NEGATIVE NUMBER OF DIGITS
        ISZ     CHK
        DCA     CHKCNT          / SAVE COUNT
        TAD     PD0M1           / GET ADDRESS OF FIRST DIGIT
        DCA     CHKPTR          / SAVE ADDRESS OF DIGIT TO TEST
CHKL,   CLA                     / AC=0
        TAD I   CHKPTR          / READ NEXT DIGIT TO TEST
        TAD     CHKTP           / COMPARE TO VALUE TO TEST
        SNA                     / SKIP IF VALUE TEST FAILS
        JMP I   CHK             / RETURN 0 IF DIGIT IN USE
        ISZ     CHKCNT          / COUNT DIGITS TESTED
        JMP     CHKL            / JUMP TO TEST NEXT
        CLA CMA                 / SET TO NONZERO
        JMP I   CHK             / RETURN NZ AS DIGIT IS UNUSED
CHKTP,  0                       / PTR TO AND VAL OF DIGIT TO TEST
CHKCNT, 0                       / NEGATIVE COUNT OF DIGITS TO TES

        *200
                                / MAIN ENTRY POINT
START,  JMS I   PTR0            / CALL SOLUTION ENGINE
        HLT                     / UH, OH. NO SOLUTION FOUND.

                                / S0 - SOLVE DIGIT D0
S0,     0
/        HLT
        CLA
        DCA     D0
        TAD     KM10
        DCA     C0
S0L,    JMS I   PTR1
        ISZ     D0
        ISZ     C0
        JMP     S0L
        JMP I   S0

                                / S1 - SOLVE DIGIT D1
S1,     0

        CLA
        DCA     D1
        TAD     KM10
        DCA     C1
S1L,    JMS     CHK
        D1
        -1
        SZA
        JMS I   PTR2
        ISZ     D1
        ISZ     C1
        JMP     S1L
        JMP I   S1

                                / S2 - SOLVE DIGIT D2
S2,     0
        CLA
        DCA     D2
        TAD     KM10
        DCA     C2
S2L,    JMS     CHK
        D2
        -2
        SZA
        JMS I   PTR3
        ISZ     D2
        ISZ     C2
        JMP     S2L
        JMP I   S2

                                / S3 - SOLVE DIGIT D3
S3,     0
        CLA
        DCA     D3
        TAD     KM10
        DCA     C3
S3L,    JMS     CHK
        D3
        -3
        SZA
        JMS I   PTR4
        ISZ     D3
        ISZ     C3
        JMP     S3L
        JMP I   S3
                                / S4 - SOLVE DIGIT D4
S4,     0
        CLA
        DCA     D4
        TAD     KM10
        DCA     C4
S4L,    JMS     CHK
        D4
        -4
        SZA

        JMS I   PTR5
        ISZ     D4
        ISZ     C4
        JMP     S4L
        JMP I   S4

                                / S5 - SOLVE DIGIT D5
S5,     0
        CLA
        DCA     D5
        TAD     KM10
        DCA     C5
S5L,    JMS     CHK
        D5
        -5
        SZA
        JMS I   PTR6
        ISZ     D5
        ISZ     C5
        JMP     S5L
        JMP I   S5

                                / S6 - SOLVE DIGIT D6
S6,     0
        CLA
        DCA     D6
        TAD     KM10
        DCA     C6
S6L,    JMS     CHK
        D6
        -6
        SZA
        JMS I   PTR7
        ISZ     D6
        ISZ     C6
        JMP     S6L
        JMP I   S6

                                / S7 - SOLVE DIGIT D7
S7,     0
        CLA
        DCA     D7
        TAD     KM10
        DCA     C7
S7L,    JMS     CHK
        D7
        -7
        SZA
        JMS I   PTR8
        ISZ     D7
        ISZ     C7
        JMP     S7L
        JMP I   S7

        *400

                                / S8 - SOLVE DIGIT D8
S8,     0
        CLA
        DCA     D8
        TAD     KM10
        DCA     C8
S8L,    JMS     CHK
        D8
        -10
        SZA
        JMS I   PTR9
        ISZ     D8
        ISZ     C8
        JMP     S8L
        JMP I   S8

                                / S9 - SOLVE DIGIT D9
S9,     0
        CLA
        DCA     D9
        TAD     KM10
        DCA     C9
S9L,    JMS     CHK
        D9
        -11
        SZA
        JMS     TESTD
        ISZ     D9
        ISZ     C9
        JMP     S9L
        JMP I   S9
                                / TESTD - ANOTHER ITER COMPLETE
TESTD,  0
        ISZ     CLO
        JMP I   TESTD
        ISZ     CHI
        JMP I   TESTD
                                / 1,000,000TH FOUND
        HLT

        JMP I   MON
MON,    7600
$
