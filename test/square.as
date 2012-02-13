/ Learn to Program (A-11)
/  Enter a two digit octal number and it will print the result (n^2)
/ In Haskell: Numeric.showOct ((Util.oct 73)^2) ""
/  where '7' and '3' are the first and second digits entered.
*200
START, CLA CLL
       TLS
       JMS CRLF
       JMS LISN
       TAD M260
       RAL CLL
       RTL
       DCA NUMBER
       JMS LISN
       TAD M260
       TAD NUMBER
       DCA NUMBER
MULT,  TAD NUMBER
       CIA
       DCA TALLY
       TAD NUMBER
       ISZ TALLY
       JMP .-2
       DCA NUMSQR
TYPSQU, TAD MESAG1
        DCA POINTR
        TAD M10
        DCA ENDCHK
        JMS MESAGE
TYPANS, TAD M4
        DCA DIGCTR
        DCA STORE
        TAD NUMSQR
        CLL RAL
UNPACK, TAD STORE
        RAL
        RTL
        DCA STORE
        TAD STORE
        AND K7
        TAD K260
        JMS TYPE
        ISZ DIGCTR
        JMP UNPACK
TYPOCT, TAD MESAG2
        DCA POINTR
        TAD M7
        DCA ENDCHK
        JMS MESAGE
        JMS CRLF
        JMP START+2
TYPE,   0
        TSF
        JMP .-1
        TLS
        CLA
        JMP I TYPE
CRLF,   0
        TAD K215
        JMS TYPE
        TAD K212
        JMS TYPE
        JMP I CRLF
LISN,   0
        KSF
        JMP .-1
        KRB
        TLS
        JMP I LISN
MESAGE, 0
        TAD I POINTR
        JMS TYPE
        ISZ POINTR
        ISZ ENDCHK
        JMP .-4
        JMP I MESAGE
NUMBER, 0
M260,   -260
TALLY, 0
NUMSQR, 0
MESAG1, START1
POINTR, 0
M10, -10
ENDCHK, 0
STORE, 0
M4, -4
DIGCTR, 0
K7, 7
M7, -7
K260, 260
K212, 212
K215, 215
MESAG2, START2
START1, 323
        321
        325
        301
        322
        305
        304
        275
START2, 240
        317
        303
        324
        301
        314
        256
$
       
