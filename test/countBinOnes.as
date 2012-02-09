/From  Learn to Program pg 3-23

START,  CLA CLL
        DCA COUNT
        TAD I WORD
        SNA
        HLT
ROTATE, RAL
        SNL
        JMP -2
        CLL
        ISZ COUNT
        SNA
        HLT
        JMP ROTATE
COUNT, 0
WORD, 3000 / ANY 12 BIT NUMBER
$
