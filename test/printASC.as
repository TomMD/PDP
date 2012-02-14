*0
START, TAD POINTR    / add in the pointer
       TLS             / print char
TEST,  TSF         
       JMP TEST        / wait for teleprinter to be ready
       CLA             / clear AC - TMD
       ISZ POINTR      / if ptr hits zero, halt
       JMP START       / otherwise continue.
       HLT
POINTR, 7600
$
