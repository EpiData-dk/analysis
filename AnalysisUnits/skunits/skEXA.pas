unit skEXA;

interface

Uses skTypes,skrandom;

Var simstart : integer;

PROCEDURE INIT_EXACT_TEST(VAR OUTFILE: TEXT; EXACT : BOOLEAN; SIMSTART: INTEGER);


implementation

 PROCEDURE INIT_EXACT_TEST;
 VAR SEED : BYTE;
 BEGIN
     IF EXACT THEN
     BEGIN
         IF SIMSTART>255  THEN SEED:=255 ELSE
         IF SIMSTART<0    THEN SEED:=0   ELSE SEED:=SIMSTART;
         CHPINIT(SEED);
     END;
 END; (* of INIT_EXACT_TEST *)

end.
