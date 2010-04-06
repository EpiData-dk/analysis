Unit EpiInfoSTATS;

Interface
  Uses SysUtils, windows, Classes,ansDataTypes;

  Const

  ChiUnCorrected=1;
  ChiMH=2;
  ChiYates=3;

    MaxPercentiles   = 3;
    confScore        = 1.960;
(*-----------------------------------------------------------------------------}
{                                                                             }
{  This unit contains routines for calculating the conditional maximum        }
{  likelihood estimate, exact confidence limits, and exact P-values for the   }
{  common odds ratio in a series of 2x2 tables or 1:R matched data.           }
{                                                                             }
{  Contents:                                                                  }
{     AssignVars      Assigns values to some variables.                       }
{     CheckData       Checks data.                                            }
{     CalcPoly        Calculates the polynomial used to calc exact estimates. }
{     CalcExactPVals  Calculates exact (Fisher and mid-P) P-values.           }
{     CalcCmle        Calculates the conditional MLE of the odds ratio.       }
{     CalcExactLim    Calculates an exact (Fisher or mid-P) conf limit.       }
{                                                                             }
{  Author: David Martin, MPH.                                                 }
{                                                                             }
{  Last Mod: 3/26/91.                                                         }
{                                                                             }
{  Adapted to EPI Info: 6/9/93 by Chris Smith, CDC                            }
   Adapted to EpiData and D4 by Salah Mahmud 2002-2003
   ADapted further to EpiData Analysis by Jens Lauritsen 2005
{-----------------------------------------------------------------------------*)
   MAXITER         = 100;       { Max # of iterations }
   TOLERANCE       = 1E-5;      { Relative tolerance in results }
   MAXTABLES       = 50;
   MAXNUMTBLS      = 20;
   MAX2X2CELL      = 5000;      { push it a bit }
   MAXDEGREE       = 3199;      { Max degree of the polynomial (see CheckData) }
   FISHWORD        = 'Exact 95%';
   LIM             = 'limits for MLE';

type
   TExtVector      = array of extended;

   TTableItem      = class
   private
      finformative: boolean;
      fa: longint;
      fb: longint;
      fc: longint;
      fd: longint;
      ffreq: word;
    fTot: Epifloat;
    fM0: Epifloat;
    fN1: Epifloat;
    fM1: Epifloat;
    fN0: Epifloat;
    fEa: Epifloat;
    fEd: Epifloat;
    fEb: Epifloat;
    fEc: Epifloat;
    fODR, fORUL, fORLL, fpOR: EpiFloat;
   public
       constructor Create(aCell, bCell, cCell, dCell: longint;aFreq: word);
//       destructor Destroy;override;
       property   a           : longint read fa write fa;
       property   b           : longint read fb write fb;
       property   c           : longint read fc write fc;
       property   d           : longint read fd write fd;

       property   N0          : Epifloat read fN0;
       property   N1          : Epifloat read fN1;
       property   M0          : Epifloat read fM0;
       property   M1          : Epifloat read fM1;
       property   Tot         : Epifloat read fTot;

       property   Ea           : Epifloat read fEa;
       property   Eb           : Epifloat read fEb;
       property   Ec           : Epifloat read fEc;
       property   Ed           : Epifloat read fEd;

       property   freq        : word  read ffreq write ffreq;
       property   informative : boolean read finformative write finformative;
       property   sOR         : EpiFloat read fODR write fODR;
       property   ORUL        : EpiFloat read fORUL write fORUL;
       property   ORLL        : EpiFloat read fORLL write fORLL;
       property   pOR         : EpiFloat read fpOR write fpOR;

   end;

   TEqualFunc=function (q: TTableItem): boolean;
   TEqualProc=procedure(p: TTableItem);

   TPolynomial = class
     vector      : TExtVector;
     degree      : integer;
   private
     procedure matchSet(p: TTableItem);
     procedure stratumSet(p: TTableItem);
   public
     constructor Create;
     destructor Destroy; virtual;
     procedure copyFrom(p: TPolynomial);
     procedure setData(p: TTableItem; match: boolean);
     procedure multiply(p1, p2: TPolynomial);
     function evaluate(r: extended): extended;
   end;

    TFisherStat = class(TList)
     private
       sumA        : longint;
       minSumA     : longint;
       maxSumA     : longint;
       PolyD       : TPolynomial;    { denominator }
       PolyN       : TPolynomial;    { numerator }
       value       : extended;
       match       : boolean;
       estimate    : integer;
       function func(approx: extended): extended;
       function checkData: integer;
       function calcPoly: TPolynomial;  { new? }
       function CalcCmle(approx: extended): extended;
       function zero(a, b, fa, fb: extended;
                         var root: extended): integer;
       function converge(approx: extended): extended;
       procedure getPVals(var up, lo :extended);
       function getLimit(lower, fisher: boolean;
                                approx: extended;
                                confLevel: extended): extended;
       function displayPValue(t : TtableItem;cmle: extended):string;
       function firstThat(aProc: TEqualFunc): TTableItem;
       function forEach(aProc: TEqualProc): TTableItem;
     public
       resultstr : string;
       constructor Create(aMatch: boolean);
       destructor Destroy; virtual;
       procedure addSet(p: TTableItem);
       function show(t: TTableItem;stratum:integer): integer; virtual;
     end;

Implementation

uses UstatFunctions, UCmdProcessor;


const
    UNDEFINED        = '??????';
    INFINITY     = 1E+30;

var
   sumPRrgb      : epifloat;

constructor TTableItem.Create(aCell, bCell, cCell, dCell: Integer;aFreq: word);
begin
     ffreq := aFreq;
     fa := aCell;
     fb := bCell;
     fc := cCell;
     fd := dCell;
     finformative := (a * d > 0) OR (b * c > 0);
     (*Calculate margin totals *)
     fN1 := A + B;
     fN0 := C + D;
     fM1 := A + C;
     fM0 := B + D;
     fTot := N1 + N0;
     if ftot > 0 then
     begin
      (*Calculate expected values*)
      fEA := M1 * N1 / Tot;
      fEB := M0 * N1 / Tot;
      fEC := M1 * N0 / Tot;
      fED := M0 * N0 / Tot;
      end
     else
      begin fEa := 0; fEb := 0;fEc := 0;fEd := 0; end; // of no use
end;


function min(a, b: longint): longint;
begin
     if (a < b) then
        min := a
     else
        min := b;
end;
                           function max(a, b: longint): longint;
begin
     if (a > b) then
        max := a
     else
       max := b;
end;


function pow(base: extended; expon: integer): extended;
begin
     pow := exp(expon * ln(base));
end;


function comb(n, r: longint): extended;
 { returns # of combinations of n things taken r at a time }
var
   i: integer;
   f: extended;
begin
   f := 1.0;
   for i := 1 to min(r, n - r) do  { _not_ invariant }
      begin
      f := f * n / i;
      dec(n);
      end;
   comb := f;
end;

constructor TPolynomial.Create;
{var
// i : integer;}
begin
   inherited Create;
   degree := 0;
   SetLength(vector, MAXDEGREE);
   fillchar(vector[0],MAXDEGREE*sizeof(extended),0);
{   for i:= 0 to MAXDEGREE-1 dosda
     vector[i]:=0;}
   vector[0] := 1.0;
end;

destructor TPolynomial.Destroy;
begin
 vector :=nil;
 inherited;
end;

procedure TPolynomial.matchSet(p: TTableItem);
var
   i: integer;
   m0, m1, n1     : longint;
   c0,  c1     : extended;
begin
    degree := p.freq;
    m0 := p.c + p.d;
    m1 := p.a + p.b;
    n1 := p.a + p.c;
    c0 := comb(m1, 0) * comb(m0,n1);
    c1 := comb(m1, 1) * comb(m0, pred(n1));
    vector[degree] := pow(c1, degree);  { binomial expansion }
    for i := pred(degree) downto 0 do
        vector[i] := vector[succ(i)] * c0 * succ(i) / (c1 * (degree-i));
end;



procedure TPolynomial.StratumSet(p: TTableItem);
var
   i: integer;
   pm0, pm1, pn1, minA, maxA   : longint;
   bb, cc, dd     : longint;
begin
   with p do
      begin
      pm0 := c + d;
      pm1 := a + b;
      pn1 := a + c;
      minA := max(0, pn1-pm0);
      maxA := min(pm1, pn1);
      degree := maxA - minA
      end;
   vector[0] := comb(pm1, minA) * comb(pm0, pn1-minA);
   vector[degree] := comb(pm1, maxA) * comb(pm0, pn1-maxA);
   bb := succ(pm1 - minA);
   cc := succ(pn1 - minA);
   dd := pm0 - pn1 + minA;
   for i := 1 to pred(degree) do
      vector[i] := vector[pred(i)] * ((bb-i) / (minA+i)) * ((cc-i) / (dd+i));
end;



procedure TPolynomial.setData(p: TTableItem; match: boolean);
begin
     if (p.informative) then
        begin
        if (match) then
            matchSet(p)
        else
            stratumSet(p);
        end
     else
        begin
          degree := 0;
          vector[0] := 1.0;
        end;
end;


procedure TPolynomial.copyFrom(p: TPolynomial);
begin
     degree := p.degree;
     move(p.vector[0], vector[0], succ(p.degree) * sizeOf(vector[0]));
end;

procedure TPolynomial.multiply(p1, p2: TPolynomial);
var
   i, j: integer;
   deg1, deg2: integer;
begin
     deg1 := p1.degree;
     deg2 := p2.degree;
     degree := deg1 + deg2;
     fillchar(vector[0], succ(degree)*sizeof(vector[0]), 0);
     for i := 0 to deg1 do
         for j := 0 to deg2 do
            vector[i+j] := p1.vector[i] * p2.vector[j] + vector[i+j];
end;


function TPolynomial.evaluate(r: extended): extended;
{ Horner's method for evaluating a Taylor series }
var
   i: integer;
   y: extended;
begin
     if (r = 0.0) then
        begin
        evaluate := vector[0];
        exit;
        end;
     if (r <= 1.0) then
        begin
        y := vector[degree];
        if R < 1.0 then                      { save a epifloating point multiply }
           for i := (degree - 1) downto 0 do { save a comparison }
               y := y * R + vector[i]
        else
           for i := (degree - 1) downto 0 do
               y := y + vector[i];
        end
     else
        begin
        y := vector[0];
        r := 1 / r;
        for i := 1 to degree do
           y := y * r + vector[i];
        end;
     evaluate := y;
end;

function TFisherStat.firstThat(aProc:TEqualFunc):TTableItem;
var
  i : integer;
begin
  Result := nil;
  for i:= 0 to count-1 do
    if aProc(items[i]) then
    begin
      result:=items[i];
      exit;
    end;
end;


function TFisherStat.forEach(aProc:TEqualProc):TTableItem;
var
  i : integer;
begin
  Result := nil;
  for i:= 0 to count-1 do
      aProc(items[i]);
end;



procedure TFisherStat.addSet(p: TTableItem);
var
   item: TTableItem;

   function isEqual(q: TTableItem): boolean;  { object of iterator }
   begin
        Result := (p.a=q.a) and (p.b=q.b) and (p.c=q.c) and (p.d=q.d);
   end;

begin
   if ( match ) then
      begin
      item := firstThat(@isEqual);
      if (item = nil) then
         add(p)
      else
         begin
           p.free;
           item.freq:=item.freq+1 ;
         end;
      end
   else
      add(p);
end;

function TFisherStat.calcPoly: TPolynomial;
var
   poly,
   poly1,
   poly2  : TPolynomial;
   i      : integer;
   p      : TTableItem;
begin
     poly := TPolynomial.Create;
     poly.setData(items[0], match);
     poly1 := TPolynomial.Create;
     poly2 := TPolynomial.Create;
     for i := 1 to count-1 do
        begin
        p := items[i];
        if (p.informative) then
           begin
           poly1.copyFrom(poly);
           poly2.setData(p, match);
           poly.multiply(poly1, poly2);
           end;
        end;
     poly2.free;
     poly1.free;
     Result := poly;
end;
{---------------------------------------------------------------------------}
{ Func is defined so that its zero (value at which Func = 0) is the CMLE or }
{ an exact confidence limit.  It is a FAR CALL function, as required by the }
{ routine Zero.                                                             }
{---------------------------------------------------------------------------}

{$f+ : FAR CALL }

function TFisherStat.func(approx: extended): extended;
{
   Global vars: value, polyN, degN, polyD, degD.

   The root of this function is the conditional MLE of the common odds ratio
   or an exact confidence limit depending on how the global variables value,
   polyN, and polyD are defined.
}
var
   numer, denom: extended;
begin
   numer := PolyN.evaluate(approx);
   denom := PolyD.evaluate(approx);
   if approx <= 1.0 then
      Func := numer / denom - value
   else
      Func := (numer / pow(approx, PolyD.degree - polyN.degree)) / denom - value;

end; { Func }

{$f- : NEAR CALL }

function TFisherStat.zero(a, b, fa, fb: extended; var root: extended): integer;
{
   This function calculates a single real root of the user defined function f in
   the given interval [a, b], to within a relative tolerance tol.  The
   procedure implements a method by Brent which uses a combinaton of bisecton,
   the secant method, and inverse quadratic interpolation.  The procedure is
   guaranteed to converge in no more than (t + 1)^2 - 2 iterations where t is
   the number of iteratons needed by bisection.  Usually convergence is MUCH
   faster than that required by bisection.  This procedure assumes that tol/4
   is not less than the relative machine precision, i.e., 1 + tol/4 > 1 in
   computer arithmetic.

   returns: 0 = no error,
            1 = fa * fb > 0 (i.e., root not between a and b),
            2 = root not found in maxiter iterations.

   Stopping criteria (root returned as b):
      1) ABS(b - c) <= ABS(b) * tol where b & c bracket the root, or
      2) f(b) = 0, or
      3) # iterations > maxiter.

   Reference: Brent, R. P.  Algorithms for Minimization without Derivitives.
              Prentice-Hall, Englewood Cliffs, N.J., 1973.
}

var
   c, d, e, fc, t,
   m, p, q, r, s: extended;     { Similar notation as given in the reference }
   iter: integer;               { Current # of iterations }
   found: boolean;              { Flags that a root has been found }
   retval: integer;
begin
   retval := 0;                  { Initialize }
   iter := 0;
   found := false;
   if (fa = 0.0) then
      begin
      b := a;
      found := true;
      end
   else
      if ((fa / Abs(fa)) * fb > 0.0) then            { Root not bracketed }
         retval := 1;
   { Converge using Brent's method }
   while not(found) and (iter < maxiter) and (retval = 0) do
      begin
      if ((fa / Abs(fa)) * fb <= 0.0) then
         begin       { a & b bracket root }
         c := a;
         fc := fa;
         d := b - a;
         e := d;
         end;
      if Abs(fc) < Abs(fb) then
         begin                  { Make b best estimate }
         a := b;
         b := c;
         c := a;
         fa := fb;
         fb := fc;
         fc := fa;
         end;
      t := Abs(b) * 0.5 * TOLERANCE;
      m := 0.5 * (c - b);
      found := (Abs(m) <= t) or (fb = 0.0);            { Convergence test }
      if not(found) then
         begin
         if (Abs(e) < t) or (Abs(fa) <= Abs(fb)) then
            begin   { Bisection }
            d := m;
            e := m;
            end
         else
            begin
            s := fb / fa;
            if a = c then
               begin                    { Linear interpolation }
               p := 2.0 * m * s;
               q := 1.0 - s;
               end { if }
            else
               begin                  { Inverse quadratic interpolation }
               q := fa / fc;
               r := fb / fc;
               p := s * (2.0 * m * q * (q - r) - (b - a) * (r - 1.0));
               q := ( q - 1.0) * (r - 1.0) * (s - 1.0);
               end; { else }
            if p > 0.0 then
               q := -q
            else
               p := -p;
            s := e;
            e := d;
            if ( (2.0 * p) < (3.0 * m * q - Abs(t * q)) ) and
               ( p < Abs(0.5 * s * q) ) then
               d := p / q
            else
               begin
               d := m;
               e := m;
               end; { else }
            end; { else }
         a := b;
         fa := fb;
         if Abs(d) > t then
            b := b + d
         else if m > 0 then
            b := b + t
         else
            b := b - t;
         fb := func(b);
         iter := iter + 1;
         end; { if not(found) }
      end; { while }
   root := b;                                 { Estimated zero }
   if not(found) and (iter >= maxiter) then   { Too many iterations }
      retval := 2;
   Zero := retval;
end; { Zero }



function TFisherStat.converge(approx: extended): extended;
var
   i: integer;
   x0, x1, f0, f1, root: extended;
begin
   if (approx <= 0.0) then         { Invalid approximation }
      approx := 1.0;
   x1 := approx;                 { Valid approx so let x1 = approx }
   x0 := 0.0;                    { The lower bound }
   f0 := Func(x0);               { Func at x0 }
   f1 := Func(x1);               { Func at x1 }

   { If necessary, increase x1 until f1 and f0 change signs }
   i := 0;
   while (i < MAXITER) and (f1 * f0 > 0.0) do
         begin
         inc(i);
         x0 := x1;
         f0 := f1;
         x1 := x1 * 1.5 * i;
         f1 := Func(x1);
         end;
   if (zero(x0, x1, f0, f1, root) = 0) then
      converge := root
   else
      converge := INFINITY;
end; { Converge }

function TFisherStat.CalcCmle( approx: extended ): extended;
   function GetCmle: extended;
   var
      i: integer;
   begin
      polyN :=  TPolynomial.Create; { polynomial numerator }
      value := sumA;                  { The sum of the observed "a" cells }
      polyN.copyFrom(polyD);
      for i := 0 to polyN.degree do
         polyN.vector[i] := polyN.vector[i] * (minSumA + i);
      GetCMLE := Converge(approx);    { Solves so that Func(cmle) = 0 }
      polyN.free;
      polyN := nil;
   end; { GetCmle }

begin { CalcCmle }
   calcCMLE := 0;
   case estimate of
      0 : calcCMLE := GetCmle;
      1 : calcCMLE := 0;
      2 : calcCMLE := INFINITY;
      else
          calcCMLE := -1.0;
   end; { case }
end; { CalcCmle }

procedure TFisherStat.getPVals(var up, lo :extended);
var
   i, diff: integer;        { Index; sumA - minSumA }
   upTail, denom: extended; { Upper tail; the whole distribution }
begin
   diff := sumA - minSumA;
   with polyD do
      begin
      upTail := vector[degree];
      for i := pred(degree) downto diff do
          upTail := upTail + vector[i];
      denom := upTail;
      for i := diff - 1 downto 0 do
          denom := denom + vector[i];
      up := upTail / denom;
      lo := 1.0 - (upTail - vector[diff]) / denom ;
//      upMidP := (upTail - 0.5 * vector[diff]) / denom;
//      loMidP := 1.0 - upMidP;
      end
end; { CalcExactPVals }

function TFisherStat.getLimit(lower, fisher: boolean; approx: extended;
                                                   confLevel: extended): extended;
   function getExactLim: extended;
   begin
        polyN :=  TPolynomial.Create;
        if lower then
           value := 0.5 * (1.0 + confLevel)  { = 1 - alpha / 2 }
        else
           value := 0.5 * (1.0 - confLevel); { = alpha / 2 }
        with polyN do
             begin
             copyFrom(polyD);
             if lower and fisher then                      { Degree of numerator poly }
                degree := sumA - minSumA - 1
             else
                degree := sumA - minSumA;
             if not(fisher) then
                vector[degree] := 0.5 * vector[degree];    { Mid-P adjustment }
             getExactLim := Converge(approx);  { Solves so that Func(limit) = 0 }
             end;
        polyN.free;
        polyN := nil;
   end;

begin
   if (confLevel < 0.0) or (confLevel >= 1.0) then
      confLevel := 0.95;
   case estimate of
         0 : getLimit := GetExactLim;
         1 : if lower then
                getLimit := 0.0
             else
                getLimit := GetExactLim;
         2 : if not(lower) then
                getLimit := INFINITY
             else
                getLimit := GetExactLim;
      else
             getLimit := -1.0;
   end;
end;

function TFisherStat.checkData: integer;
var
   SumAD,
   SumBC,i   : longint;
   procedure calcSums(p: TTableItem);
   var
      a, b, c, d : longint;
      freq : word;
   begin
      if (P.informative) then
         begin
         a := p.a;
         b := p.b;
         c := p.c;
         d := p.d;
         freq := p.freq;
         inc(sumA, (freq * a));
         inc(minSumA, (max(0, a-d) * freq));
         inc(maxSumA, (min(a+d, a+c) * freq));
         inc(SumAD, a * d * freq);
         inc(SumBC, b * c * freq);
         //dm.info(format('%d  %d  %d  %d',[a,b,c,d]));
         end;
   end;
begin
   sumA := 0;
   minSumA := 0;
   maxSumA := 0;
   SumAD := 0;
   SumBC := 0;
//   forEach(@calcSums);
  for i:= 0 to count-1 do
      calcSums(items[i]);
   if (maxSumA-minSumA > MAXDEGREE) then
        begin
        checkdata := -1;
        exit;
        end;
   if (sumAD > 0) and (sumBC > 0) then          { can calculate OR }
        checkData := 0
   else if (sumAD = 0) and (sumBC > 0) then     { OR = 0 }
        checkData := 1
   else if (sumAD > 0) and (sumBC = 0) then     { OR = infinity }
        checkData := 2
   else
        checkData := 5;                         { no informative strata }
end;

function TFisherStat.displayPValue(t : TtableItem;cmle: extended):string;
const
     LE       = '<= ';
     GTE      = '>= ';
var
   val, upper, lower :extended;
   temp: string;
   s   : string;
   begin
     getPVals(upper, lower);
     if (cmle = INFINITY) then
        temp := '??????'
     else
        str(cmle:5:2, temp);
     s := ' <br> p (MLE OR ';
     if (cmle < 1.0) then
        begin
        val := lower;
        s := s + LE;
        end
     else
        begin
        val := upper;
        s := s + GTE;
        end;
     {s := s + temp;
     //s := s + ' if population OR = 1.0)=';
        Result:= s + format('%9.4f',[val]);
       Result:= s + format('%12.11f',[val]) ;
       dm.info(s);
     }
     if val = INFINITY then val := -9999;
     t.pOR := val;
end;

function TFisherStat.show(t: TtableItem; stratum:integer): integer;

   function badCell(p: TTableItem):boolean;
   begin
      badCell := p.informative and
                     ((p.a < 0) or (p.a > MAX2X2CELL) or
                      (p.b < 0) or (p.b > MAX2X2CELL) or
                      (p.c < 0) or (p.c > MAX2X2CELL) or
                      (P.d < 0) or (p.d > MAX2X2CELL));
   end;

var
   cmle,
   upper,
   lower,
   up,
   lo,
   approx   : extended;
   i        : integer;
   s: string;

begin
  s := '';
     show := 0;
     resultstr := '' ;// For better resulthandling return as string
     if (match and (count > MAXTABLES)) or
        (not match and (count > min(MAXNUMTBLS, MAXTABLES))) then
        begin
        show := 3;
        exit;
        end;
     if (firstThat(@badCell) <> nil) then
        begin
        show := 4;
        exit;
        end;
     estimate := checkData;
     if (estimate < 0) then
        begin
        show := 2;
        exit;
        end;
     if (estimate = 5) then
        begin
        show := 5;         { no informative strata }
        exit;
        end;
     show := 0;
     PolyD := calcPoly;
     cmle := calcCMLE(1.0);

     displaypvalue(t,cmle);      // copy P for OR to t table
    {dm.writeln(FormatOR('MLE estimate of OR ', dm.FloatFormat,cmle) + ' p= '
            + trim(format(' %s - i: %d',[displayPValue(t,cmle),stratum])));
     }
     upper := getLimit(false, true, cmle, 0.95);
     if (upper <> INFINITY) then
        approx := cmle*cmle/upper
     else
        approx := 1.0;

     // copy results to ttableitem:
     if cmle <> INFINITY then t.sor  := cmle
       else t.sor := -9999;

     t.orll := getLimit(true, true, approx, 0.95);
       if t.orll = INFINITY then t.orll := -9999;

     t.orul := upper;
       if t.orul = INFINITY then t.orul := -9999;

     {// check results:
     resultstr:='<br>'
                + Format('MLE: OR= %7.4f (%7.4f - %7.4f) p=%7.5f i= %d',[t.sor,t.ORLL,t.orul,t.por ,   stratum]);
     dm.info(resultstr);
     //}
     PolyD.free;
     PolyD := nil;
end;

constructor TFisherStat.Create(aMatch: boolean);
begin
     inherited Create;
     match := aMatch;
     PolyD := nil;
     PolyN := nil;
end;

destructor TFisherStat.Destroy;
begin
     if (PolyN <> nil) then
        PolyN.free;
     if (PolyD <> nil) then
        PolyD.free;
     inherited;
end;

End.


