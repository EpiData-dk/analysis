(*
         ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
         ³  This is the original source text for the Turbo Pascal    ³
         ³  unit                                                     ³
         ³                    ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ                     ³
         ³                        DISTR.PAS                          ³
         ³                    ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ                     ³
         ³                                                           ³
         ³  If you modify this unit, please                          ³
         ³                                                           ³
         ³     (1) Make sure that you have a copy of the original    ³
         ³         file.                                             ³
         ³                                                           ³
         ³     (2) Delete this comment (21 lines).                   ³
         ³                                                           ³
         ³  Original size of file: 11241 bytes.     feb 17 1992      ³
         ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
             Copyright       Tue Tjur
                             Institute of Mathematical Statistics
                             University of Copenhagen
*)

unit Distr;

interface

function lnGamma(f:longint):extended;

      { Computes the logarithm of the gamma function at f/2. }


function pGamma(f:longint;y:extended):extended;

      { Returns the right tail probability in the gamma
        distribution with lambda = f/2. }


function pNormal(u:extended):extended;

      { Returns the right tail probability in the normal distribution. }


function pChi2(f:longint;y:extended):extended;

      { Returns the right tail probability in the chi square distribution
        with f degrees of freedom. }


function pBeta(f1,f2:longint;y:extended):extended;

      { Returns the LEFT tail probability in the beta distribution
        with paramters lambda1=f1/2 and lambda2=f2/2. Use only
        f1 and f2 < 1E6.  }


function pFdistr(f1,f2:longint;y:extended):extended;

      { Returns the right tail probability in the F distribution
        with (f1,f2) degrees of freedom.
        Use only f1 and f2 < 1E6. }


function pTdistr(f:longint;y:extended):extended;

      { Returns the right tail probability in the T distribution.
        Use only  f < 1E6. }


function pNormalInv(p:extended):extended;

      { Inverse of pNormal. }


function pGammaInv(f:longint;p:extended):extended;

      { Inverse of pGamma(f,*). }


function pChi2Inv(f:longint;p:extended):extended;

      { Inverse of pChi2(f,*). }


function pBetaInv(f1,f2:longint;p:extended):extended;

      { Inverse of pBeta(f1,f2,*) (notice: LEFT tail). }


function pFdistrInv(f1,f2:longint;p:extended):extended;

      { 1-p percentile of F distribution. }

function pTdistrInv(f:longint;p:extended):extended;

      { 1-p percentile of T distribution. }


function pPoiss(lambda:extended; n:longint): extended;

      { Returns the right tail probability in the Poisson distribution. }


function PoissCL(n:longint; p:extended): extended;

      { Lower 1-p confidence limits for lambda in Poisson distribution
        when n is observed. }


function pBin(n,x:longint; p:extended): extended;

      { Returns the binomial right tail probability. }


function BinCL(n,x:longint; pp:extended): extended;

      { Returns confidence limit for binomial probability parameter,
        i.e. inverse to pBin(n,*). }


{---------------------------------------------------------------------------}
implementation

function lnGamma;

var sum,term,y   : extended;
    k           : longint;

begin   y:=f/2;
if f>500 then
    begin
    sum:= ln(2*pi)/2  + (y-1/2)*ln(y); sum:=sum -y + 1/(12*y);
    sum:=sum - 1/360/y/y/y;
    lnGamma:=sum;
    end
else
    begin
    k:=f; sum:=0;
    while k>2 do
        begin
        k:=k-2;
        sum:=sum+ln(k/2);
        end;
    if k=1 then sum:=sum+ln(pi)/2;
    lnGamma:=sum;
    end;
end;
{---------------------------------------------------------------------------}
function pGamma;

var  term,sum: extended;
     k      : longint;

begin if (y<=0) then pGamma:=1 else
if (y<f/2) or (y<42) then
    begin
    term:=(f/2)*ln(y)-y-lnGamma(f+2);
    if term>-1000 then term:=exp(term) else term:=0;
    sum:=0; k:=0;
    while ((f+k)*term>(f+k-2*y)*1E-20) do
        begin
        sum:=sum+term;
        term:=2*term*y/(f+k+2);
        k:=k+2;
        end;
    pGamma:=abs(1-sum);
    end
else
    begin
    term:=(f/2-1)*ln(y)-y-lnGamma(f);
    if term>-1000 then term:=exp(term) else term:=0;
    sum:=0; k:=0;
    while (term*y > (2*y-f+k)*0.5E-20) and (f-k>1) do
        begin
        sum:=sum+term;
        k:=k+2;
        term:=term*(f-k)/2/y;
        end;
    pGamma:=abs(sum);
    end;
end;
{---------------------------------------------------------------------------}
function pNormal;

var p: extended;

begin
p:= pGamma(1,u*u/2)/2;
if u<0 then p:=1-p;
pNormal:=p;
end;
{---------------------------------------------------------------------------}
function pChi2;

begin
pChi2:= pGamma(f,y/2);
end;
{---------------------------------------------------------------------------}
function pBeta0(f1,f2:longint; y:extended): extended;

      { Returns the left tail probability of the beta distribution
        with paramters lambda1=f1/2 and lambda2=f2/2. Use only f1+f2<40.
        Accuracy around +/- 1E-16 . }

var sum,term             : extended;
    k                   : longint;

begin
sum:=0; k:=0;
term:=lnGamma(f1+f2)-lnGamma(f2);
term:=term-lnGamma(f1+2)+f1*ln(y)/2;
term:=exp(term);
while (k<f2) or (abs(term) > 1E-20) do
    begin
    sum:=sum+term;
    k:=k+2;
    term:=-term*y*(f2-k)*(f1+k-2)/k/(f1+k);
    end;
pBeta0:=sum;
end;
{---------------------------------------------------------------------------}
function pBeta;

var sum,term             : extended;
    k                   : longint;
    intch               : boolean;

begin  if (f1=f2) and (y=0.5) then pBeta:=0.5 else
       if y<=0 then pBeta:=0 else
       if y>=1 then pBeta:=1 else
    begin
    intch:=false;
    if y>(1-y) then
        begin intch:=true;
        k:=f1; f1:=f2; f2:=k;
        y:=1-y;
        end;
    if f1+f2<41 then sum:=pBeta0(f1,f2,y) else
        begin
        term:= (f2/2-1)*ln(1-y) + (f1/2)*ln(y)
            + lnGamma(f1+f2) - lnGamma(f1+2);
        term:=term - lnGamma(f2);
        if term > -1000 then term:=exp(term) else term:=0;
        if (term<1E-35) and (y<f1/(f1+f2)) then sum:=0
        else if  (term<1E-35) and (y>f1/(f1+f2)) then sum:=1
        else
            begin
            k:=0; sum:=0;
            while (abs(term)>1E-25) or (y*(f2-k) > (1-y)*(f1+k)) do
                begin sum:=sum+term;
                k:=k+2;
                term:= term*y*(f2-k)/(1-y)/(f1+k);
                end;
            end;
        end;
    if intch then sum:=1-sum;
    pBeta:= abs(sum);
    end;
end;
{---------------------------------------------------------------------------}
function pFdistr;

begin
pFdistr:=pBeta(f2,f1,f2/(f1*y+f2));
end;
{---------------------------------------------------------------------------}
function pTdistr;

var  p: extended;

begin
if y=0 then pTdistr:=0.5 else
    begin
    p:=f/(y*y+f);
    p:=pBeta(f,1,p);  p:=p/2;
    if y<0 then p:=1-p;
    pTdistr:=p;
    end;
end;
{---------------------------------------------------------------------------}
function pNormalInv;

var  pp,y,a,b,y0       :extended;

begin
y:= 0;  y0:=1;
pp:=0.5;
while y0>1E-10 do
    begin y0:=y;
    a:=-ln(2*pi)/2-y*y/2;
    b:=y;
    if abs(b)<1E-2 then y:=y+(pp-p)*exp(-a)
    else y:=y+ln(1+b*(pp-p)*exp(-a))/b;
    pp:=pNormal(y);  y0:=abs(y-y0);
    end;
pNormalInv:=y;
end;
{---------------------------------------------------------------------------}
function pGammaInv;

var  pp,y,y0,a,b,a0       :extended;

begin a0:=-lnGamma(f);
if f=1 then
    begin
    y:=pNormalInv(p/2); y:=y*y/2;
    end
else
    begin if f>100 then
        begin y:= sqrt(2*f-1)+pNormalInv(p); y:=y*y/4;
        end
    else y:=f/2;
    y0:=1;
    pp:=pGamma(f,y);
    while y0>1E-7 do
        begin y0:=y;
        a:=a0+(f/2-1)*ln(y)-y;
        b:=(f/2-1)/y-1;
        if abs(b*(pp-p)*exp(-a))<1E-5 then y:=y+(pp-p)*exp(-a)
        else y:=y+ln(1+b*(pp-p)*exp(-a))/b;
        pp:=pGamma(f,y);
        y0:=abs(y-y0);
        end;
    end;
pGammaInv:=y;
end;
{---------------------------------------------------------------------------}
function pChi2Inv;

var y:extended;

begin
y:=pGammaInv(f,p);
pChi2Inv:=2*y;
end;
{---------------------------------------------------------------------------}
function pBetaInv1(f1,f2:longint;p:extended):extended;

var  pp,y,y0,a,b,a0       :extended;

begin
if p<=0 then y:=0
else if p>=1 then y:=1
else if (f1=1) and (f2=1) then y:=sin(p*pi/2)*sin(p*pi/2)
else if (f1=1) and (f2=2) then y:=p*p
else if (f1=2) and (f2=1) then y:=1-(1-p)*(1-p)
else if (f1=2) and (f2=2) then y:=p
else
    begin
    a0:=-lnGamma(f1)-lnGamma(f2); a0:=a0+lnGamma(f1+f2);
    y:=f1/(f1+f2);
    if f1=1 then
        begin
        y:= pGammaInv(1,1-p);
        y:= 2*y/(2*y+f2-1/2);
        end;
    y0:=1;
    pp:=pBeta(f1,f2,y);
    while y0>1E-8 do
        begin
        a:=a0+(f1/2-1)*ln(y)+(f2/2-1)*ln(1-y);
        b:=(f1/2-1)/y-(f2/2-1)/(1-y);
        if abs(b*(pp-p))*exp(-a)<1E-5 then y0:=-(pp-p)*exp(-a)
        else y0:=ln(1-b*(pp-p)*exp(-a))/b;
        y:=y+y0;
        pp:=pBeta(f1,f2,y);
        y0:=abs(y0)/y/(1-y);
        end;
    end;
pBetaInv1:=y;
end;
{---------------------------------------------------------------------------}
function pBetaInv;

var y: extended;

begin if f1<=f2 then y:=pBetaInv1(f1,f2,p)
else y:= 1-pBetaInv1(f2,f1,1-p);
pBetaInv := y;
end;
{---------------------------------------------------------------------------}
function pFdistrInv;

var  y : extended;

begin
y:=pBetaInv(f2,f1,p);
pFdistrInv:=f2/f1*(1-y)/y;
end;
{---------------------------------------------------------------------------}
function pTdistrInv;

var t:extended;

begin if p<=0.5 then t:=sqrt(pFdistrInv(1,f,2*p))
else t:=-sqrt(pFdistrInv(1,f,2*(1-p)));
pTdistrInv:=t;
end;
{---------------------------------------------------------------------------}
function pPoiss;

begin pPoiss:= 1-pGamma(2*n,lambda);
end;
{---------------------------------------------------------------------------}
function pBin;

begin pBin:= pBeta(2*x,2+2*(n-x),p);
end;
{---------------------------------------------------------------------------}
function PoissCL;

begin PoissCL := pGammaInv(2*n,1-p);
end;
{---------------------------------------------------------------------------}
function BinCL;

begin BinCL:= pBetaInv(2*x,2+2*(n-x),pp);
end;
{---------------------------------------------------------------------------}

end.