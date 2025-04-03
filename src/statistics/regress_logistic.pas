unit regress_logistic;

{$mode ObjFPC}{$H+}

{ ******************************************************************
  This unit fits the logistic function :

                                     1
                        y = ----------------------
                            1 + exp(-∑(b[i]*x[i]))

  ******************************************************************
  Modern iteration solution
  See https://en.wikipedia.org/wiki/Logistic_regression
  Binary logistic regression (y=0 or y=1) can, for example, be calculated using
  iteratively reweighted least squares (IRLS), which is equivalent to maximizing
  the log-likelihood of a Bernoulli distributed process using Newton's method.
  If the problem is written in vector matrix form, with
  X the matrix of independent variables, with 1's in the first column)
  y is the vector of responses (0,1)
  parameters wT = [β0,β1,β2,…]  // T indicates transpose
  explanatory variables x(i) = [1, x1(i), x2(i), …]T
  and expected value of the Bernoulli distribution μ(i) = 1 / (1+e(−wT.x(i))) [i=1..n]

  The parameters can be found using the following iterative algorithm:

  w[k+1] = (XT.Sk.X)^(-1). XT.(S[k].X.w[k] + y - μ[k])

  where S is diag(μ(i)(1-μ(i))) is a diagnonal weighting matrix, and
  μ is the vector of expected values.

  }

interface

uses
  Classes, SysUtils, outputcreator, epidatafilestypes, epidatafiles,
  lmath, utypes, regress_types,
  executor, ast, regress;

type

{ TlRegTest }

TlRegTest = record
  Dn : Float; // Null model deviance
  Dm : Float; // Regression deviance
  Dr : Float; // Residual deviance
end;

{ TRegressLogit }

TRegressLogistic = class(TRegressModel)
private
  Debug: Boolean;
  lrRegTest: TlRegTest;
protected
  { fit of logistic function
    Input parameters:  X, Y     = point coordinates
                       Lb, Ub   = array bounds
                       MaxIter  = max. number of iterations
                       Tol      = tolerance on parameters
    Output parameters: B        = regression parameters
                       V        = inverse matrix }
  procedure epiLogiFit(X        : TMatrix;
                       Y        : TVector;
                       MaxIter  : Integer;
                       Tol      : Float;
                 out   B        : TVector;
                 out   V        : TMatrix);
  function epiLogiFit_Func(X: TMatrix; B : TVector) : TVector;
  function epiLogiIterate(X: TMatrix; Y: TVector; W: TVector; out V:TMatrix): TVector;
  procedure epiLogiRegTest(Y: TVector;   // Y
                           F: TVector;   // Fitted values
                           InV: TMatrix; // do we need?
                      out  lrF: TlRegTest); // output
  procedure dm(s: string; m: TMatrix);
  procedure dv(s: string; m: TVector);
public
  constructor Create(AExecutor: TExecutor; AOutputCreator: TOutputCreator; ST: TRegressCommand); override;
  function GetRegressModelClass(): TRegressClass;
  procedure SetFormula(VarNames: TStrings); override;
  function  Estimate(): UTF8String; override;
  procedure GetFittedVar(DF: TEpiDatafile; EF: TEpiField); override;
  procedure DoOutput(); override;
end;


implementation

uses
  epifields_helper, ulogifit, uregtest, uibtdist, uerrors, umatrix, umeansd, umulfit,
  umath, ulinfit, unlfit, ulineq, uvecfunc, uigmdist, generalutils;

{ TRegressLogistic}

constructor TRegressLogistic.Create(AExecutor: TExecutor; AOutputCreator: TOutputCreator; ST: TRegressCommand);
begin
  inherited Create(AExecutor, AOutputCreator, ST);
  FDoAnova := false;
  FDoVariance := false;
  Debug := false;
end;

procedure TRegressLogistic.dm(s: string; m: TMatrix);
begin
  if (Debug) then
    FExecutor.Error('Matrix ' + s + ' is ' + length(m).ToString + ' by ' + length(m[0]).toString);
end;
procedure TRegressLogistic.dv(s: string; m: TVector);
begin
  if (Debug) then
    FExecutor.Error('Vector ' + s + ' is ' + length(m).ToString);
end;

function TRegressLogistic.GetRegressModelClass(): TRegressClass;
begin
  result := TRegressLogistic;
end;

procedure TRegressLogistic.SetFormula(VarNames: TStrings);
var
  i,l : Integer;
  v: UTF8String;
begin
  inherited SetFormula(Varnames);
  l := VarNames.Count;
  DimVector(FCoeff,l-1);
  setLength(FB, l);
//  FParamCt := l;
  FB[0].Name := 'Constant';
  FB[0].pLabel := 'Constant';
  v := 'c';
  for i := 1 to l - 1 do
    begin
      FB[i].Name := VarNames[i];
      v += ' + ' + VarNames[i];
    end;
  FModel += 'Logit(' + v + ')';
end;

procedure TRegressLogistic.DoOutput();
var
  T, V: TOutputTable;
  i,
  offset: Integer;
begin
  T := FOutputCreator.AddTable;
  T.Header.Text := LineEnding + sRegTitle + LineEnding + LineEnding +
                   sRegModel + ' : ' + FModel + LineEnding + LineEnding ;
  T.ColCount := 5;
  T.RowCount := Length(FB) + 1;
  // Header row
  T.Cell[0,0].Text := sRegTerm;
  T.Cell[1,0].Text := sRegCoefficient;
  T.Cell[2,0].Text := 'Variance';
  T.Cell[3,0].Text := 'Wald Chi^2';
  T.Cell[4,0].Text := 'p';
  offset := 1;

  for i := 0 to High(FB) do begin
    T.Cell[0,offset].Text := FB[i].pLabel;
    T.Cell[1,offset].Text := StatFloatDisplay(StatFmt, FB[i].Estimate);
    T.Cell[2,offset].Text := StatFloatDisplay(StatFmt, FB[i].se);
    T.Cell[3,offset].Text := StatFloatDisplay(StatFmt, FB[i].t);
    T.Cell[4,offset].Text := FormatP(FB[i].p, false);
    offset += 1;
  end;

  V := FOutputCreator.AddTable;
  V.Header.Text := 'Analysis of Deviance';
  V.ColCount := 4;
  V.RowCount := 4;
  V.Cell[0,0].Text := 'Source';
  V.Cell[1,0].Text := 'df';
  V.Cell[2,0].Text := 'Deviance';
  V.Cell[3,0].Text := 'p';
  V.Cell[0,1].Text := 'Regression';
  V.Cell[1,1].Text := (FParamCt-1).ToString;
  V.Cell[2,1].Text := StatFloatDisplay(StatFmt, lrRegTest.Dm);
  V.Cell[0,2].Text := 'Residual';
  V.Cell[1,2].Text := (FObs - FParamCt).ToString;
  V.Cell[2,2].Text :=  StatFloatDisplay(StatFmt, lrRegTest.Dr);
  V.Cell[0,3].Text := 'Total';
  V.Cell[1,3].Text := (FObs - 1).ToString;
  V.Cell[2,3].Text :=  StatFloatDisplay(StatFmt, lrRegTest.Dn);
end;

procedure TRegressLogistic.epiLogiRegTest(Y: TVector; F: TVector;
                                          InV: TMatrix; out lrF: TlRegTest);
var
  LLm, LLn: Float; // log-likelhood of model and null model
  d: Float;
  i, n, s: Integer;
begin
  LLm := 0;
  s := 0;
  n := length(Y);
  for i := 1 to n - 1 do
    begin
      if (Y[i] = 1) then
        begin
          LLm += ln(1/F[i]);
          inc(s);
        end
      else begin
        LLm += ln(1/(1-F[i]));
      end;
    end;
  { calculate null model deviance
  b[0] = ln(Ym/(1-Ym), where Ym is mean of Y's  (Sum(Y) / n)
  max log likelihood = n(Ym x ln(Ym) + (1-Ym) x ln(1-Ym))
  model log likelihood = sum(Y x ln(MU) + (1-Y)ln(1-MU))
  }
  d := s.ToDouble / n.ToDouble;
  LLn := n*(d*ln(d)+(1-d)*ln(1-d));
  lrF.Dn := -2 * LLn;
  lrF.Dr :=  2 * LLm;
  lrF.Dm := lrF.Dn - lrF.Dr;
end;

function TRegressLogistic.Estimate(): UTF8String;
var
  InV: TMatrix;
  i, i0: Integer;
  s, t: EpiFloat;
begin
  FConstant := true;
  DimMatrix(Inv, FParamCt-1, FParamCt-1);
  // get betas
  dm('FIndepV',FIndepV);
  epiLogiFit(FIndepV, FDepV, 60, 0.000000001, FCoeff, InV);
  dv('FCoeff',FCoeff);
  if (MathErr = MathOk) then
    Result := ''
  else
    begin
      FExecutor.Error(Result);
      exit;
    end;
  // get fitted values
  DimVector(FFitted, FObs - 1);
  DimVector(FResidual,FObs - 1);
  FFitted := epiLogiFit_Func(FIndepV, FCoeff);
  FResidual := FFitted - FDepV;
  epiLogiRegTest(FDepV, FFitted, InV, lrRegTest);
  for i:= 0 to FParamCt-1 do begin
    FB[i].Estimate := FCoeff[i];
    FB[i].Se := sqrt(Inv[i,i]);
    FB[i].t := FCoeff[i] * FCoeff[i] / Inv[i,i];
    FB[i].p := PKhi2(1, FB[i].t);
  end;
end;

procedure  TRegressLogistic.GetFittedVar(DF: TEpiDatafile; EF: TEpiField);
var
  i, j: Integer;
  b: EpiFloat;
  v: TEpiField;
begin
  EF.ResetData;
  for i := 0 to DF.Size-1 do
    begin
      EF.AsFloat[i] := FCoeff[0];
    end;
  for j := 1 to length(FB) -1 do
    begin
      v := DF.Fields.FieldByName[FB[j].Name];
      b := FCoeff[j];
      for i := 0 to DF.Size -1 do
        begin
        if not (EF.IsMissing[i]) then
          if (v.IsMissing[i]) then
            EF.IsMissing[i] := true
          else
            EF.AsFloat[i] := EF.AsFloat[i] + (v.AsFloat[i] * b);
        end;
    end;
  for i := 0 to DF.Size-1 do
    if not (EF.IsMissing[i]) then
      EF.AsFloat[i] := 1 / (1 + Expo(-EF.AsFloat[i]));
end;

  function TRegressLogistic.epiLogiFit_Func(X: TMatrix; B : TVector) : TVector;
  { ------------------------------------------------------------------
    Computes the regression function at point X.
    B is the vector of parameters
    ------------------------------------------------------------------ }
  var
    D : Float;
    i: Integer;
  begin
    DimVector(result,FObs - 1);
    dm('X in Fit_Func',X);
    dv('B in Fit_Func',B);
    result := MatVecMul(X, B, 0);
    if (MathErr <> MathOK) then
      begin
        FExecutor.Error('Error in epiLogiFit_Func: ' + MathErrMessage);
        result := nil;
        exit;
      end;
    for i := 0 to FObs -1 do
      result[i] := 1 / (1 + Expo(-result[i]));
  end;

  function TRegressLogistic.epiLogiIterate(X: TMatrix; Y: TVector; W: TVector; out V:TMatrix): TVector;
  var
    i, j, k, n, l: Integer;
    c, Det: Float;
    Dummy: TVector;
    S: TMatrix;
    Xt: TMatrix;
    XtS: TMatrix;
    XtSX: TMatrix;
    T: TMatrix;
    SX: TMatrix;
    SXW: TVector;
    MU: TVector;
  begin
    // create diagonal S
    n := length(Y)-1;
    l := length(W)-1;
    DimVector(MU, n);
    DimMatrix(S, n, n);
    DimMatrix(V, l, l);
    DimVector(result, l);
    DimVector(Dummy, l);
    for i := 0 to l do
      Dummy[i] := 0;
    dv('MU',MU);
    dm('S',S);
    dm('V',V);
    dv('result',result);
    MU := epiLogiFit_Func(X, W);
    for i := 0 to n do
      begin
        for j := 0 to n do
          S[i,j] := 0;
      S[i,i] := MU[i] * (1 - MU[i]);
    end;
    // Transpose X
    Xt := MatTranspose(X,0); // should move this out; it never changes
    // one iteration
    XtS := MatMul(Xt,S,0);    // X' S
    XtSX := MatMul(XtS,X,0);  // X' S X

    // alternate method to get X'SX
    for i := 0 to high(V) do
      for j := i to high(V) do
        begin
          c := 0;
          for k := 0 to high(X) do
            c += X[k,i]*s[k,k]*x[k,j];
          V[i,j] := c;
          if (i <> j) then
            V[j,i] := c;
        end;
    dm('XtSX',XtSX);
    // Invert XtSX; // use linear equation solver for this; also get determinant
    LinEq(XtSX,Dummy,0,l,Det);
    V := XtSX;
    if (MathErr <> MathOK) then
      begin
        FExecutor.Error('Error inverting matrix: ' + MathErrMessage);
        exit;
      end;
    dm('XtSX',XtSX);
    T := MatMul(XtSX,XT,0);     // (X' S X)^-1 XT
    SX := MatMul(S,X,0);        // S X
    SXW := MatVecMul(SX, W, 0);
    if(MathErr<>MathOK) then
      FExecutor.Error('Error in LogIterate: ' + MathErrMessage);
//    dv('SXW in LogIterate after MatVecMul',SXW);
    dv('Y',Y);
    dv('MU',MU);
    SXW := SXW + Y;
    SXW := SXW - MU;
    dm('T in LogIterate',T);
    dm('SX in LogIterate',SX);
    dm('XT in LogIterate',XT);
    dv('W in LogIterate',W);
    dv('SXW in LogIterate',SXW);
    result := MatVecMul(T, SXW, 0);
    dv('result at end of LogiIterate',result);
  end;

  procedure TRegressLogistic.epiLogiFit(X: TMatrix; Y: TVector;
            MaxIter: Integer; Tol: Float; out B: TVector; out V: TMatrix);
  var
    i, l, iter: Integer;
    M: TVector;
    D: TVector;
    T: Float;
  begin
    if MaxIter = 0 then Exit;
    l := FParamCt-1;
    DimVector(M,l);
    DimVector(D,l);
    dv('D in logiFit',D);
    dv('B in logiFit',B);
    dv('M in logiFit',M);
    M[0] := 1;
    for i := 1 to l do
      M[i] := 0;
    for i := 0 to MaxIter do
      begin
        B := epiLogiIterate(X, Y, M, V);
        D := M - B;
        M := B;
        dv('D in logiFit 2',D);
        dv('B in logiFit 2',B);
        dv('M in logiFit 2',M);
        VecAbs(D, 0, l);
        T := max(D);
//        FExecutor.Error('Iteration ' + i.ToString + ' T=' + T.ToString);
//        FExecutor.Error('FCoeff: ' + B[0].ToString + ' ' + B[1].ToString + ' ' + B[2].ToString + ' ' + B[3].ToString);
        if (T < Tol) then
          exit;
      end;
  end;

  initialization
  RegisterRegressModel(rtLogistic, TRegressLogistic);

end.

