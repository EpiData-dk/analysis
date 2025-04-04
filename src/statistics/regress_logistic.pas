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
  p  : Float; // Chi-square p (for Dm only)
end;

TlrOdds = record
  oddsRatio : Float;
  llOR      : Float;
  ulOR      : Float;
end;

{ TRegressLogit }

TRegressLogistic = class(TRegressModel)
private
  lrRegTest: TlRegTest;
  F_OR: array of TlrOdds;
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
  function epiLogiIterate(X: TMatrix; Y: TVector; W: TVector; out XtSX:TMatrix): TVector;
  procedure epiLogiRegTest(Y: TVector;   // Y
                           F: TVector;   // Fitted values
                           InV: TMatrix; // do we need?
                      out  lrF: TlRegTest); // output
public
  constructor Create(AExecutor: TExecutor; AOutputCreator: TOutputCreator; ST: TRegressCommand); override;
  function  GetRegressModelClass(): TRegressClass;
  procedure SetDepV(F: TEpiField); override;
  procedure SetFormula(VarNames: TStrings); override;
  function  Estimate(): UTF8String; override;
  procedure GetFittedVar(DF: TEpiDatafile; EF: TEpiField); override;
  procedure DoOutput(); override;
end;


implementation

uses
  generalutils, ana_globals, statfunctions, uerrors, umatrix, umeansd,
  umath, unlfit, ulineq, uvecfunc, uigmdist;

{ TRegressLogistic}

constructor TRegressLogistic.Create(AExecutor: TExecutor; AOutputCreator: TOutputCreator; ST: TRegressCommand);
begin
  inherited Create(AExecutor, AOutputCreator, ST);
  FDoAnova := false;
  FDoVariance := false;
end;

function TRegressLogistic.GetRegressModelClass(): TRegressClass;
begin
  result := TRegressLogistic;
end;

procedure TRegressLogistic.SetDepV(F: TEpiField);
var
  i, n: Integer;
begin
  inherited SetDepV(F);
  n := 0;
  for i := 0 to high(FDepV) do
    if (FDepV[i] = 1) then
      inc(n)
    else if (FDepV[i] <>0) then
      begin
        FExecutor.Error('Dependant variable must have values 0 and 1 only');
        FDataError := true;
        exit;
      end;
  if (n*10 < length(FDepV)) or (n > 0.9 * length(FDepv)) then
    FExecutor.Error('Warning: dependant variable is unbalanced.');
end;

procedure TRegressLogistic.SetFormula(VarNames: TStrings);
var
  i,l : Integer;
  v: UTF8String;
begin
  inherited SetFormula(Varnames);
  l := VarNames.Count;
  DimVector(FCoeff,l-1);
  setLength(F_OR, l);
  setLength(FB, l);
  FB[0].Name := 'Constant';
  FB[0].pLabel := 'Constant';
  v := 'c';
  for i := 1 to high(FB) do
    begin
      FB[i].Name := VarNames[i];
      v += ' + ' + VarNames[i];
    end;
  FModel += 'Logit(' + v + ')';
end;

procedure TRegressLogistic.DoOutput();
var
  T, O, V: TOutputTable;
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

  O := FOutputCreator.AddTable;
  O.Header.Text := 'Odds ratios and ' + IntToStr(FConf) + '% CI';
  O.ColCount := 4;
  O.RowCount := Length(FB);
  O.Cell[0,0].Text := sRegTerm;
  O.Cell[1,0].Text := 'Odds ratio';
  O.Cell[2,0].Text := 'Lower limit';
  O.Cell[3,0].Text := 'Upper limit';

  for i := 1 to High(F_OR) do begin
    O.Cell[0,i].Text := FB[i].pLabel;
    O.Cell[1,i].Text := StatFloatDisplay(StatFmt, F_OR[i].oddsRatio);
    O.Cell[2,i].Text := StatFloatDisplay(StatFmt, F_OR[i].llOR);
    O.Cell[3,i].Text := StatFloatDisplay(StatFmt, F_OR[i].ulOR);
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
  V.Cell[3,1].Text := FormatP(lrRegTest.p, false);
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
  for i := 0 to high(Y) do
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
  lrF.p  := pKhi2(1,lrF.Dr);
end;

function TRegressLogistic.Estimate(): UTF8String;
var
  InV: TMatrix;
  i: Integer;
  zConf: Float;
begin
  FConstant := true;
  DimMatrix(Inv, FParamCt-1, FParamCt-1);
  // get betas
  epiLogiFit(FIndepV, FDepV, 60, 0.000000001, FCoeff, InV);
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
  for i:= 0 to high(FB) do begin
    FB[i].Estimate := FCoeff[i];
    FB[i].Se := sqrt(Inv[i,i]);
    FB[i].t := FCoeff[i] * FCoeff[i] / Inv[i,i];
    FB[i].p := PKhi2(1, FB[i].t);
  end;
  FConf := StrToInt(FExecutor.SetOptionValue[ANA_SO_CONFIDENCE_INTERVAL]);
  Zconf := PNormalInv((1 - (FConf / 100)) / 2);
  for i:=1 to high(F_OR) do begin
    F_OR[i].oddsRatio:=exp(FB[i].Estimate);
    F_OR[i].llOR := exp(FB[i].Estimate - ZConf*FB[i].Se);
    F_OR[i].ulOR := exp(FB[i].Estimate + ZConf*FB[i].Se);
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
  for j := 1 to high(FB) do
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
    i: Integer;
  begin
    DimVector(result,FObs - 1);
    result := MatVecMul(X, B, 0);
    if (MathErr <> MathOK) then
      begin
        FExecutor.Error('Error in epiLogiFit_Func: ' + MathErrMessage);
        result := nil;
        FDataError := true;
        exit;
      end;
    for i := 0 to high(result) do
      result[i] := 1 / (1 + Expo(-result[i]));
  end;

  function TRegressLogistic.epiLogiIterate(X: TMatrix; Y: TVector; W: TVector; out XtSX:TMatrix): TVector;
  var
    i, j, k, n, l: Integer;
    c, Det: Float;
    Dummy: TVector;
    S: TVector;
    SX: TMatrix;
    MU: TVector;
  begin
    n := length(Y)-1;
    l := length(W)-1;
    DimVector(MU, n);
    DimVector(S, n);
    DimMatrix(SX, n, l);
    DimMatrix(XtSX, l, l);
    DimVector(result, l);
//    Dummy := Fill(0,l,0);
    DimVector(Dummy, l);
    for i := 0 to high(Dummy) do
      Dummy[i] := 0;
    MU := epiLogiFit_Func(X, W);
    if (FDataError) then exit;
    // get S as vector, not diagonal matrix
    for i := 0 to high(S) do
      S[i] := MU[i] * (1 - MU[i]);
    // alternate method to get SX
    for i := 0 to High(X) do
      for j := 0 to High(X[0]) do
        SX[i,j] := X[i,j]*S[i];
    // alternate method to get X'SX
    for i := 0 to high(XtSX) do
      for j := i to high(XtSX) do
        begin
          c := 0;
          for k := 0 to high(y) do
            c += X[k,i]*s[k]*X[k,j];
          XtSX[i,j] := c;
          if (i <> j) then
            XtSX[j,i] := c;
        end;
    // Invert XtSX; // use linear equation solver for this; also get determinant
    LinEq(XtSX,Dummy,0,l,Det);
    if (MathErr <> MathOK) then
      begin
        FExecutor.Error('Error inverting matrix: ' + MathErrMessage);
        exit;
      end;
    result := MatVecMul(MatMul(XtSX,MatTranspose(X,0),0),(MatVecMul(SX,W,0)+Y-MU),0);
    exit;
  end;

  procedure TRegressLogistic.epiLogiFit(X: TMatrix; Y: TVector;
            MaxIter: Integer; Tol: Float; out B: TVector; out V: TMatrix);
  var
    i, l: Integer;
    M: TVector;
    D: TVector;
    T: Float;
  begin
    if MaxIter = 0 then Exit;
    l := FParamCt-1;
    DimVector(M,l);
    DimVector(D,l);
    M[0] := 1;
    for i := 1 to high(M) do
      M[i] := 0;
    for i := 0 to MaxIter do
      begin
        B := epiLogiIterate(X, Y, M, V);
        D := M - B;
        M := B;
        VecAbs(D, 0, l);
        T := max(D);
        if (T < Tol) or (FDataError) then
          exit;
      end;
  end;

  initialization
  RegisterRegressModel(rtLogistic, TRegressLogistic);

end.

