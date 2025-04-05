unit regress_polynomial;

{$mode ObjFPC}{$H+}

{
Note: polynomial model always has a constant
}

interface

uses
  Classes, SysUtils, outputcreator, epidatafilestypes,epidatafiles,
  lmath, utypes, regress_types,
  executor, ast, regress;

type

{ TRegressPolynomial }

TRegressPolynomial = class(TRegressModel)
private
  FDegree: Integer;
  FPolyIndepV: TVector;
public
  constructor Create(AExecutor: TExecutor; AOutputCreator: TOutputCreator; ST: TRegressCommand); override;
  function GetRegressModelClass(): TRegressClass;
  procedure SetFormula(VarNames: TStrings); override;
  procedure SetIndepV(F: TEpiField; Ix: Integer); override;
  function Fit(x: EpiFloat): EpiFloat; 
  function  Estimate(): UTF8String; override;
  procedure  GetFittedVar(DF: TEpiDatafile; EF: TEpiField); override;
end;

implementation

uses
   epifields_helper, upolfit, uregtest, uibtdist, uerrors, umatrix;

{ TRegressPolynomial}

constructor TRegressPolynomial.Create(AExecutor: TExecutor; AOutputCreator: TOutputCreator; ST: TRegressCommand);
var
  opt: TOption;
begin
  inherited Create(AExecutor, AOutputCreator, ST);
  ST.HasOption('poly', opt);
  FDegree := opt.Expr.AsInteger;
  FConstant := true;
end;

function TRegressPolynomial.GetRegressModelClass(): TRegressClass;
begin
  result := TRegressPolynomial;
end;

function TRegressPolynomial.Fit(x: EpiFloat): EpiFloat;
var
  i, j: Integer;
  m, v: EpiFloat;
begin
  if (x = TEpiFloatField.DefaultMissing) then
    begin
      result := TEpiFloatField.DefaultMissing;
      exit;
    end;
  m := 1;
  result := 0;
  for j := 0 to FDegree do
    begin
      result += m * FCoeff[j];
      m := m * x;
    end;
end;

procedure TRegressPolynomial.SetFormula(VarNames: TStrings);
var
  i: Integer;
  plusSign, exponent: String;
  xTerm: UTF8String;
begin
  inherited SetFormula(Varnames);
  setLength(FB, 1 + FDegree);
  DimVector(FCoeff,1 + FDegree);
  plusSign := '';
  FB[0].pLabel := 'Intercept';
  FModel += 'c + ';
  exponent := '';
  xTerm := VarNames[1];
  for i := 1 to FDegree do begin
    FB[i].pLabel := xTerm + exponent;
    FModel += plusSign + xTerm + exponent;
    plusSign := ' + ';
    exponent := '^' + (i+1).ToString;
  end;
end;

procedure TRegressPolynomial.SetIndepV(F: TEpiField; Ix: Integer);
var
  i: Integer;
begin
  if (FObs = 0) then
    FObs := F.Size;
  if (Length(FB) <= FDegree) then
    setLength(FB,FDegree + 1);
  FB[1].pLabel := F.GetVariableLabel(FVariableLabelOutput);
  DimVector(FPolyIndepV, FObs - 1);
  for i := 0 to FObs - 1 do
    FPolyIndepV[i] := F.AsFloat[i];
end;

function TRegressPolynomial.Estimate(): UTF8String;
var
  InV: TMatrix;
  i, i0: Integer;
  msg: UTF8String;
begin
  DimMatrix(Inv, FDegree+1, FDegree+1);
  // get betas
  PolFit(FPolyIndepV, FDepV, 0, FObs-1, FDegree, FCoeff, InV);
  if (MathErr = MathOk) then
    Result := ''
  else
    begin
      Result := 'Regression' + ' ' + 'failed with error' + ' : ' + MathErrMessage;
      exit;
    end;
  // get fitted values
  DimVector(FFitted, FObs - 1);
  for i := 0 to FObs - 1 do
    FFitted[i] := Fit(FPolyIndepV[i]);
  // get residuals
  DimVector(FResidual,FObs - 1);
  VecSubtr(FFitted, FDepV, FResidual);
  RegTest(FDepV, FFitted, 0, FObs-1, InV, 0, FParamCt, FRegFit);
  FFp := 1 - FSnedecor(FRegFit.Nu1, FRegFit.Nu2, FRegFit.F);

  for i:= 0 to FDegree do begin
    FB[i].Estimate := FCoeff[i];
    FB[i].Se := sqrt(Inv[i,i]);
    FB[i].t := FCoeff[i] / sqrt(Inv[i,i]);
    FB[i].p := PStudent(FRegFit.Nu2,FB[i].t);
  end;
  // Anova
  if (FDoAnova) then with FAnova do
    begin
      SST := getSS(FDepV, 0, FObs-1);
      SSW := getSS(FResidual, 0, FObs-1);
      SSB := SST - SSW;
      DFW := FRegFit.Nu2;
      DFB := FRegFit.Nu1;
      MSW := SSW / DFW;
      MSB := SSB / DFB;
      DFT := DFW + DFB;
      F   := MSB / MSW;
    end;
end;

procedure  TRegressPolynomial.GetFittedVar(DF: TEpiDatafile; EF: TEpiField);
var
  i, j: Integer;
  b: EpiFloat;
  v: TEpiField;
begin
  EF.ResetData;
  v := DF.Fields.FieldByName[FB[j].Name];
  for i := 0 to DF.Size -1 do
    EF.AsFloat[i] := Fit(v.AsFloat[i]);
end;

initialization
  RegisterRegressModel(rtPolynomial, TRegressPolynomial);

end.

