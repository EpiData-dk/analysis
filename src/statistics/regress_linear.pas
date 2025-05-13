unit regress_linear;

{$mode ObjFPC}{$H+}

{ Note: with !nocon, RegTest results are OK for beta and se(beta),
        but other results are meaningless (R2, aR2, F)
  For now, make them missing in results
  R will calculate R2 differently with no constant, so should we?
  It's unclear what NIST has done, but something to explore

  Also, if Vr is very, very small (arbitrarily less than MinVarianceResidual
  set F = +Inf and p = zero
}

interface

uses
  Classes, SysUtils, outputcreator, epidatafilestypes, epidatafiles,
  lmath, utypes, regress_types,
  executor, ast, regress;

type

{ TRegressLinear }

TRegressLinear = class(TRegressModel)
public
  constructor Create(AExecutor: TExecutor; AOutputCreator: TOutputCreator; ST: TRegressCommand); override;
  function GetRegressModelClass(): TRegressClass;
  procedure SetFormula(VarNames: TStrings); override;
  function Fitted(Msg: UTF8String): TVector; override;
  function  Estimate(): UTF8String; override;
  procedure  GetFittedVar(DF: TEpiDatafile; EF: TEpiField); override;
end;

implementation

uses
   umulfit, uregtest, uibtdist, uerrors, umatrix;

{ TRegressLinear}

constructor TRegressLinear.Create(AExecutor: TExecutor; AOutputCreator: TOutputCreator; ST: TRegressCommand);
begin
  inherited Create(AExecutor, AOutputCreator, ST);
end;

function TRegressLinear.GetRegressModelClass(): TRegressClass;
begin
  result := TRegressLinear;
end;

function TRegressLinear.Fitted(Msg: UTF8String): TVector;
begin
  Msg := '';
  Result := MatVecMul(FIndepV, FCoeff, 0);
  if (MathErr <> MathOk) then
    Msg := sErrMatMult + ' : ' + MathErrMessage;
end;

procedure TRegressLinear.SetFormula(VarNames: TStrings);
var
  i: Integer;
  plusSign: String;
begin
  inherited SetFormula(Varnames);
  DimVector(FCoeff,FParamCt-1);
  setLength(FB, FParamCt);
  plusSign := '';
  if (FConstant) then begin
    // add constant term to model
    FB[0].pLabel := 'Intercept';
    FModel += 'c + ';
  end;
  for i := 1 to FParamCt-1 do begin
    FB[i].Name := VarNames[i];
    FModel += plusSign + VarNames[i];
    plusSign := ' + ';
  end;
end;

function TRegressLinear.Estimate(): UTF8String;
var
  InV: TMatrix;
  i, i0: Integer;
  cf: Float;
begin
  DimMatrix(Inv, FParamCt-1, FParamCt-1);
  // get betas
  MulFit(FIndepV, FDepV, 0, FObs-1, FParamCt-1, FConstant, FCoeff, InV);
  if (MathErr = MathOk) then
    Result := ''
  else
    begin
      Result := sRegCommand + ' ' + sFailedErr + ': ' + MathErrMessage;
      exit;
    end;
  // get fitted values
  FFitted := MatVecMul(FIndepV, FCoeff, 0);
  if (MathErr <> MathOk) then
    begin
      Result := sErrMatMult + ' : ' + MathErrMessage;
      exit;
    end;
  // get residuals
  DimVector(FResidual,FObs - 1);
  VecSubtr(FFitted, FDepV, FResidual);
  if (FConstant) then
    i0 := 0
  else
    i0 := 1;
  RegTest(FDepV, FFitted, 0, FObs-1, InV, i0, FParamCt-1, FRegFit);
  if (FConstant and (FRegFit.Vr > MinVarianceResidual)) then
    FFp := 1 - FSnedecor(FRegFit.Nu1, FRegFit.Nu2, FRegFit.F)
  else
    begin
      FRegFit.F := TEpiFloatField.DefaultMissing;
      FFp := 0;
    end;
  // move this to regress_types? Is it always the same? It is for linear models.
  // Result := inherited Estimate();
  for i:= i0 to FParamCt-1 do begin
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

procedure  TRegressLinear.GetFittedVar(DF: TEpiDatafile; EF: TEpiField);
var
  i, j: Integer;
  b: EpiFloat;
  v: TEpiField;
begin
  EF.ResetData;
  if (FConstant) then
    b := FCoeff[0]
  else
    b := 0;
  for i := 0 to DF.Size -1 do
    EF.AsFloat[i] := b;

  for j := 1 to FParamCt-1 do begin
    v := DF.Fields.FieldByName[FB[j].Name];
    b := FCoeff[j];
    for i := 0 to DF.Size -1 do
      if not (EF.IsMissing[i]) then
        if v.IsMissing[i] then
          EF.IsMissing[i] := true
        else
          EF.AsFloat[i] := EF.AsFloat[i] + v.asFloat[i] * b;
  end;
end;

initialization
  RegisterRegressModel(rtLinear, TRegressLinear);

end.

