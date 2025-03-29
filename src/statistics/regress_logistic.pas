unit regress_logistic;

{$mode ObjFPC}{$H+}

{
 This is not the logistic regression we want. It fits the model:
            B - A
y = A + -----------------
        1 + exp(-a.x + b)

and expects x to have real values (not just 0,1)
we can constrain A to be 0, but the model estimates B
}

interface

uses
  Classes, SysUtils, outputcreator, epidatafilestypes, epidatafiles,
  lmath, utypes, regress_types,
  executor, ast, regress;

type

{ TRegressLogit }

TRegressLogistic = class(TRegressModel)
private
  FLogiIndepV: TVector;

public
  constructor Create(AExecutor: TExecutor; AOutputCreator: TOutputCreator; ST: TRegressCommand); override;
  function GetRegressModelClass(): TRegressClass;
  procedure SetFormula(VarNames: TStrings); override;
  procedure SetIndepV(F: TEpiField; Ix: Integer); override;
  function  Estimate(): UTF8String; override;
  procedure  GetFittedVar(DF: TEpiDatafile; EF: TEpiField); override;
end;

implementation

uses
   epifields_helper, ulogifit, uregtest, uibtdist, uerrors, umatrix, umeansd;

{ TRegressLogistic}

constructor TRegressLogistic.Create(AExecutor: TExecutor; AOutputCreator: TOutputCreator; ST: TRegressCommand);
begin
  inherited Create(AExecutor, AOutputCreator, ST);
end;

function TRegressLogistic.GetRegressModelClass(): TRegressClass;
begin
  result := TRegressLogistic;
end;

procedure TRegressLogistic.SetFormula(VarNames: TStrings);
var
  i: Integer;
  plusSign: String;
begin
  inherited SetFormula(Varnames);
  DimVector(FCoeff,3);
  setLength(FB, 4);
  FParamCt := 4;
  FConstant := false;
  plusSign := '';
  {if (FConstant) then begin
    // add constant term to model
    FB[0].pLabel := 'Intercept';
    FModel += '';
  end;  }
  FB[2].Name := 'Intercept';
  FB[2].pLabel := 'Intercept';
  FB[3].Name := VarNames[1];
  {for i := 1 to 2 do begin
    FB[i].Name := VarNames[i];
    FModel += plusSign + VarNames[i];
    plusSign := ' + ';
  end;  }
  FModel := 'Logit(' + VarNames[1] + ')';
end;

procedure TRegressLogistic.SetIndepV(F: TEpiField; Ix: Integer);
var
  i: Integer;
begin
  if (FObs = 0) then
    FObs := F.Size;
  FB[3].pLabel := F.GetVariableLabel(FVariableLabelOutput);
  DimVector(FLogiIndepV, FObs - 1);
  for i := 0 to FObs - 1 do
    FLogiIndepV[i] := F.AsFloat[i];
end;

function TRegressLogistic.Estimate(): UTF8String;
var
  InV: TMatrix;
  i, i0: Integer;
  s, t: EpiFloat;
begin
  DimMatrix(Inv, 3, 3) ;//DimMatrix(Inv, FParamCt, FParamCt);
  // get betas
  LogiFit(FLogiIndepV, FDepV, 0, FObs-1, false, false, 20, 0.00000001, FCoeff, InV);
  if (MathErr = MathOk) then
    Result := ''
  else
    begin
      // debug diagnostics
      Result := 'Logistic Regression ' + FModel + ' ' + 'failed with error' + ' : ' + MathErrMessage;
      s := Min(FDepV); t := Max(FDepV);
      Result += LineEnding + 'Dep var min max are ' + s.ToString() + ' ' + t.ToString();
      s := Min(FLogiIndepV); t := Max(FLogiIndepV);
      Result += LineEnding + 'Indep var min max are ' + s.ToString() + ' ' + t.ToString();
      FExecutor.Error(Result);
      exit;
    end;
  // get fitted values
  DimVector(FFitted, FObs - 1);
  DimVector(FResidual,FObs - 1);
  for i := 0 to FObs - 1 do
    begin
      FFitted[i] := LogiFit_Func(FLogiIndepV[i], FCoeff);
    end;
  VecSubtr(FFitted, FDepV, FResidual);
  RegTest(FDepV, FFitted, 0, FObs-1, InV, 1, 3, FRegFit);
  for i:= 1 to 3 do begin
    FB[i].Estimate := FCoeff[i];
    FB[i].Se := sqrt(Inv[i,i]);
    FB[i].t := FCoeff[i] / sqrt(Inv[i,i]);
    FB[i].p := PStudent(FRegFit.Nu2,FB[i].t);
  end;
  {// Anova
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
  }
end;

procedure  TRegressLogistic.GetFittedVar(DF: TEpiDatafile; EF: TEpiField);
var
  i, j: Integer;
  b: EpiFloat;
  v: TEpiField;
begin
  EF.ResetData;
  v := DF.Fields.FieldByName[FB[j].Name];
  for i := 0 to DF.Size -1 do
    EF.AsFloat[i] := LogiFit_Func(v.AsFloat[i], FCoeff);
end;

initialization
  RegisterRegressModel(rtLogistic, TRegressLogistic);

end.

