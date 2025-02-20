unit regress;

{$codepage UTF-8}
{$mode objfpc}{$H+}
 // TODO: Jamie  create DoCalcRegress procedure!
 interface

uses
  Classes, SysUtils, StrUtils, ast, epidatafiles, epidatafilestypes, epicustombase,
  executor, result_variables, interval_types, epifields_helper, ana_globals,
  lmath, umulfit, ulinfit, utypes, uregtest, uerrors,
  outputcreator;

type

  { TRegressParameter }

  TRegressParameter = record
    Name: UTF8String;
    Estimate: Float;
    Se: Float;
    t: Float;
    p: Float
  end;

  { TRegress }

  TRegress = class
  private
    FDecimals: Integer;
    FConf: Integer;
    FValuelabelOutput: TEpiGetValueLabelType;
    FVariableLabelOutput: TEpiGetVariableLabelType;
    FVariables: TStrings;
    FDepVarName: UTF8String;
    FConstant: Boolean;
    FModel: UTF8String;
    FObs,
    FNVar: Integer;
    FRegFit: TRegTest;
    FDepV: TVector;
    FIndepV: TMatrix;
    FFitted: TVector;
    FResidual: TVector;
    FBeta: TVector;
    FB: array of TRegressParameter;
    FFp: Float;
  protected
    FExecutor: TExecutor;
    FOutputCreator: TOutputCreator;
    procedure DoResultVariables(); virtual;
    procedure DoOutputRegress(); virtual;
    function DoCalcRegress(): Boolean;
    procedure SaveVar(f: TEpiField; V: TVector);
  public
    constructor Create(AExecutor: TExecutor; OutputCreator: TOutputCreator);
    destructor Destroy; override;

    // Method called from Executor, does calculation + result vars + output
    procedure ExecRegress(ST: TRegressCommand);
//    procedure ExecRegress(DataFile: TEpiDataFile; ST: TRegressCommand);
{    // Method to be used from elsewhere. Does only calculations and returns the result as a specialized dataset
    function CalcRegress(DataFile: TEpiDataFile; Const CountVarName, StratifyVarName: UTF8String;
      ValueLabelOutput: TEpiGetValueLabelType = gvtValue; VariableLabelOutput: TEpiGetVariableLabelType = gvtVarName): TRegressDatafile;
}
  end;

  {LMath helper functions}

  function FieldToVector(F: TEpiField; Ub, Ix: Integer): TVector;
  procedure FieldToMatrix(M: TMatrix; F: TEpiField; Ub, Ix: Integer);
  function CalcEstimates(DepV: TVector; Nobs: Integer; IndepV: TMatrix; Beta: TVector;
    Constant: Boolean; out Resid: TVector): TVector;
implementation

uses
  generalutils, Math, options_utils,
  uibtdist;

{ TRegress }

procedure TRegress.DoResultVariables();
var
  lrBeta,
  lrSErr,
  lrt,
  lrp: TCustomExecutorDataVariable;
  i: Integer;
  Prefix: String;
begin
  Prefix := '$regress_';
  with FExecutor do begin
    AddResultConst(Prefix + 'model', ftString).AsStringVector[0]  := FModel;
    AddResultConst(Prefix + 'nvar',  ftInteger).AsIntegerVector[0] := FNVar;
    AddResultConst(Prefix + 'Vr',    ftFloat).AsFloatVector[0]     := FRegFit.Vr;
    AddResultConst(Prefix + 'R2',    ftFloat).AsFloatVector[0]     := FRegFit.R2;
    AddResultConst(Prefix + 'R2a',   ftFloat).AsFloatVector[0]     := FRegFit.R2a;
    AddResultConst(Prefix + 'F',     ftFloat).AsFloatVector[0]     := FRegFit.F;
    AddResultConst(Prefix + 'df1',   ftInteger).AsIntegerVector[0] := FRegFit.Nu1;
    AddResultConst(Prefix + 'df2',   ftInteger).AsIntegerVector[0] := FRegFit.Nu2;
    AddResultConst(Prefix + 'Fp',    ftFloat).AsFloatVector[0]     := FFp;
    lrBeta := AddResultVector(Prefix + 'beta', ftFloat, FNVar);
    lrSErr := AddResultVector(Prefix + 'se', ftFloat, FNVar);
    lrt := AddResultVector(Prefix + 't', ftFloat, FNVar);
    lrp := AddResultVector(Prefix + 'tp', ftFloat, FNVar);
    for i := 0 to FNVar - 1 do begin
      lrBeta.AsFloatVector[i] := FB[i].Estimate;
      lrSerr.AsFloatVector[i] := FB[i].Se;
      lrt.AsFloatVector[i] := FB[i].t;
      lrp.AsFloatVector[i] := FB[i].p;
    end;
  end;
end;

procedure TRegress.DoOutputRegress();
var
  T: TOutputTable;
  StatFmt: String;
  Offset, i, i0: Integer;
//  maxVforDisplay: Extended;

  function StatFloatDisplay(const fmt: String; const val: EpiFloat):string;
  begin
    if (val = TEpiFloatField.DefaultMissing) then
      Result := TEpiStringField.DefaultMissing
    else
      Result := Format(fmt, [val]);
  end;

begin
  T := FOutputCreator.AddTable;
  T.Header.Text := LineEnding + 'Linear regression' + LineEnding + LineEnding +
                   'Model: ' + FModel + LineEnding + LineEnding ;
  StatFmt := '%8.' + IntToStr(FDecimals) + 'F';
  T.ColCount := 5;
  T.RowCount := FNVar;
  // Header row
  T.Cell[0,0].Text := 'Term';
  T.Cell[1,0].Text := 'Coefficient';
  T.Cell[2,0].Text := 's.e.';
  T.Cell[3,0].Text := 't';
  T.Cell[4,0].Text := 'p';
  Offset := 1;
  if (FConstant) then begin
    T.RowCount := T.RowCount + 1;
    i0 := 0;
    end
  else
      i0 := 1;

    for i := i0 to FNVar - 1 do begin
      T.Cell[0,Offset].Text := FB[i].Name;
      T.Cell[1,Offset].Text := StatFloatDisplay(StatFmt, FB[i].Estimate);
      T.Cell[2,Offset].Text := StatFloatDisplay(StatFmt, FB[i].se);
      T.Cell[3,Offset].Text := StatFloatDisplay(StatFmt, FB[i].t);
      T.Cell[4,Offset].Text := FormatP(FB[i].p, false);
      Offset += 1;
    end;

  T.Footer.Text := 'R^2 = ' + StatFloatDisplay(StatFmt, FRegFit.R2) +
            '  Adjusted R^2 = ' + StatFloatDisplay(StatFmt, FRegFit.R2a) +
            LineEnding +
            'F = ' + StatFloatDisplay(StatFmt, FRegFit.F) +
            ' on ' + FRegFit.Nu1.ToString + ' and ' + FRegFit.Nu2.ToString + ' df (' +
            FormatP(FFp, true) + ')';

end;

constructor TRegress.Create(AExecutor: TExecutor;
  OutputCreator: TOutputCreator);
begin
  FExecutor := AExecutor;
  FOutputCreator := OutputCreator;
end;

destructor TRegress.Destroy;
begin
  inherited Destroy;
end;

function TRegress.DoCalcRegress(): Boolean;
var
  Inv: TMatrix;
  i: Integer;
  i0: Integer;
begin
  Result := false;
  DimVector(FBeta, FNVar-1);
  DimMatrix(InV, FNVar-1, FNVar-1);
  MulFit(FIndepV, FDepV, 0, FObs-1, FNVar-1, FConstant, FBeta, InV);
  if (MathErr = MathOk) then
    Result := true
  else
    FExecutor.Error('Regression failed with error: ' + MathErrMessage);
  if (Result) then begin
    // calculate y-estimates and residuals
    FFitted := CalcEstimates(FDepV, FObs, FIndepV, FBeta, FConstant, FResidual);
    // get the quality of fit  unit uregtest
    RegTest(FDepV, FFitted, 0, FObs - 1, InV, 0, FNVar - 1, FRegFit);
    FFp := 1 - FSnedecor(FRegFit.Nu1, FRegFit.Nu2, FRegFit.F);
    if (FConstant) then
      i0 := 0
    else
      i0 := 1;
    for i:= i0 to FNVar-1 do begin
      FB[i].Estimate := FBeta[i];
      FB[i].Se := sqrt(Inv[i,i]);
      FB[i].t := FBeta[i] / sqrt(Inv[i,i]);
      FB[i].p := PStudent(FRegFit.Nu2,FB[i].t);
    end;
  end;
end;

procedure TRegress.SaveVar(f: TEpiField; V: TVector);
var
  i: Integer;
begin
  f.ResetData;
  for i := 0 to length(V) -1 do
    f.AsFloat[i] := V[i];
end;

procedure TRegress.ExecRegress(ST: TRegressCommand);
var
  DF: TEpiDataFile;
  Opt: TOption;
  i: Integer;
  plusSign: String;
  saveField: TEpiField;
begin
  FExecutor.ClearResults('$regress');
  FConf := StrToInt(FExecutor.SetOptionValue[ANA_SO_CONFIDENCE_INTERVAL]);
  FDecimals := DecimalFromOption(ST.Options, 4);
  FVariableLabelOutput := VariableLabelTypeFromOptionList(ST.Options, FExecutor.SetOptions);
  FValuelabelOutput    := ValueLabelTypeFromOptionList(ST.Options, FExecutor.SetOptions);
  FVariables := ST.VariableList.GetIdentsAsList;
  FDepVarName := FVariables[0];
  ST.ExecResult := csrFailed;

  try
    DF := FExecutor.PrepareDatafile(FVariables, FVariables);
    FObs := DF.Size;

    if FObs = 0 then
      begin
        FExecutor.Error('No data!');
        DF.Free;
        Exit;
      end;

    FNVar := FVariables.Count;
    FConstant := not (ST.HasOption('nocon'));
    SetLength(FB,FNVar);
    FModel := FVariables[0] + ' ~ ';
    plusSign := '';
    if (FConstant) then begin
      FB[0].Name := 'Intercept';
    end;

    DimVector(FDepV,FObs - 1);
    DimMatrix(FIndepV,FObs - 1, FNVar);
    FDepV := FieldToVector(DF.Fields.FieldByName[FVariables[0]], FObs-1, 0);
    for i := 1 to FNVar - 1 do begin
      FieldToMatrix(FIndepV, DF.Fields.FieldByName[FVariables[i]], FObs - 1, i);
      FB[i].Name := FVariables[i];
      FModel += plusSign + FVariables[i];
      plusSign := ' + ';
    end;

    if (DoCalcRegress()) then begin
      if (ST.HasOption('est',Opt)) then
        begin
          saveField := FExecutor.DataFile.Fields.FieldByName[Opt.Expr.AsIdent];
          SaveVar(saveField, FFitted);
        end;

      if (ST.HasOption('res',Opt)) then
        begin
          saveField := FExecutor.DataFile.Fields.FieldByName[Opt.Expr.AsIdent];
          SaveVar(saveField, FResidual);
        end;

      DoResultVariables();

      if (not ST.HasOption('q')) then
        DoOutputRegress();

      ST.ExecResult := csrSuccess;
    end;
    DF.Free;
  finally
end;

end;

{ // Omit direct call
function TRegress.CalcRegress(DataFile: TEpiDataFile;
  const DependentVarName: UTF8String;
  IndependentVarNames: array of UTF8String;
  ValueLabelOutput: TEpiGetValueLabelType;
  VariableLabelOutput: TEpiGetVariableLabelType): TRegressDatafile;
begin
  FValuelabelOutput := ValueLabelOutput;
  FVariableLabelOutput := VariableLabelOutput;

  Result := DoCalcRegress(DataFile, DependentVarName, IndependentVarNames);
end;  }

function CalcEstimates(DepV: TVector; Nobs: Integer; IndepV: TMatrix; Beta: TVector;
  Constant: Boolean; out Resid: TVector): TVector;
var
  i, j: Integer;
  x, c: Float;
begin
  DimVector(Result, Nobs-1);
  DimVector(Resid, Nobs-1);
  if (Constant) then
    c := Beta[0]
  else
    c := 0;
  for i := 0 to Nobs-1 do begin
    x := c;
    for j := 1 to Length(Beta)-1 do
      x += Beta[j] * IndepV[i,j];
    Result[i] := x;
    Resid[i] := DepV[i] - x;
  end;
end;

function FieldToVector(F: TEpiField; Ub, Ix: Integer): TVector;
var
  i: Integer;
begin
  DimVector(Result, Ub);
  for i := 0 to Ub do
    Result[i] := F.AsFloat[i];
end;

procedure FieldToMatrix(M: TMatrix; F: TEpiField; Ub, Ix: Integer);
var
  i: Integer;
begin
    for i := 0 to Ub do
      M[i,Ix] := F.AsFloat[i];
end;

end.
