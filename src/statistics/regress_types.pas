unit regress_types;

{$interfaces CORBA}
{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, epidatafilestypes, epidatafiles, epifields_helper,
  outputcreator, fgl,
//  Generics.Collection,
  executor, ast, ana_globals, result_variables, interval_types,
  math, lMath, utypes;

resourcestring
  sDegFreedomAbbr      = 'df';
  sFailedErr           = 'failed with error';
  sStErrorAbbr         = 's.e.';
  sRegAdjustedR2       = 'Adjusted R^2';
  sRegCoefficient      = 'Coefficient';
  sRegCommand          = 'Regression';
  sRegErrMatrixMult    = 'Matrix multiplication error';
  sRegFNotSupported    = 'F-test is not supported with no constant term';
  sRegFdf1             = ' on ';
  sRegFdf2             = ' and ';
  sRegIntercept        = 'Intercept';
  sRegModel            = 'Model';
  sRegR2               = 'R^2';
  sRegResidualVariance = 'Residual variance';
  sRegTerm             = 'Term';
  sRegTitle            = 'Regression Analysis';

const
  MinVarianceResidual = 1.0E-25;

type
  TRegressParameter = record
    Name,
    pLabel: UTF8String;
    Estimate: EpiFloat;
    Se,
    t,
    p: EpiFloat
  end;

  TRegressType = (
    rtLinear,
    rtMultilinear,
    rtPolynomial,
    rtLogistic
  );

  { TRegressModel }

  TRegressModel = class
  protected
    FExecutor: TExecutor;
    FOutputCreator: TOutputCreator;
    FObs: Integer;
    FParamCt: Integer;
    FModel: UTF8String;
    FPModel: Pointer;
    FConstant: Boolean;
    FDepVLabel,
    FDepVName: UTF8String;
    FB: array of TRegressParameter;
    FDepV: TVector;
    FIndepV: TMatrix;
    FFitted: TVector;
    FResidual: TVector;
    FCoeff: TVector;
    FRegFit: TRegTest;
    FFp: EpiFloat;
    FDoAnova: Boolean;
    FAnova: TAnovaRecord;
    FConf: Integer;
    FDecimals: Integer;
    FValuelabelOutput: TEpiGetValueLabelType;
    FVariableLabelOutput: TEpiGetVariableLabelType;
  public
    constructor Create(AExecutor: TExecutor; AOutputCreator: TOutputCreator; ST: TRegressCommand); virtual;
    procedure  SetFormula(VarNames: TStrings); virtual;
    procedure  SetDepV(F: TEpiField);
    procedure  SetIndepV(F: TEpiField; Ix: Integer); virtual;
    procedure  DoOutput();
    procedure  DoResultVariables();
    function   Estimate(): UTF8String; virtual; abstract;
    function   Fitted(Msg: UTF8String): TVector; virtual; abstract;
    procedure  GetFittedVar(DF: TEpiDatafile; EF: TEpiField); virtual; abstract;
    function   getSS(V: TVector; Lb,Ub: Integer): EpiFloat;
    property   Model: UTF8String read FModel;
    property   Constant: Boolean read FConstant write FConstant;
    property   Obs: Integer read FObs write FObs;
  end;
  TRegressClass = class of TRegressModel;

    TRegressMap = specialize TFPGMap<TRegressType, TRegressClass>;

implementation

uses
  LazUTF8, generalutils, options_utils, umeansd;

constructor TRegressModel.Create(AExecutor: TExecutor; AOutputCreator: TOutputCreator; ST: TRegressCommand);
begin
  FObs := 0;
  FExecutor := AExecutor;
  FOutputCreator := AOutputCreator;
  FConf := StrToInt(FExecutor.SetOptionValue[ANA_SO_CONFIDENCE_INTERVAL]);
  FDecimals := DecimalFromOption(ST.Options, 4);
  FVariableLabelOutput := VariableLabelTypeFromOptionList(ST.Options, FExecutor.SetOptions);
  FValuelabelOutput    := ValueLabelTypeFromOptionList(ST.Options, FExecutor.SetOptions);
  FDoAnova := ST.HasOption('anova');
  FConstant := not ST.HasOption('nocon');
end;

procedure TRegressModel.SetFormula(VarNames: TStrings);
begin
  FParamCt := VarNames.Count;
  FDepVName := VarNames[0];
  FModel := FDepVName + ' ~ ';
end;

procedure TRegressModel.SetDepV(F: TEpiField);
var
  i: Integer;
begin
  if (FObs = 0) then
    FObs := F.Size;
  FDepVLabel := F.GetVariableLabel(FVariableLabelOutput);
  DimVector(FDepV, FObs - 1);
  for i := 0 to FObs - 1 do
    FDepV[i] := F.AsFloat[i];
end;

procedure TRegressModel.SetIndepV(F: TEpiField; ix: Integer);
var
  i: Integer;
begin
  if (FObs = 0) then
    FObs := F.Size;
  if (FIndepV = nil) then
    begin
      DimMatrix(FIndepV, FObs - 1, FParamCt - 1);
      // put constant (1) into first column?
      if (FConstant) then
        for i := 0 to FObs - 1 do
          FIndepV[i, 0] := 1;
    end;
  if (ix > 0) and (ix <= FParamCt) then
    begin
      FB[ix].pLabel := F.GetVariableLabel(FVariableLabelOutput);
      for i := 0 to FObs - 1 do
        FIndepV[i, ix] := F.AsFloat[i];
    end;
end;

procedure TRegressModel.DoOutput();
var
  T,
  A: TOutputTable;
  StatFmt: String;
  Params,
  Offset, i, i0: Integer;

  function StatFloatDisplay(const fmt: String; const val: EpiFloat):string;
  begin
    if (val = TEpiFloatField.DefaultMissing) then
      Result := '.'
    else
      Result := Format(fmt, [val]);
  end;

begin
  T := FOutputCreator.AddTable;
  T.Header.Text := LineEnding + sRegTitle + LineEnding + LineEnding +
                   sRegModel + ' : ' + FModel + LineEnding + LineEnding ;
  StatFmt := '%8.' + IntToStr(FDecimals) + 'F';
  T.ColCount := 5;
  Params := Length(FB) - 1;
  T.RowCount := Params + 2;
  // Header row
  T.Cell[0,0].Text := sRegTerm;
  T.Cell[1,0].Text := sRegCoefficient;
  T.Cell[2,0].Text := sStErrorAbbr;
  T.Cell[3,0].Text := 't';
  T.Cell[4,0].Text := 'p';
  Offset := 1;
  if (FConstant) then begin
    T.RowCount := T.RowCount + 1;
    i0 := 0;
    end
  else
      i0 := 1;

    for i := i0 to Params do begin
      T.Cell[0,Offset].Text := FB[i].pLabel;
      T.Cell[1,Offset].Text := StatFloatDisplay(StatFmt, FB[i].Estimate);
      T.Cell[2,Offset].Text := StatFloatDisplay(StatFmt, FB[i].se);
      T.Cell[3,Offset].Text := StatFloatDisplay(StatFmt, FB[i].t);
      T.Cell[4,Offset].Text := FormatP(FB[i].p, false);
      Offset += 1;
    end;

    if (FConstant) then
       T.Footer.Text := sRegResidualVariance + ':' + StatFloatDisplay(StatFmt, FRegFit.Vr) +
            LineEnding +
            sRegR2 + ':' + StatFloatDisplay(StatFmt, FRegFit.R2) +
            '  ' + sRegAdjustedR2 + ':' + StatFloatDisplay(StatFmt, FRegFit.R2a) +
            LineEnding +
            'F:' + StatFloatDisplay(StatFmt, FRegFit.F) +
            sRegFdf1 + FRegFit.Nu1.ToString + sRegFdf2 +
            FRegFit.Nu2.ToString + ' ' + sDegFreedomAbbr + ' (' +
            FormatP(FFp, true) + ')'
    else
      T.Footer.Text := sRegFNotSupported;

    if (FDoAnova) then with FAnova do
      begin
        A := FOutputCreator.AddTable;
        A.Header.Text := 'Analysis of Variance';
        A.ColCount := 5;
        A.RowCount := 4;
        A.Cell[0,0].Text := 'Source';
        A.Cell[1,0].Text := 'df';
        A.Cell[2,0].Text := 'Sum of Squares';
        A.Cell[3,0].Text := 'Mean Square';
        A.Cell[4,0].Text := 'F';
        A.Cell[0,1].Text := 'Regression';
        A.Cell[1,1].Text := DFB.ToString;
        A.Cell[2,1].Text := StatFloatDisplay(StatFmt, SSB);
        A.Cell[3,1].Text := StatFloatDisplay(StatFmt, MSB);
        A.Cell[4,1].Text := StatFloatDisplay(StatFmt, F);
        A.Cell[0,2].Text := 'Residual';
        A.Cell[1,2].Text := DFW.ToString;
        A.Cell[2,2].Text := StatFloatDisplay(StatFmt, SSW);
        A.Cell[3,2].Text := StatFloatDisplay(StatFmt, MSW);
        A.Cell[0,3].Text := 'Total';
        A.Cell[1,3].Text := DFT.ToString;
        A.Cell[2,3].Text := StatFloatDisplay(StatFmt, SST);
      end;
end;

procedure TRegressModel.DoResultVariables();
var
  lrBeta,
  lrSErr,
  lrt,
  lrp: TCustomExecutorDataVariable;
  i, Params: Integer;
  Prefix: String;
begin
  Prefix := '$r_';
  with FExecutor do begin
    AddResultConst(Prefix + 'model',    ftString).AsStringVector[0]   := FModel;
    AddResultConst(Prefix + 'nIndVar',  ftInteger).AsIntegerVector[0] := FParamCt-1;
    AddResultConst(Prefix + 'Vr',       ftFloat).AsFloatVector[0]     := FRegFit.Vr;
    if (FConstant) then
      begin
        AddResultConst(Prefix + 'R2',    ftFloat).AsFloatVector[0]     := FRegFit.R2;
        AddResultConst(Prefix + 'R2a',   ftFloat).AsFloatVector[0]     := FRegFit.R2a;
        AddResultConst(Prefix + 'F',     ftFloat).AsFloatVector[0]     := FRegFit.F;
        AddResultConst(Prefix + 'df1',   ftInteger).AsIntegerVector[0] := FRegFit.Nu1;
        AddResultConst(Prefix + 'df2',   ftInteger).AsIntegerVector[0] := FRegFit.Nu2;
        AddResultConst(Prefix + 'Fp',    ftFloat).AsFloatVector[0]     := FFp;
      end;
    Params := length(FB);
    lrBeta := AddResultVector(Prefix + 'beta', ftFloat, Params);
    lrSErr := AddResultVector(Prefix + 'se',   ftFloat, Params);
    lrt    := AddResultVector(Prefix + 't',    ftFloat, Params);
    lrp    := AddResultVector(Prefix + 'tp',   ftFloat, Params);
    for i := 0 to (Params - 1) do begin
      lrBeta.AsFloatVector[i] := FB[i].Estimate;
      lrSerr.AsFloatVector[i] := FB[i].Se;
      lrt.AsFloatVector[i]    := FB[i].t;
      lrp.AsFloatVector[i]    := FB[i].p;
    end;
  end;
end;

function TRegressModel.getSS(V: TVector; Lb,Ub: Integer): EpiFloat;
var
  i: Integer;
  m: Float;
begin
  result := 0;
  m := Mean(V, Lb, Ub);
  for i := Lb to Ub do
    result += (V[i] - m)**2;
end;

end.

