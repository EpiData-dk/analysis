unit regress_types;

{$interfaces CORBA}
{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, epidatafilestypes, epidatafiles, epifields_helper,
  outputcreator, fgl, fileutil,
  executor, ast, ana_globals, result_variables, interval_types,
  math, lMath, utypes;

{ resource strings for all regress units}
resourcestring
  sANOVA                = 'Analysis of Variance';
  sDegFreedomAbbr       = 'df';
  sFailedErr            = 'failed with error';
  sMeanSquare           = 'Mean Square';
  sStErrorAbbr          = 's.e.';
  sSumSquares           = 'Sum of Squares';
  sTotal                = 'Total';
  sRegAdjustedR2        = 'Adjusted R^2';
  sRegCoefficient       = 'Coefficient';
  sRegCommand           = 'Regression';
  sRegErrMatrixMult     = 'Matrix multiplication error';
  sRegFNotSupported     = 'F-test is not supported with no constant term';
  sRegFdf1              = ' on ';
  sRegFdf2              = ' and ';
  sRegIntercept         = 'Intercept';
  sRegModel             = 'Model';
  sRegR2                = 'R^2';
  sRegResidual          = 'Residual';
  sRegResidualVariance  = 'Residual variance';
  sRegSource            = 'Source';
  sRegSumDSCreated      = 'Summary dataset created';
  sRegTerm              = 'Term';
  sRegTitle             = 'Regression Analysis';
  sNoData               = 'No data';
  sRegNoconWithLogistic = 'Logistic model must have a constant';
  sRegNoconWithPoly     = 'Polynomial model must have a constant';
  sRegTooManyTypes      = 'Too many regression types';
  sRegTypeNotFound      = 'Regression type not found';
  sDepVarBinary         = 'Dependant variable must have values 0 and 1 only';
  sDepVarUnbalanced = 'Warning: dependant variable is unbalanced';
  sConstant = 'Constant';
  sVariance = 'Variance';
  sWaldChi2 = 'Wald Chi^2';
  sOddsRatios = 'Odds ratios';
  sCIAbbr = 'CI';
  sAnd = 'and';
  sOddsRatio = 'Odds ratio';
  sLowerLimit = 'Lower limit';
  sUpperLimit = 'Upper limit';
  sAnalysisDeviance = 'Analysis of Deviance';
  sDeviance = 'Deviance';
  sMcFPseudoR2 = 'McFadden pseudo-R-square';
  sRunDate = 'Run date';
  sPseudoR2 = 'pseudo-R2';
  sErrMatrixInversion = 'Error inverting matrix';

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
    { Exec methods }
  protected
    FExecutor: TExecutor;
    FOutputCreator: TOutputCreator;

    { Switches / Appearance }
    FDataError: Boolean;
    FConstant: Boolean;
    FDoAnova: Boolean;
    FDoVariance: Boolean;
    FConf: Integer;
    FDecimals: Integer;
    FValuelabelOutput: TEpiGetValueLabelType;
    FVariableLabelOutput: TEpiGetVariableLabelType;
    FStatFmt: String;

    { Data }
    FDepV: TVector;
    FIndepV: TMatrix;
    FFitted: TVector;
    FResidual: TVector;
    FObs: Integer;
    FDepVLabel,
    FDepVName: UTF8String;

    { Model }
    FParamCt: Integer;
    FModel: UTF8String;

    { Regression results }
    FB: array of TRegressParameter;
    FCoeff: TVector;
    FRegFit: TRegTest;
    FFp: EpiFloat;
    FAnova: TAnovaRecord;
    FSumDF: TEpiDataFile;
    function   getSS(V: TVector; Lb,Ub: Integer): EpiFloat;

  protected
    { summary dataset }
    FSummaryDataFileClass: TEpiDataFileClass;
    procedure VerifyField(ADatafile: TEpiDatafile; AName: UTF8String;
              AType: TEpiFieldType);
    procedure GetSummaryDF(AName: UTF8String;
              const vars: array of string; const vtypes: array of TEpiFieldType);
  public
    constructor Create(AExecutor: TExecutor; AOutputCreator: TOutputCreator; ST: TRegressCommand); virtual;
    procedure  SetFormula(VarNames: TStrings); virtual;
    procedure  SetDepV(F: TEpiField); virtual;
    procedure  SetIndepV(F: TEpiField; Ix: Integer); virtual;
    procedure  DoOutput(); virtual;
    procedure  DoResultVariables(); virtual;
    procedure  Summary(); virtual;
    procedure  DoOutputSummary(); virtual;
    function   Estimate(): UTF8String; virtual; abstract;
    function   Fitted(Msg: UTF8String): TVector; virtual; abstract;
    procedure  GetFittedVar(DF: TEpiDatafile; EF: TEpiField); virtual; abstract;
    property   Model: UTF8String read FModel;
    property   Constant: Boolean read FConstant write FConstant;
    property   Obs: Integer read FObs write FObs;
    property   StatFmt: String read FStatFmt write FStatFmt;
    property   DataError: Boolean read FDataError;
    property   SummaryDataFileClass: TEpiDataFileClass read FSummaryDataFileClass;
  end;
  TRegressClass = class of TRegressModel;

    TRegressMap = specialize TFPGMap<TRegressType, TRegressClass>;

implementation

uses
  LazUTF8,
  epicustombase, epidocument, epidatafilerelations,
  generalutils, options_utils, umeansd;

constructor TRegressModel.Create(AExecutor: TExecutor; AOutputCreator: TOutputCreator; ST: TRegressCommand);
begin
  FObs := 0;
  FExecutor := AExecutor;
  FOutputCreator := AOutputCreator;
  FConf := StrToInt(FExecutor.SetOptionValue[ANA_SO_CONFIDENCE_INTERVAL]);
  FDecimals := DecimalFromOption(ST.Options, 4);
  FStatFmt := '%8.' + IntToStr(FDecimals) + 'F';
  FVariableLabelOutput := VariableLabelTypeFromOptionList(ST.Options, FExecutor.SetOptions);
  FValuelabelOutput    := ValueLabelTypeFromOptionList(ST.Options, FExecutor.SetOptions);
  FDoVariance := true;
  FDoAnova := ST.HasOption('anova');
  FConstant := not ST.HasOption('nocon');
  FDataError := false;
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
  if (ix > 0) and (ix < FParamCt) then
    begin
      FB[ix].pLabel := F.GetVariableLabel(FVariableLabelOutput);
      for i := 0 to FObs - 1 do
        FIndepV[i, ix] := F.AsFloat[i];
    end;
end;

procedure TRegressModel.DoOutput();
var
  T,
  V,
  A: TOutputTable;
  Params,
  Offset, i, i0: Integer;

begin
  T := FOutputCreator.AddTable;
  T.Header.Text := LineEnding + sRegTitle + LineEnding + LineEnding +
                   sRegModel + ' : ' + FModel + LineEnding + LineEnding ;
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

  for i := i0 to High(FB) do begin
    T.Cell[0,Offset].Text := FB[i].pLabel;
    T.Cell[1,Offset].Text := StatFloatDisplay(StatFmt, FB[i].Estimate);
    T.Cell[2,Offset].Text := StatFloatDisplay(StatFmt, FB[i].se);
    T.Cell[3,Offset].Text := StatFloatDisplay(StatFmt, FB[i].t);
    T.Cell[4,Offset].Text := FormatP(FB[i].p, false);
    Offset += 1;
  end;

  if (FDoVariance) then
    begin
      V := FOutputCreator.AddTable;
      V.ColCount := 1;
      V.RowCount := 3;
      if (FConstant) then
        begin
          V.Cell[0,0].Text := sRegResidualVariance + ':' + StatFloatDisplay(StatFmt, FRegFit.Vr);
          V.Cell[0,1].Text :=      sRegR2 + ':' + StatFloatDisplay(StatFmt, FRegFit.R2) +
              '  ' + sRegAdjustedR2 + ':' + StatFloatDisplay(StatFmt, FRegFit.R2a);
          V.Cell[0,2].Text := 'F:' + StatFloatDisplay(StatFmt, FRegFit.F) +
              sRegFdf1 + FRegFit.Nu1.ToString + sRegFdf2 +
              FRegFit.Nu2.ToString + ' ' + sDegFreedomAbbr + ' (' +
              FormatP(FFp, true) + ')';
        end
      else
        V.Header.Text := sRegFNotSupported;
    end;

  if (FDoAnova) then
    begin
      A := FOutputCreator.AddTable;
      A.ColCount := 5;
      A.RowCount := 4;
      with FAnova do
        begin
          A.Header.Text := sANOVA;
          A.ColCount := 5;
          A.RowCount := 4;
          A.Cell[0,0].Text := sRegSource;
          A.Cell[1,0].Text := sDegFreedomAbbr;
          A.Cell[2,0].Text := sSumSquares;
          A.Cell[3,0].Text := sMeanSquare;
          A.Cell[4,0].Text := 'F';
          A.Cell[0,1].Text := sRegCommand;
          A.Cell[1,1].Text := DFB.ToString;
          A.Cell[2,1].Text := StatFloatDisplay(StatFmt, SSB);
          A.Cell[3,1].Text := StatFloatDisplay(StatFmt, MSB);
          A.Cell[4,1].Text := StatFloatDisplay(StatFmt, F);
          A.Cell[0,2].Text := sRegResidual;
          A.Cell[1,2].Text := DFW.ToString;
          A.Cell[2,2].Text := StatFloatDisplay(StatFmt, SSW);
          A.Cell[3,2].Text := StatFloatDisplay(StatFmt, MSW);
          A.Cell[0,3].Text := sTotal;
          A.Cell[1,3].Text := DFT.ToString;
          A.Cell[2,3].Text := StatFloatDisplay(StatFmt, SST);
        end;
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

procedure TRegressModel.Summary();
begin

end;

procedure TRegressModel.DoOutputSummary();
var
  SortFields: TEpiFields;
begin
  if not Assigned(FSumDF) then
    begin
      FOutputCreator.DoWarning('!sum ignored for this type of regression');
      exit;
    end;
  SortFields := TEpiFields.Create(nil);
  SortFields.AddItem(FSumDF.Fields.FieldByName['DepVar']);
  SortFields.AddItem(FSumDF.Fields.FieldByName['Model']);
  FSumDF.SortRecords(SortFields);
  SortFields.Destroy;
end;

procedure TRegressModel.GetSummaryDF(AName: UTF8String;
          const vars: array of string; const vtypes: array of TEpiFieldType);
var
  MR: TEpiMasterRelation;
  Rel: TEpiDatafileRelationList;
  i: Integer;
begin
  FSumDF := FExecutor.Document.DataFiles.GetDataFileByName(AName);
  if not Assigned(FSumDF) then
    begin;
      // Create the summary dataset
      Rel := Fexecutor.Document.Relations;
      MR := Rel.NewMasterRelation;
      FSumDF := TEpiDataFile.Create(nil, 0);
      FSumDF.SetLanguage(FExecutor.Datafile.DefaultLang, true);
      FOutputCreator.DoWarning(sRegSumDSCreated);
      FSumDF.Name := AName;
      MR.Datafile := FSumDF;
      FExecutor.Document.DataFiles.AddItem(FSumDF);
      FExecutor.UpdateDatasetResultVar;
    end;
  // check that all variables exist and create any that are missing
  // this allows us to use a summary dataset from an old version
  for i := 0 to High(vars) do
      VerifyField(FSumDF, vars[i], vtypes[i]);
end;

procedure TRegressModel.VerifyField(ADatafile: TEpiDatafile; AName: UTF8String;
          AType: TEpiFieldType);
var
  Field: TEpiField;
begin
  if Assigned(ADatafile.Fields.FieldByName[AName]) then
    exit;
  Field := TEpiField.CreateField(nil, AType);
  Field.Name := AName;
  ADatafile.MainSection.Fields.AddItem(Field);
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

