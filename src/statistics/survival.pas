unit survival;

{$codepage UTF-8}
{$mode objfpc}{$H+}
{TODO:
1. DONE check success of command before doing output
   function CalcSurvival: boolean?
2. stratified analysis
3. allow dates for fields 2 and 3
}
interface

uses
  Classes, SysUtils, StrUtils, ast, epidatafiles, epidatafilestypes, epicustombase,
  tables_types, tables,
  executor, result_variables, interval_types, epifields_helper, ana_globals,
  outputcreator;

type

  { TSurvivalDatafile }

  {TSurvivalDatafile = class(TEpiDataFile)
  private
    FStratumIndex: Integer;
    FAtRisk: TEpiField;
    FSurvival: TEpiField;
    FFail: TEpiField;
    FLowCI: TEpiField;
    FHighCI: TEpiField;
  public
    property StratumIndex: Integer read FStratumIndex write FStratumIndex;
    property AtRisk: TEpiField read FAtRisk write FAtRisk;
    property Survival: TEpiField read FSurvival write FSurvival;
    property Fail: TEpiField read FFail write FFail;
    property LowCI: TEpiField read FLowCI write FLowCI;
    property HighCI: TEpiField read FHighCI write FHighCI;
  end;
   }
  { TSurvival }

  TSurvival = class
  private
    FDecimals: Integer;
    FConf: Integer;
    FValuelabelOutput: TEpiGetValueLabelType;
    FVariableLabelOutput: TEpiGetVariableLabelType;
    FFailOutcomeValue: UTF8String;
  // table of outcome by time
    FSurvivalTable: TTwoWayTables;
    FStrata: Integer;
  // survival table results
    FInterval: Array of Array of UTF8String;
    FAtRisk:   Array of Array of Integer;
    FFail:     Array of Array of Integer;
    FSurvival: Array of Array of EpiFloat;
    FLowCI:    Array of Array of EpiFloat;
    FHighCI:   Array of Array of EpiFloat;

    FWeightVarName: UTF8String;
    FStratVarName: UTF8String;
    FOutcomeVarLabel: UTF8String;
    FTimeVarLabel: UTF8String;

  protected
    FExecutor: TExecutor;
    FOutputCreator: TOutputCreator;

    procedure DoCalcSurvival(InputDF: TEpiDataFile; Variables: TStrings; StratVariable: TStringList;
      FailOutcomeValue: UTF8String; ST: TCustomVariableCommand);
//    procedure DoResultVariables(ST: TCustomVariableCommand); virtual;
    procedure DoOutputSurvival(ST:TCustomVariableCommand); virtual;
//    procedure DoOutputTest(); virtual;
    procedure DoOutputGraph(); virtual;
  public
    constructor Create(AExecutor: TExecutor; OutputCreator: TOutputCreator);
    destructor Destroy; override;

    // Method called from Executor, does calculation + result vars + output
    procedure ExecSurvival(Variables: TStrings; ST: TCustomVariableCommand);
//    procedure ExecSurvival(DataFile: TEpiDataFile; ST: TSurvivalCommand);
    // Method to be used from elsewhere. Does only calculations and returns the result as a specialized dataset
{    function CalcSurvival(DataFile: TEpiDataFile; Variables: TStrings; StratVariable: TStringList;
      FailOutcomeValue: Integer; ST: TOptionList;
      ValueLabelOutput: TEpiGetValueLabelType = gvtValue; VariableLabelOutput: TEpiGetVariableLabelType = gvtVarName): TSurvivalDatafile;
      }
  end;

implementation

uses
  generalutils, Math, statfunctions, options_utils;

{ TSurvivalDatafile }

{constructor TSurvivalDatafile.Create(AOwner: TEpiCustomBase; const ASize: integer);
begin
  inherited Create(AOwner, ASize);
end;
}

{ TSurvival }

procedure TSurvival.DoCalcSurvival(InputDF: TEpiDataFile;
  Variables: TStrings; StratVariable: TStringList;
  FailOutcomeValue: UTF8String; ST: TCustomVariableCommand);

var
  StartIdx, Ix, i, EndIdx, Failures, FailIx, N, Obs, Row, Col: Integer;
  Intervals: Integer;
  Interval, Stratum: Integer;
  NAtRisk, Lost: Integer;
  S, SE, SumF, CIMult: EpiFloat;
  T: TTables;
  O:   TEpiReferenceMap;
  Statistics: TTableStatistics;
  ASurvivalTable: TTwowayTable;
  TablesRefMap: TEpiReferenceMap;

begin

// Use TABLES to get counts of outcomes by time for each stratum
  T := TTables.Create(FExecutor, FOutputCreator);
  FSurvivalTable  := T.CalcTables(InputDF, Variables,
    StratVariable, FWeightVarName, ST.Options, TablesRefMap, Statistics);

  // Set size of survival table arrays based on results from CalcTables

  Intervals := FSurvivalTable.UnstratifiedTable.RowCount;
  FStrata := FSurvivalTable.Count;
  SetLength(FInterval, FStrata + 1, Intervals);
  SetLength(FAtRisk, FStrata + 1, Intervals);
  SetLength(FFail, FStrata + 1, Intervals);
  SetLength(FSurvival, FStrata + 1, Intervals);
  SetLength(FLowCI, FStrata + 1, Intervals);
  SetLength(FHighCI, FStrata + 1, Intervals);
// find index of outcome = fail
  ASurvivalTable := FSurvivalTable.UnstratifiedTable;
  FailIx := -1;
  for Col := 0 to ASurvivalTable.ColCount - 1 do
    if (ASurvivalTable.ColVariable.AsString[Col] = FailOutcomeValue) then
      begin
        FailIx := Col;
        break;
      end;
  if (FailIx < 0) then
    begin
      FExecutor.Error('No records with ' + Variables[0] + ' = ' + FailOutcomeValue);
      exit;
    end;
  FFailOutcomeValue := FailOutcomeValue;

  // set up confidence interval
  FConf := StrToInt(FExecutor.SetOptionValue[ANA_SO_CONFIDENCE_INTERVAL]);
  CIMult := PNORMALINV((Float(100 - FConf) / 200.0));

  // unstratified table first

  for Stratum := 0 to FStrata do
    begin
      if (Stratum > 0) then ASurvivalTable := FSurvivalTable.Tables[Stratum - 1];
      NAtRisk := ASurvivalTable.Total;
      S := 1.0;
      Lost := 0;
      SumF := 0.0;

      for Row := 0 to Intervals - 1 do
       begin
        Failures := ASurvivalTable.Cell[FailIx,Row].N;
        S        := S * Float((NAtRisk - Failures)) / Float(NAtRisk);
        SumF     += Float(Failures) / Float(NAtRisk*(NAtRisk - Failures));
        SE       := CIMult * S * SQRT(SumF);
        FInterval[Stratum, Row] := ASurvivalTable.RowVariable.GetValueLabel(Row , FValuelabelOutput);
        FAtRisk  [Stratum, Row] := NAtRisk;
        FFail    [Stratum, Row] := Failures;
        FSurvival[Stratum, Row] := S;
        FLowCI   [Stratum, Row] := max(S - SE , 0);
        FHighCI  [Stratum, Row] := min(S + SE , 1);
        Lost     := ASurvivalTable.RowTotal[Row];
        NAtRisk  := NAtRisk - Lost;
      end;
    end;

  ST.ExecResult := csrSuccess;

  T.Free;
end;

{procedure TSurvival.DoResultVariables(ResultDF: TSurvivalDatafile);
var
  CatV, ObsV, SumV, MeanV, SvV, SdV, SerrV, CfilV, CfihV, SkewV, KurtV,
  MinV, P05V, P10V, P25V, MedV, P75V, P90V, P95V, MaxV: TCustomExecutorDataVariable;
  Sz, i: Integer;
  Prefix: String;
begin
  Sz := ResultDF.Size;
  Prefix := '$Survival_';
  with FExecutor do
  begin
    if Sz = 1 then
      begin
        CatV  := AddResultConst(Prefix + 'category', ftString);
        ObsV  := AddResultConst(Prefix + 'obs',      ftInteger);
        SumV  := AddResultConst(Prefix + 'sum',      ftFloat);
        MeanV := AddResultConst(Prefix + 'mean',     ftFloat);
        MinV  := AddResultConst(Prefix + 'min',      ftFloat);
        P05V  := AddResultConst(Prefix + 'p05',      ftFloat);
        P10V  := AddResultConst(Prefix + 'p10',      ftFloat);
        P25V  := AddResultConst(Prefix + 'p25',      ftFloat);
        MedV  := AddResultConst(Prefix + 'median',   ftFloat);
        P75V  := AddResultConst(Prefix + 'p75',      ftFloat);
        P90V  := AddResultConst(Prefix + 'p90',      ftFloat);
        P95V  := AddResultConst(Prefix + 'p95',      ftFloat);
        MaxV  := AddResultConst(Prefix + 'max',      ftFloat);
        SvV   := AddResultConst(Prefix + 'variance', ftFloat);
        SdV   := AddResultConst(Prefix + 'sd',       ftFloat);
        SerrV := AddResultConst(Prefix + 'stderr',   ftFloat);
        CfilV := AddResultConst(Prefix + 'cfil',     ftFloat);
        CfihV := AddResultConst(Prefix + 'cfih',     ftFloat);
        SkewV := AddResultConst(Prefix + 'skew',     ftFloat);
        KurtV := AddResultConst(Prefix + 'kurt',     ftFloat);
        AddResultConst(Prefix + 'catvar', ftString).AsStringVector[0]  := '';
      end
    else
      begin
        CatV  := AddResultVector(Prefix + 'category', ftString, Sz);
        ObsV  := AddResultVector(Prefix + 'obs',      ftInteger, Sz);
        SumV  := AddResultVector(Prefix + 'sum',      ftFloat, Sz);
        MeanV := AddResultVector(Prefix + 'mean',     ftFloat, Sz);
        MinV  := AddResultVector(Prefix + 'min',      ftFloat, Sz);
        P05V  := AddResultVector(Prefix + 'p05',      ftFloat, Sz);
        P10V  := AddResultVector(Prefix + 'p10',      ftFloat, Sz);
        P25V  := AddResultVector(Prefix + 'p25',      ftFloat, Sz);
        MedV  := AddResultVector(Prefix + 'median',   ftFloat, Sz);
        P75V  := AddResultVector(Prefix + 'p75',      ftFloat, Sz);
        P90V  := AddResultVector(Prefix + 'p90',      ftFloat, Sz);
        P95V  := AddResultVector(Prefix + 'p95',      ftFloat, Sz);
        MaxV  := AddResultVector(Prefix + 'max',      ftFloat, Sz);
        SvV   := AddResultVector(Prefix + 'sv',       ftFloat, Sz);
        SdV   := AddResultVector(Prefix + 'sd',       ftFloat, Sz);
        SerrV := AddResultVector(Prefix + 'stderr',   ftFloat, Sz);
        CfilV := AddResultVector(Prefix + 'cfil',     ftFloat, Sz);
        CfihV := AddResultVector(Prefix + 'cfih',     ftFloat, Sz);
        SkewV := AddResultVector(Prefix + 'skew',     ftFloat, Sz);
        KurtV := AddResultVector(Prefix + 'kurt',     ftFloat, Sz);

        AddResultConst(Prefix + 'catvar', ftString).AsStringVector[0]  := ResultDF.FStratifyVarText;
     end;

    AddResultConst(Prefix + 'size',  ftInteger).AsIntegerVector[0] := Sz;
//    AddResultConst(Prefix + 'var',  ftString).AsStringVector[0]    := ResultDF.CountVarText;
 end;

  for i := 0 to ResultDF.Size - 1 do
  with ResultDF do
    begin
      CatV.AsStringVector[i]  := Category.AsString[i];
      ObsV.AsIntegerVector[i] := N.AsInteger[i];
      SumV.AsFloatVector[i]   := Sum.AsFloat[i];
      MeanV.AsFloatVector[i]  := Mean.AsFloat[i];
      MinV.AsFloatVector[i]   := Min.AsFloat[i];
      P05V.AsFloatVector[i]   := P05.AsFloat[i];
      P10V.AsFloatVector[i]   := P10.AsFloat[i];
      P25V.AsFloatVector[i]   := P25.AsFloat[i];
      MedV.AsFloatVector[i]   := Median.AsFloat[i];
      P75V.AsFloatVector[i]   := P75.AsFloat[i];
      P90V.AsFloatVector[i]   := P90.AsFloat[i];
      P95V.AsFloatVector[i]   := P95.AsFloat[i];
      MaxV.AsFloatVector[i]   := Max.AsFloat[i];

      SvV.AsFloatVector[i]    := StdVar.AsFloat[i];
      SdV.AsFloatVector[i]    := StdDev.AsFloat[i];
      SerrV.AsFloatVector[i]  := StdErr.AsFloat[i];
      CfilV.AsFloatVector[i]  := CfiL.AsFloat[i];
      CfihV.AsFloatVector[i]  := CfiH.AsFloat[i];
      SkewV.AsFloatVector[i]  := Skew.AsFloat[i];
      KurtV.AsFloatVector[i]  := Kurt.AsFloat[i];

    end;

  With ResultDF.AnovaRecord do
    if ResultDF.Size > 1 then
      begin
        FExecutor.AddResultConst(Prefix + 'DFB',   ftInteger).AsIntegerVector[0] := DFB;
        FExecutor.AddResultConst(Prefix + 'SSB',   ftFloat).AsFloatVector[0]     := SSB;
        FExecutor.AddResultConst(Prefix + 'MSB',   ftFloat).AsFloatVector[0]     := MSB;
        FExecutor.AddResultConst(Prefix + 'F',     ftFloat).AsFloatVector[0]     := F;
        FExecutor.AddResultConst(Prefix + 'PROB',  ftFloat).AsFloatVector[0]     := PROB;
        FExecutor.AddResultConst(Prefix + 'DFW',   ftInteger).AsIntegerVector[0] := DFW;
        FExecutor.AddResultConst(Prefix + 'SSW',   ftFloat).AsFloatVector[0]     := SSW;
        FExecutor.AddResultConst(Prefix + 'MSW',   ftFloat).AsFloatVector[0]     := MSW;
        FExecutor.AddResultConst(Prefix + 'BART',  ftFloat).AsFloatVector[0]     := BART;
        FExecutor.AddResultConst(Prefix + 'pBART', ftFloat).AsFloatVector[0]     := PBART;
      end
    else
      begin
        FExecutor.AddResultConst(Prefix + 'T',     ftFloat).AsFloatVector[0]     := F;
        FExecutor.AddResultConst(Prefix + 'PROB',  ftfloat).AsFloatVector[0]     := PROB;
      end;
end;
}
procedure TSurvival.DoOutputSurvival(ST:TCustomVariableCommand);
var
  T: TOutputTable;
  SmallNumFmt, StatFmt: String;
  Sz, Offset, i, Stratum, Idx: Integer;
  FirstStratum, LastStratum: Integer;
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
  T.Header.Text := 'Survival Analysis';
  SmallNumFmt := '%8.2F';
  Sz := Length(FAtRisk[0]);
  T.RowCount := 0;

  // set up output table size based on strata and options

  if (ST.HasOption('nu')) then
    begin
      if (ST.HasOption('ns')) then exit;
      T.ColCount := 1;
      FirstStratum := 1;
      Offset := 1;
    end
  else
    begin
      T.ColCount := 6;
      T.RowCount := 2;
      T.Cell[1,0].Text := 'Unstratified';
      FirstStratum := 0;
      Offset := 6;
    end;

  if (ST.HasOption('ns')) then
    LastStratum := 0
  else
    begin
      T.RowCount := 3;
      T.ColCount := T.ColCount + 5 * FStrata;
      for i := 0 to FStrata -1 do
        begin
          T.Cell[Offset + 5 * i, 0].Text := FStratVarName + ' = ';
          T.Cell[Offset + 1 + 5 * i, 0].Text := FSurvivalTable.StratifyVariables.Field[0].GetValueLabel(i);
        end;
      LastStratum := FStrata;
    end;

  T.Cell[0,1].Text := 'Follow';
  T.Cell[0, 2].Text := FTimeVarLabel;

  T.RowCount := Sz + 3;
  Offset := 1;
  // Column headers
  // TODO: add 5 columns per stratum; ditto for results below
  for Stratum := FirstStratum to LastStratum do
    begin
      T.Cell[    Offset, 1].Text := '# At';
      T.Cell[1 + Offset, 1].Text := FOutcomeVarLabel;
      T.Cell[2 + Offset, 1].Text := ' ';
      T.Cell[3 + Offset, 1].Text := ' ';
      T.Cell[4 + Offset, 1].Text := ' ';
      T.Cell[    Offset, 2].Text := 'Risk';
      T.Cell[1 + Offset, 2].Text := FFailOutcomeValue;
      T.Cell[2 + Offset, 2].Text := 'Survival';
      T.Cell[3 + Offset, 2].Text := '( ' + IntToStr(FConf) + '%';
      T.Cell[4 + Offset, 2].Text := ' CI)';
      Offset += 5;
    end;
  T.SetRowAlignment(1, taRightJustify);
  T.SetRowBoxBorder(1);
  T.SetRowBoxBorder(2);
  StatFmt := '%8.' + IntToStr(FDecimals) + 'F';

  // intervals
  for i := 0 to Sz - 1 do
    T.Cell[0, i + 3].Text := FInterval[0, i];
  Offset := 1;
  for Stratum := FirstStratum to LastStratum do
    begin
      for i := 0 to Sz - 1 do
        begin
          T.Cell[    Offset, i + 3].Text := IntToStr(FAtRisk[Stratum, i]);
          T.Cell[1 + Offset, i + 3].Text := IntToStr(FFail[Stratum, i]);
          T.Cell[2 + Offset, i + 3].Text := Format(StatFmt, [FSurvival[Stratum, i]]);
          T.Cell[3 + Offset, i + 3].Text := StatFloatDisplay(StatFmt, FLowCI[Stratum, i]);
          T.Cell[4 + Offset, i + 3].Text := StatFloatDisplay(StatFmt, FHighCI[Stratum, i]);
          T.SetRowAlignment(i + 3, taRightJustify);
// Need to set this after the entire row has been set to right justify
//          if Offset > 0 then T.Cell[0, i + 1].Alignment := taLeftJustify;
        end;
      Offset += 5;
    end;

end;

constructor TSurvival.Create(AExecutor: TExecutor;
  OutputCreator: TOutputCreator);
begin
  FExecutor := AExecutor;
  FOutputCreator := OutputCreator;
end;

destructor TSurvival.Destroy;
begin
  inherited Destroy;
end;

{procedure TSurvival.DoOutputTest(ResultDF: TSurvivalRecord);
var
  T: TOutputTable;
  B: TOutputTable;
begin
  T := FOutputCreator.AddTable;
  T.Header.Text := 'Are strata survivals equal?';
  if (not SurvivalRec.Valid) then
    begin
      T.ColCount := 1;
      T.RowCount := 1;
      T.Cell[0,0].Text := 'Cannot compare survival curves with these values';
      exit
    end;
  {T.ColCount := 6;
  T.RowCount := 4;
  T.Cell[0,0].Text := 'Source';
  T.Cell[1,0].Text := 'DF';
  T.Cell[2,0].Text := 'SS';
  T.Cell[3,0].Text := 'MS';
  T.Cell[4,0].Text := 'F';
  T.Cell[5,0].Text := 'p Value';
  T.SetRowAlignment(0, taCenter);

  B := FOutputCreator.AddTable;
  B.Header.Text    := 'Bartlett''s Test of homogeneity of variances';
  B.ColCount       := 3;
  B.RowCount       := 2;
  B.Cell[0,0].Text := 'DF';
  B.Cell[1,0].Text := 'Chi-square';
  B.Cell[2,0].Text := 'p Value';
  B.SetRowAlignment(0, taCenter);

  with AnovaRec do
  begin
    T.Cell[0,1].Text := 'Between';
    T.Cell[1,1].Text := IntToStr(DFB);
    T.Cell[2,1].Text := Format('%8.2f', [SSB]);
    T.Cell[3,1].Text := Format('%8.2f', [MSB]);
    T.Cell[4,1].Text := Format('%8.2f', [F]);
    T.Cell[5,1].Text := Format('%8.3f', [PROB]);
    if ((PROB < 0.0) or (PROB > 1.0)) then
      T.Cell[5,1].Text := ' (Invalid p=' + T.Cell[5,1].Text + ' - Program error)';

    T.Cell[0,2].Text := 'Within';
    T.Cell[1,2].Text := IntToStr(DFW);
    T.Cell[2,2].Text := Format('%8.2f', [SSW]);
    T.Cell[3,2].Text := Format('%8.2f', [MSW]);

    T.Cell[0,3].Text := 'Total';
    T.Cell[1,3].Text := IntToStr(DFT);
    T.Cell[2,3].Text := Format('%8.2f', [SST]);
    T.Cell[3,3].Text := Format('%8.2f', [MST]);

    B.Cell[0,1].Text := IntToStr(DFB);
    B.Cell[1,1].Text := Format('%8.2f', [BART]);
    B.Cell[2,1].Text := Format('%8.3f', [PBART]);

  end;}
end;
}
procedure TSurvival.DoOutputGraph();
var
  T: TOutputTable;
begin
  T := FOutputCreator.AddTable;
  T.Header.Text := 'Survival Graph';
  T.ColCount       := 1;
  T.RowCount       := 1;
  T.Cell[0,0].Text := 'Graphs not yet implemented';
end;

procedure TSurvival.ExecSurvival(Variables: TStrings; ST: TCustomVariableCommand);
var
//  Variables: TStrings;
  AllVariables: TStrings;
  StratVariable: TStringList;
//  ResultDF: TSurvivalDatafile;
//  FollowVarName, OutcomeVarName, StratifyVarName: UTF8String;
  FailOutcomeValue: UTF8String;
  Opt: TOption;
  HasBy: Boolean;
  DF: TEpiDataFile;
begin
// reset result variables;
  FExecutor.ClearResults('$Survival');

  FDecimals := 3; //DecimalFromOption(ST.Options);
  FVariableLabelOutput := VariableLabelTypeFromOptionList(ST.Options, FExecutor.SetOptions);
  FValuelabelOutput    := ValueLabelTypeFromOptionList(ST.Options, FExecutor.SetOptions);

  Variables := ST.VariableList.GetIdentsAsList;
  StratVariable := TStringList.Create;
  HasBy := false;
  ST.ExecResult := csrFailed;
  FailOutcomeValue := '0';
  try
    for Opt in ST.Options do
    begin
      // get death outcome value (only integer for now)
      if (Opt.Ident = 'o') then
        begin
          FailOutcomeValue := Opt.Expr.AsString;
        end;
      // check for  weight variable
     if (Opt.Ident = 'w') then
       FWeightVarName := Opt.Expr.AsIdent;

      // check for more than one !by
     if (Opt.Ident = 'by') then
       if (HasBy) then
         begin
           FExecutor.Error('Can only stratify by one variable; !by:=' + Opt.Expr.AsIdent + ' is invalid');
           exit;
         end
      else
        begin
          HasBy := true;
          if ( (Variables[0] = Opt.Expr.AsIdent) or (Variables[1] = Opt.Expr.AsIdent) ) then
            begin
              FExecutor.Error('Cannot stratify by this variable: ' + Opt.expr.AsIdent);
              Exit;
            end;
          FStratVarName := Opt.Expr.AsIdent;
          StratVariable.Add(FStratVarName);
         end;
      end;

    AllVariables := TStringList.Create;
    AllVariables.AddStrings(Variables);
    AllVariables.AddStrings(StratVariable);
    if ST.HasOption('w') then
      AllVariables.Add(FWeightVarName);

    DF := FExecutor.PrepareDatafile(AllVariables, AllVariables);
    FTimeVarLabel := DF.Field[1].GetVariableLabel(FVariableLabelOutput);
    FOutcomeVarLabel := DF.Field[0].GetVariableLabel(FVariableLabelOutput);

    if DF.Size = 0 then
      begin
        FExecutor.Error('No data!');
        DF.Free;
        Exit;
      end;

    DoCalcSurvival(DF, Variables, StratVariable, FailOutcomeValue, ST);
//    DoResultVariables(ResultDF);

    if (ST.ExecResult = csrSuccess) then
      begin

      if (not ST.HasOption('q')) then
        begin
          if (not ST.HasOption('nt')) then
            DoOutputSurvival(ST);

          if (not ST.HasOption('ng')) then
            DoOutputGraph();

          if (ST.HasOption('t')) then
              FExecutor.Error('Stratified analysis not yet implemented');
        end;

      end;
//    ResultDF.Free;
    DF.Free;
  finally
//    Variables.Free;
end;

end;

{function TSurvival.CalcSurvival(DataFile: TEpiDataFile; Variables: TStrings; StratVariable: TStringList;
  FailOutcomeValue: Integer; ST: TOptionList;
  ValueLabelOutput: TEpiGetValueLabelType;
  VariableLabelOutput: TEpiGetVariableLabelType): TSurvivalDatafile;
begin
  FValuelabelOutput := ValueLabelOutput;
  FVariableLabelOutput := VariableLabelOutput;

  Result := DoCalcSurvival(DataFile, Variables, StratVariable, FailOutcomeValue, ST);
end;
}
end.
