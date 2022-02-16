unit survival;

{$codepage UTF-8}
{$mode objfpc}{$H+}
{TODO: reference to CalcTables should be as used in ctable unit
       see ExecCTable}
interface

uses
  Classes, SysUtils, StrUtils, ast, epidatafiles, epidatafilestypes, epicustombase,
  tables_types, tables,
  executor, result_variables, interval_types, epifields_helper, ana_globals,
  outputcreator;

type

  { TSurvivalDatafile }

  TSurvivalDatafile = class(TEpiDataFile)
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

  { TSurvival }

  TSurvival = class
  private
    FDecimals: Integer;
    FConf: Integer;
    FValuelabelOutput: TEpiGetValueLabelType;
    FVariableLabelOutput: TEpiGetVariableLabelType;
    FSurvivalTable: TTwoWayTables;
//    FStratVariable: TStringList;
    FWeightName: UTF8String;
    FStratVarName: UTF8String;
  protected
    FExecutor: TExecutor;
    FOutputCreator: TOutputCreator;

    function  DoCalcSurvival(InputDF: TEpiDataFile; Variables: TStrings; StratVariable: TStringList;
      FailOutcomeValue: Integer; ST: TOptionList): TSurvivalDatafile; virtual;
//    procedure DoResultVariables(ResultDF: TSurvivalDatafile); virtual;
    procedure DoOutputSurvival(ResultDF: TSurvivalDatafile); virtual;
//    procedure DoOutputTest(ResultDF: TSurvivalDatafile); virtual;
    procedure DoOutputGraph(ResultDF: TSurvivalDatafile); virtual;
  public
    constructor Create(AExecutor: TExecutor; OutputCreator: TOutputCreator);
    destructor Destroy; override;

    // Method called from Executor, does calculation + result vars + output
    procedure ExecSurvival(Variables: TStrings; ST: TSurvivalCommand);
//    procedure ExecSurvival(DataFile: TEpiDataFile; ST: TSurvivalCommand);
    // Method to be used from elsewhere. Does only calculations and returns the result as a specialized dataset
    function CalcSurvival(DataFile: TEpiDataFile; Variables: TStrings; StratVariable: TStringList;
      FailOutcomeValue: Integer; ST: TOptionList;
      ValueLabelOutput: TEpiGetValueLabelType = gvtValue; VariableLabelOutput: TEpiGetVariableLabelType = gvtVarName): TSurvivalDatafile;
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

function TSurvival.DoCalcSurvival(InputDF: TEpiDataFile;
  Variables: TStrings; StratVariable: TStringList;
  FailOutcomeValue: Integer; ST: TOptionList): TSurvivalDatafile;

var
  StartIdx, Ix, i, EndIdx, FailIx, N, Obs, Row: Integer;
  NAtRisk, Lost: Integer;
  S, SE, SumF, CIMult: EpiFloat;
  T: TTables;
  O:   TEpiReferenceMap;
  Statistics: TTableStatistics;
  ASurvivalTable: TTwowayTable;
  TablesRefMap: TEpiReferenceMap;

begin
  Result := TSurvivalDatafile.Create(nil, 0);

  //FollowVar := InputDF.Fields.FieldByName[FollowVarName];
  //OutcomeVar := InputDF.Fields.FieldByName[OutcomeVarName];
{  FAtRisk: TEpiField;
  FFail: TEpiField;
  FSurvival: TEpiField;
  FLowCI: TEpiField;
  FHighCI: TEpiField;}

// Use TABLES to get counts of outcomes by time for each stratum
  T := TTables.Create(FExecutor, FOutputCreator);
  FSurvivalTable  := T.CalcTables(InputDF, Variables,
    StratVariable, FWeightName, ST, TablesRefMap, Statistics);

// find index of outcome = fail
  ASurvivalTable := FSurvivalTable[0];
  FailIx := -1;
  for Ix := 0 to ASurvivalTable.ColCount - 1 do
    if (ASurvivalTable.ColVariable.AsInteger[Ix] = FailOutcomeValue) then
      begin
        FailIx := Ix;
        break;
      end;
  if (FailIx < 0) then
    begin
      FExecutor.Error('No records with Failure outcome value = ' + IntToStr(FailOutcomeValue));
      exit;
    end;

  if (FStratVarName <> '') then
    begin
      FExecutor.Error('Stratified tables not handled yet');
      exit;
    end
  else
    begin
      Ix := Result.NewRecords();
      NAtRisk := ASurvivalTable.Total;
      S := 1.0;
      Lost := 0;
      SumF := 0.0;

      FConf := StrToInt(FExecutor.SetOptionValue[ANA_SO_CONFIDENCE_INTERVAL]);
      CIMult := PNORMALINV(Float(FConf));

      with Result do
      begin
        for Row := 0 to ASurvivalTable.RowCount - 1 do
          begin
            S := S * Math.Float((NAtRisk - ASurvivalTable.Cell[FailIx,Row].N)) / Math.Float(NAtRisk);
            AtRisk.AsInteger[Row] := NAtRisk;
            Survival.AsFloat[Row] := S;
            // add in calcs for Lower & Upper Limits
            SumF += S / (NAtRisk*(NAtRisk - ASurvivalTable.Cell[FailIx,Row].N));
            SE := CIMult * S * SQRT(SumF);
            FLowCI.AsFloat[Row] := S - SE;
            FHighCI.AsFloat[Row]  := S + SE;
            NAtRisk := NAtRisk - Lost;
            Lost := ASurvivalTable.RowTotal[Row];
          end;
      end;
    end;
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
procedure TSurvival.DoOutputSurvival(ResultDF: TSurvivalDatafile);
var
  T: TOutputTable;
  SmallNumFmt, StatFmt: String;
  Sz, Offset, i, Idx: Integer;
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
  Sz := ResultDF.Size;
  Offset := 0;
  T.ColCount := 5;
  T.RowCount := Sz + 1;
 {
  if (ResultDF.StratifyVarText = '') then
    begin
      Offset := 0;
      T.ColCount := 11;
      T.RowCount := 5;
//      maxVforDisplay := ResultDF.Max.AsFloat[0];
    end
  else
    begin

      Offset     := 1;
      T.ColCount := 12;
      T.RowCount := 3 + (Sz * 2);
//      maxVforDisplay := ResultDF.Max.AsFloat[Sz - 1];
    end;
}
  // Column headers
  if Offset > 0 then T.Cell[0,0].Text := FStratVarName;

  T.Cell[0 + Offset, 0].Text := 'Time';
  T.Cell[1 + Offset, 0].Text := 'At Risk';
  T.Cell[2 + Offset, 0].Text := 'Survival';
  T.Cell[3 + Offset, 0].Text := '( ' + IntToStr(FConf);
  T.Cell[4 + Offset, 0].Text := '% CI)';
  T.SetRowAlignment(0, taRightJustify);
  T.SetRowBoxBorder(0);

  StatFmt := '%8.' + IntToStr(FDecimals) + 'F';
{
  // if option !dx was not specified and max value < 10, then show 2 decimal places instead of the default
  {if (maxVforDisplay < 10.0) and
     (not (ST.HasOption('d' + IntToStr(FDecimals))))
  then
    StatFmt := SmallNumFmt;   }
}
  with ResultDF do
    begin
      for i := 0 to Sz - 1 do
        begin
//          if Offset > 0 then T.Cell[0, i + 1].Text := Category.AsString[i];
          T.Cell[0 + Offset, i + 1].Text := AtRisk.AsString[i];
          T.Cell[1 + Offset, i + 1].Text := Format(StatFmt, [Survival.AsFloat[i]]);
          T.Cell[3 + Offset, i + 1].Text := StatFloatDisplay(StatFmt, LowCI.AsFloat[i]);
          T.Cell[4 + Offset, i + 1].Text := StatFloatDisplay(StatFmt, HighCI.AsFloat[i]);
          T.SetRowAlignment(i + 1, taRightJustify);
          // Need to set this after the entire row has been set to right justify
//          if Offset > 0 then T.Cell[0, i + 1].Alignment := taLeftJustify;
        end;
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
procedure TSurvival.DoOutputGraph(ResultDF: TSurvivalDataFile);
var
  T: TOutputTable;
begin
  T := FOutputCreator.AddTable;
  T.Header.Text := 'Survival Graph';
  T.ColCount       := 1;
  T.RowCount       := 1;
  T.Cell[0,0].Text := 'Graphs not yet implemented';
end;

procedure TSurvival.ExecSurvival(Variables: TStrings; ST: TSurvivalCommand);
var
//  Variables: TStrings;
  AllVariables: TStrings;
  StratVariable: TStringList;
  ResultDF: TSurvivalDatafile;
//  FollowVarName, OutcomeVarName, StratifyVarName: UTF8String;
  FailOutcomeValue: Integer;
  Opt: TOption;
  HasBy: Boolean;
  DF: TEpiDataFile;
begin
// reset result variables;
  FExecutor.ClearResults('$Survival');

  FDecimals := DecimalFromOption(ST.Options);
  FVariableLabelOutput := VariableLabelTypeFromOptionList(ST.Options, FExecutor.SetOptions);
  FValuelabelOutput    := ValueLabelTypeFromOptionList(ST.Options, FExecutor.SetOptions);

  Variables := ST.VariableList.GetIdentsAsList;
  StratVariable := TStringList.Create;
  HasBy := false;
  ST.ExecResult := csrFailed;
  FailOutcomeValue := 0;
  try
    for Opt in ST.Options do
    begin
      // get death outcome value (only integer for now)
      if (Opt.Ident = 'f') then
        begin
          FailOutcomeValue := Opt.Expr.AsInteger;
        end;
      // check for  weight variable
     if (Opt.Ident = 'w') then
       FWeightName := Opt.Expr.AsIdent;

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
      AllVariables.Add(FWeightName);

    DF := FExecutor.PrepareDatafile(AllVariables, AllVariables);

    if DF.Size = 0 then
      begin
        FExecutor.Error('No data!');
        DF.Free;
        Exit;
      end;

    ResultDF := DoCalcSurvival(DF, Variables, StratVariable, FailOutcomeValue, ST.Options);
//    DoResultVariables(ResultDF);

    if (not ST.HasOption('q')) then
      begin
        DoOutputSurvival(ResultDF);

        if (ST.HasOption('g')) then
          DoOutputGraph(ResultDF);

        if (ST.HasOption('t')) then
//          if (ResultDF.StratVariable = '') then
//            FExecutor.Error('!t not valid without !by');
            FExecutor.Error('Stratified analysis not yet implemented');
      end;
    ST.ExecResult := csrSuccess;
    ResultDF.Free;
    DF.Free;
  finally
//    Variables.Free;
end;

end;

function TSurvival.CalcSurvival(DataFile: TEpiDataFile; Variables: TStrings; StratVariable: TStringList;
  FailOutcomeValue: Integer; ST: TOptionList;
  ValueLabelOutput: TEpiGetValueLabelType;
  VariableLabelOutput: TEpiGetVariableLabelType): TSurvivalDatafile;
begin
  FValuelabelOutput := ValueLabelOutput;
  FVariableLabelOutput := VariableLabelOutput;

  Result := DoCalcSurvival(DataFile, Variables, StratVariable, FailOutcomeValue, ST);
end;

end.
