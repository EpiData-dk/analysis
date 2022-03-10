unit survival;

{$codepage UTF-8}
{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ast, epidatafiles, epidatafilestypes, epicustombase,
  tables_types, tables,
  executor, result_variables, epifields_helper, ana_globals,
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
    FDecimals,
    FConf:                Integer;
    FValueLabelOutput:    TEpiGetValueLabelType;
    FVariableLabelOutput: TEpiGetVariableLabelType;
    FFailOutcomeValue:    UTF8String;
    FFailOutcomeText:     UTF8String;
  // table of outcome by time
    FSurvivalTable:       TTwoWayTables;
    FStrata,
    FIntervals:           Integer;
  // survival table results
    FInterval:            Array of Array of UTF8String;
    FTime,
    FAtRisk,
    FFail:                Array of Array of Integer;
    FSurvival,
    FLowCI,
    FHighCI:              Array of Array of EpiFloat;
  // summary results
    FMedian:              Array of UTF8String;
    FLRChi, FLRP:         EpiFloat;
    FWeightVarName,
    FStratVarName,
    FOutcomeVarLabel,
    FTimeVarLabel:        UTF8String;
    FStratLabels:         array of UTF8String;

  protected
    FExecutor: TExecutor;
    FOutputCreator: TOutputCreator;

    procedure DoCalcSurvival(InputDF: TEpiDataFile; Variables: TStrings; StratVariable: TStringList;
                             FailOutcomeValue: UTF8String; ST: TCustomVariableCommand);
    procedure DoResultVariables(ST: TCustomVariableCommand); virtual;
    procedure DoOneResult(Stratum: Integer; Name: UTF8String); virtual;
    procedure DoOutputSurvival(ST:TCustomVariableCommand); virtual;
    procedure DoOutputSummary(ST:TCustomVariableCommand); virtual;
    procedure DoLogRank(); virtual;
    procedure DoOutputGraph(); virtual;
  public
    constructor Create(AExecutor: TExecutor; OutputCreator: TOutputCreator);
    destructor Destroy; override;

    // Method called from Executor, does calculation + result vars + output
    procedure ExecSurvival(Variables: TStrings; ST: TCustomVariableCommand);

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
  i, Failures, FailIx, Row, Col, Stratum, NAtRisk: Integer;
  S, SE, SumF, CIMult: EpiFloat;
  T: TTables;
  Statistics: TTableStatistics;
  ASurvivalTable: TTwowayTable;
  TablesRefMap: TEpiReferenceMap;
  GetMedian: Boolean;
//  SelectRow: array of Boolean;
begin

// Use TABLES to get counts of outcomes by time for each stratum
  T := TTables.Create(FExecutor, FOutputCreator);
  FSurvivalTable  := T.CalcTables(InputDF, Variables,
    StratVariable, FWeightVarName, ST.Options, TablesRefMap, Statistics);

  with FSurvivalTable.UnstratifiedTable do
  begin
    // find index of outcome = fail
    FailIx := -1;
    for Col := 0 to ColCount - 1 do
      if (ColVariable.AsString[Col] = FailOutcomeValue) then
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
    FFailOutcomeText  := ColVariable.GetValueLabelFormatted(FailIx,FValueLabelOutput)
  end;

  FIntervals := FSurvivalTable.UnstratifiedTable.RowCount;
  FStrata :=    FSurvivalTable.Count;
  SetLength(FStratlabels, FStrata);
  SetLength(FInterval, FStrata + 1, FIntervals);
  SetLength(FTime,     FStrata + 1, FIntervals);
  SetLength(FAtRisk,   FStrata + 1, FIntervals);
  SetLength(FFail,     FStrata + 1, FIntervals);
  SetLength(FSurvival, FStrata + 1, FIntervals);
  SetLength(FLowCI,    FStrata + 1, FIntervals);
  SetLength(FHighCI,   FStrata + 1, FIntervals);
  SetLength(FMedian,   Fstrata + 1);


  // set up confidence interval
  FConf := StrToInt(FExecutor.SetOptionValue[ANA_SO_CONFIDENCE_INTERVAL]);
  CIMult := PNORMALINV((Float(100 - FConf) / 200.0));

  // unstratified table first
  ASurvivalTable := FSurvivalTable.UnstratifiedTable;

  for Stratum := 0 to FStrata do
    begin
      if (Stratum > 0) then
      begin
        ASurvivalTable := FSurvivalTable.Tables[Stratum - 1];
        FStratLabels[Stratum - 1] := FSurvivalTable.StratifyVariables.Field[0].GetValueLabel(Stratum - 1);
      end;
      NAtRisk   := ASurvivalTable.Total;
      S         := 1.0;
      SumF      := 0.0;
      GetMedian := true;
      Row := 0;
      for i := 0 to ASurvivalTable.RowCount - 1 do
      begin
        if (NAtRisk > 0) then
          begin
            Failures := ASurvivalTable.Cell[FailIx, i].N;
            S        := S * Float((NAtRisk - Failures)) / Float(NAtRisk);
            SumF     += Float(Failures) / Float(NAtRisk*(NAtRisk - Failures));
            SE       := CIMult * S * SQRT(SumF);
            FInterval[Stratum, Row] := ASurvivalTable.RowVariable.GetValueLabel(i, FValuelabelOutput);
            FTime    [Stratum, Row] := ASurvivalTable.RowVariable.AsInteger[i];
            FAtRisk  [Stratum, Row] := NAtRisk;
            FFail    [Stratum, Row] := Failures;
            FSurvival[Stratum, Row] := S;
            FLowCI   [Stratum, Row] := max(S - SE , 0);
            FHighCI  [Stratum, Row] := min(S + SE , 1);
            if (GetMedian) then
              if (S <= 0.5) then
                begin
                  FMedian[Stratum] := FInterval[Stratum, Row];
                  GetMedian        := false;
                end;
            Row += 1;
          end;

        NAtRisk  := NAtRisk - ASurvivalTable.RowTotal[i];
      end;
    end;
  ST.ExecResult := csrSuccess;

  T.Free;
end;

procedure TSurvival.DoLogRank();
var
  SumExp: EpiFloat;
  Exp: EpiFloat;
  SumFail: Integer;
  d: EpiFloat;
  i, Stratum: Integer;
begin
  if (FStrata = 0) then exit;  // Error - should not happen

  FLRChi := 0;
  for Stratum := 1 to FStrata do
    begin
      SumExp := 0;
      SumFail := 0;
      for i := 0 to FIntervals - 1 do
        if (FFail[0,i] > 0) then
          begin
            Exp     := float(FFail[0, i] * FAtRisk[Stratum, i])/float(FAtRisk[0, i]);
            SumExp  +=  Exp;
            SumFail += FFail[Stratum, i];
          end;

      d := float(SumFail) - SumExp;
      FLRChi += (d * d) / SumExp;
    end;
  FLRP := ChiPValue(FLRChi , FStrata - 1);
end;

procedure TSurvival.DoOneResult(Stratum: Integer; Name: UTF8String);
var
  i, Sz, rSz: Integer;
  Vt, Vs: array of EpiFloat;
  rVt, rVs: TExecVarVector;
begin
  // put data in temp arrays, so we know the size
  Sz := Length(FSurvival[Stratum]);
  SetLength(Vt, Sz);
  SetLength(Vs, Sz);
  rSz := 0;
  for i := 0 to Sz - 1 do
    if FFail[Stratum, i] > 0 then
    begin
      Vt[rSz] := FTime[Stratum, i];
      Vs[rSz] := FSurvival[Stratum, i];
      rSz += 1;
    end;
  // result vectors
  with FExecutor do
  begin
    rVt := AddResultVector('$survival_time_' + Name, ftFloat, rSz);
    rVs := AddResultVector('$survival_estimate_' + Name, ftFloat, rSz);
    for i := 0 to rSz - 1 do
      begin
        rVt.AsFloatVector[i] := FTime[Stratum, i];
        rVs.AsFloatVector[i] := FSurvival[Stratum, i];
      end;
  end;
end;

procedure TSurvival.DoResultVariables(ST:TCustomVariableCommand);
var
  Stratum: Integer;
  sNames: TExecVarVector;
begin
  DoOneResult(0, 'all');

  if FStrata > 0 then
  begin
    sNames := FExecutor.AddResultVector('$survival_strata', ftString, FStrata);
    if (ST.HasOption('t') and (FStrata > 0)) then
    begin
      FExecutor.AddResultConst('$survival_' + 'chi2', ftFloat).AsFloatVector[0] := FLRP;
      FExecutor.AddResultConst('$survival_' + 'chip', ftFloat).AsFloatVector[0] := FLRChi;
    end;
    for Stratum := 1 to FStrata do
      begin
        sNames.AsStringVector[Stratum-1] := FStratLabels[Stratum-1];
        DoOneResult(Stratum, FStratLabels[Stratum-1]);
      end;
  end;
end;
procedure TSurvival.DoOutputSurvival(ST:TCustomVariableCommand);
var
  T: TOutputTable;
  StatFmt: String;
  Sz, Offset, i: Integer;
  Stratum, FirstStratum, LastStratum, ColPerStratum: Integer;

  function StatFloatDisplay(const fmt: String; const val: EpiFloat):string;
  begin
    if (val = TEpiFloatField.DefaultMissing) then
      Result := TEpiStringField.DefaultMissing
    else
      Result := Format(fmt, [val]);
  end;

begin
  T             := FOutputCreator.AddTable;
  T.Header.Text := 'Kaplan Meier Survival Analysis - Life Tables';
  // show only rows with failures
  Sz := 0;
  for i := 0 to Length(FFail[0]) - 1 do
    if (FFail[0,1] > 0) then
      Sz += 1;
//  Sz            := Length(FAtRisk[0]);
  T.RowCount    := 0;
  ColPerStratum := 4;

  // set up output table size based on strata and options

  if (ST.HasOption('nou')) then
    begin
      if (ST.HasOption('nos')) then exit;
      T.RowCount   := 3;
      T.ColCount   := 1;
      FirstStratum := 1;
      Offset       := 1;
    end
  else
    begin
      T.RowCount       := 3;
      T.ColCount       := 1 + ColPerStratum;
      T.Cell[1,0].Text := 'All data';
      FirstStratum     := 0;
      Offset           := 1 + ColPerStratum;
    end;

  if (ST.HasOption('nos')) then
    LastStratum := 0
  else
    begin
      T.ColCount := T.ColCount + ColPerStratum * FStrata;
      for i := 0 to FStrata -1 do
        begin
          T.Cell[Offset + ColPerStratum * i,     0].Text := FStratVarName + ' = ';
          T.Cell[Offset + ColPerStratum * i + 1, 0].Text := FStratLabels[i];
        end;
      LastStratum := FStrata;
    end;

  T.Cell[0, 1].Text := 'Follow';
  T.Cell[0, 2].Text := FTimeVarLabel;
  Offset            := 1;

  // Column headers
  for Stratum := FirstStratum to LastStratum do
    begin
      T.Cell[    Offset, 1].Text := '# At';
      T.Cell[1 + Offset, 1].Text := FOutcomeVarLabel;
      T.Cell[2 + Offset, 1].Text := ' ';
      T.Cell[3 + Offset, 1].Text := ' ';
      T.Cell[    Offset, 2].Text := 'Risk';
      T.Cell[1 + Offset, 2].Text := FFailOutcomeText;
      T.Cell[2 + Offset, 2].Text := 'Survival';
      T.Cell[3 + Offset, 2].Text := '(' + IntToStr(FConf) + '% CI)';
      Offset += ColPerStratum;
    end;
  T.SetRowAlignment(1, taRightJustify);
  StatFmt := '%' + IntToStr(3 + FDecimals) + '.' + IntToStr(FDecimals) + 'F';

  // show rows with failures only
  // show stratum results with failures only
  Sz := 3;
  for i := 0 to Length(FFail[0]) - 1 do
    if (FFail[0, i] > 0) then
    begin
      T.RowCount := Sz + 1;
      T.Cell[0, Sz].Text := FInterval[0, i];
      Offset := 1;
      for Stratum := FirstStratum to LastStratum do
        begin
          if (FFail[Stratum, i] > 0) then
            begin
              T.Cell[Offset    , Sz].Text := IntToStr(FAtRisk[Stratum, i]);
              T.Cell[Offset + 1, Sz].Text := IntToStr(FFail[Stratum, i]);
              T.Cell[Offset + 2, Sz].Text := Format(StatFmt, [FSurvival[Stratum, i]]);
              T.Cell[Offset + 3, Sz].Text := FormatCI(FLowCI[Stratum, i], FHighCI[Stratum, i], 0, ST.Options);
              T.SetRowAlignment(Sz, taRightJustify);
            end;
          Offset += ColPerStratum;
        end;
      Sz +=1;
    end;

  Offset := 0;
{  for Stratum := FirstStratum to LastStratum do   // causes double-spaced output!
    begin
      T.SetColBorders(Offset, [cbRight]);
      Offset += ColPerStratum;
    ; }
  T.SetRowBorders(1, [cbTop]);
  T.SetRowBorders(2, [cbBottom]);

end;

procedure TSurvival.DoOutputSummary(ST:TCustomVariableCommand);
var
  T: TOutputTable;
  StatFmt: String;
  i:  Integer;
begin
  StatFmt := '%' + IntToStr(3 + FDecimals) + '.' + IntToStr(FDecimals) + 'F';

  T                 := FOutputCreator.AddTable;
  T.Header.Text     := 'Kaplan-Meier Survival Analysis - Summary';
  T.ColCount        := FStrata + 2;
  T.RowCount        := 3;
  T.Cell[1, 1].Text := 'All Data';
  T.Cell[0, 2].Text := 'Median Survival';
  if (FStrata > 0) then
    T.Cell[2, 0].Text := FStratVarName;

  for i := 0 to FStrata do
    begin
      if (i > 0) then
        T.Cell[i + 1, 1].Text := FStratLabels[i-1];
      T.Cell[i + 1, 2].Text := FMedian[i];
    end;
    T.SetRowBorders(1, [cbBottom]);

  if ((FStrata > 0) and (ST.HasOption('t'))) then
    T.Footer.Text := 'Log-Rank Chi-square = ' + Format(StatFmt, [FLRChi]) + ' ' + FormatP(FLRP, true);
  T.SetRowBorders(1, [cbTop]);
  T.SetRowBorders(2, [cbBottom]);
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

procedure TSurvival.DoOutputGraph();
var
  Tab: TOutputTable;
//  SPlot: array of array of EpiFloat;  // holds plot points for this graph
  Stratum, Row, i, s1:   Integer;
  LastS, LastLL, LastUL: EpiFloat;
  s, d, d0, t:           UTF8String;
begin
  Tab := FOutputCreator.AddTable;
  s := 'Survival Graph - plot points';

  with Tab do
  // loop from here for all strata; only call once
  begin
    Header.Text    := s;

//  Delimited output
    d              := FExecutor.SetOptions.GetValue(ANA_SO_CLIPBOARD_DELIMITER).Value;
    d0             := d + d + d;
    ColCount       := 1;
    RowCount       := 3;
    Cell[0,0].Text := d;
    Cell[0,1].Text := 'Time';
    Cell[0,2].Text := '0';
    if (FStrata > 0) then              // only provide plot points for strata if !by was used
      s1 := 1
    else
      s1 := 0;
    for Stratum := s1 to FStrata do
      begin
        if (Stratum = 0) then
          Cell[0,0].Text := Cell[0,0].Text + 'all data' + d0
        else
          Cell[0,0].Text := Cell[0,0].Text + FStratVarName + ' = ' + FStratLabels[Stratum - 1] + d0;
        Cell[0,1].Text := Cell[0,1].Text + d + 'Survival' + d + 'CIlow' + d + 'CIHigh';
        Cell[0,2].Text := Cell[0,2].Text + d + '1.00' + d + '1.00' + d + '1.00';

        LastS          := 1;
        LastUL         := 1;
        LastLL         := 1;
        Row            := 3;
        for i := 0 to length(FAtRisk[Stratum]) - 1 do
          // only include rows where there were failures
          if (FFail[0, i] > 0) then
          begin
            if (Stratum = s1) then
            begin
              RowCount            := Row + 2;
              Cell[0, Row    ].Text   := IntToStr(FTime[Stratum, i]);
              Cell[0, Row + 1].Text   := IntToStr(FTime[Stratum, i]);
            end;
            if (FAtRisk[Stratum, i] > 0) then
            begin
              Cell[0, Row].Text     := Cell[0, Row].Text +
                                       d + trim(Format('%6.3f', [LastS])) +
                                       d + trim(Format('%6.3f', [LastLL])) +
                                       d + trim(Format('%6.3f', [LastUL]));
              LastS                 := FSurvival[Stratum, i];
              LastLL                := FLowCI[Stratum, i];
              LastUL                := FHighCI[Stratum, i];
              Cell[0, Row + 1].Text := Cell[0, Row + 1].Text +
                                       d + trim(Format('%6.3f', [LastS])) +
                                       d + trim(Format('%6.3f', [LastLL])) +
                                       d + trim(Format('%6.3f', [LastUL]));
            end
            else
            begin
              Cell[0, Row    ].Text := Cell[0, Row    ].Text + d0;
              Cell[0, Row + 1].Text := Cell[0, Row + 1].Text + d0;
            end;
            Row  += 2;
          end;
//        d0 := d;
      end;
    SetColAlignment(0, taLeftJustify);
    Footer.Text := 'To copy to graphing software, select the plot points, right-click, copy selected text'
  end;
end;

procedure TSurvival.ExecSurvival(Variables: TStrings; ST: TCustomVariableCommand);
var
  AllVariables:     TStrings;
  StratVariable:    TStringList;
  FailOutcomeValue: UTF8String;
  Opt:              TOption;
  HasBy, HasO:      Boolean;
  DF:               TEpiDataFile;
  i:                Integer;
begin
// reset result variables;
  FExecutor.ClearResults('$survival');

  StratVariable        := TStringList.Create;
  FDecimals            := DecimalFromOption(ST.Options, 3);
  FVariableLabelOutput := VariableLabelTypeFromOptionList(ST.Options, FExecutor.SetOptions);
  FValueLabelOutput    := ValueLabelTypeFromOptionList(ST.Options, FExecutor.SetOptions);
  AllVariables         := ST.VariableList.GetIdentsAsList;
//  AllVariables.AddStrings(Variables);

  HasBy := false;
  HasO  := false;
  ST.ExecResult := csrFailed;
  FailOutcomeValue := '0';
  try
    for Opt in ST.Options do
    begin
      // get death outcome value
      if (Opt.Ident = 'o') then
        begin
          // check for more than one !o
          if (HasO) then
            begin
              FExecutor.Error('Cannot specify more than one outcome. !o:=' + Opt.Expr.AsIdent + ' is invalid');
              exit;
            end;
          FailOutcomeValue := Opt.Expr.AsString;
          HasO := true;
        end;

      if (Opt.Ident = 'by') then
        // check for more than one !by
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
                FExecutor.Error('Cannot stratify by ' + Opt.expr.AsIdent);
                Exit;
              end;
            FStratVarName := Opt.Expr.AsIdent;
            StratVariable.Add(FStratVarName);
            AllVariables.AddStrings(FStratVarName);
          end;

    // check for  weight variable
    if (Opt.Ident = 'w') then
      begin
        if ( (Variables[0] = Opt.Expr.AsIdent) or (Variables[1] = Opt.Expr.AsIdent) ) then
          begin
            FExecutor.Error(Opt.expr.AsIdent + ' cannot be used as a weight');
            Exit;
          end;
        FWeightVarName := Opt.Expr.AsIdent;
        AllVariables.Add(FWeightVarName);
      end;
    end;

    DF := FExecutor.PrepareDatafile(AllVariables, AllVariables);

    if DF.Size = 0 then
      begin
        FExecutor.Error('No data!');
        DF.Free;
        Exit;
      end;

    // save labels for other procedures
    FTimeVarLabel    := DF.Fields.FieldByName[Variables[1]].GetVariableLabel(FVariableLabelOutput);
    FOutcomeVarLabel := DF.Fields.FieldByName[Variables[0]].GetVariableLabel(FVariableLabelOutput);

    DoCalcSurvival(DF, Variables, StratVariable, FailOutcomeValue, ST);

    if (ST.ExecResult = csrSuccess) then
      begin

        if (not ST.HasOption('q')) then
          begin
            if (not ST.HasOption('nt')) then
              DoOutputSurvival(ST);

            if (ST.HasOption('t') and (FStrata > 0)) then
              DoLogRank();

            if (not ST.HasOption('ns')) then
              DoOutputSummary(ST);

            if (not ST.HasOption('ng')) then
              DoOutputGraph();

          end;

      DoResultVariables(ST);
      end;
    DF.Free;
  finally
    StratVariable.Free;
    AllVariables.Free;
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

