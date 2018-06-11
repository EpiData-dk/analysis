unit tables;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, math, epidatafiles, ast, executor, result_variables,
  outputcreator, epicustombase, tables_types;

type

  { TTwoWayDatafile }

  TTwoWayDatafile = class(TEpiDataFile, ITableVariables)
  private
    // Aggregated two way variables
    FColVar: TEpiField;
    FRowVar: TEpiField;
  private
    function CheckIndex(Col, Row: Integer): Integer;
    function GetCounts(const Col, Row: Integer): Integer;
    function GetNVar: TEpiField;
    function GetColVar: TEpiField;
    function GetRowVar: TEpiField;
  protected
    property NVar: TEpiField read GetNVar;
  public
    property Counts[Const Col, Row: Integer]: Integer read GetCounts;
    property ColVariable: TEpiField read GetColVar;
    property RowVariable: TEpiField read GetRowVar;
  end;

  { TTables }

  TTables = class
  private
    FExecutor: TExecutor;
    FOutputCreator: TOutputCreator;
    function  DoCalcTables(InputDF: TEpiDataFile;
      Varnames, StratifyNames: TStrings; Const WeightVariable: UTF8String = ''): TTwoWayTables;
    procedure OutputTwoWayTable(Table: TTwoWayTable; StratifyVariables: TEpiFields; ST: TTablesCommand; IsUnstratifiedTable: boolean);
    procedure OutputSummaryTable(Tables: TTwoWayTables; ST: TTablesCommand);
    function  PackStratifiedDataset(Sender: TEpiDataFile; Index: Integer; Data: Pointer): boolean;
    function  GetStatisticOptions(ST: TTablesCommand): TTableStatistics;
  protected
    procedure DoOutputTables(Tables: TTwoWayTables; ST: TTablesCommand);
    procedure DoTableStatistics(Tables: TTwoWayTables; Statistics: TTableStatistics);
    function  DoSortTables(Tables: TTwoWayTables; ST: TTablesCommand): boolean;
    procedure DoResultVariables(Tables: TTwoWayTables);
  public
    constructor Create(AExecutor: TExecutor; AOutputCreator: TOutputCreator);
    destructor Destroy; override;
    // Method called from Executor, does calculation + result vars + output
    procedure ExecTables(DF: TEpiDataFile; ST: TTablesCommand);
    // Method to be used from elsewhere. Does only calculations and returns the result as a specialized dataset
    function  CalcTables(InputDF: TEpiDataFile; VariableNames: TStrings;
      StratifyNames: TStrings; Const WeightName: UTF8String;

      Out RefMap: TEpiReferenceMap; Statistics: TTableStatistics = []): TTwoWayTables;
  end;

procedure RegisterTableStatistic(Statistic: TTableStatistic; StatClass: TTwoWayStatisticsClass);

implementation

uses
  aggregate, aggregate_types, epimiscutils, epidatafileutils, epifields_helper,
  options_utils, LazUTF8, ana_globals, epidatafilestypes, options_table, strutils,
  tables_stat_chi2, tables_stat_fexp;

type
  PBoundArray = ^TBoundArray;
  TPackRecord = record
    Runners: PBoundArray;
    Variables: TEpiFields;
  end;

var
  StatisticsMap: TStatisticsMap;

procedure RegisterTableStatistic(Statistic: TTableStatistic;
  StatClass: TTwoWayStatisticsClass);
begin
  if (not Assigned(StatisticsMap)) then
    StatisticsMap := TStatisticsMap.Create;

  StatisticsMap.Add(Statistic, StatClass);
end;

{ TTwoWayDatafile }

function TTwoWayDatafile.CheckIndex(Col, Row: Integer): Integer;
begin
  if (Col < 0) or (Col >= ColVariable.Size) or
     (Row < 0) or (Row >= RowVariable.Size)
  then
    raise Exception.Create('TTwoWayDatafile: Index out of bounds!');

  Result := Col * RowVariable.Size + Row;
end;

function TTwoWayDatafile.GetCounts(const Col, Row: Integer): Integer;
var
  Index: Integer;
begin
  Index := CheckIndex(Col, Row);
  if NVar.IsMissing[Index] then
    Result := 0
  else
    Result := NVar.AsInteger[Index];
end;

function TTwoWayDatafile.GetNVar: TEpiField;
begin
  result := Fields[Fields.Count - 1];
end;

function TTwoWayDatafile.GetColVar: TEpiField;
begin
  result := FColVar;
end;

function TTwoWayDatafile.GetRowVar: TEpiField;
begin
  result := FRowVar;
end;

{ TTables }

function TTables.DoCalcTables(InputDF: TEpiDataFile;
  Varnames, StratifyNames: TStrings; const WeightVariable: UTF8String): TTwoWayTables;
var
  FuncList: TAggrFuncList;
  Aggr: TAggregate;
  RefMap: TEpiReferenceMap;
  AggrVars: TStringList;
  AggregatedVariables, TwoWayVariables, StratifyVariables: TEpiFields;
  S: String;
  i: Integer;
  Runners: TBoundArray;
  PackRecord: TPackRecord;
  F: TEpiField;
  SubTable: TTwoWayTable;
  TempDF, AggregatedDF: TTwoWayDatafile;

begin
  // Create counts of the Stratification variables.
  FuncList := TAggrFuncList.Create(true);
  if (WeightVariable <> '') then
    FuncList.Add(TAggrSum.Create('n', InputDF.Fields.FieldByName[WeightVariable]))
  else
    FuncList.Add(TAggrCount.Create('n', nil, acAll));

  // Combine the two-way variables with all by variables
  // - these variables will be aggregated (and counted)
  AggrVars := TStringList.Create;
  AggrVars.AddStrings(Varnames);
  AggrVars.AddStrings(StratifyNames);

  Aggr := TAggregate.Create(FExecutor, FOutputCreator);
  Aggr.ResultDataFileClass := TTwoWayDatafile;
  AggregatedDF := TTwoWayDatafile(Aggr.CalcAggregate(InputDF, AggrVars, FuncList, True, AggregatedVariables, RefMap));
  AggrVars.Free;

  FExecutor.Document.DataFiles.AddItem(AggregatedDF);
  RefMap.FixupReferences;

  TwoWayVariables := TEpiFields.Create(nil);
  TwoWayVariables.SetLanguage(AggregatedVariables.DefaultLang, true);
  TwoWayVariables.AddItem(AggregatedVariables.DeleteItem(0));
  TwoWayVariables.AddItem(AggregatedVariables.DeleteItem(0));

  AggregatedDF.FColVar := TwoWayVariables.Field[0];
  AggregatedDF.FRowVar := TwoWayVariables.Field[1];

  StratifyVariables := TEpiFields.Create(nil);
  StratifyVariables.ItemOwner := true;
  StratifyVariables.SetLanguage(AggregatedVariables.DefaultLang, true);
  for i := AggregatedVariables.Count - 1 downto 0 do
    StratifyVariables.InsertItem(0, AggregatedVariables.DeleteItem(i));

  Result := TTwoWayTables.Create(StratifyVariables);

  AggregatedVariables.Free;

  // Create a series of Datafiles for each stratum
  if (StratifyVariables.Count > 0) then
    begin
      SetLength(Runners, StratifyVariables.Count);
      FillDWord(Runners[0], Length(Runners), 0);

      PackRecord.Variables := StratifyVariables;
      PackRecord.Runners   := @Runners;

      while (Runners[0] < StratifyVariables[0].Size) do
        begin
          TempDF := TTwoWayDatafile(AggregatedDF.Clone(nil, RefMap));
          TempDF.Name := '@tab' + IntToStr(Result.Count);
          TempDF.Pack(@PackStratifiedDataset, @PackRecord);

          TempDF.FColVar := TwoWayVariables.Field[0];
          TempDF.FRowVar := TwoWayVariables.Field[1];

          SubTable := TTwoWayTable.Create(TempDF, Runners);
          TempDF.Free;

          Result.AddTable(SubTable);

          for i := High(Runners) downto Low(Runners) do
            begin
              Runners[i] := Runners[i] + 1;

              // Have we reach "end-of-size" for current runner?
              if (Runners[i] < StratifyVariables[i].Size) then
                break;

              // Yes - then reset it back to 0
              if (I > 0) then
                Runners[i] := 0;
            end;
        end;

      TempDF := TTwoWayDatafile(Aggr.CalcAggregate(InputDF, Varnames, FuncList, True, AggregatedVariables, RefMap));
      TempDF.FColVar := TwoWayVariables.Field[0];
      TempDF.FRowVar := TwoWayVariables.Field[1];
      SubTable := TTwoWayTable.Create(TempDF, nil);
      Result.AddTable(SubTable, true);
    end
  else
    begin
      SubTable := TTwoWayTable.Create(AggregatedDF, nil);
      Result.AddTable(SubTable, true);
    end;

  AggregatedDF.Free;
  Aggr.Free;
  FuncList.Free;
end;

procedure TTables.OutputTwoWayTable(Table: TTwoWayTable;
  StratifyVariables: TEpiFields; ST: TTablesCommand; IsUnstratifiedTable: boolean);
var
  T: TOutputTable;
  VariableLabelType: TEpiGetVariableLabelType;
  ValueLabelType: TEpiGetValueLabelType;
  Col, Row, i, ColumnFactor, Idx, Decimals: Integer;
  S, ColSeparator: String;
  Opt: TOption;
  ShowRowPercent, ShowColPercent, ShowTotPercent: Boolean;
  Stat: TTwoWayStatistic;
  ColOption, RowOption, TotOption: TTablePercentFormatOption;

  function FormatPercent(Value: EpiFloat; SetOption: TTablePercentFormatOption): UTF8String;
  begin
    if IsNan(Value) or
       IsInfinite(Value)
    then
      Result := '-'
    else
      Result := OutputCreatorNormalizeText(SetOption.LeftChar) +
                Format('%.' + IntToStr(Decimals) + 'f', [Value * 100]) +
                OutputCreatorNormalizeText(SetOption.RigthChar);
  end;
 // TODO: Do not display a row if row total is zero; should be same for columns
begin
  ColSeparator      := '';
  if (ST.HasOption('cs', Opt)) then
    ColSeparator    := Opt.Expr.AsString;
  Decimals          := DecimalFromOption(ST.Options);
  VariableLabelType := VariableLabelTypeFromOptionList(ST.Options, FExecutor.SetOptions);
  ValueLabelType    := ValueLabelTypeFromOptionList(ST.Options, FExecutor.SetOptions);

  // Table setup
  T := FOutputCreator.AddTable;

  // Header
  if IsUnstratifiedTable then
    T.Header.Text := Table.ColVariable.GetVariableLabel(VariableLabelType)
  else
    begin
      S := '';

      for i := Low(Table.StratifyIndices) to High(Table.StratifyIndices) do
        S := S + StratifyVariables[i].GetVariableLabel(VariableLabelType) + ': ' + StratifyVariables[i].GetValueLabel(Table.StratifyIndices[i], ValueLabelType) + LineEnding;

      T.Header.Text := S + Table.ColVariable.GetVariableLabel(VariableLabelType);
    end;

  // Add percentages?
  ColumnFactor := 1;
  ShowRowPercent := ST.HasOption('pr');
  ShowColPercent := ST.HasOption('pc');
  ShowTotPercent := ST.HasOption('pt');
  if ShowRowPercent then Inc(ColumnFactor);
  if ShowColPercent then Inc(ColumnFactor);
  if ShowTotPercent then Inc(ColumnFactor);
  ColOption := TTablePercentFormatOption(FExecutor.SetOptions[ANA_SO_TABLE_PERCENT_FORMAT_COL]);
  RowOption := TTablePercentFormatOption(FExecutor.SetOptions[ANA_SO_TABLE_PERCENT_FORMAT_ROW]);
  TotOption := TTablePercentFormatOption(FExecutor.SetOptions[ANA_SO_TABLE_PERCENT_FORMAT_TOTAL]);

  //               Row header   + Table colums (and percents)     + Row totals (and percentes)
  T.ColCount    := 1 +            (Table.ColCount * ColumnFactor) + (1 * ColumnFactor);
  //               Col header   + Table rows                      + Col totals
  T.RowCount    := 1            + Table.RowCount                  + 1;

  T.Cell[0, 0].Text := Table.RowVariable.GetVariableLabel(VariableLabelType);

  // Col headers
  for Col := 0 to Table.ColCount - 1 do
    begin
      Idx := (Col * ColumnFactor) + 1;

      S := '';
{      if Col > 0 then
        S := S + ColSeparator;}
      S := S + Table.ColVariable.GetValueLabel(Col, ValueLabelType);

      T.Cell[PostInc(Idx), 0].Text                        := S;
      if ShowRowPercent then T.Cell[PostInc(Idx), 0].Text := FExecutor.SetOptionValue[ANA_SO_TABLE_PERCENT_HEADER];
      if ShowColPercent then T.Cell[PostInc(Idx), 0].Text := FExecutor.SetOptionValue[ANA_SO_TABLE_PERCENT_HEADER];
      if ShowTotPercent then T.Cell[PostInc(Idx), 0].Text := FExecutor.SetOptionValue[ANA_SO_TABLE_PERCENT_HEADER];
    end;
  T.Cell[T.ColCount - 1, 0].Text := 'Total';
  T.SetRowBorders(0, [cbBottom, cbTop]);

  // Row headers
  for Row := 0 to Table.RowCount - 1 do
    T.Cell[0, Row + 1].Text := Table.RowVariable.GetValueLabel(Row, ValueLabelType);
  T.Cell[0, T.RowCount - 1].Text := 'Total';

  // Counts
  for Col := 0 to Table.ColCount - 1 do
    for Row := 0 to Table.RowCount - 1 do
      begin
        Idx := (Col * ColumnFactor) + 1;

        S := ColSeparator + IntToStr(Table.Cell[Col, Row].N);

        T.Cell[PostInc(Idx), Row + 1].Text                        := S;
        if ShowRowPercent then T.Cell[PostInc(Idx), Row + 1].Text := FormatPercent(Table.Cell[Col, Row].RowPct,   RowOption);
        if ShowColPercent then T.Cell[PostInc(Idx), Row + 1].Text := FormatPercent(Table.Cell[Col, Row].ColPct,   ColOption);
        if ShowTotPercent then T.Cell[PostInc(Idx), Row + 1].Text := FormatPercent(Table.Cell[Col, Row].TotalPct, TotOption);
      end;

  // Col Totals
  for Col := 0 to Table.ColCount - 1 do
    begin
      Idx := (Col * ColumnFactor) + 1;
      Row := T.RowCount - 1;

      T.Cell[PostInc(Idx), T.RowCount - 1].Text             := IntToStr(Table.ColTotal[Col]);
      if ShowRowPercent then T.Cell[PostInc(Idx), Row].Text := FormatPercent(Table.ColTotal[Col] / Table.Total, RowOption);
      if ShowColPercent then T.Cell[PostInc(Idx), Row].Text := FormatPercent(1, ColOption);
      if ShowTotPercent then T.Cell[PostInc(Idx), Row].Text := FormatPercent(Table.ColTotal[Col] / Table.Total, TotOption);
    end;

  // Row Totals
  for Row := 0 to Table.RowCount - 1 do
    begin
      Idx := T.ColCount - (1 * ColumnFactor);

      T.Cell[PostInc(Idx), Row + 1].Text := IntToStr(Table.RowTotal[Row]);
      if ShowRowPercent then T.Cell[PostInc(Idx), Row + 1].Text := FormatPercent(1, RowOption);
      if ShowColPercent then T.Cell[PostInc(Idx), Row + 1].Text := FormatPercent(Table.RowTotal[Row] / Table.Total, ColOption);
      if ShowTotPercent then T.Cell[PostInc(Idx), Row + 1].Text := FormatPercent(Table.RowTotal[Row] / Table.Total, TotOption);
    end;

  // Grand total
  T.Cell[T.ColCount - (1 * ColumnFactor), T.RowCount - 1].Text := IntToStr(Table.Total);

  T.SetRowBorders(T.RowCount - 1, [cbBottom]);

  // Footer
  T.Footer.Alignment := taLeftJustify;
  S := '';
  if (ST.HasOption('pc')) then S := S + RowOption.LeftChar + 'Row'   + RowOption.RigthChar;
  if (ST.HasOption('pr')) then S := S + ColOption.LeftChar + 'Col'   + ColOption.RigthChar;
  if (ST.HasOption('pt')) then S := S + TotOption.LeftChar + 'Total' + TotOption.RigthChar;
  if (S <> '') then
    T.Footer.Text := T.Footer.Text + 'Percents: ' + OutputCreatorNormalizeText(S) + LineEnding;

  if (ST.HasOption('w', Opt)) then
    T.Footer.Text := T.Footer.Text + 'Weight: ' + Opt.Expr.AsIdent;



  for i := 0 to Table.StatisticsCount - 1 do
    begin
      Stat := Table.Statistics[i];
      Stat.AddToOutput(T);

      if (ST.HasOption('debug')) then
        Stat.DebugOutput(FOutputCreator);
    end;
end;

procedure TTables.OutputSummaryTable(Tables: TTwoWayTables; ST: TTablesCommand);
var
  T: TOutputTable;
  VariableLabelType: TEpiGetVariableLabelType;
  ValueLabelType: TEpiGetValueLabelType;
  S, S1: UTF8String;
  F: TEpiField;
  Tab: TTwoWayTable;
  RowIdx, i: Integer;
begin
  // Formatting of variables and values
  VariableLabelType := VariableLabelTypeFromOptionList(ST.Options, FExecutor.SetOptions);
  ValueLabelType    := ValueLabelTypeFromOptionList(ST.Options, FExecutor.SetOptions);

  // Create basic summary table
  T := FOutputCreator.AddTable;
  T.ColCount := 2;
  T.RowCount := Tables.Count + 3;
  T.SetColAlignment(0, taLeftJustify);

  // Collect header information
  S := Tables.UnstratifiedTable.ColVariable.GetVariableLabel(VariableLabelType) + ' by ' +
       Tables.UnstratifiedTable.RowVariable.GetVariableLabel(VariableLabelType);

  if (Assigned(Tables.StratifyVariables)) then
    begin
      S := S + ' adjusted for:';

      S1 := ' ';
      if (VariableLabelType <> gvtVarName) then
        S1 := LineEnding;

      for F in Tables.StratifyVariables do
        S := S + S1 + F.GetVariableLabel(VariableLabelType);
    end;
  T.Header.Text := S;

  // Basic summary information
  T.Cell[1, 0].Text := 'N';
  T.Cell[0, 1].Text := 'Crude';
  T.Cell[1, 1].Text := IntToStr(Tables.UnstratifiedTable.Total);
  T.Cell[0, 2].Text := 'Adjusted';
  T.Cell[1, 2].Text := '-';

  RowIdx := 3;
  for Tab in Tables do
    begin
      S := '';

      for i := 0 to Tables.StratifyVariables.Count - 1 do
        begin
          F := Tables.StratifyVariables[i];
          S := S + ' ' + F.GetVariableLabel(VariableLabelType) + ': ' + F.GetValueLabel(Tab.StratifyIndices[i], ValueLabelType) + LineEnding;
        end;
      if (Tables.StratifyVariables.Count = 1) then
        S := UTF8Trim(S, [u8tKeepStart])
      else
        S := S + ' ';

      T.Cell[0, RowIdx].Text := S;
      T.Cell[1, RowIdx].Text := IntToStr(Tab.Total);
      Inc(RowIdx);
    end;

  for i := 0 to Tables.StatisticsCount -1  do
    Tables.Statistics[i].AddToSummaryTable(T);


  // Setting up borders
  T.SetRowBorders(0, [cbBottom, cbTop]);
  T.SetRowBorders(T.RowCount - 1, [cbBottom]);
end;

function TTables.PackStratifiedDataset(Sender: TEpiDataFile; Index: Integer;
  Data: Pointer): boolean;
var
  R: TBoundArray;
  Vars: TEpiFields;
  F1, F2: TEpiField;
  i: Integer;
  Cmp: TValueSign;
begin
  Result := false;

  R := TPackRecord(Data^).Runners^;
  Vars := TPackRecord(Data^).Variables;

  for i := 0 to Vars.Count - 1 do
    begin
      F1 := Vars[i];
      F2 := Sender.Fields.FieldByName[F1.Name];

      CompareFieldRecords(Cmp, F1, F2, R[i], Index);
      if (Cmp <> 0) then
        Exit(true);
    end;
end;

function TTables.GetStatisticOptions(ST: TTablesCommand): TTableStatistics;
begin
  result := [];

  if (ST.HasOption('t')) then
     Include(Result, tsChi2);
  if (ST.HasOption('ex')) then
     Include(Result, tsFExP);
  if (ST.HasOption('odds')) then
     Include(Result, tsOR);
  if (ST.HasOption('rr')) then
     Include(Result, tsRR);

end;

procedure TTables.DoOutputTables(Tables: TTwoWayTables; ST: TTablesCommand);
var
  Tab: TTwoWayTable;
begin
  if (not ST.HasOption('nc')) then
    OutputTwoWayTable(Tables.UnstratifiedTable, Tables.StratifyVariables, ST, true);

  // Sub table output
  if (not ST.HasOption('nb')) then
    for Tab in Tables do
      OutputTwoWayTable(Tab, Tables.StratifyVariables, ST, False);

  if (not ST.HasOption('ns')) and
     (Tables.Count > 1)   // no summary if only one table
  then
    OutputSummaryTable(Tables, ST);
end;

procedure TTables.DoTableStatistics(Tables: TTwoWayTables;
          Statistics: TTableStatistics);
var
  Stat: TTableStatistic;
  StatObj: TTwoWayStatistics;
  Index: Integer;
  procedure RaiseError;
  begin
    raise Exception.Create('A table statistic was not correctly registered!');
  end;

begin

  for Stat in Statistics do
    begin
      // This should only happen if a statistic unit did not call
      // RegisterTableStatistic in the initialization part!
      if (not StatisticsMap.Find(Stat, Index)) then
        RaiseError;

      StatObj := StatisticsMap.Data[Index].Create;
      StatObj.CalcTables(Tables);
      Tables.AddStatistic(StatObj);
    end;
end;

function TTables.DoSortTables(Tables: TTwoWayTables; ST: TTablesCommand
  ): boolean;
var
  Tab: TTwoWayTable;

  function SortTable(T: TTwoWayTable): boolean;
  var
    Opt: TOption;
  begin
    Result := false;

    if ST.HasOption('sa') then
      begin
        T.SortByRowValue(false);
        T.SortByColValue(false);
      end;

    if ST.HasOption('sd') then
      begin
        T.SortByRowValue(true);
        T.SortByColValue(true);
      end;

    if ST.HasOption('sla') then
      begin
        T.SortByRowLabel(false);
        T.SortByColLabel(false);
      end;

    if ST.HasOption('sld') then
      begin
        T.SortByRowLabel(true);
        T.SortByColLabel(true);
      end;

    try
      if ST.HasOption('sra', Opt) then T.SortByRow(Opt.Expr.AsInteger - 1, false);
      if ST.HasOption('srd', Opt) then T.SortByRow(Opt.Expr.AsInteger - 1, true);
      if ST.HasOption('sca', Opt) then T.SortByCol(Opt.Expr.AsInteger - 1, false);
      if ST.HasOption('scd', Opt) then T.SortByCol(Opt.Expr.AsInteger - 1, true);
    except
      On E: ETwoWaySortException do
        begin
          FExecutor.Error(E.Message);
          ST.ExecResult := csrFailed;
          Exit;
        end;

      else
        raise;
    end;

    if ST.HasOption('srta') then T.SortByRowTotal(false);
    if ST.HasOption('srtd') then T.SortByRowTotal(true);
    if ST.HasOption('scta') then T.SortByColTotal(false);
    if ST.HasOption('sctd') then T.SortByColTotal(true);

    Result := True;
  end;

begin
  Result := SortTable(Tables.UnstratifiedTable);

  for Tab in Tables do
    begin
      if (not Result) then
        exit;

      Result := SortTable(Tab);
    end;
end;

procedure TTables.DoResultVariables(Tables: TTwoWayTables);
var
  i, Cols, Rows: Integer;
  RTableNames, RStratNames: TExecVarVector;
  S: UTF8String;
  Tab: TTwoWayTable;
  F: TEpiField;

  procedure AddResultTable(Table: TTwoWayTable; Const Name: UTF8String);
  var
    RTable: TExecVarMatrix;
    Col, Row: Integer;
    RStratValues: TExecVarVector;
  begin
    FExecutor.AddResultConst(Name + '_df', ftInteger).AsIntegerVector[0] := Table.DF;
    RTable := FExecutor.AddResultMatrix(Name, ftInteger, Table.ColCount + 1, Table.RowCount + 1);

    if Length(Table.StratifyIndices) > 0 then
      begin
        RStratValues := FExecutor.AddResultVector(Name + '_stratvalues', ftString, Length(Table.StratifyIndices));

        for Col := Low(TAble.StratifyIndices) to High(Table.StratifyIndices) do
          RStratValues.AsStringVector[Col] := Tables.StratifyVariables[Col].AsString[Table.StratifyIndices[Col]];
      end;

    // Fill data
    for Col := 0 to Table.ColCount - 1 do
      for Row := 0 to Table.RowCount - 1 do
        RTable.AsIntegerMatrix[Col, Row] := Table.Cell[Col, Row].N;

    // Fill col totals
    for Col := 0 to Table.ColCount - 1 do
      RTable.AsIntegerMatrix[Col, RTable.Rows - 1] := Table.ColTotal[Col];

    // Fill row totals
    for Row := 0 to Table.RowCount - 1 do
      RTable.AsIntegerMatrix[RTable.Cols - 1, Row] := Table.RowTotal[Row];

    RTable.AsIntegerMatrix[RTable.Cols - 1, RTable.Rows - 1] := Table.Total;
  end;

begin
  FExecutor.ClearResults('$tables_');
  FExecutor.AddResultConst('$tables_cols', ftInteger).AsIntegerVector[0] := Tables.UnstratifiedTable.ColCount + 1;
  FExecutor.AddResultConst('$tables_rows', ftInteger).AsIntegerVector[0] := Tables.UnstratifiedTable.RowCount + 1;

  i := 0;
  if (Assigned(Tables.StratifyVariables)) and
     (Tables.StratifyVariables.Count > 0)
  then
    begin
      RStratNames := FExecutor.AddResultVector('$tables_stratifynames', ftString, Tables.StratifyVariables.Count);
      for F in Tables.StratifyVariables do
        RStratNames.AsStringVector[PostInc(i)] := F.Name;
    end;

  RTableNames := FExecutor.AddResultVector('$tables_tablenames', ftString,  Tables.Count + 1);

  S := '$tables_unstratified';
  AddResultTable(Tables.UnstratifiedTable, S);
  RTableNames.AsStringVector[0] := S;

  i := 1;
  for Tab in Tables do
    begin
      S := '$tables_table' + IntToStr(i);
      AddResultTable(Tab, S);
      RTableNames.AsStringVector[i] := S;
      Inc(i);
    end;

  // Need to treat summary statistics differently
  for i := 0 to Tables.StatisticsCount - 1 do
    Tables.Statistics[i].CreateResultVariables(Tables, FExecutor);

end;

constructor TTables.Create(AExecutor: TExecutor; AOutputCreator: TOutputCreator
  );
begin
  FExecutor := AExecutor;
  FOutputCreator := AOutputCreator;
end;

destructor TTables.Destroy;
begin
  inherited Destroy;
end;

procedure TTables.ExecTables(DF: TEpiDataFile; ST: TTablesCommand);
var
  VarNames: TStrings;
  Opt: TOption;
  StratifyVarnames: TStringList;
  AllTables: TTwoWayTables;
  WeightName: String;
begin
  Varnames := ST.VariableList.GetIdentsAsList;

  StratifyVarnames := TStringList.Create;
  for Opt in ST.Options do
    begin
      if (Opt.Ident <> 'by') then
        Continue;

      StratifyVarnames.Add(Opt.Expr.AsIdent);
    end;

  WeightName := '';
  if (ST.HasOption('w', Opt)) then
    WeightName := Opt.Expr.AsIdent;

  AllTables := DoCalcTables(DF, VarNames, StratifyVarnames, WeightName);

  if (not DoSortTables(AllTables, ST)) then
    Exit;

  DoTableStatistics(AllTables, GetStatisticOptions(ST));

  DoResultVariables(AllTables);

  if (not ST.HasOption('q')) then
    DoOutputTables(AllTables, ST);
end;

function TTables.CalcTables(InputDF: TEpiDataFile; VariableNames: TStrings;
  StratifyNames: TStrings; const WeightName: UTF8String; out
  RefMap: TEpiReferenceMap; Statistics: TTableStatistics): TTwoWayTables;
begin
  Result := DoCalcTables(InputDF, VariableNames, StratifyNames, WeightName);
  DoTableStatistics(Result, Statistics);
end;

finalization
  StatisticsMap.Free;

end.

