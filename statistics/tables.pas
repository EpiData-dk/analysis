unit tables;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, math, epidatafiles, ast, executor, outputcreator,
  epicustombase, tables_types;

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
    function  DoCalcTables(InputDF: TEpiDataFile; Varnames, StratifyNames: TStrings; Const WeightVariable: UTF8String = ''): TTwoWayTables;
    procedure OutputStratifyTable(Table: TTwoWayTable; StratifyVariables: TEpiFields; ST: TTablesCommand; IsGrandTable: boolean);
    procedure OutputSummaryTable(Tables: TTwoWayTables; ST: TTablesCommand);
    function  PackStratifiedDataset(Sender: TEpiDataFile; Index: Integer; Data: Pointer): boolean;
    function  GetStatisticOptions(ST: TTablesCommand): TTableStatistics;
  protected
    procedure DoOutputTables(Tables: TTwoWayTables; ST: TTablesCommand);
    procedure DoTableStatistics(Tables: TTwoWayTables; Statistics: TTableStatistics);
    function  DoSortTables(Tables: TTwoWayTables; ST: TTablesCommand): boolean;
  public
    constructor Create(AExecutor: TExecutor; AOutputCreator: TOutputCreator);
    destructor Destroy; override;
    // Method called from Executor, does calculation + result vars + output
    procedure ExecTables(DF: TEpiDataFile; ST: TTablesCommand);
    // Method to be used from elsewhere. Does only calculations and returns the result as a specialized dataset
    function  CalcTables(DF: TEpiDataFile; Out RefMap: TEpiReferenceMap; Statistics: TTableStatistics): TTwoWayTables;
  end;

procedure RegisterTableStatistic(Statistic: TTableStatistic; StatClass: TTwoWayStatisticsClass);

implementation

uses
  aggregate, aggregate_types, epimiscutils, epidatafileutils, epifields_helper,
  options_utils, LazUTF8, ana_globals, epidatafilestypes, options_table, strutils,
  tables_stat_chi2;

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

function TTables.DoCalcTables(InputDF: TEpiDataFile; Varnames,
  StratifyNames: TStrings; const WeightVariable: UTF8String): TTwoWayTables;
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

procedure TTables.OutputStratifyTable(Table: TTwoWayTable;
  StratifyVariables: TEpiFields; ST: TTablesCommand; IsGrandTable: boolean);
var
  T: TOutputTable;
  VariableLabelType: TEpiGetVariableLabelType;
  ValueLabelType: TEpiGetValueLabelType;
  Col, Row, i, ColumnFactor, Idx: Integer;
  S: String;
  Opt: TOption;
  ShowRowPercent, ShowColPercent, ShowTotPercent: Boolean;
  Stat: TTwoWayStatistic;

  function FormatPercent(Value: EpiFloat; SetOption: TTablePercentFormatOption): UTF8String;
  begin
    Result := OutputCreatorNormalizeText(SetOption.LeftChar) +
              Format('%.1f', [Value * 100]) +
              OutputCreatorNormalizeText(SetOption.RigthChar);
  end;

begin
  VariableLabelType := VariableLabelTypeFromOptionList(ST.Options, FExecutor.SetOptions);
  ValueLabelType    := ValueLabelTypeFromOptionList(ST.Options, FExecutor.SetOptions);

  // Table setup
  T := FOutputCreator.AddTable;

  // Header
  if IsGrandTable then
    T.Header.Text := Table.ColVariable.GetVariableLabel(VariableLabelType)
  else
    begin
      S := '';
      for i := Low(Table.StratifyIndices) to High(Table.StratifyIndices) do
        S := S + StratifyVariables[i].GetVariableLabel(VariableLabelType) + ': ' + StratifyVariables[i].GetValueLabel(Table.StratifyIndices[i], ValueLabelType) + LineEnding;

      T.Header.Text := S + Table.ColVariable.GetVariableLabel(VariableLabelType);
    end;

  // Footer
  if (ST.HasOption('w', Opt)) then
    begin
      T.Footer.Text := 'Weight: ' + Opt.Expr.AsIdent;
      T.Footer.Alignment := taLeftJustify;
    end;

  // Add percentages?
  ColumnFactor := 1;
  ShowRowPercent := ST.HasOption('pr');
  ShowColPercent := ST.HasOption('pc');
  ShowTotPercent := ST.HasOption('pt');
  if ShowRowPercent then Inc(ColumnFactor);
  if ShowColPercent then Inc(ColumnFactor);
  if ShowTotPercent then Inc(ColumnFactor);

  //               Row header   + Table colums (and percents)     + Row totals (and percentes)
  T.ColCount    := 1 +            (Table.ColCount * ColumnFactor) + (1 * ColumnFactor);
  //               Col header   + Table rows                      + Col totals
  T.RowCount    := 1            + Table.RowCount                  + 1;

  T.Cell[0, 0].Text := Table.RowVariable.GetVariableLabel(VariableLabelType);

  // Col headers
  for Col := 0 to Table.ColCount - 1 do
    begin
      Idx := (Col * ColumnFactor) + 1;

      T.Cell[PostInc(Idx), 0].Text                        := Table.ColVariable.GetValueLabel(Col, ValueLabelType);
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

        T.Cell[PostInc(Idx), Row + 1].Text                        := IntToStr(Table.Cell[Col, Row].N);
        if ShowRowPercent then T.Cell[PostInc(Idx), Row + 1].Text := FormatPercent(Table.Cell[Col, Row].RowPct,   TTablePercentFormatOption(FExecutor.SetOptions[ANA_SO_TABLE_PERCENT_FORMAT_ROW]));
        if ShowColPercent then T.Cell[PostInc(Idx), Row + 1].Text := FormatPercent(Table.Cell[Col, Row].ColPct,   TTablePercentFormatOption(FExecutor.SetOptions[ANA_SO_TABLE_PERCENT_FORMAT_COL]));
        if ShowTotPercent then T.Cell[PostInc(Idx), Row + 1].Text := FormatPercent(Table.Cell[Col, Row].TotalPct, TTablePercentFormatOption(FExecutor.SetOptions[ANA_SO_TABLE_PERCENT_FORMAT_TOTAL]));
      end;

  // Col Totals
  for Col := 0 to Table.ColCount - 1 do
    begin
      Idx := (Col * ColumnFactor) + 1;
      Row := T.RowCount - 1;

      T.Cell[PostInc(Idx), T.RowCount - 1].Text             := IntToStr(Table.ColTotal[Col]);
      if ShowRowPercent then T.Cell[PostInc(Idx), Row].Text := FormatPercent(Table.ColTotal[Col] / Table.Total, TTablePercentFormatOption(FExecutor.SetOptions[ANA_SO_TABLE_PERCENT_FORMAT_ROW]));
      if ShowColPercent then T.Cell[PostInc(Idx), Row].Text := FormatPercent(1, TTablePercentFormatOption(FExecutor.SetOptions[ANA_SO_TABLE_PERCENT_FORMAT_COL]));
      if ShowTotPercent then T.Cell[PostInc(Idx), Row].Text := FormatPercent(Table.ColTotal[Col] / Table.Total, TTablePercentFormatOption(FExecutor.SetOptions[ANA_SO_TABLE_PERCENT_FORMAT_TOTAL]));
    end;

  // Row Totals
  for Row := 0 to Table.RowCount - 1 do
    begin
      Idx := T.ColCount - (1 * ColumnFactor);

      T.Cell[PostInc(Idx), Row + 1].Text := IntToStr(Table.RowTotal[Row]);
      if ShowRowPercent then T.Cell[PostInc(Idx), Row + 1].Text := FormatPercent(1, TTablePercentFormatOption(FExecutor.SetOptions[ANA_SO_TABLE_PERCENT_FORMAT_ROW]));
      if ShowColPercent then T.Cell[PostInc(Idx), Row + 1].Text := FormatPercent(Table.RowTotal[Row] / Table.Total, TTablePercentFormatOption(FExecutor.SetOptions[ANA_SO_TABLE_PERCENT_FORMAT_COL]));
      if ShowTotPercent then T.Cell[PostInc(Idx), Row + 1].Text := FormatPercent(Table.RowTotal[Row] / Table.Total, TTablePercentFormatOption(FExecutor.SetOptions[ANA_SO_TABLE_PERCENT_FORMAT_TOTAL]));
    end;

  // Grand total
  T.Cell[T.ColCount - (1 * ColumnFactor), T.RowCount - 1].Text := IntToStr(Table.Total);

  T.SetRowBorders(T.RowCount - 1, [cbBottom]);

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
  S: UTF8String;
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
  T.RowCount := (Tables.Count * 2) + 3;
  T.SetColAlignment(0, taLeftJustify);

  // Collect header information
  S := Tables.UnstratifiedTable.ColVariable.GetVariableLabel(VariableLabelType) + ' by ' +
       Tables.UnstratifiedTable.RowVariable.GetVariableLabel(VariableLabelType);
  if (Assigned(Tables.StratifyVariables)) then
    begin
      S := S + ' adjusted for:';
      for F in Tables.StratifyVariables do
        S := S + ' ' + F.GetVariableLabel(VariableLabelType);
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
      S := UTF8Trim(S, [u8tKeepStart]);

      T.Cell[0, RowIdx + 1].Text := S;
      T.Cell[1, RowIdx + 1].Text := IntToStr(Tab.Total);
      Inc(RowIdx, 2);
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

  if (ST.HasOption('t')) then Include(Result, tsChi2);
end;

procedure TTables.DoOutputTables(Tables: TTwoWayTables; ST: TTablesCommand);
var
  Tab: TTwoWayTable;
begin
  if (not ST.HasOption('nc')) then
    OutputStratifyTable(Tables.UnstratifiedTable, Tables.StratifyVariables, ST, true);

  // Sub table output
  if (not ST.HasOption('nb')) then
    for Tab in Tables do
      OutputStratifyTable(Tab, Tables.StratifyVariables, ST, False);

  if (not ST.HasOption('ns')) then
    OutputSummaryTable(Tables, ST);
end;

procedure TTables.DoTableStatistics(Tables: TTwoWayTables; Statistics: TTableStatistics);
var
  Tab: TTwoWayTable;
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
  T: TTwoWayTable;
  i: Integer;
  Opt: TOption;
begin
  result := false;

  for i := 0 to Tables.Count -1 do
    begin
      T := Tables[i];

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

      if ST.HasOption('srta')     then T.SortByRowTotal(false);
      if ST.HasOption('srtd')     then T.SortByRowTotal(true);
      if ST.HasOption('scta')     then T.SortByColTotal(false);
      if ST.HasOption('sctd')     then T.SortByColTotal(true);
    end;

  Result := true;
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

  DoTableStatistics(AllTables, GetStatisticOptions(ST));

  if (not DoSortTables(AllTables, ST)) then
    Exit;

  if (not ST.HasOption('q')) then
    DoOutputTables(AllTables, ST);
end;

function TTables.CalcTables(DF: TEpiDataFile; out RefMap: TEpiReferenceMap;
  Statistics: TTableStatistics): TTwoWayTables;
begin

end;

finalization
  StatisticsMap.Free;

end.

