unit ctable;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, epidatafiles, ast, executor, result_variables,
  outputcreator, epicustombase, tables_types, tables;

type

  TTableResults = array of TStatResult;

  {CTable}

  TCTable = class
    private
      FExecutor: TExecutor;
      FOutputCreator: TOutputCreator;
      function GetStatisticOptions(ST: TCTableCommand): TTableStatistics;

      function CreateOutputHeader(Tables: TTwoWayTables; ST: TCTableCommand): TOutputTable;
      procedure DoOutputCTableRow(Tables: TTwoWayTables; ST: TCTableCommand; T: TOutputTable);
      procedure PrepareResultVariables(Tables: TTwoWayTables; VarNames: TStrings);
      procedure DoResultVariables(Tables: TTwoWayTables; Index: Integer);
    private
      FStratifyVarNames: TStringList;
      FWeightName: UTF8String;
      FCTableStatistics: TTableStatistics;
      FOutputCol: array of integer;
      FFooterText: UTF8String;
      FResultCounts: TExecVarVector;
      FResultRows: Integer;
      FAllTables: array of TTwoWayTables;
      FSortBy: TTableStatistic;
      FSortIndex: Integer;
      FSortDescending: Boolean;
    public
      TableResults: TTableResults;
      property ResultRows: Integer read FResultRows;
      constructor Create(AExecutor: TExecutor; AOutputCreator: TOutputCreator);
      destructor Destroy; override;
      // Method called from Executor, does calculation + result vars + output
      procedure ExecCTable(VarNames: TStrings; ST: TCTableCommand);

  end;

implementation

uses
  epimiscutils, epifields_helper,
  options_utils, LazUTF8, epidatafilestypes;

{CTable}

function TCTable.CreateOutputHeader(Tables: TTwoWayTables; ST: TCTableCommand) : TOutputTable;
var
  VariableLabelType: TEpiGetVariableLabelType;
  ValueLabelType: TEpiGetValueLabelType;
  SH, S1, SF: UTF8String;
  F: TEpiField;
  i: Integer;

begin
  // Formatting of variables and values
  VariableLabelType := VariableLabelTypeFromOptionList(ST.Options, FExecutor.SetOptions);
  ValueLabelType    := ValueLabelTypeFromOptionList(ST.Options, FExecutor.SetOptions);

  // Create basic summary table
  Result := FOutputCreator.AddTable;
  Result.ColCount := 2;
  Result.RowCount := 2;
  Result.Footer.Alignment := taLeftJustify;

  // Collect header information
  SH := Tables.UnstratifiedTable.ColVariable.GetVariableLabel(VariableLabelType);
  if (FStratifyVarNames.Count>0) then
    begin
      SH := SH + LineEnding + ' adjusted for:';

      if (VariableLabelType = gvtVarName) then
        S1 := ' '
      else
        S1 := LineEnding;
      for F in Tables.StratifyVariables do
        SH := SH + S1 + F.GetVariableLabel(VariableLabelType);
    end;
  Result.Header.Text := SH;

  // Summary table headers
  Result.Cell[0, 0].Text := 'by';
  Result.Cell[0, 1].Text := 'Var';
  Result.Cell[1, 1].Text := 'N';
  if (Tables.StatisticsCount > 0) then
    begin
      SetLength(FOutputCol,Tables.StatisticsCount);
      for i := 0 to Tables.StatisticsCount - 1 do
        begin
          FOutputCol[i] := Result.ColCount;
          Tables.Statistics[i].AddToCompactHeader(Result,ST.Options);
       end;
   end;

  SF := '';
  if (ST.HasOption('ar') or ST.HasOption('rr') or ST.HasOption('en')) then
    begin
      if Tables.UnstratifiedTable.ColCount > 1 then
      result.Header.Text := result.Header.Text + LineEnding + 'O+ = ' +
        Tables.UnstratifiedTable.ColVariable.GetValueLabelFormatted(0,ValueLabelType) +
        ' / O- = ' +
        Tables.UnstratifiedTable.ColVariable.GetValueLabelFormatted(1,ValueLabelType) +   // TODO: dies here is no 2x2 tables at all
        LineEnding;
      if (FStratifyVarNames.Count>0) then
        SF := SF + LineEnding + '(attack rates are for unstratified data)';
    end;
      if (ST.HasOption('ex')) then
        SF := SF + LineEnding + '(Fisher Exact test p is for unstratified data)';

  result.Footer.Text := result.Footer.Text + SF;
end;

procedure TCTable.DoOutputCTableRow(Tables: TTwoWayTables; ST: TCTableCommand; T: TOutputTable);
var

  VariableLabelType: TEpiGetVariableLabelType;
  Tab: TTwoWayTable;
  RowIdx, ColIdx, i: Integer;
begin
  VariableLabelType := VariableLabelTypeFromOptionList(ST.Options, FExecutor.SetOptions);
  RowIdx := T.RowCount;
  Tab    := Tables.UnstratifiedTable;
  T.RowCount := RowIdx + 1;
  T.Cell[0, RowIdx].Text := Tab.RowVariable.GetVariableLabel(VariableLabelType);
  T.Cell[1, RowIdx].Text := IntToStr(Tab.Total);

  for i := 0 to Tables.StatisticsCount -1  do
    begin
      ColIdx := FOutputCol[i];
      Tables.Statistics[i].AddToCompactTable(FExecutor, T, RowIdx, ColIdx, ST.Options);
    end;
  // Setting up borders at the top
  T.SetRowBorders(0, [cbTop]);
  T.SetRowBorders(1, [cbBottom]);
end;

procedure TCTable.PrepareResultVariables(Tables: TTwoWayTables; VarNames: TStrings);
var
  i: Integer;
  RTableNames, RStratNames: TExecVarVector;
  F: UTF8String;

begin
  FExecutor.ClearResults('$ctable');
  FResultRows := VarNames.Count -1;
  FExecutor.AddResultConst('$ctable_basevar',   ftString).AsStringVector[0] := VarNames[0];
  RTableNames := FExecutor.AddResultVector('$ctable_varnames', ftString, FResultRows);
  for i := 1 to VarNames.Count - 1 do
    begin
      RTableNames.AsStringVector[i-1] := VarNames[i];
    end;
  FResultCounts := FExecutor.AddResultVector('$ctable_n', ftInteger, FResultRows);
  i := 0;
  if (Assigned(FStratifyVarNames)) and
     (FStratifyVarNames.Count > 0)
  then
    begin
      RStratNames := FExecutor.AddResultVector('$ctable_stratifynames', ftString, FStratifyVarNames.Count);
      for F in FStratifyVarNames do
        RStratNames.AsStringVector[PostInc(i)] := F;
    end;
  if (FWeightName <> '') then
    FExecutor.AddResultConst('$ctable_weightvar',   ftString).AsStringVector[0] := FWeightName;

  SetLength(TableResults, Tables.StatisticsCount);
  for i := 0 to Tables.StatisticsCount - 1 do
  begin
    TableResults[i] := Tables.Statistics[i].CreateCompactResultVariables(FExecutor, '$ctable_', FResultRows); // add results for Tables to TableResults
  end;

end;

procedure TCTable.DoResultVariables(Tables: TTwoWayTables; Index: Integer);
var
  i: Integer;
begin
  FResultCounts.AsIntegerVector[Index-1] := Tables.UnstratifiedTable.Total;

  for i := 0 to Tables.StatisticsCount - 1 do
  begin
    Tables.Statistics[i].AddCompactResultVariables(FExecutor, Index-1, TableResults[i]); // add results for Tables to TableResults

  end;
end;
constructor TCTable.Create(AExecutor: TExecutor; AOutputCreator: TOutputCreator
  );
begin
  FExecutor := AExecutor;
  FOutputCreator := AOutputCreator;
end;

destructor TCTable.Destroy;
begin
  inherited Destroy;
end;

function TCTable.GetStatisticOptions(ST: TCTableCommand): TTableStatistics;
// find statistics options and set the sort statistic in case !so is specified

begin
  result := [];
  FSortDescending := false;
  if (ST.HasOption('t')) then
    begin
      Include(Result, tsChi2);
      FSortBy := tsChi2;
    end;
  if (ST.HasOption('ex')) then
    begin
      Include(Result, tsFExP);
      FSortBy := tsFExP;
    end;
  if (ST.HasOption('odds')) then
    begin
      Include(Result, tsOR);
      FSortBy := tsOR;
      FSortDescending := true;
    end;
  if (ST.HasOption('rr') or ST.HasOption('ar') or ST.HasOption('en')) then
    begin
      Include(Result, tsRR);
      FSortBy := tsRR;
      FSortDescending := true;
    end;
end;

procedure TCTable.ExecCTable(VarNames: TStrings; ST: TCTableCommand);
var
  SummaryTable: TOutputTable;
  debugst1, debugst2: String;
  DF: TEpiDataFile;
  TwoVarNames: TStrings;
  CrossVars: TEpiFields;
  CrossVarNames: TStringList;
  HoldVarLabels, SortVarLabels: TStringList;
  CrossVarLabel: String;
  VariableLabelType: TEpiGetVariableLabelType;
  Opt: TOption;
  i, j, ix, VarCount, TableCount: Integer;
  SortValue: EpiFloat;
  SortValues: array of EpiFloat;
  SortIndex: array of Integer;
  TableData: TTables;
  Table: TTwoWayTables;
  TablesRefMap: TEpiReferenceMap;
  FirstPass: Boolean;
  HeaderDone: Boolean;

  // comparison for sorting table by statistic

  function Compare(a,b: EpiFloat): Boolean;
  begin
    if (FSortDescending) then result := (b > a)
    else result := (b < a);
  end;

  // invoke CalcTables for one pair of variables
  function DoOneCTable: Boolean;
  var
    AllVarNames: TStrings;
  begin;
    result := false;
    AllVarNames := TStringList.Create;
    // local AllVarNames has only the two variables being analyzed in this pass
    // must add the stratifying and weight variables
    AllVarNames.AddStrings(TwoVarNames);
    // add in stratify and weight variables before preparing the datafile
    if ST.HasOption('by') then
      AllVarNames.AddStrings(FStratifyVarNames);
    if St.HasOption('w') then
      AllVarNames.Add(FWeightName);
    if ST.HasOption('m') then
      DF := FExecutor.PrepareDatafile(AllVarNames, nil)
    else
      DF := FExecutor.PrepareDatafile(AllVarNames, AllVarNames);
    try
      if (DF.Size = 0) then
        FOutputCreator.DoWarning(AllVarNames[1] + ': No data')
      else
      begin
        Table := TableData.CalcTables(DF, TwoVarNames, FStratifyVarNames, FWeightName,
             ST.Options, TablesRefMap, FCTableStatistics);
        result := true;

        if (FirstPass) then
          PrepareResultVariables(Table, VarNames); // set up result variables
        DoResultVariables(Table, i); // results for one table row

        FirstPass := FALSE;
      end;
    finally
      DF.Free;
      AllVarNames.Free;
    end;

  end;  {DoOneCTable}

begin
  FStratifyVarNames := TStringList.Create;
  for Opt in ST.Options do
    begin
      if (Opt.Ident = 'by') then
        FStratifyVarNames.Add(Opt.Expr.AsIdent);
    end;

  FWeightName := '';
  if (ST.HasOption('w', Opt)) then
    FWeightName := Opt.Expr.AsIdent;
  TwoVarNames := TStringList.Create;
  TwoVarNames.Add(VarNames[0]);

{ This is where to sort the remaining table variables (1 ... n)
       !sn --> sort output by variable name
       !sl --> sort by variable label
}
  CrossVarNames := TStringList.Create;
  if (ST.HasOption('sl')) then
    begin
      CrossVars := TEpiFields.Create(nil);
      for i:= 1 to VarNames.Count - 1 do
        CrossVars.AddItem(FExecutor.SortedFields.FieldByName[VarNames[i]]);
      VariableLabelType := gvtVarLabel;
      SortVarLabels := TStringList.Create;
      HoldVarLabels := TStringList.Create;
      HoldVarLabels.Sorted := False;
      for i:= 1 to VarNames.Count - 1 do
        begin
          CrossVarLabel := CrossVars[i-1].GetVariableLabel(VariableLabelType) + VarNames[i];
          SortVarLabels.Add(CrossVarLabel);
          HoldVarLabels.Add(CrossVarLabel);
        end;
      SortVarLabels.Sort;
      for i:= 0 to HoldVarLabels.Count - 1 do
        begin
          ix := HoldVarLabels.IndexOf(SortVarLabels[i]);
          debugst1 := SortVarLabels[i];
          debugst2 := VarNames[ix + 1];
          CrossVarNames.Add(VarNames[ix + 1]);
       end;
      CrossVars.Free;
      HoldVarLabels.Free;
      SortVarLabels.Free;
    end
  else
    begin
      for i := 1 to VarNames.Count -1  do
        CrossVarNames.Add(VarNames[i]);
      if (ST.HasOption('sn')) then
        CrossVarNames.Sort;
    end;

  FirstPass := TRUE;
  // get each set of results based on first variable and each of the other variables
  VarCount := CrossVarNames.Count;
  if (VarCount < 1) then
  begin
    // error message (should not happen)
    exit;
  end;

  FFooterText := '';
  FCTableStatistics := GetStatisticOptions(ST);
  TableData  := TTables.Create(FExecutor, FOutputCreator);
  setlength(FAllTables,VarCount);
  setlength(SortIndex,VarCount);
  setlength(SortValues,VarCount);

  TableCount := -1;
  FSortIndex := 0;

  for i := 1 to VarCount do
  begin
    TwoVarNames.Add(CrossVarNames[i-1]);
    if (DoOneCTable) then
    begin
      TableCount += 1;
      FAllTables[TableCount] := Table;
      if ((Table.StatisticsCount > 0) and (ST.HasOption('ss'))) then
      begin
          SortValue := Table.Statistics[FSortIndex].CompactSortValue;
          SortValues[TableCount] := SortValue;
          SortIndex[TableCount] := TableCount;
          j := TableCount;
          while ((j > 0) and (Compare(SortValues[SortIndex[j-1]], SortValue))) do
          begin
            SortIndex[j]  := SortIndex[j-1];
            SortIndex[j-1]  := TableCount;
            j -= 1;
          end;
      end
      else
        SortIndex[TableCount] := TableCount;
    end;
    TwoVarNames.Delete(1);
  end;

  if (not ST.HasOption('q')) then
  begin
    HeaderDone := false;
    if (ST.HasOption('rr') or ST.HasOption('or') or ST.HasOption('ex') or ST.HasOption('en') or ST.HasOption('ar')) then
    // first find a 2x2 table to create the header and footer if RR, OR or FExP requested
      for i:= 0 to TableCount do
        if (FAllTables[i].UnstratifiedTable.ColCount = 2) then
        begin
          SummaryTable := CreateOutputHeader(FAllTables[i], ST);
          HeaderDone := true;
          break;
        end;
    if (not HeaderDone) then
      SummaryTable := CreateOutputHeader(FAllTables[0], ST);
    // now output the table rows
    for i:= 0 to TableCount do
      DoOutputCTableRow(FAllTables[SortIndex[i]], ST, SummaryTable);
    SummaryTable.SetColAlignment(0, taLeftJustify); // variable name column
    SummaryTable.SetRowBorders(SummaryTable.RowCount - 1, [cbBottom]);
  end;


  TableData.Free;
  FStratifyVarNames.Free;
  TwoVarNames.Free;
  CrossVarNames.Free;
end;


end.
