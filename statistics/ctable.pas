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
      procedure CreateResultVariables(Tables: TTwoWayTables; VarNames: TStrings);
      procedure DoResultVariables(Tables: TTwoWayTables; Index: Integer);
    private
      FStratifyVarNames: TStringList;
      FWeightName: UTF8String;
      FCTableStatistics: TTableStatistics;
      FOutputCol: array [0..5] of integer;  // TODO: make this variable length
      FFooterText: UTF8String;
      FResultCounts: TExecVarVector;
      FResultRows: Integer;
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

// create result variables for summary tables only

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
  SF := '';

  // Collect header information
  SH := Tables.UnstratifiedTable.ColVariable.GetVariableLabel(VariableLabelType);
  if (ST.HasOption('ar') or ST.HasOption('rr')) then
    begin
      SF := 'O+ = ' +
        Tables.UnstratifiedTable.ColVariable.GetValueLabelFormatted(0,ValueLabelType) +
        ' / O- = ' +
        Tables.UnstratifiedTable.ColVariable.GetValueLabelFormatted(1,ValueLabelType) +
        LineEnding;
      if (FStratifyVarNames.Count>0) then
        SF := SF + '(attack rates are for unstratified data)' + LineEnding;
    end;

  if (FStratifyVarNames.Count>0) then
    begin
      SH := SH + LineEnding + ' adjusted for:';

      if (VariableLabelType = gvtVarName) then
        S1 := ' '
      else
        S1 := LineEnding;
      if (ST.HasOption('ex')) then
        SF := SF + LineEnding + '(Fisher Exact test p is for unstratified data)';
      for F in Tables.StratifyVariables do
        SH := SH + S1 + F.GetVariableLabel(VariableLabelType);
    end;
  Result.Header.Text := SH;
  Result.Footer.Text := SF;

  // Summary table headers
  Result.Cell[0, 0].Text := 'by';
  Result.Cell[0, 1].Text := 'Var';
  Result.Cell[1, 1].Text := 'N';
  if (Tables.StatisticsCount > 0) then
  for i := 0 to Tables.StatisticsCount - 1 do
    begin
      FOutputCol[i] := Result.ColCount;
      Tables.Statistics[i].AddToCompactHeader(Result,ST.Options);
   end;

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

procedure TCTable.CreateResultVariables(Tables: TTwoWayTables; VarNames: TStrings);
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

  SetLength(TableResults,Tables.StatisticsCount);
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
begin
  result := [];
  if (ST.HasOption('ex')) then
      Include(Result, tsFExP);
  if (ST.HasOption('t')) then
      Include(Result, tsChi2);
  if (ST.HasOption('odds')) then
      Include(Result, tsOR);
  if (ST.HasOption('rr') or ST.HasOption('ar')) then
      Include(Result, tsRR);

end;

procedure TCTable.ExecCTable(VarNames: TStrings; ST: TCTableCommand);
var
  SummaryTable: TOutputTable;
  DF: TEpiDataFile;
  TwoVarNames: TStrings;
  Opt: TOption;
  i, VarCount: Integer;
  TableData: TTables;
  Table: TTwoWayTables;
  TablesRefMap: TEpiReferenceMap;
  FirstPass: Boolean; // marks first table (no table if no data for a variable)

  // invoke CalcTables for one pair of variables
  procedure DoOneCTable;
  var
    AllVarNames: TStrings;
  begin;
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

        if (FirstPass) then
          CreateResultVariables(Table, VarNames); // set up result variables
        DoResultVariables(Table, i); // results for one table row

        if (not ST.HasOption('q')) then
        begin
          if (FirstPass) then // cannot create header until we have done calctable once
            SummaryTable := CreateOutputHeader(Table, ST);
          DoOutputCTableRow(Table, ST, SummaryTable);
        end;
        FirstPass := FALSE;
      end;
    finally
      DF.Free;
      AllVarNames.Free;
    end;

  end;

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
  FirstPass := TRUE;
  // get each set of results based on first variable and each of the other variables
  VarCount := VarNames.Count;
  if (VarCount < 2) then
  begin
    // error message (should not happen)
    exit;
  end;

  FFooterText := '';
  FCTableStatistics := GetStatisticOptions(ST);
  TableData  := TTables.Create(FExecutor, FOutputCreator);
  setlength(TableResults,VarCount - 1);

  for i := 1 to VarCount - 1 do
  begin
    TwoVarNames.Add(VarNames[i]);
    DoOneCTable;
    TwoVarNames.Delete(1);
  end;
  SummaryTable.SetColAlignment(0, taLeftJustify); // variable name column
  SummaryTable.SetRowBorders(SummaryTable.RowCount - 1, [cbBottom]);
  TableData.Free;
end;


end.

