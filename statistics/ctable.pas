unit ctable;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, math, epidatafiles, ast, executor, result_variables,
  outputcreator, epicustombase, tables_types, tables;

type

{CTable}

  TCTable = class
    private
      FExecutor: TExecutor;
      FOutputCreator: TOutputCreator;
      function GetStatisticOptions(ST: TCTableCommand): TTableStatistics;

      function CreateOutputHeader(Tables: TTwoWayTables; ST: TCTableCommand): TOutputTable;
      procedure DoOutputCTableRow(Tables: TTwoWayTables; ST: TCTableCommand; T: TOutputTable);
      procedure DoResultVariables(Tables: TTwoWayTables);
    private
        StratifyVarnames: TStringList;
        OutputCol: array [0..5] of integer;
    public
      constructor Create(AExecutor: TExecutor; AOutputCreator: TOutputCreator);
      destructor Destroy; override;
      // Method called from Executor, does calculation + result vars + output
      procedure ExecCTable(DF: TEpiDataFile; ST: TCTableCommand);

  end;

implementation

uses
  epimiscutils, epidatafileutils, epifields_helper,
  options_utils, LazUTF8, epidatafilestypes, options_table, strutils;

{CTable}

// parse out variables, stratify variables and weight variable
//
// fix first variable
// for each of the other variables
//     at the simplest, pass the two variables plus the rest of the command line to CalcTables
//     for each stat
//         format results into output tables (crude or stratified) based on option !nc (no crude)
// create result variables for summary tables only

function TCTable.CreateOutputHeader(Tables: TTwoWayTables; ST: TCTableCommand) : TOutputTable;
var
  VariableLabelType: TEpiGetVariableLabelType;
  ValueLabelType: TEpiGetValueLabelType;
  S, S1: UTF8String;
  F: TEpiField;
  RowIdx, ColIdx, i: Integer;

begin
  // Formatting of variables and values
  VariableLabelType := VariableLabelTypeFromOptionList(ST.Options, FExecutor.SetOptions);
  ValueLabelType    := ValueLabelTypeFromOptionList(ST.Options, FExecutor.SetOptions);

  // Create basic summary table
  Result := FOutputCreator.AddTable;
  Result.ColCount := 2;
  Result.RowCount := 2;
  Result.SetColAlignment(0, taLeftJustify);

  // Collect header information
  S := Tables.UnstratifiedTable.ColVariable.GetVariableLabel(VariableLabelType) + LineEnding +
       'Outcome (O+) = ' +
       Tables.UnstratifiedTable.ColVariable.GetValueLabelFormatted(0,ValueLabelType) + LineEnding +
       'Not outcome (O-) = ' +
       Tables.UnstratifiedTable.ColVariable.GetValueLabelFormatted(1,ValueLabelType);

  if (StratifyVarNames.Count>0) then
    begin
      S := S + ' adjusted for:';

      S1 := ' ';
      if (VariableLabelType <> gvtVarName) then
        S1 := LineEnding;

      for F in Tables.StratifyVariables do
        S := S + S1 + F.GetVariableLabel(VariableLabelType);
    end;
  Result.Header.Text := S;

  // Summary table headers
  Result.Cell[0, 0].Text := 'by';
  Result.Cell[0, 1].Text := 'var';
  Result.Cell[1, 1].Text := 'N';
  ColIdx                 := 2;

  if (ST.HasOption('ar') or ST.HasOption('rr')) then
    begin
      ColIdx                      := Result.ColCount;
      Result.ColCount             := ColIdx + 1;
//      Result.Cell[ColIdx, 0].Text := 'Exposed';
      Result.Cell[ColIdx, 1].Text := 'E+ =';
    end;
  // optional results
  if (ST.HasOption('ar')) then
    begin
      ColIdx                           := Result.ColCount;
      Result.ColCount                  := ColIdx + 6;
      Result.Cell[ColIdx     , 0].Text := 'E+';
      Result.Cell[ColIdx + 3 , 0].Text := 'E-';
      Result.Cell[ColIdx     , 1].Text := 'O+';
      Result.Cell[ColIdx + 1 , 1].Text := 'O-';
      Result.Cell[ColIdx + 2 , 1].Text := 'AR';
      Result.Cell[ColIdx + 3 , 1].Text := 'O+';
      Result.Cell[ColIdx + 4 , 1].Text := 'O-';
      Result.Cell[ColIdx + 5 , 1].Text := 'AR';
     end;
  if (ST.HasOption('rr') or ST.HasOption('ar')) then
    begin
      ColIdx                           := Result.ColCount;
      Result.ColCount                  := ColIdx + 2;
      Result.Cell[ColIdx     , 1].Text := 'RR';
      Result.Cell[ColIdx + 1 , 1].Text := 'x% CI';
    end;
  if (ST.HasOption('odds')) then
    begin
      ColIdx                           := Result.ColCount;
      Result.ColCount                  := ColIdx + 2;
      Result.Cell[ColIdx     , 1].Text := 'OR';
      Result.Cell[ColIdx + 1 , 1].Text := 'x % CI';
    end;
  if (ST.HasOption('t')) then
    begin
      ColIdx                           := Result.ColCount;
      Result.ColCount                  := ColIdx + 2;
      Result.Cell[ColIdx ,     1].Text := 'Chi2';
      Result.Cell[ColIdx + 1 , 1].Text := 'p';
    end;
  if (ST.HasOption('ex')) then
    begin
      ColIdx                           := Result.ColCount;
      Result.ColCount                  := ColIdx + 1;
      Result.Cell[ColIdx ,     1].Text := 'Fisher P';
    end;

end;

procedure TCTable.DoOutputCTableRow(Tables: TTwoWayTables; ST: TCTableCommand; T: TOutputTable);
var

  VariableLabelType: TEpiGetVariableLabelType;
  ValueLabelType: TEpiGetValueLabelType;
  S, S1: UTF8String;
  F: TEpiField;
  Tab: TTwoWayTable;
  Stat: String;
  RowIdx, ColIdx, i, Decimals, a, b, c, d: Integer;
  ExpAR, NexpAR: EpiFloat;
begin
  Decimals          := DecimalFromOption(ST.Options);
  VariableLabelType := VariableLabelTypeFromOptionList(ST.Options, FExecutor.SetOptions);
  ValueLabelType    := ValueLabelTypeFromOptionList(ST.Options, FExecutor.SetOptions);

  RowIdx := T.RowCount;
  Tab    := Tables.UnstratifiedTable;
  T.RowCount := RowIdx + 1;
  T.Cell[0, RowIdx].Text := Tab.RowVariable.GetVariableLabel(VariableLabelType);
  T.Cell[1, RowIdx].Text := IntToStr(Tab.Total);
  if (ST.HasOption('ar') or ST.HasOption('rr')) then
    T.Cell[2, RowIdx].Text := Tab.RowVariable.GetValueLabel(0, ValueLabelType);

  // TODO: add method to add results to output table for each statistic
  //       left-align first column
  //       if !ar, add in the attack rates

  for i := 0 to Tables.StatisticsCount -1  do
    begin
      ColIdx := OutputCol[i];
      Tables.Statistics[i].AddToCompactTable(T, RowIdx, ColIdx, ST.Options);   //*
    end;
  // Setting up borders at the top
  T.SetRowBorders(0, [cbTop]);
  T.SetRowBorders(1, [cbBottom]);
end;

procedure TCTable.DoResultVariables(Tables: TTwoWayTables);
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
  FExecutor.ClearResults('$ctable_');

  i := 0;
  if (Assigned(Tables.StratifyVariables)) and
     (Tables.StratifyVariables.Count > 0)
  then
    begin
      RStratNames := FExecutor.AddResultVector('$ctable_stratifynames', ftString, Tables.StratifyVariables.Count);
      for F in Tables.StratifyVariables do
        RStratNames.AsStringVector[PostInc(i)] := F.Name;
    end;

  RTableNames := FExecutor.AddResultVector('$ctables_tablenames', ftString,  Tables.Count + 1);

  S := '$ctable_unstratified';
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
var
  StatCount, Col1: Integer;
begin
  result := [];
  StatCount := 0;
  Col1      := 3;
// order determines order of output to compact table
  if (ST.HasOption('ar')) then
    begin
//      OutputCol[StatCount] := Col1;
//      Inc(StatCount);
      Col1 += 6;
      // consider making TTwoWayStatisticsAR, in which case need to use OutputCol for it
    end;
  if (ST.HasOption('rr') or ST.HasOption('ar')) then
    begin
      OutputCol[StatCount] := Col1;
      Inc(StatCount);
      Col1 += 2;
      Include(Result, tsRR);
    end;
  if (ST.HasOption('odds')) then
    begin
      OutputCol[StatCount] := Col1;
      Inc(StatCount);
      Col1 += 2;
      Include(Result, tsOR);
    end;
  if (ST.HasOption('t')) then
    begin
      OutputCol[StatCount] := Col1;
      Inc(StatCount);
      Col1 += 2;
      Include(Result, tsChi2);
    end;
  if (ST.HasOption('ex')) then
      begin
      OutputCol[StatCount] := Col1;
      Inc(StatCount);
      Col1 += 1;
      Include(Result, tsFExP);
    end;

end;

procedure TCTable.ExecCTable(DF: TEpiDataFile; ST: TCTableCommand);
var
  SummaryTable: TOutputTable;
  VarNames: TStrings;
  TwoVarNames: TStrings;
  Opt: TOption;
//  StratifyVarnames: TStringList;
  WeightName: String;
  i, VarCount: Integer;
  TableData: TTables;
  Table: TTwoWayTables;
  TablesRefMap: TEpiReferenceMap;
begin
  Varnames := ST.VariableList.GetIdentsAsList;
  TwoVarNames := TStringList.Create;

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

  // get each set of results based on first variable and variable i
  VarCount := VarNames.Count;
  if (VarCount < 2) then
  begin
    // error message (should not happen)
    exit;
  end;

  TableData  := TTables.Create(FExecutor, FOutputCreator);
  TwoVarNames.Add(VarNames[0]);
  for i := 1 to VarCount - 1 do
  begin
    TwoVarNames.Add(VarNames[i]); // ?.GetVariable(i));
    Table := TableData.CalcTables(DF, TwoVarNames, StratifyVarNames, WeightName,
             ST.Options, TablesRefMap, GetStatisticOptions(ST));

//    DoResultVariables(Table);

    if (not ST.HasOption('q')) then
    begin
      if (i = 1) then
        SummaryTable := CreateOutputHeader(Table, ST);
      DoOutputCTableRow(Table, ST, SummaryTable);
    end;
    TwoVarNames.Delete(1);
  end;
  SummaryTable.SetRowBorders(SummaryTable.RowCount - 1, [cbBottom]);
  TableData.Free;
end;


end.

