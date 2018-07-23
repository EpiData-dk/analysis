unit ctable;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, epidatafiles, ast, executor, result_variables,
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
      FStratifyVarnames: TStringList;
      FCTableStatistics: TTableStatistics;
      FOutputCol: array [0..5] of integer;
      FFooterText: UTF8String;
    public
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
  S := Tables.UnstratifiedTable.ColVariable.GetVariableLabel(VariableLabelType) + LineEnding +
       'Outcome (O+) = ' +
       Tables.UnstratifiedTable.ColVariable.GetValueLabelFormatted(0,ValueLabelType) + LineEnding +
       'Not outcome (O-) = ' +
       Tables.UnstratifiedTable.ColVariable.GetValueLabelFormatted(1,ValueLabelType);

  if (FStratifyVarNames.Count>0) then
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
  ValueLabelType: TEpiGetValueLabelType;
  Tab: TTwoWayTable;
  RowIdx, ColIdx, i: Integer;
begin
  VariableLabelType := VariableLabelTypeFromOptionList(ST.Options, FExecutor.SetOptions);
  ValueLabelType    := ValueLabelTypeFromOptionList(ST.Options, FExecutor.SetOptions);
  RowIdx := T.RowCount;
  Tab    := Tables.UnstratifiedTable;
  T.RowCount := RowIdx + 1;
  T.Cell[0, RowIdx].Text := Tab.RowVariable.GetVariableLabel(VariableLabelType);
  T.Cell[1, RowIdx].Text := IntToStr(Tab.Total);
 // TODO: add method to add results to output table for each statistic
  //       left-align first column
  //       if !ar, add in the attack rates

  for i := 0 to Tables.StatisticsCount -1  do
    begin
      ColIdx := FOutputCol[i];
      Tables.Statistics[i].AddToCompactTable(ValueLabelType, T, RowIdx, ColIdx, ST.Options);
    end;
  // Setting up borders at the top
  T.SetRowBorders(0, [cbTop]);
  T.SetRowBorders(1, [cbBottom]);
end;

procedure TCTable.DoResultVariables(Tables: TTwoWayTables);
var
  i: Integer;
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
  S: UTF8String;
  WeightName: String;
  i, VarCount: Integer;
  TableData: TTables;
  Table: TTwoWayTables;
  TablesRefMap: TEpiReferenceMap;

  procedure DoOneCTable;
// invoke CalcTables for one pair of variables
  begin;
    if ST.HasOption('m') then
      DF := FExecutor.PrepareDatafile(TwoVarNames, nil)
    else
      DF := FExecutor.PrepareDatafile(TwoVarNames, TwoVarNames);
    S := '';
    try
      if (DF.Size = 0) then
        begin
          S := VarNames[0] + ': No data' + LineEnding;
          Exit;
        end;
      Table := TableData.CalcTables(DF, TwoVarNames, FStratifyVarNames, WeightName,
             ST.Options, TablesRefMap, FCTableStatistics);

//    DoResultVariables(Table);

      if (not ST.HasOption('q')) then
      begin
        if (i = 1) then // cannot create header until we have done calctable once
          SummaryTable := CreateOutputHeader(Table, ST);
        DoOutputCTableRow(Table, ST, SummaryTable);
        SummaryTable.Footer.Text := SummaryTable.Footer.Text + S;
      end;

    finally
      DF.Free;

    end;
  end;

begin
  FStratifyVarnames := TStringList.Create;
  for Opt in ST.Options do
    begin
      if (Opt.Ident = 'by') then
        FStratifyVarnames.Add(Opt.Expr.AsIdent);
    end;

  WeightName := '';
  if (ST.HasOption('w', Opt)) then
    WeightName := Opt.Expr.AsIdent;

  TwoVarNames := TStringList.Create;
  TwoVarNames.Add(VarNames[0]);

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

