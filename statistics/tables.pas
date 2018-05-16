unit tables;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, math, epidatafiles, ast, executor, outputcreator,
  epicustombase, tables_types;

type

  { TTwoWayDatafile }

  TTwoWayDatafile = class(TEpiDataFile, ITableCounts)
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
    function  DoCalcTables(InputDF: TEpiDataFile; Varnames, StratifyNames: TStrings; WeightVariable: UTF8String = ''): TTwoWayTables;
    procedure OutputStratifyTable(Table: TTwoWayTable; StratifyVariables: TEpiFields; ST: TTablesCommand; IsGrandTable: boolean);
    procedure OutputSummaryTable(Tables: TTwoWayTables; ST: TTablesCommand);
    function  PackStratifiedDataset(Sender: TEpiDataFile; Index: Integer; Data: Pointer): boolean;
  protected
    procedure DoOutputTables(Tables: TTwoWayTables; ST: TTablesCommand);
    function  DoSortTables(Tables: TTwoWayTables; ST: TTablesCommand): boolean;
  public
    constructor Create(AExecutor: TExecutor; AOutputCreator: TOutputCreator);
    destructor Destroy; override;
    // Method called from Executor, does calculation + result vars + output
    procedure ExecTables(DF: TEpiDataFile; ST: TTablesCommand);
    // Method to be used from elsewhere. Does only calculations and returns the result as a specialized dataset
    function  CalcTables(DF: TEpiDataFile; Out RefMap: TEpiReferenceMap): TEpiDataFile;
  end;

implementation

uses
  aggregate, aggregate_types, epimiscutils, epidatafileutils, epifields_helper,
  options_utils, LazUTF8;

type
  PBoundArray = ^TBoundArray;
  TPackRecord = record
    Runners: PBoundArray;
    Variables: TEpiFields;
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
  StratifyNames: TStrings; WeightVariable: UTF8String): TTwoWayTables;
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

  for F in AggregatedVariables do
    FOutputCreator.DoNormal('DoCalcTables: ' + F.GetVariableLabel());

  TwoWayVariables := TEpiFields.Create(nil);
  TwoWayVariables.AddItem(AggregatedVariables.DeleteItem(0));
  TwoWayVariables.AddItem(AggregatedVariables.DeleteItem(0));

  AggregatedDF.FColVar := TwoWayVariables.Field[0];
  AggregatedDF.FRowVar := TwoWayVariables.Field[1];

  StratifyVariables := TEpiFields.Create(nil);
  StratifyVariables.ItemOwner := true;
  for i := AggregatedVariables.Count - 1 downto 0 do
    StratifyVariables.AddItem(AggregatedVariables.DeleteItem(i));

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
  Col, Row, i: Integer;
  S: String;
  Opt: TOption;
begin
  T := FOutputCreator.AddTable;
  T.ColCount    := Table.ColCount + 2;
  T.RowCount    := Table.RowCount + 2;
  if (ST.HasOption('w', Opt)) then
    begin
      T.Footer.Text := 'Weight: ' + Opt.Expr.AsIdent;
      T.Footer.Alignment := taLeftJustify;
    end;

  VariableLabelType := VariableLabelTypeFromOptionList(ST.Options, FExecutor.SetOptions);
  ValueLabelType    := ValueLabelTypeFromOptionList(ST.Options, FExecutor.SetOptions);

  if IsGrandTable then
    T.Header.Text := Table.ColVariable.GetVariableLabel(VariableLabelType)
  else
    begin
      S := '';
      for i := Low(Table.StratifyIndices) to High(Table.StratifyIndices) do
        S := S + StratifyVariables[i].GetVariableLabel(VariableLabelType) + ': ' + StratifyVariables[i].GetValueLabel(Table.StratifyIndices[i], ValueLabelType) + LineEnding;

      T.Header.Text := S + Table.ColVariable.GetVariableLabel(VariableLabelType);
    end;

  T.Cell[0, 0].Text := Table.RowVariable.GetVariableLabel(VariableLabelType);

  // Col headers
  for Col := 0 to Table.ColCount - 1 do
    T.Cell[Col + 1, 0].Text := Table.ColVariable.GetValueLabel(Col, ValueLabelType);
  T.Cell[T.ColCount - 1, 0].Text := 'Total';
  T.SetRowBorders(0, [cbBottom, cbTop]);

  // Row headers
  for Row := 0 to Table.RowCount - 1 do
    T.Cell[0, Row + 1].Text := Table.RowVariable.GetValueLabel(Row, ValueLabelType);
  T.Cell[0, T.RowCount - 1].Text := 'Total';

  // Counts
  for Col := 0 to Table.ColCount - 1 do
    for Row := 0 to Table.RowCount - 1 do
      T.Cell[Col + 1, Row + 1].Text := IntToStr(Table.Cell[Col, Row].N);

  // Col Totals
  for Col := 0 to Table.ColCount - 1 do
    T.Cell[Col + 1, T.RowCount - 1].Text := IntToStr(Table.ColTotal[Col]);

  // Row Totals
  for Row := 0 to Table.RowCount - 1 do
    T.Cell[T.ColCount - 1, Row + 1].Text := IntToStr(Table.RowTotal[Row]);

  // Grand total
  T.Cell[T.ColCount - 1, T.RowCount - 1].Text := IntToStr(Table.Total);

  T.SetRowBorders(T.RowCount - 1, [cbBottom]);
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
  T.RowCount := (Tables.Count - 1) + 3;

  // Collect header information
  S := Tables[0].ColVariable.GetVariableLabel(VariableLabelType) + ' by ' +
       Tables[0].ColVariable.GetVariableLabel(VariableLabelType);
  if (Assigned(Tables.StratifyVariables)) then
    begin
      S := S + ' adjusted for: ';
      for F in Tables.StratifyVariables do
        S := S + F.GetVariableLabel(VariableLabelType);
    end;
  T.Header.Text := S;

  // Basic summary information
  T.Cell[0, 0].Text := Format('N = %d', [Tables[0].Total]);
  T.Cell[1, 0].Text := 'N';
  T.Cell[0, 1].Text := 'Crude';
  T.Cell[1, 1].Text := IntToStr(Tables[0].Total);
  T.Cell[0, 2].Text := 'Adjusted';
  T.Cell[1, 2].Text := 'no available yet';

  RowIdx := 3;
  for Tab in Tables do
    begin
      if Tab = Tables[0] then continue;

      S := '';

      for i := 0 to Tables.StratifyVariables.Count - 1 do
        begin
          F := Tables.StratifyVariables[i];
          S := S + F.GetVariableLabel(VariableLabelType) + ': ' + F.GetValueLabel(Tab.StratifyIndices[i], ValueLabelType) + LineEnding;
        end;
      S := UTF8Trim(S);

      T.Cell[0, RowIdx].Text := S;
      T.Cell[1, RowIdx].Text := IntToStr(Tab.Total);
      Inc(RowIdx);
    end;
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

procedure TTables.DoOutputTables(Tables: TTwoWayTables; ST: TTablesCommand);
var
  i: Integer;
begin

  if (not ST.HasOption('nc')) then
    OutputStratifyTable(Tables[0], Tables.StratifyVariables, ST, true);

  // Sub table output
  if (not ST.HasOption('nt')) then
    for i := 1 to Tables.Count - 1 do
      OutputStratifyTable(Tables.Tables[i], Tables.StratifyVariables, ST, False);


//  OutputSummaryTable(Tables, ST);
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

  if (not DoSortTables(AllTables, ST)) then
    Exit;

  if (not ST.HasOption('q')) then
    DoOutputTables(AllTables, ST);
end;

function TTables.CalcTables(DF: TEpiDataFile; out RefMap: TEpiReferenceMap
  ): TEpiDataFile;
begin

end;

end.

