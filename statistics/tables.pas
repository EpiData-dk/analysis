unit tables;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, math, epidatafiles, ast, executor, outputcreator,
  epicustombase;

type

  TTwoWayDatafile = class;

  { TTwoWayDataFiles }

  TTwoWayDataFiles = class(TEpiDataFiles)
  protected
    function GetItems(Index: integer): TTwoWayDatafile;
  public
    property Items[Const Index: Integer]: TTwoWayDatafile read GetItems;
  end;

  { TTwoWayDatafile }

  TTwoWayDatafile = class(TEpiDataFile)
  private
    // Aggregated two way variables
    FColVar: TEpiField;
    FRowVar: TEpiField;
  private
    procedure CheckIndex(Col, Row: Integer);
    function GetColCount: Integer;
    function GetCounts(const Col, Row: Integer): Integer;
    function GetNVar: TEpiField;
    function GetRowCount: Integer;
  protected
    property NVar: TEpiField read GetNVar;
  public
    property Counts[Const Col, Row: Integer]: Integer read GetCounts;
    property ColCount: Integer read GetColCount;
    property RowCount: Integer read GetRowCount;
    property ColVariable: TEpiField read FColVar;
    property RowVariable: TEpiField read FRowVar;
  end;

  { TTables }

  TTables = class
  private
    FExecutor: TExecutor;
    FOutputCreator: TOutputCreator;
    function  DoCalcTables(InputDF: TEpiDataFile; Varnames, StratifyNames: TStrings): TTwoWayDataFiles;
    procedure InternalOutputTable(DF: TTwoWayDataFile; ST: TTablesCommand);
    function  PackStratifiedDataset(Sender: TEpiDataFile; Index: Integer; Data: Pointer): boolean;
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
  aggregate, aggregate_types, epimiscutils, epidatafileutils, epifields_helper, options_utils;

type
  PBoundArray = ^TBoundArray;
  TPackRecord = record
    Runners: PBoundArray;
    Variables: TEpiFields;
  end;

{ TTwoWayDataFiles }

function TTwoWayDataFiles.GetItems(Index: integer): TTwoWayDatafile;
begin
  result := TTwoWayDatafile(inherited GetItems(Index));
end;

{ TTwoWayDatafile }

procedure TTwoWayDatafile.CheckIndex(Col, Row: Integer);
begin
  if (Col < 0) or (Col >= ColCount) or
     (Row < 0) or (Row >= RowCount)
  then
    raise Exception.Create('TTwoWayDatafile: Index out of bounds!');
end;

function TTwoWayDatafile.GetColCount: Integer;
begin
  result := FColVar.Size;
end;

function TTwoWayDatafile.GetCounts(const Col, Row: Integer): Integer;
var
  Index: Integer;
begin
  CheckIndex(Col, Row);
  Index := Col * RowCount + Row;
  if NVar.IsMissing[Index] then
    Result := 0
  else
    Result := NVar.AsInteger[Index];
end;

function TTwoWayDatafile.GetNVar: TEpiField;
begin
  result := Fields[Fields.Count - 1];
end;

function TTwoWayDatafile.GetRowCount: Integer;
begin
  result := FRowVar.Size;
end;

{ TTables }

function TTables.DoCalcTables(InputDF: TEpiDataFile; Varnames,
  StratifyNames: TStrings): TTwoWayDataFiles;
var
  FuncList: TAggrFuncList;
  Aggr: TAggregate;
  ResultDF, TempDF: TEpiDataFile;
  RefMap: TEpiReferenceMap;
  AggrVars: TStringList;
  AggregatedVariables, TwoWayVariables, StratifyVariables: TEpiFields;
  S: String;
  i: Integer;
  Runners: TBoundArray;
  PackRecord: TPackRecord;

begin
  Result := TTwoWayDataFiles.Create(nil);

  // Create counts of the Stratification variables.
  FuncList := TAggrFuncList.Create(true);
  FuncList.Add(TAggrCount.Create('n', nil, acAll));

  // Combine the two-way variables with all by variables
  // - these variables will be aggregated (and counted)
  AggrVars := TStringList.Create;
  AggrVars.AddStrings(Varnames);
  AggrVars.AddStrings(StratifyNames);

  Aggr := TAggregate.Create(FExecutor, FOutputCreator);
  Aggr.ResultDataFileClass := TTwoWayDatafile;
  ResultDF := Aggr.CalcAggregate(InputDF, AggrVars, FuncList, True, AggregatedVariables, RefMap);
  AggrVars.Free;

  TwoWayVariables := TEpiFields.Create(nil);
  TwoWayVariables.AddItem(AggregatedVariables.DeleteItem(0));
  TwoWayVariables.AddItem(AggregatedVariables.DeleteItem(0));

  TTwoWayDatafile(ResultDF).FColVar := TwoWayVariables.Field[0];
  TTwoWayDatafile(ResultDF).FRowVar := TwoWayVariables.Field[1];

  StratifyVariables := TEpiFields.Create(nil);
  StratifyVariables.ItemOwner := true;
  for i := AggregatedVariables.Count - 1 downto 0 do
    StratifyVariables.AddItem(AggregatedVariables.DeleteItem(i));

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
          TempDF := TEpiDataFile(ResultDF.Clone(nil, RefMap));
          TempDF.Name := '@tab' + IntToStr(Result.Count);
          TempDF.Pack(@PackStratifiedDataset, @PackRecord);

          TTwoWayDatafile(TempDF).FColVar := TwoWayVariables.Field[0];
          TTwoWayDatafile(TempDF).FRowVar := TwoWayVariables.Field[1];

          Result.AddItem(TempDF);

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

      ResultDF := Aggr.CalcAggregate(InputDF, Varnames, FuncList, True, AggregatedVariables, RefMap);
      TTwoWayDatafile(ResultDF).FColVar := TwoWayVariables.Field[0];
      TTwoWayDatafile(ResultDF).FRowVar := TwoWayVariables.Field[1];
      Result.InsertItem(0, ResultDF);
    end
  else
    Result.AddItem(ResultDF);

  Aggr.Free;
  FuncList.Free;
end;

procedure TTables.InternalOutputTable(DF: TTwoWayDataFile; ST: TTablesCommand);
var
  T: TOutputTable;
  F: TEpiField;
  i, j, Col, Row: Integer;
  VariableLabelType: TEpiGetVariableLabelType;
  ValueLabelType: TEpiGetValueLabelType;
begin
  // Just output something
  T := FOutputCreator.AddTable;
  T.Header.Text := DF.Caption.Text;
  T.ColCount := DF.ColCount + 1;
  T.RowCount := DF.RowCount + 1;

  i := 0;

  VariableLabelType := VariableLabelTypeFromOptionList(ST.Options, FExecutor.SetOptions);
  ValueLabelType    := ValueLabelTypeFromOptionList(ST.Options, FExecutor.SetOptions);

  for Col := 0 to DF.ColCount - 1 do
    T.Cell[Col + 1, 0].Text := DF.ColVariable.GetValueLabel(Col, ValueLabelType);

  for Row := 0 to DF.RowCount - 1 do
    T.Cell[0, Row + 1].Text := DF.RowVariable.GetValueLabel(Row, ValueLabelType);

  for Col := 0 to DF.ColCount - 1 do
    for Row := 0 to DF.RowCount - 1 do
      T.Cell[Col + 1, Row + 1].Text := IntToStr(DF.Counts[Col, Row]);
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
  ResultDFs: TTwoWayDataFiles;
  TempDF: TEpiDataFile;
begin
  Varnames := ST.VariableList.GetIdentsAsList;

  StratifyVarnames := TStringList.Create;
  for Opt in ST.Options do
    begin
      if (Opt.Ident <> 'by') then
        Continue;

      StratifyVarnames.Add(Opt.Expr.AsIdent);
    end;

  ResultDFs := DoCalcTables(DF, VarNames, StratifyVarnames);

  for TempDF in ResultDFs do
    InternalOutputTable(TTwoWayDataFile(TempDF), ST);
end;

function TTables.CalcTables(DF: TEpiDataFile; out RefMap: TEpiReferenceMap
  ): TEpiDataFile;
begin

end;

end.

