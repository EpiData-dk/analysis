unit tables;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, math, epidatafiles, ast, executor, outputcreator,
  epicustombase;

type

  { TTables }

  TTables = class
  private
    FExecutor: TExecutor;
    FOutputCreator: TOutputCreator;
    function  DoCalcTables(InputDF: TEpiDataFile; Varnames, StratifyNames: TStrings): TEpiDataFiles;
    procedure InternalOutputTable(DF: TEpiDataFile);
    function PackStratifiedDataset(Sender: TEpiDataFile; Index: Integer; Data: Pointer): boolean;
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
  aggregate, aggregate_types, epimiscutils, epidatafileutils;

type
  PBoundArray = ^TBoundArray;
  TPackRecord = record
    Runners: PBoundArray;
    Variables: TEpiFields;
  end;

{ TTables }

function TTables.DoCalcTables(InputDF: TEpiDataFile; Varnames,
  StratifyNames: TStrings): TEpiDataFiles;
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
  Result := TEpiDataFiles.Create(nil);

  // Create counts of the Stratification variables.
  FuncList := TAggrFuncList.Create(true);
  FuncList.Add(TAggrCount.Create('n', nil, acAll));

  // Combine the two-way variables with all by variables
  // - these variables will be aggregated (and counted)
  AggrVars := TStringList.Create;
  AggrVars.AddStrings(Varnames);
  AggrVars.AddStrings(StratifyNames);

  Aggr := TAggregate.Create(FExecutor, FOutputCreator);
  ResultDF := Aggr.CalcAggregate(InputDF, AggrVars, FuncList, True, AggregatedVariables, RefMap);
  AggrVars.Free;

  TwoWayVariables := TEpiFields.Create(nil);
  TwoWayVariables.ItemOwner := true;
  TwoWayVariables.AddItem(AggregatedVariables.DeleteItem(0));
  TwoWayVariables.AddItem(AggregatedVariables.DeleteItem(0));

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
    end
  else
    Result.AddItem(ResultDF);
end;

procedure TTables.InternalOutputTable(DF: TEpiDataFile);
var
  T: TOutputTable;
  F: TEpiField;
  i, j: Integer;
begin
  // Just output something
  T := FOutputCreator.AddTable;
  T.Header.Text := DF.Caption.Text;
  T.ColCount := DF.Fields.Count;
  T.RowCount := DF.Size + 1;

  i := 0;
  for F in DF.Fields do
    begin
      T.Cell[i, 0].Text := F.Name;

      for j := 0 to DF.Size - 1 do
        if F.IsMissing[j] then
          T.Cell[i, j + 1].Text := '0'
        else
          T.Cell[i, j + 1].Text := F.AsString[j];

      Inc(i);
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
  ResultDFs: TEpiDataFiles;
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
    InternalOutputTable(TempDF);
end;

function TTables.CalcTables(DF: TEpiDataFile; out RefMap: TEpiReferenceMap
  ): TEpiDataFile;
begin

end;

end.

