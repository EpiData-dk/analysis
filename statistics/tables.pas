unit tables;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, epidatafiles, ast, executor, outputcreator, epicustombase;

type

  { TTables }

  TTables = class
  private
    FExecutor: TExecutor;
    FOutputCreator: TOutputCreator;
    function  DoCalcTables(InputDF: TEpiDataFile; Varnames, StratifyNames: TStrings): TEpiDataFiles;
    procedure InternalOutputTable(DF: TEpiDataFile);
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
  aggregate, aggregate_types, epimiscutils;

{ TTables }

function TTables.DoCalcTables(InputDF: TEpiDataFile; Varnames,
  StratifyNames: TStrings): TEpiDataFiles;
var
  FuncList: TAggrFuncList;
  Aggr: TAggregate;
  ResultDF: TEpiDataFile;
  RefMap: TEpiReferenceMap;
begin
  Result := TEpiDataFiles.Create(nil);

  FuncList := TAggrFuncList.Create(true);
  FuncList.Add(TAggrCount.Create('n', nil, acAll));

  Aggr := TAggregate.Create(FExecutor, FOutputCreator);
  ResultDF := Aggr.CalcAggregate(InputDF, VarNames, FuncList, RefMap);

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
        T.Cell[i, j + 1].Text := F.AsString[j];

      Inc(i);
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
  Aggr: TAggregate;
  FuncList: TAggrFuncList;
  VarNames: TStrings;
  Opt: TOption;
  RefMap: TEpiReferenceMap;
  ResultDF: TEpiDataFile;
  StratifyVarnames: TStringList;
  ResultDFs: TEpiDataFiles;
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

  FOutputCreator.DoInfoAll('Testing tables');
end;

function TTables.CalcTables(DF: TEpiDataFile; out RefMap: TEpiReferenceMap
  ): TEpiDataFile;
begin

end;

end.

