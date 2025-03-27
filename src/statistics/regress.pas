unit regress;

{$codepage UTF-8}
{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ast, epidatafiles, epicustombase,
  executor, result_variables, epifields_helper, ana_globals,
  lmath, outputcreator, regress_types;

resourcestring
  sNoData              = 'No data';
  sRegTooManyTypes     = 'Too many regression types';
  sRegTypeNotFound     = 'Regression type not found';

type

  { TRegress }

  TRegress = class
  private
    FVariables: TStrings;
    FNVar: Integer;
  protected
    FExecutor: TExecutor;
    FOutputCreator: TOutputCreator;
    function GetRegressModelClass: TRegressClass; virtual; abstract;
    function GetRegressModel(ST: TRegressCommand; out Model: TRegressModel): Boolean;
  public
    constructor Create(AExecutor: TExecutor; OutputCreator: TOutputCreator);
    destructor Destroy; override;
  // Method called from Executor, does calculation + result vars + output
    procedure ExecRegress(ST: TRegressCommand);
  end;

  procedure RegisterRegressModel(Model: TRegressType; RegressClass: TRegressClass);

implementation

uses
  uerrors, options_utils, regress_linear, regress_polynomial;

var
  RegressMap: TRegressMap;

procedure RegisterRegressModel(Model: TRegressType; RegressClass: TRegressClass);
begin
  if (not Assigned(RegressMap)) then
    RegressMap := TRegressMap.Create;
  RegressMap.Add(Model, RegressClass);
end;

{ TRegress }

constructor TRegress.Create(AExecutor: TExecutor;
  OutputCreator: TOutputCreator);
begin
  FExecutor := AExecutor;
  FOutputCreator := OutputCreator;
end;

destructor TRegress.Destroy;
begin
  inherited Destroy;
end;

procedure TRegress.ExecRegress(ST: TRegressCommand);
var
  DF: TEpiDataFile;
  Model: TRegressModel;
  Opt: TOption;
  i, d: Integer;
  saveField: TEpiField;
  msg: UTF8String;
begin
  FExecutor.ClearResults('$r_');
  FVariables := ST.VariableList.GetIdentsAsList;
  ST.ExecResult := csrFailed;

  try
    DF := FExecutor.PrepareDatafile(FVariables, FVariables);

    if DF.Size = 0 then
      begin
        FExecutor.Error(sNoData);
        DF.Free;
        Exit;
      end;

    FNVar := FVariables.Count;
    if (not GetRegressModel(ST, Model)) then
      begin
        DF.Free;
        exit;
      end;
    Model.SetFormula(FVariables);
    Model.SetDepV(DF.Fields.FieldByName[FVariables[0]]);
    for i := 1 to FNVar - 1 do
      Model.SetIndepV(DF.Fields.FieldByName[FVariables[i]],i);
    msg := Model.Estimate();
    if (msg = '') then
      begin
        if (ST.HasOption('fit',opt)) then
          begin
            DF.Free;
            DF := FExecutor.PrepareDatafile(FVariables, nil);
            saveField := FExecutor.DataFile.Fields.FieldByName[Opt.Expr.AsIdent];
            Model.GetFittedVar(DF,saveField);
          end;
        Model.DoResultVariables();
        if (not ST.HasOption('q')) then
          Model.DoOutput();
      end;
    DF.Free;
  finally
end;

end;

function TRegress.GetRegressModel(ST: TRegressCommand; out Model: TRegressModel): Boolean;
var
  opt: TOption;
  Index: Integer;
  typeFound: String;
  typesFound: Integer;
  rType: TRegressType;
begin
  rType := rtLinear;
  typeFound := '';
  typesFound := 0;
  result := true;
  if (ST.HasOption('poly',opt)) then
    begin
      rType := rtPolynomial;
      typeFound += ' ' + opt.Ident;
      inc(typesFound);
    end;
  if (ST.HasOption('logit', opt)) then
    begin
      rType := rtLogistic;
      typeFound += ' ' + opt.Ident;
      inc(typesFound);
    end;
  if (typesFound > 1) then
    begin
      FExecutor.Error(sRegTooManyTypes + ': ' + typeFound);
      result := false;
      exit;
    end;
  Index := RegressMap.IndexOf(rType);
  if (Index < 0) then
    begin
      FExecutor.Error(sRegTypeNotFound + ': ' + typeFound);
      result := false;
      exit;
    end;
  Model := RegressMap.Data[Index].Create(FExecutor, FOutputCreator, ST);
end;

end.
