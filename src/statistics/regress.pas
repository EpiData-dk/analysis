unit regress;

{$codepage UTF-8}
{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ast, epidatafiles, epicustombase,
  executor, result_variables, epifields_helper, ana_globals,
  lmath, outputcreator, regress_types;

resourcestring
  sNoData               = 'No data';
  sRegNoconWithLogistic = 'Logistic model must have a constant';
  sRegNoconWithPoly     = 'Polynomial model must have a constant';
  sRegTooManyTypes      = 'Too many regression types';
  sRegTypeNotFound      = 'Regression type not found';

type

  { TRegress }

  TRegress = class
  protected
    FExecutor: TExecutor;
    FOutputCreator: TOutputCreator;
    function CheckUniqueVariables(Varnames: TStrings): boolean;
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
  uerrors, options_utils, regress_linear, regress_polynomial, regress_logistic;

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
  Variables: TStrings;
  Opt: TOption;
  i, d, nVar: Integer;
  saveField: TEpiField;
  msg: UTF8String;
begin
  FExecutor.ClearResults('$r_');
  Variables := ST.VariableList.GetIdentsAsList;
  ST.ExecResult := csrFailed;
  if (not CheckUniqueVariables(Variables)) then
    Exit;

  try
    DF := FExecutor.PrepareDatafile(Variables, Variables);

    if DF.Size = 0 then
      begin
        FExecutor.Error(sNoData);
        DF.Free;
        Exit;
      end;

    nVar := Variables.Count;
    if (not GetRegressModel(ST, Model)) then
      begin
        DF.Free;
        exit;
      end;
    Model.SetFormula(Variables);
    Model.SetDepV(DF.Fields.FieldByName[Variables[0]]);
    if (Model.DataError) then
      begin
        DF.Free;
        Exit;
      end;
    for i := 1 to nVar - 1 do
      Model.SetIndepV(DF.Fields.FieldByName[Variables[i]],i);
    if (Model.DataError) then
      begin
        DF.Free;
        Exit;
      end;
    msg := Model.Estimate();
    if (msg = '') then
      begin
        if (ST.HasOption('fit',opt)) then
          begin
            DF.Free;
            DF := FExecutor.PrepareDatafile(Variables, nil);
            saveField := FExecutor.DataFile.Fields.FieldByName[Opt.Expr.AsIdent];
            Model.GetFittedVar(DF,saveField);
          end;
        Model.DoResultVariables();
        if (not ST.HasOption('q')) then
          Model.DoOutput();
        if (ST.HasOption(['summary','sum'],opt)) then
              Model.Summary();
              if (not ST.HasOption('q')) then
                Model.DoOutputSummary();
      end
  else
    FExecutor.Error(msg);
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
      if (ST.HasOption('nocon')) then
        begin
          FExecutor.Error(sRegNoconWithPoly);
          result := false;
          exit;
        end;
    end;
  if (ST.HasOption('logit', opt)) then
    begin
      rType := rtLogistic;
      typeFound += ' ' + opt.Ident;
      inc(typesFound);
      if (ST.HasOption('nocon')) then
        begin
          FExecutor.Error(sRegNoconWithLogistic);
          result := false;
          exit;
        end;
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

function TRegress.CheckUniqueVariables(Varnames: TStrings): boolean;
var
  CheckList: TStringList;
  Name: UTF8String;
  DummyIdx: Integer;

  function CheckStrings(Strings: TStrings; ErrorMsg: UTF8String): boolean;
  begin
    for Name in Strings do
      if (CheckList.Find(Name, DummyIdx)) then
        begin
          FExecutor.Error(ErrorMsg + Name);
          Exit(false);
        end
    else
      CheckList.Add(Name);

    Result := true;
  end;

begin
  CheckList := TStringList.Create;
  CheckList.Sorted := true;
  Result := CheckStrings(Varnames, 'Regress command requires unique variable names: ');
end;

end.
