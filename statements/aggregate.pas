unit aggregate;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, epidatafiles, executor, outputcreator, ast, epicustombase,
  aggregate_types;

type

  { TAggregateDatafile }

  TAggregateDatafile = class(TEpiDataFile)
  public
    constructor Create(AOwner: TEpiCustomBase; const ASize: integer = 0); override;
  end;

  TAggregate = class(TObject)
  private
    FExecutor: TExecutor;
    FOutputCreator: TOutputCreator;
  protected
    // Takes the option and creates function(s) if the idens matches a function, return true - else return false
    function OptionToFunctions(InputDF: TEpiDataFile; Opt: TOption; FunctionList: TAggrFuncList): boolean; virtual;
  public
    constructor Create(AExecutor: TExecutor; OutputCreator: TOutputCreator); virtual;
    destructor Destroy; override;

    // Method called from Executor, does calculation + result vars + output
    procedure ExecAggregate(InputDF: TEpiDataFile; ST: TAggregateCommand);

    // Method to be used from elsewhere. Does only calculations and returns the result as a specialized dataset
    function  CalcAggregate(InputDF: TEpiDataFile{; FunctionList: TAggrFuncList}): TAggregateDatafile;
  end;

implementation


{ TAggregateDatafile }

constructor TAggregateDatafile.Create(AOwner: TEpiCustomBase;
  const ASize: integer);
begin
  inherited Create(AOwner, ASize);
end;

{ TAggregate }

function TAggregate.OptionToFunctions(InputDF: TEpiDataFile; Opt: TOption;
  FunctionList: TAggrFuncList): boolean;
var
  S: UTF8String;
  AggregateVariable: TEpiField;
begin
  S := Opt.Expr.AsIdent;
  AggregateVariable := InputDF.Fields.FieldByName[S];

  case Opt.Ident of
    'des':
      begin
        FunctionList.Add(TAggrMinMax.Create('','', true));
        FunctionList.Add(TAggrPercentile.Create('','', ap50));
        FunctionList.Add(TAggrMinMax.Create('','', true));
      end;

    'iqr':
      begin
        FunctionList.Add(TAggrPercentile.Create('','', ap25));
        FunctionList.Add(TAggrPercentile.Create('','', ap75));
      end;

    'idr':
      begin
        FunctionList.Add(TAggrPercentile.Create('','', ap10));
        FunctionList.Add(TAggrPercentile.Create('','', ap90));
      end;

    'isr':
      begin
        FunctionList.Add(TAggrPercentile.Create('','', ap5));
        FunctionList.Add(TAggrPercentile.Create('','', ap95));
      end;

    'mci':
      FunctionList.Add(TAggrMean.Create('', '', amMCI));

    'mv':
      begin
        FunctionList.Add(TAggrCount.Create('','', acMissing));
        FunctionList.Add(TAggrCount.Create('','', acMissingValue));
      end;

    'mean':
      FunctionList.Add(TAggrMean.Create('', '', amMean));

    'sd':
      FunctionList.Add(TAggrMean.Create('', '', amStdDev));

    'sv':
      FunctionList.Add(TAggrMean.Create('', '', amStdVar));

    'min':
      FunctionList.Add(TAggrMinMax.Create('', '', True));

    'p1':
      FunctionList.Add(TAggrPercentile.Create('', '', ap1));

    'p5':
      FunctionList.Add(TAggrPercentile.Create('', '', ap5));

    'p10':
      FunctionList.Add(TAggrPercentile.Create('', '', ap10));

    'p25':
      FunctionList.Add(TAggrPercentile.Create('', '', ap25));

    'p50',
    'med':
      FunctionList.Add(TAggrPercentile.Create('', '', ap50));

    'p75':
      FunctionList.Add(TAggrPercentile.Create('', '', ap75));

    'p90':
      FunctionList.Add(TAggrPercentile.Create('', '', ap90));

    'p95':
      FunctionList.Add(TAggrPercentile.Create('', '', ap95));

    'p99':
      FunctionList.Add(TAggrPercentile.Create('', '', ap99));

    'max':
      FunctionList.Add(TAggrMinMax.Create('', '', True));

    'sum':
      FunctionList.Add(TAggrSum.Create('', ''));

  else
    Result := false
  end;
end;

constructor TAggregate.Create(AExecutor: TExecutor;
  OutputCreator: TOutputCreator);
begin
  FExecutor := AExecutor;
  FOutputCreator := OutputCreator;
end;

destructor TAggregate.Destroy;
begin
  inherited Destroy;
end;

procedure TAggregate.ExecAggregate(InputDF: TEpiDataFile; ST: TAggregateCommand
  );
var
  VarNames: TStrings;
  Opt: TOption;
  FunctionList: TAggrFuncList;
begin
  VarNames := ST.VariableList.GetIdentsAsList;

  FunctionList := TAggrFuncList;
  for i := 0 to ST.Options.Count - 1 do
    begin
      Opt := ST.Options[i];

      // If this options is a function, create it/them and continue to next option.
      if OptionToFunctions(Opt, FunctionList) then
        Continue;

      //TODO: Check for !by options
    end;

  TempDF := DoCalcAggregate(InputDF, Varnames, AggrFuncList);
end;

function TAggregate.CalcAggregate(InputDF: TEpiDataFile): TAggregateDatafile;
begin

end;

end.

