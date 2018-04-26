unit aggregate_types;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Executor, epidatafiles, epidatafilestypes, epifields_helper;

type

  TAggrFuncType = (afCount, afSum, afMean, afMinMax, afPercentile,
                   afSD, afSV, afIQR, afISR, afIDR, afDES, afMV, afUNKNOWN);


  { TAggrFunc }

  TAggrFunc = class(TObject)
  private
    FResultVariableName: UTF8String;
    FAggregateVector: TEpiField;
    FFuncType: TAggrFuncType;
    FResultVector: TEpiField;
  protected
    constructor Create(AResultVarName: UTF8String; AAggregateVector: TEpiField; AFuncType: TAggrFuncType); virtual;
  public
    procedure Execute(Idx: integer); virtual; abstract;
    procedure SetOutput(Idx: integer); virtual; abstract;
    procedure Reset(); virtual; abstract;
    procedure CreateResultVector(DataFile: TEpiDataFile; VariableLabelType: TEpiGetVariableLabelType); virtual; abstract;
    property  ResultVariableName: UTF8String read FResultVariableName;
    property  AggregateVector: TEpiField read FAggregateVector;
    property  ResultVector: TEpiField read FResultVector;
    property  FuncType: TAggrFuncType read FFuncType;
  end;

  { TAggrSum }

  TAggrSum = class(TAggrFunc)
  private
    Sum: EpiFloat;
  public
    constructor Create(AResultVarName: UTF8String; AAggregateVector: TEpiField);
    procedure Execute(Idx: integer); override;
    procedure SetOutput(Idx: integer); override;
    procedure Reset(); override;
    procedure CreateResultVector(DataFile: TEpiDataFile; VariableLabelType: TEpiGetVariableLabelType); override;
  end;

  TAggrCountType = (acMissing, acMissingValue, acNotMissing, acAll);

  { TAggrCount }

  TAggrCount = class(TAggrFunc)
  private
    FCount: EpiInteger;
    FCountType: TAggrCountType;
  public
    constructor Create(AResultVarName: UTF8String; AAggregateVector: TEpiField;  ACountType: TAggrCountType);
    procedure Execute(Idx: integer); override;
    procedure SetOutput(Idx: integer); override;
    procedure Reset(); override;
    procedure CreateResultVector(DataFile: TEpiDataFile; VariableLabelType: TEpiGetVariableLabelType); override;
    property  CountType: TAggrCountType read FCountType;
    property  Count: EpiInteger read FCount;
  end;

  TAggrMeanType = (amMean, amMCI, amStdVar, amStdDev);

  { TAggrMean }

  TAggrMean = class(TAggrFunc)
  private
    Sum, Stdvar: EpiFloat;
    Count: EpiInt;
    MeanType: TAggrMeanType;
    fLowerCI, fUpperCI: TEpiVector;
  public
    constructor Create(ResultVar, AggregateVar: string; AMeanType: TAggrMeanType);
    procedure Execute(Idx: integer); override;
    procedure SetOutput(Idx: integer); override;
    function ContainVarname(const Varname: string): boolean; override;
    procedure Reset(); override;
    procedure CreateResultVector(Dataframe: TEpiDataframe); override;
  end;

  { TAggrMinMax }

  TAggrMinMax = class(TAggrFunc)
  private
    Value: EpiFloat;
    Minimum: Boolean;
  public
    constructor Create(ResultVar, AggregateVar: string; FindMin: boolean);
    procedure Execute(Idx: integer); override;
    procedure SetOutput(Idx: integer); override;
    procedure Reset(); override;
    procedure CreateResultVector(Dataframe: TEpiDataframe); override;
  end;

  TAggrPercentileType = (ap1, ap5, ap10, ap25, ap50, ap75, ap90, ap95, ap99);

  { TAggrPercentile }

  TAggrPercentile = class(TAggrFunc)
  private
    Count, LastCount, MissingCount: EpiInt;
    PercentileType: TAggrPercentileType;
  public
    constructor Create(ResultVar, AggregateVar: string; APercentileType: TAggrPercentileType);
    procedure Execute(Idx: integer); override;
    procedure SetOutput(Idx: integer); override;
    procedure Reset(); override;
    procedure CreateResultVector(Dataframe: TEpiDataframe); override;
  end;

  { TAggrFuncList }

  TAggrFuncList = class
  private
    fList: TList;
    function GetItem(const index: integer): TAggrFunc;
    function Unique(Func: TAggrFunc): boolean;
    function Extract(Index: Integer): TAggrFunc;
  public
    constructor Create();
    destructor Destroy(); override;
    procedure Add(Func: TAggrFunc);
    function ExtractPercentiles(): TAggrFuncList;
    function IndexOf(const Name: string): Integer;
    procedure Insert(const Index: integer; Func: TAggrFunc);
    procedure SetOutputs(Idx: integer);
    procedure ResetAll();
    function Count(): integer;
    function GetVarList(): TStrings;
    property Items[const Index: integer]: TAggrFunc read GetItem;
  end;

implementation


{ TAggrFunc }

constructor TAggrFunc.Create(AResultVarName: UTF8String;
  AAggregateVector: TEpiField; AFuncType: TAggrFuncType);
begin
  FAggregateVector := AggregateVector;
  FResultVariableName := AResultVarName;
  FFuncType := AFuncType;
end;

{ TAggrSum }

constructor TAggrSum.Create(AResultVarName: UTF8String;
  AAggregateVector: TEpiField);
begin
  inherited Create(AResultVarName, AAggregateVector, afSum);
end;

procedure TAggrSum.Execute(Idx: integer);
begin
  if FAggregateVector.IsMissing[idx] then exit;
  Sum := Sum + FAggregateVector.AsFloat[idx];
end;

procedure TAggrSum.SetOutput(Idx: integer);
begin
  fResultVector.AsFloat[idx] := Sum;
end;

procedure TAggrSum.Reset();
begin
  Sum := 0.0;
end;

procedure TAggrSum.CreateResultVector(DataFile: TEpiDataFile;
  VariableLabelType: TEpiGetVariableLabelType);
begin
  FResultVector := DataFile.NewField(AggregateVector.FieldType);
  FResultVector.Question.Text := '(SUM) ' + AggregateVector.GetVariableLabel(VariableLabelType);
end;

{ TAggrCount }

constructor TAggrCount.Create(AResultVarName: UTF8String;
  AAggregateVector: TEpiField; ACountType: TAggrCountType);
begin
  inherited Create(AResultVarName, AAggregateVector, afCount);
  FCountType := ACountType;
end;

procedure TAggrCount.Execute(Idx: integer);
begin
  case CountType of
    acMissing:
      if (fAggregateVector.IsMissing[Idx]) then
        Inc(FCount);

    acMissingValue:
      if (fAggregateVector.IsMissingValue[Idx]) then
        Inc(FCount);

    acNotMissing:
      if not (fAggregateVector.IsMissingValue[Idx] or
              fAggregateVector.IsMissing[Idx])
      then
        Inc(FCount);

    acAll:
      Inc(FCount);
  end;
end;

procedure TAggrCount.SetOutput(Idx: integer);
begin
  ResultVector.AsInteger[Idx] := Count;
end;

procedure TAggrCount.Reset();
begin
  FCount := 0;
end;

procedure TAggrCount.CreateResultVector(DataFile: TEpiDataFile;
  VariableLabelType: TEpiGetVariableLabelType);
begin
  FResultVector := DataFile.NewField(ftInteger);
  FResultVector.Name := FResultVariableName;
  FResultVector.Question.Text := '(N) ' + AggregateVector.GetVariableLabel(VariableLabelType);
end;

{ TAggrMean }

constructor TAggrMean.Create(ResultVar, AggregateVar: string;
  AMeanType: TAggrMeanType);
begin

end;

procedure TAggrMean.Execute(Idx: integer);
begin

end;

procedure TAggrMean.SetOutput(Idx: integer);
begin

end;

function TAggrMean.ContainVarname(const Varname: string): boolean;
begin
  Result := inherited ContainVarname(Varname);
end;

procedure TAggrMean.Reset();
begin

end;

procedure TAggrMean.CreateResultVector(Dataframe: TEpiDataframe);
begin

end;

{ TAggrMinMax }

constructor TAggrMinMax.Create(ResultVar, AggregateVar: string; FindMin: boolean
  );
begin

end;

procedure TAggrMinMax.Execute(Idx: integer);
begin

end;

procedure TAggrMinMax.SetOutput(Idx: integer);
begin

end;

procedure TAggrMinMax.Reset();
begin

end;

procedure TAggrMinMax.CreateResultVector(Dataframe: TEpiDataframe);
begin

end;

{ TAggrPercentile }

constructor TAggrPercentile.Create(ResultVar, AggregateVar: string;
  APercentileType: TAggrPercentileType);
begin

end;

procedure TAggrPercentile.Execute(Idx: integer);
begin

end;

procedure TAggrPercentile.SetOutput(Idx: integer);
begin

end;

procedure TAggrPercentile.Reset();
begin

end;

procedure TAggrPercentile.CreateResultVector(Dataframe: TEpiDataframe);
begin

end;

{ TAggrFuncList }

function TAggrFuncList.GetItem(const index: integer): TAggrFunc;
begin

end;

function TAggrFuncList.Unique(Func: TAggrFunc): boolean;
begin

end;

function TAggrFuncList.Extract(Index: Integer): TAggrFunc;
begin

end;

constructor TAggrFuncList.Create();
begin

end;

destructor TAggrFuncList.Destroy();
begin
  inherited Destroy();
end;

procedure TAggrFuncList.Add(Func: TAggrFunc);
begin

end;

function TAggrFuncList.ExtractPercentiles(): TAggrFuncList;
begin

end;

function TAggrFuncList.IndexOf(const Name: string): Integer;
begin

end;

procedure TAggrFuncList.Insert(const Index: integer; Func: TAggrFunc);
begin

end;

procedure TAggrFuncList.SetOutputs(Idx: integer);
begin

end;

procedure TAggrFuncList.ResetAll();
begin

end;

function TAggrFuncList.Count(): integer;
begin

end;

function TAggrFuncList.GetVarList(): TStrings;
begin

end;

end.

