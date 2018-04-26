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
    FSum, FStdvar: EpiFloat;
    FCount: EpiInteger;
    FMeanType: TAggrMeanType;
    FLowerCI, FUpperCI: TEpiField;
  public
    constructor Create(AResultVarName: UTF8String; AAggregateVector: TEpiField;  AMeanType: TAggrMeanType);
    procedure Execute(Idx: integer); override;
    procedure SetOutput(Idx: integer); override;
    procedure Reset(); override;
    procedure CreateResultVector(DataFile: TEpiDataFile; VariableLabelType: TEpiGetVariableLabelType); override;
    property MeanType: TAggrMeanType read FMeanType;
    property Sum: EpiFloat read FSum;
    property StdVar: EpiFloat read FStdvar;
    property Count: EpiInteger read FCount;
    property LowerCI: TEpiField read FLowerCI;
    property UpperCI: TEpiField read FUpperCI;
  end;

  { TAggrMinMax }

  TAggrMinMax = class(TAggrFunc)
  private
    FValue: EpiFloat;
    FMinimum: Boolean;
  public
    constructor Create(AResultVarName: UTF8String; AAggregateVector: TEpiField; FindMin: boolean);
    procedure Execute(Idx: integer); override;
    procedure SetOutput(Idx: integer); override;
    procedure Reset(); override;
    procedure CreateResultVector(DataFile: TEpiDataFile; VariableLabelType: TEpiGetVariableLabelType); override;
    property Value: EpiFloat read FValue;
    property IsMinimum: Boolean read FMinimum;
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

uses
  statfunctions;


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

constructor TAggrMean.Create(AResultVarName: UTF8String;
  AAggregateVector: TEpiField; AMeanType: TAggrMeanType);
begin
  inherited Create(AResultVarName, AAggregateVector, afMean);
  FMeanType := AMeanType;
end;

procedure TAggrMean.Execute(Idx: integer);
var
  Val: Extended;
begin
  if AggregateVector.IsMissing[Idx] then exit;
  Val := AggregateVector.AsFloat[Idx];

  FSum := FSum + Val;
  FStdvar := FStdvar + (Val * Val);
  Inc(FCount);
end;

procedure TAggrMean.SetOutput(Idx: integer);
var
  Mean, lStdVar, lStdDev, lLCI, lUCI: EpiFloat;
begin
  if Count = 0 then
    FResultVector.IsMissing[idx] := True
  else
    begin
      Mean := Sum / Count;
      if Count = 1 then
        begin
          lStdVar := TEpiFloatField.DefaultMissing;
          lStdDev := TEpiFloatField.DefaultMissing;
          lLCI    := TEpiFloatField.DefaultMissing;
          lUCI    := TEpiFloatField.DefaultMissing;
        end
      else
        begin
          lStdVar := (lStdVar + (count * (Mean * Mean) - 2 * Mean * Sum)) / (Count - 1);
          lStdDev := sqrt(lStdVar);
          F       := PTDISTRINV((Count - 1), 0.025) * System.Sqrt(lStdVar / Count);
          lLCI    := Mean - F;
          lUCI    := Mean + F;
        end;

      case MeanType of
        amMCI,
        amMean:   fResultVector.AsFloat[idx] := Mean;
        amStdVar: fResultVector.AsFloat[idx] := lStdVar;
        amStdDev: fResultVector.AsFloat[idx] := lStdDev;
      end;

      if MeanType = amMCI then
      begin
        fLowerCI.AsFloat[Idx] := lLCI;
        fUpperCI.AsFloat[Idx] := lUCI;
      end;
    end;
end;

procedure TAggrMean.Reset();
begin
  FSum    := 0.0;
  FCount  := 0;
  FStdVar := 0;
end;

procedure TAggrMean.CreateResultVector(DataFile: TEpiDataFile;
  VariableLabelType: TEpiGetVariableLabelType);
begin
  FResultVector := DataFile.NewField(ftFloat);
  FResultVector.Name := ResultVariableName;

  Case MeanType of
    amMean,
    amMCI:
      FResultVector.Question.Text := '(Mean) ' + FAggregateVector.GetVariableLabel(VariableLabelType);

    amStdVar:
      FResultVector.Question.Text := '(Variance) ' + FAggregateVector.GetVariableLabel(VariableLabelType);

    amStdDev:
      FResultVector.Question.Text := '(Deviance) ' + FAggregateVector.GetVariableLabel(VariableLabelType);
  end;

  if (MeanType = amMCI) then
    begin
      FLowerCI := DataFile.NewField(ftFloat);
      FLowerCI.Name := 'LowCi' + ResultVariableName;
      FLowerCI.Question.Text := '(Lower 95% CI - Mean) ' + FAggregateVector.GetVariableLabel(VariableLabelType);

      FUpperCI := DataFile.NewField(ftFloat);
      FUpperCI.Name := 'UpperCi' + ResultVariableName;
      FUpperCI.Question.Text := '(Upper95% CI - Mean) ' + FAggregateVector.GetVariableLabel(VariableLabelType);
    end;
end;

{ TAggrMinMax }

constructor TAggrMinMax.Create(AResultVarName: UTF8String;
  AAggregateVector: TEpiField; FindMin: boolean);
begin
  inherited Create(AResultVarName, AAggregateVector, afMinMax);
  FMinimum := FindMin;
end;

procedure TAggrMinMax.Execute(Idx: integer);
begin
  if fAggregateVector.IsMissing[idx] then exit;

  if TEpiFloatField.CheckMissing(Value) then
    FValue := FAggregateVector.AsFloat[Idx]
  else
    if Minimum then
      FValue := Math.Min(FAggregateVector.AsFloat[Idx], Value)
    else
      FValue := Math.Max(FAggregateVector.AsFloat[Idx], Value);
end;

procedure TAggrMinMax.SetOutput(Idx: integer);
begin
  FResultVector.AsFloat[Idx] := Value;
end;

procedure TAggrMinMax.Reset();
begin
  Value := TEpiFloatField.DefaultMissing;
end;

procedure TAggrMinMax.CreateResultVector(DataFile: TEpiDataFile;
  VariableLabelType: TEpiGetVariableLabelType);
begin
  FResultVector := DataFile.NewField(AggregateVector.FieldType);
  FResultVector.Name := ResultVariableName;

  if IsMinimum then
    FResultVector.Question.Text := '(MIN) '+ fAggregateVector.GetVariableLabel(VariableLabelType)
  else
    FResultVector.Question.Text := '(MAX) '+ fAggregateVector.GetVariableLabel(VariableLabelType);
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

