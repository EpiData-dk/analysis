unit aggregate_types;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Executor, epidatafiles, epidatafilestypes, epifields_helper;

type

  EAggrFunc = class(Exception);

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
    FCount, FLastCount, FMissingCount: EpiInteger;
    FPercentileType: TAggrPercentileType;
  public
    constructor Create(AResultVarName: UTF8String; AAggregateVector: TEpiField; APercentileType: TAggrPercentileType);
    procedure Execute(Idx: integer); override;
    procedure SetOutput(Idx: integer); override;
    procedure Reset(); override;
    procedure CreateResultVector(DataFile: TEpiDataFile; VariableLabelType: TEpiGetVariableLabelType); override;
    property Count: EpiInteger read FCount;
    property LastCount: EpiInteger read FLastCount;
    property MissingCount: EpiInteger read FMissingCount;
    property PercentileType: TAggrPercentileType read FPercentileType;
  end;

  { TAggrFuncList }

  TAggrFuncList = class
  private
    fList: TList;
    function GetItem(const index: integer): TAggrFunc;
    function Unique(Func: TAggrFunc): boolean;
    function Extract(Index: Integer): TAggrFunc;
    procedure DoError(Const Msg: UTF8String);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Add(Func: TAggrFunc);
    function ExtractPercentiles: TAggrFuncList;
    function IndexOf(const Name: string): Integer;
    procedure Insert(const Index: integer; Func: TAggrFunc);
    procedure SetOutputs(Idx: integer);
    procedure ResetAll;
    function Count: integer;
    function GetVarList: TStrings;
    property Items[const Index: integer]: TAggrFunc read GetItem; default;
  end;

implementation

uses
  statfunctions, math, generalutils, LazUTF8;


{ TAggrFunc }

constructor TAggrFunc.Create(AResultVarName: UTF8String;
  AAggregateVector: TEpiField; AFuncType: TAggrFuncType);
begin
  FAggregateVector := AAggregateVector;
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
var
  S: String;
begin
  FResultVector := DataFile.NewField(ftInteger);
  FResultVector.Name := FResultVariableName;

  S := '(N)';
  if Assigned(AggregateVector) then
    S := S + AggregateVector.GetVariableLabel(VariableLabelType);

  FResultVector.Question.Text := S;
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
  F: Extended;
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
    if FMinimum then
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
  FValue := TEpiFloatField.DefaultMissing;
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

constructor TAggrPercentile.Create(AResultVarName: UTF8String;
  AAggregateVector: TEpiField; APercentileType: TAggrPercentileType);
begin
  inherited Create(AResultVarName, AAggregateVector, afPercentile);
  FPercentileType := APercentileType;
  FLastCount := 0;
  FMissingCount := 0;
end;

procedure TAggrPercentile.Execute(Idx: integer);
begin
  Inc(FCount);
  if not (FAggregateVector.IsMissing[Idx] or FAggregateVector.IsMissingValue[Idx]) then
    Inc(FMissingCount);
end;

procedure TAggrPercentile.SetOutput(Idx: integer);
var
  d, ix, offset: integer;
  w: EpiFloat;
begin
  if MissingCount = 0 then
  begin
    FResultVector.IsMissing[Idx] := true;
    Exit;
  end;

  offset := LastCount;
  case PercentileType of
    ap1 : d := 1;
    ap5 : d := 5;
    ap10: d := 10;
    ap25: d := 25;
    ap50: d := 50;
    ap75: d := 75;
    ap90: d := 90;
    ap95: d := 95;
    ap99: d := 99;
  else
    d := 0;
  end;

  ix := PercentileIndexNIST(MissingCount, d/100, w);
  if w = 0 then
    FResultVector.AsFloat[idx] := FAggregateVector.AsFloat[ix + offset]
  else
    FResultVector.AsFloat[idx] := FAggregateVector.AsFloat[ix + offset] +
                                  (FAggregateVector.AsFloat[ix+offset+1] - FAggregateVector.AsFloat[ix+offset]) * (w);
end;

procedure TAggrPercentile.Reset();
begin
  FLastCount := LastCount + Count;
  FMissingCount := 0;
  FCount := 0;
end;

procedure TAggrPercentile.CreateResultVector(DataFile: TEpiDataFile;
  VariableLabelType: TEpiGetVariableLabelType);
begin
  FResultVector := DataFile.NewField(ftFloat);
  case PercentileType of
    ap1 : FResultVector.Question.Text := FAggregateVector.GetVariableLabel(VariableLabelType) + ' 1% Percentile';
    ap5 : FResultVector.Question.Text := FAggregateVector.GetVariableLabel(VariableLabelType) + ' 5% Percentile ';
    ap10: FResultVector.Question.Text := FAggregateVector.GetVariableLabel(VariableLabelType) + ' 10% Percentile ';
    ap25: FResultVector.Question.Text := FAggregateVector.GetVariableLabel(VariableLabelType) + ' 25% Percentile ';
    ap50: FResultVector.Question.Text := FAggregateVector.GetVariableLabel(VariableLabelType) + ' Median ';
    ap75: FResultVector.Question.Text := FAggregateVector.GetVariableLabel(VariableLabelType) + ' 75% Percentile ';
    ap90: FResultVector.Question.Text := FAggregateVector.GetVariableLabel(VariableLabelType) + ' 90% Percentile ';
    ap95: FResultVector.Question.Text := FAggregateVector.GetVariableLabel(VariableLabelType) + ' 95% Percentile ';
    ap99: FResultVector.Question.Text := FAggregateVector.GetVariableLabel(VariableLabelType) + ' 99% Percentile ';
  end;
end;

{ TAggrFuncList }

function TAggrFuncList.GetItem(const index: integer): TAggrFunc;
begin
  result := TAggrFunc(fList[index]);
end;

function TAggrFuncList.Unique(Func: TAggrFunc): boolean;
var
  i: Integer;
begin
  result := true;

  for i := 0 to Count -1 do
    if UTF8CompareStr(Items[i].ResultVariableName, Func.ResultVariableName) = 0 then
      begin
        Result := false;
        Break;
      end;
end;

function TAggrFuncList.Extract(Index: Integer): TAggrFunc;
begin
  Result := TAggrFunc(fList.Extract(fList.Items[Index]));
end;

procedure TAggrFuncList.DoError(const Msg: UTF8String);
begin
  raise EAggrFunc.Create(Msg);
end;

constructor TAggrFuncList.Create;
begin
  fList := TList.Create;
end;

destructor TAggrFuncList.Destroy;
var
  i: Integer;
begin
  for i := fList.Count - 1 downto 0 do
    Items[i].Free;
  fList.Free;

  inherited Destroy();
end;

procedure TAggrFuncList.Add(Func: TAggrFunc);
begin
  if Unique(Func) then
    fList.Add(Func)
  else
    DoError(Format('Aggregate function already used for %s', [Func.AggregateVector.Name]));
end;

function TAggrFuncList.ExtractPercentiles: TAggrFuncList;
var
  i: Integer;
begin
  Result := TAggrFuncList.Create;

  for i := Count - 1 downto 0 do
    if Items[i].FuncType = afPercentile then
      result.Add(Extract(i));
end;

function TAggrFuncList.IndexOf(const Name: string): Integer;
begin
{  for result := 0 to fList.Count -1 do
    if Items[result].ContainVarname(Name) then exit; }
  result := -1;
end;

procedure TAggrFuncList.Insert(const Index: integer; Func: TAggrFunc);
begin
  if Unique(Func) then
    fList.Insert(index, Func)
  else
    DoError(Format('Aggregate function already used for %s', [Func.AggregateVector.Name]));
end;

procedure TAggrFuncList.SetOutputs(Idx: integer);
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
    Items[i].SetOutput(Idx);
end;

procedure TAggrFuncList.ResetAll;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
    Items[i].Reset();
end;

function TAggrFuncList.Count: integer;
begin
  Result := fList.Count;
end;

function TAggrFuncList.GetVarList: TStrings;
var
  i: Integer;
begin
  Result := TStringList.Create;

  for i := 0 to Count - 1 do
  begin
    if Items[i].AggregateVector.Name = '' then continue;

    if Result.IndexOf(Items[i].AggregateVector.Name) = -1 then
      Result.Add(Items[i].AggregateVector.Name);
  end;
end;

end.

