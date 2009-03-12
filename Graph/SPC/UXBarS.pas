unit UXBarS;

interface

uses SysUtils, UVectors, classes, ansDataTypes, UOutput, UCommands, Windows, UAnaToken,
     Chart, UChartArray, USPCBase;

type

  TXBarS = class(TCustomSPCChart)
  private
    XBarBar: EpiFloat;
    SigmaHat: EpiFloat;
    NSum: EpiInt;
    HSum: EpiFloat;
    function C4(N: Integer): EpiFloat;
    function GetNVector(): TEpiVector;
  protected
    function AllowFreeze: Boolean; override;
    procedure CalcMean; override;
    procedure CheckVarnames(); override;
    procedure CleanupOutput(OutputTable: TStatTable); override;
    procedure CreateCharts(ChartArray: TChartArray; const Dataframe: TEpiDataFrame);  override;
    function CreateOutputTable: TStatTable;  override;
    function ExcludeFunction(index: Integer; df: TEpiDataFrame): Boolean; override;
    function ExcludeValueFunction(index: Integer; df: TEpiDataFrame): Extended; override;
    procedure ExecuteMeanFail(LoopIdx: Integer; LastIdx: Integer); override;
    procedure ExecuteMeanSuccess(LoopIdx: Integer; LastIdx: Integer); override;
    function GetExclusionVector(const Dataframe: TEpiDataframe): TEpiVector; override;
    function GetExcludedVector(ChartNo: Integer): TEpiVector; override;
    function GetSigma(LoopIndex, ChartNo: integer): Extended; override;
    function GetTimeVec(Dataframe: TEpiDataframe): TEpiVector; override;
    function GetXVector: TEpiVector; override;
    function GetYVector: TEpiVector; override;
    function GetZVector: TEpiVector; override;
    function GetCenter(LoopIndex: Integer; ChartNo: Integer): Extended; override;
    function GetCtrlVal(LoopIndex: Integer; ChartNo: Integer): Extended; override;
    function GetCenterText(ChartNo: integer): string; override;
    function GetChartTypeText(ChartNo: integer): string; override;
    function GetCtrlText(const Dataframe: TEpiDataFrame; const ChartNo: integer): string; override;
    function GetLCLInfoTxt(ChartNo: Integer): String; override;
    function GetUCLInfoTxt(ChartNo: Integer): String; override;
    function MakeOutputLine(ChartNo: Integer; OutputTable: TStatTable): Extended; override;
    function PreAggregate(const Dataframe: TEpiDataFrame): TEpiDataFrame;
      override;
    procedure PrepareVarnames(IncludeVarnames: TStrings;
      MissingVarnames: TStrings); override;
    procedure ResetMean; override;
    procedure SigmaResults(ChartNo: Integer; SigmaNo: Integer;
      BreakIndex: Integer); override;
    property NVec: TEpiVector read GetNVector;
  public
    constructor Create; override;
    destructor Destroy; override;
  published 

  end;

implementation

uses UCmdProcessor, UDebug, USPCUtils, Series, UGraph, GeneralUtils, Graphics,
 TeEngine, Math, UAggregate;

{ TXBarS }

function TXBarS.AllowFreeze: Boolean;
begin
  result := false;
end;

function TXBarS.C4(N: Integer): EpiFloat;

  function FracFac(N: EpiFloat): EpiFloat;
  begin
    result := 1;
    while N > 0 do
    begin
      Result := Result * N;
      N := N - 1;
    end;
    Result := Result * System.Sqrt(System.Pi);
  end;

  function Fac(N: Integer): Integer;
  begin
    Result := 1;
    while N > 1 do
    begin
      Result := Result * N;
      Dec(N);
    end;
  end;

begin
  result := Sqrt(2 / (N - 1));
  if Odd(N) then
    result := result * (FracFac((N - 2) / 2) / Fac((N - 3) div 2))
  else
    result := result * (Fac((N - 2) div 2) / FracFac((N - 3) / 2));
end;

procedure TXBarS.CalcMean;
begin
  inherited;
  XBarBar := XBarBar / NSum;
  SigmaHat := SigmaHat / HSum;
end;

procedure TXBarS.CheckVarnames;
begin
  if ((varnames.count = 4)  and (Cmd.ParamByName['XLABEL'] = nil)) or
     ((varnames.count = 3) and (Cmd.ParamByName['XLABEL'] <> nil)) then
       dm.Error('At least %d variables for %s!', [2,'XBarS'], 34001);
end;

procedure TXBarS.CleanupOutput(OutputTable: TStatTable);
begin
  // None
end;

constructor TXBarS.Create;
begin
  inherited;

end;

procedure TXBarS.CreateCharts(ChartArray: TChartArray;
  const Dataframe: TEpiDataFrame);
var
  Chart: TChart;
begin
  Chart := CreateStandardChart;
  Chart.LeftAxis.MinimumOffset := 5;
//  Chart.BottomAxis.Labels := false;
  Chart.BottomAxis.Title.Visible := false;
  Chart.Foot.Visible := false;
  Chart.Title.Caption := 'XBarS - ' + Dataframe.FindVector(Varnames[0]).GetVariableLabel(Cmd.ParameterList);
  ChartArray.Add(Chart, 'Mean');

  Chart := CreateStandardChart;
  Chart.LeftAxis.MinimumOffset := 5;
  ChartArray.Add(Chart, 'Sigma');
end;

function TXBarS.CreateOutputTable: TStatTable;
begin
  Result := dm.OutputList.NewTable(3, 1);
  Result.TableType := sttNormal;
  Result.Cell[1,1] := 'Period:';
  Result.Cell[2,1] := 'Chart:';
  Result.Cell[3,1] := 'Mean:';
end;

destructor TXBarS.Destroy;
begin

  inherited;
end;

function TXBarS.ExcludeFunction(index: Integer;
  df: TEpiDataFrame): Boolean;
begin
  result := false;
  if df.VectorByName['$N'].AsInteger[Index] = 1 then
    result := true;
end;

function TXBarS.ExcludeValueFunction(index: Integer;
  df: TEpiDataFrame): Extended;
begin
  result := df.VectorByName['$MEAN'].AsFloat[Index];
end;

procedure TXBarS.ExecuteMeanFail(LoopIdx, LastIdx: Integer);
begin
  if (Length(CtrlVec) = 2) and (Assigned(CtrlVec[1])) then
  begin
    CtrlVec[1].IsMissing[LoopIdx] := True;
    ExcludeVec[1].AsFloat[LoopIdx] := ZVec.AsFloat[LoopIdx];
  end;
end;

procedure TXBarS.ExecuteMeanSuccess(LoopIdx, LastIdx: Integer);
var
  H: EpiFloat;
  C4Const: EpiFloat;
begin
  if NVec.AsInteger[LoopIdx] = 1 then
    dm.Error('Too few samples', []);

  Inc(NSum);
  XBarBar := XBarBar + YVec.AsFloat[LoopIdx];
  C4Const := C4(NVec.AsInteger[LoopIdx]);

  H := IntPower(C4Const, 2) / (1 - IntPower(C4Const, 2));
  HSum := HSum + H;
  SigmaHat := SigmaHat + ((H * ZVec.AsFloat[LoopIdx]) / C4Const);

  if (Length(CtrlVec) = 2) and (Assigned(CtrlVec[1])) then
  begin
    CtrlVec[1].AsFloat[LoopIdx] := ZVec.AsFloat[LoopIdx];
    ExcludeVec[1].AsFloat[LoopIdx] := ZVec.AsFloat[LoopIdx];
  end;
end;

function TXBarS.GetCenter(LoopIndex, ChartNo: Integer): Extended;
begin
  if ChartNo = 0 then
    Result := XBarBar
  else
    if NVec.AsInteger[LoopIndex] = 1 then
      Result := 0
    else
      Result := SigmaHat * C4(NVec.AsInteger[LoopIndex]);
end;

function TXBarS.GetCenterText(ChartNo: integer): string;
begin
  if ChartNo = 0 then
    Result := 'Mean'
  else
    Result := 'Mean Sigma';
end;

function TXBarS.GetChartTypeText(ChartNo: integer): string;
begin
  if ChartNo = 0 then
    result := 'XBarS'
  else
    result := 'Sigma';
end;

function TXBarS.GetCtrlText(const Dataframe: TEpiDataFrame;
  const ChartNo: integer): string;
begin
  if ChartNo = 0 then
    result := 'Average'
  else
    result := 'Std-dev';
end;

function TXBarS.GetCtrlVal(LoopIndex, ChartNo: Integer): Extended;
begin
  if ChartNo = 0 then
    result := YVec.AsFloat[LoopIndex]
  else
    result := CtrlVec[ChartNo].AsFloat[LoopIndex];
end;

function TXBarS.GetExcludedVector(ChartNo: Integer): TEpiVector;
begin
  if ChartNo = 0 then
    result := Df.FindVector('$EXCLUDED')
  else begin
    result := TEpiFloatVector.Create('$EXCLUDED', Df.RowCount);
    Df.Vectors.Add(Result);
  end;
end;

function TXBarS.GetExclusionVector(
  const Dataframe: TEpiDataframe): TEpiVector;
begin
  result := Dataframe.FindVector('$MEAN');
end;

function TXBarS.GetLCLInfoTxt(ChartNo: Integer): String;
begin
  result := '';
end;

function TXBarS.GetNVector: TEpiVector;
begin
  result := df.FindVector('$N');
end;

function TXBarS.GetSigma(LoopIndex, ChartNo: integer): Extended;
var
  C4Const: EpiFloat;
  N: Integer;
begin
  Result := 0;
  N := NVec.AsInteger[LoopIndex];
  if N = 1 then exit;
  C4Const := C4(N);
  if ChartNo = 0 then
    result := SigmaHat / Sqrt(N)
  else
    result := SigmaHat * System.Sqrt(1 - C4Const);
end;

function TXBarS.GetTimeVec(Dataframe: TEpiDataframe): TEpiVector;
begin
  result := Dataframe.FindVector(Varnames[1]);
end;

function TXBarS.GetUCLInfoTxt(ChartNo: Integer): String;
begin
  result := '';
end;

function TXBarS.GetXVector: TEpiVector;
begin
  result := df.FindVector(Varnames[1]);
end;

function TXBarS.GetYVector: TEpiVector;
begin
  result := df.FindVector('$MEAN');
end;

function TXBarS.GetZVector: TEpiVector;
begin
  Result := df.FindVector('$STDDEV');
end;

function TXBarS.MakeOutputLine(ChartNo: Integer;
  OutputTable: TStatTable): Extended;
begin
  if ChartNo = 0 then
  begin
    OutputTable.Cell[1, OutputTable.RowCount] := Format('%s - %s', [LVec.AsString[1], LVec.AsString[df.RowCount]]);
    OutputTable.Cell[2, OutputTable.RowCount] := 'XBar';
    OutputTable.Cell[3, OutputTable.RowCount] := Format('%.2f', [XBarBar]);
  end else begin
    OutputTable.Cell[1, OutputTable.RowCount] := '';
    OutputTable.Cell[2, OutputTable.RowCount] := 'Sigma';
    OutputTable.Cell[3, OutputTable.RowCount] := Format('%.2f', [0.0]);
  end;
end;

function TXBarS.PreAggregate(
  const Dataframe: TEpiDataFrame): TEpiDataFrame;
var
  Agl: TAggrList;
  AggByVars: TStrings;
begin
  AggByVars := TStringList.Create;
  AggByVars.Add(Varnames[1]);
  if (Cmd.ParamExists['XLABEL']) then
    AggByVars.Add(Cmd.ParamByName['XLABEL'].AsString);

  Agl := TAggrList.Create;
  Agl.Add(TAggrCount.Create('$N', Varnames[0], acAll));
  Agl.Add(TAggrMean.Create('$MEAN', Varnames[0], amMean));
  Agl.Add(TAggrMean.Create('$STDDEV', Varnames[0], amStdDev));
  Result := OAggregate.AggregateDataframe(Dataframe, TStringList(AggByVars), Agl, Cmd);
end;

procedure TXBarS.PrepareVarnames(IncludeVarnames,
  MissingVarnames: TStrings);
begin
  IncludeVarnames.Add(Varnames[1]);
  IncludeVarnames.Add('$N');
  if (Cmd.ParamExists['XLABEL']) then
    IncludeVarnames.Add(Cmd.ParamByName['XLABEL'].AsString);

  MissingVarnames.Assign(IncludeVarnames);

  IncludeVarnames.Add('$MEAN');
  IncludeVarnames.Add('$STDDEV');
  IncludeVarnames.Add('$EXCLUDED');
end;

procedure TXBarS.ResetMean;
begin
  XBarBar := 0;
  SigmaHat := 0;
  HSum := 0;
  NSum := 0;
end;

procedure TXBarS.SigmaResults(ChartNo, SigmaNo, BreakIndex: Integer);
begin
  // do nothing;
end;

end.
