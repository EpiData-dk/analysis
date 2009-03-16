unit UXBarR;

interface

uses SysUtils, UVectors, classes, ansDataTypes, UOutput, UCommands, Windows, UAnaToken,
     Chart, UChartArray, USPCBase;

type

  TXBarR = class(TCustomSPCChart)
  private
    XBarBar: EpiFloat;
    RBar: EpiFloat;
    Count: Integer;
    function GetNVector(): TEpiVector;
    function GetMaxVector(): TEpiVector;
    function GetMinVector(): TEpiVector;
  protected
    function AllowFreeze(SpcLine: TSPCLine): Boolean; override;
    procedure CalcMean; override;
    procedure CheckVarnames(); override;
    procedure CleanupOutput(OutputTable: TStatTable); override;
    procedure CreateCharts(ChartArray: TChartArray; const Dataframe: TEpiDataFrame);  override;
    function CreateOutputTable: TStatTable;  override;
    function ExcludeFunction(Index: Integer; Df: TEpiDataFrame): Boolean; override;
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
    property MaxVec: TEpiVector read GetMaxVector;
    property MinVec: TEpiVector read GetMinVector;
  public
    constructor Create; override;
    destructor Destroy; override;
  published 

  end;

implementation

uses UCmdProcessor, UDebug, USPCUtils, Series, UGraph, GeneralUtils, Graphics,
 TeEngine, Math, UAggregate;

const
{$I A2.inc}
{$I D3_2.inc}

{ TXBarR }

function TXBarR.AllowFreeze(SpcLine: TSPCLine): Boolean;
begin
  result := false;
  if SpcLine = slCenter then result := true;
end;

procedure TXBarR.CalcMean;
begin
  if Frozen then exit;
  XBarBar := XBarBar / Count;
  RBar := RBar / Count;
end;

procedure TXBarR.CheckVarnames;
begin
  if ((varnames.count = 4)  and (Cmd.ParamByName['XLABEL'] = nil)) or
     ((varnames.count = 3) and (Cmd.ParamByName['XLABEL'] <> nil)) then
       dm.Error('At least %d variables for %s!', [2,'XBarS'], 34001);
end;

procedure TXBarR.CleanupOutput(OutputTable: TStatTable);
begin
  //
end;

constructor TXBarR.Create;
begin
  inherited;

end;

procedure TXBarR.CreateCharts(ChartArray: TChartArray;
  const Dataframe: TEpiDataFrame);
var
  Chart: TChart;
begin
  Chart := CreateStandardChart;
  Chart.LeftAxis.MinimumOffset := 5;
//  Chart.BottomAxis.Labels := false;
  Chart.BottomAxis.Title.Visible := false;
  Chart.Foot.Visible := false;
  Chart.Title.Caption := 'XBar - ' + Dataframe.FindVector(Varnames[0]).GetVariableLabel(Cmd.ParameterList);
  ChartArray.Add(Chart, 'XBar');

  Chart := CreateStandardChart;
  Chart.LeftAxis.MinimumOffset := 5;
  ChartArray.Add(Chart, 'Range');
end;

function TXBarR.CreateOutputTable: TStatTable;
begin
  Result := dm.OutputList.NewTable(3, 1);
  Result.TableType := sttNormal;
  Result.Cell[1,1] := 'Period:';
  Result.Cell[2,1] := 'Chart:';
  Result.Cell[3,1] := 'Mean:';
end;

destructor TXBarR.Destroy;
begin

  inherited;
end;

function TXBarR.ExcludeFunction(Index: Integer;
  Df: TEpiDataFrame): Boolean;
begin
  result := false;
  if df.VectorByName['$N'].AsInteger[Index] = 1 then
    result := true;
end;

function TXBarR.ExcludeValueFunction(index: Integer;
  df: TEpiDataFrame): Extended;
begin
  result := df.VectorByName['$MEAN'].AsFloat[Index];
end;

procedure TXBarR.ExecuteMeanFail(LoopIdx, LastIdx: Integer);
begin
  if Assigned(CtrlVec[1]) then
  begin
    CtrlVec[1].IsMissing[LoopIdx] := True;
    ExcludeVec[1].AsFloat[LoopIdx] :=
      MaxVec.AsFloat[LoopIdx] - MinVec.AsFloat[LoopIdx];
  end;
end;

procedure TXBarR.ExecuteMeanSuccess(LoopIdx, LastIdx: Integer);
begin
  if (NVec.AsInteger[LoopIdx] < 2) or (NVec.AsInteger[LoopIdx] > 100) then
    dm.Error('%s accepts only between %d and %d values in each subgroup.', ['XBar-Range', 2, 100], 37002);
  Inc(Count);
  if Assigned(CtrlVec[1]) then
  begin
    CtrlVec[1].AsFloat[LoopIdx] := MaxVec.AsFloat[LoopIdx] - MinVec.AsFloat[LoopIdx];
    ExcludeVec[1].AsFloat[LoopIdx] :=
      MaxVec.AsFloat[LoopIdx] - MinVec.AsFloat[LoopIdx];
  end;

  if Frozen then exit;
  XBarBar := XBarBar + YVec.AsFloat[LoopIdx];
  RBar := RBar + (MaxVec.AsFloat[LoopIdx] - MinVec.AsFloat[LoopIdx]);
end;

function TXBarR.GetCenter(LoopIndex, ChartNo: Integer): Extended;
begin
  if ChartNo = 0 then
    result := XBarBar
  else
    result := RBar;
end;

function TXBarR.GetCenterText(ChartNo: integer): string;
begin
  if ChartNo = 0 then
    Result := 'XBarBar'
  else
    Result := 'RBar';
end;

function TXBarR.GetChartTypeText(ChartNo: integer): string;
begin
  if ChartNo = 0 then
    result := 'XBar'
  else
    Result := 'Range';
end;

function TXBarR.GetCtrlText(const Dataframe: TEpiDataFrame;
  const ChartNo: integer): string;
begin
  if ChartNo = 0 then
    result := 'Average'
  else
    Result := 'Range';
end;

function TXBarR.GetCtrlVal(LoopIndex, ChartNo: Integer): Extended;
begin
  if ChartNo = 0 then
    result := YVec.AsFloat[LoopIndex]
  else
    result := CtrlVec[ChartNo].AsFloat[LoopIndex];
end;

function TXBarR.GetExcludedVector(ChartNo: Integer): TEpiVector;
begin
  if ChartNo = 0 then
    result := Df.FindVector('$EXCLUDED')
  else begin
    result := TEpiFloatVector.Create('$EXCLUDED', Df.RowCount);
    Df.Vectors.Add(Result);
  end;
end;

function TXBarR.GetExclusionVector(
  const Dataframe: TEpiDataframe): TEpiVector;
begin
  result := Dataframe.FindVector('$MEAN');
end;

function TXBarR.GetLCLInfoTxt(ChartNo: Integer): String;
begin
  result := '';
end;

function TXBarR.GetMaxVector: TEpiVector;
begin
  Result := Df.FindVector('$MAX');
end;

function TXBarR.GetMinVector: TEpiVector;
begin
  Result := Df.FindVector('$MIN');
end;

function TXBarR.GetNVector: TEpiVector;
begin
  Result := Df.FindVector('$N');
end;

function TXBarR.GetSigma(LoopIndex, ChartNo: integer): Extended;
begin
  if ChartNo = 0 then
    result := (A2[NVec.AsInteger[LoopIndex]] * RBar) / 3
  else
    result := D3_2[NVec.AsInteger[LoopIndex]] * RBar;
end;

function TXBarR.GetTimeVec(Dataframe: TEpiDataframe): TEpiVector;
begin
  result := Dataframe.FindVector(Varnames[1]);
end;

function TXBarR.GetUCLInfoTxt(ChartNo: Integer): String;
begin
  result := '';
end;

function TXBarR.GetXVector: TEpiVector;
begin
  result := df.FindVector(Varnames[1]);
end;

function TXBarR.GetYVector: TEpiVector;
begin
  result := df.FindVector('$MEAN');
end;

function TXBarR.GetZVector: TEpiVector;
begin
  result := nil;
end;

function TXBarR.MakeOutputLine(ChartNo: Integer;
  OutputTable: TStatTable): Extended;
begin
  if ChartNo = 0 then
  begin
    OutputTable.Cell[1, OutputTable.RowCount] := Format('%s - %s', [LVec.AsString[1], LVec.AsString[df.RowCount]]);
    OutputTable.Cell[2, OutputTable.RowCount] := 'XBar';
    OutputTable.Cell[3, OutputTable.RowCount] := Format('%.2f', [XBarBar]);
  end else begin
    OutputTable.Cell[1, OutputTable.RowCount] := '';
    OutputTable.Cell[2, OutputTable.RowCount] := 'Range';
    OutputTable.Cell[3, OutputTable.RowCount] := Format('%.2f', [RBar]);
  end;
end;

function TXBarR.PreAggregate(
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
  Agl.Add(TAggrMinMax.Create('$MIN', Varnames[0], true));
  Agl.Add(TAggrMinMax.Create('$MAX', Varnames[0], false));
  Result := OAggregate.AggregateDataframe(Dataframe, TStringList(AggByVars), Agl, Cmd);
end;

procedure TXBarR.PrepareVarnames(IncludeVarnames,
  MissingVarnames: TStrings);
begin
  IncludeVarnames.Add(Varnames[1]);
  IncludeVarnames.Add('$N');
  IncludeVarnames.Add('$MIN');
  IncludeVarnames.Add('$MAX');
  if (Cmd.ParamExists['XLABEL']) then
    IncludeVarnames.Add(Cmd.ParamByName['XLABEL'].AsString);

  MissingVarnames.Assign(IncludeVarnames);

  IncludeVarnames.Add('$MEAN');
  IncludeVarnames.Add('$EXCLUDED');
end;

procedure TXBarR.ResetMean;
begin
  if Frozen then exit;
  XBarBar := 0;
  RBar := 0;
  Count := 0;
end;

procedure TXBarR.SigmaResults(ChartNo, SigmaNo, BreakIndex: Integer);
begin
  //
end;

end.
 