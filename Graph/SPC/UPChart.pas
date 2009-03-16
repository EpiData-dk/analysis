unit UPChart;

interface

uses SysUtils, UVectors, classes, ansDataTypes, UOutput, UCommands, Windows, UAnaToken,
     Chart, UChartArray, USPCBase;

type

  TPChart = class(TCustomSPCChart)
  private
    // For internally available methods!
    Mean: EpiFloat;
    Count: Integer;
  protected
    //
    procedure CheckVarnames(); override;
    procedure CreateCharts(ChartArray: TChartArray; const Dataframe: TEpiDataFrame);  override;
    function CreateOutputTable: TStatTable;  override;
    function GetSigma(LoopIndex, ChartNo: integer): Extended; override;
    function GetTimeVec(Dataframe: TEpiDataframe): TEpiVector; override;
    function GetXVector: TEpiVector; override;
    function GetYVector: TEpiVector; override;
    function GetZVector: TEpiVector; override;
    procedure PrepareVarnames(IncludeVarnames: TStrings;
      MissingVarnames: TStrings); override;
    procedure ResetMean; override;
    procedure CalcMean; override;
    procedure ExecuteMeanSuccess(LoopIdx: Integer; LastIdx: Integer); override;
    procedure ExecuteMeanFail(LoopIdx: Integer; LastIdx: Integer); override;
    function GetCenter(LoopIndex: Integer; ChartNo: Integer): Extended; override;
    function ExcludeValueFunction(index: Integer; df: TEpiDataFrame): Extended; override;
    function GetCtrlVal(LoopIndex: Integer; ChartNo: Integer): Extended; override;
    function MakeOutputLine(ChartNo: Integer; OutputTable: TStatTable): Extended; override;
    function GetCenterText(ChartNo: integer): string; override;
    function GetChartTypeText(ChartNo: integer): string; override;
    function GetCtrlText(const Dataframe: TEpiDataFrame; const ChartNo: integer): string; override;
    procedure CleanupOutput(OutputTable: TStatTable); override;
    function GetLCLInfoTxt(ChartNo: Integer): String; override;
    function GetUCLInfoTxt(ChartNo: Integer): String; override;
    procedure SigmaResults(ChartNo: Integer; SigmaNo: Integer;
      BreakIndex: Integer); override;
    function AllowFreeze(SpcLine: TSPCLine): Boolean; override;
  public
    // For Externaly available methods.
    constructor Create(); override;
    destructor Destroy(); override;
//    function DoPChart(Dataframe: TEpiDataframe; Varnames: TStrings; var OutputTable: TStatTable): TChart;
  end;

implementation

uses UCmdProcessor, UDebug, USPCUtils, Series, UGraph, GeneralUtils, Graphics,
 TeEngine, Math;

const
  UnitName = 'UIChart';

// ============================================================================
// Public methodes.
// ============================================================================
constructor TPChart.Create();
begin
  inherited Create();
  //
end;

destructor TPChart.Destroy();
begin
  inherited Destroy();
  //
end;

procedure TPChart.CalcMean;
begin
  if Frozen then exit;
  Mean := Mean / Count; 
end;

procedure TPChart.CheckVarnames;
begin
  if ((varnames.count <> 4) and (Cmd.ParamExists['XLABEL'])) or
     ((varnames.Count <> 3) and (not Cmd.ParamExists['XLABEL'])) then
    dm.Error('Always %d variables for %s!', [3, 'DoPChart'], 34001);
end;

procedure TPChart.CleanupOutput(OutputTable: TStatTable);
begin

end;

procedure TPChart.CreateCharts(ChartArray: TChartArray;
  const Dataframe: TEpiDataFrame);
var
  Chart: TChart;
begin
  Chart := CreateStandardChart();
  Chart.LeftAxis.MinimumOffset := 8;
  Chart.LeftAxis.MaximumOffset := 8;
  Chart.Title.Caption := 'PChart - ' +
    Dataframe.VectorByName[Varnames[0]].GetVariableLabel(Cmd.ParameterList) + ' / ' +
    Dataframe.VectorByName[Varnames[1]].GetVariableLabel(Cmd.ParameterList);
  if Dataframe.VectorByName[Varnames[2]].GetVariableLabel(Cmd.ParameterList)[1] = '$' then
    Chart.BottomAxis.Title.Visible := false
  else
    Chart.BottomAxis.Title.Caption := Dataframe.VectorByName[Varnames[2]].GetVariableLabel(Cmd.ParameterList);
  Chart.LeftAxis.Title.Caption := 'Proportion';
  ChartArray.Add(Chart, 'PChart');
end;

function TPChart.CreateOutputTable: TStatTable;
begin
  Result := dm.OutputList.NewTable(2, 1);
  Result.TableType := sttNormal;
  Result.Cell[1,1] := 'Period:';
  Result.Cell[2,1] := 'Mean:';
end;

function TPChart.ExcludeValueFunction(index: Integer;
  df: TEpiDataFrame): Extended;
begin
  result := (df.Vectors[0].AsFloat[index] / df.Vectors[1].AsFloat[index]) * 100;
end;

procedure TPChart.ExecuteMeanFail(LoopIdx, LastIdx: Integer);
begin
  CtrlVec[0].IsMissing[LoopIdx] := true;
end;

procedure TPChart.ExecuteMeanSuccess(LoopIdx, LastIdx: Integer);
begin
  if ZVec.AsFloat[LoopIdx] = 0 then
    dm.Error('Total must not be null (0)', [], 46000);
  if Assigned(CtrlVec[0]) then
    CtrlVec[0].AsFloat[LoopIdx] := YVec.AsFloat[LoopIdx] / ZVec.AsFloat[LoopIdx];

  if Frozen then exit;
  Mean := Mean + YVec.AsFloat[LoopIdx] / ZVec.AsFloat[LoopIdx];
  Inc(Count);
end;

function TPChart.GetCenter(LoopIndex, ChartNo: Integer): Extended;
begin
  result := Mean * 100;
end;

function TPChart.GetCenterText(ChartNo: integer): string;
begin
  result := 'Mean';
end;

function TPChart.GetChartTypeText(ChartNo: integer): string;
begin
  result := 'PChart';
end;

function TPChart.GetCtrlText(const Dataframe: TEpiDataFrame;
  const ChartNo: integer): string;
begin
  result := 'P';
end;

function TPChart.GetCtrlVal(LoopIndex, ChartNo: Integer): Extended;
begin
  if CtrlVec[ChartNo].IsMissing[LoopIndex] then
    result := CtrlVec[ChartNo].AsFloat[LoopIndex]
  else
    result := CtrlVec[ChartNo].AsFloat[LoopIndex] * 100;
end;

function TPChart.GetSigma(LoopIndex, ChartNo: integer): Extended;
var
  CentVal: EpiFloat;
begin
  result := Sqrt((Mean * (1 - Mean)) / ZVec.AsFloat[LoopIndex]) * 100;
end;

function TPChart.GetTimeVec(Dataframe: TEpiDataframe): TEpiVector;
begin
  result := Dataframe.VectorByName[Varnames[2]];
end;

function TPChart.GetXVector: TEpiVector;
begin
  result := df.VectorByName[Varnames[2]];
end;

function TPChart.GetYVector: TEpiVector;
begin
  result := df.VectorByName[Varnames[0]];
end;

function TPChart.GetZVector: TEpiVector;
begin
  result := df.VectorByName[Varnames[1]];
end;

function TPChart.MakeOutputLine(ChartNo: Integer;
  OutputTable: TStatTable): Extended;
begin
  OutputTable.Cell[1, OutputTable.RowCount] := Format('%s - %s', [LVec.AsString[1], LVec.AsString[df.RowCount]]);
  OutputTable.Cell[2, OutputTable.RowCount] := Format('%.2f', [CenterVec[ChartNo].AsFloat[1]]);
end;

procedure TPChart.PrepareVarnames(IncludeVarnames,
  MissingVarnames: TStrings);
begin
  IncludeVarnames.Add(Varnames[0]);
  IncludeVarnames.Add(Varnames[1]);
  IncludeVarnames.Add(Varnames[2]);
  if (Cmd.ParamExists['XLABEL']) then
    IncludeVarnames.Add(Cmd.ParamByName['XLABEL'].AsString);

  MissingVarnames.Assign(IncludeVarnames);
  MissingVarnames.Delete(0);

  IncludeVarnames.Add('$EXCLUDED');
end;

procedure TPChart.ResetMean;
begin
  if Frozen then exit;
  Mean := 0;
  Count := 0;
end;

function TPChart.GetLCLInfoTxt(ChartNo: Integer): String;
begin
  Result := '';
end;

function TPChart.GetUCLInfoTxt(ChartNo: Integer): String;
begin
  Result := '';
end;

procedure TPChart.SigmaResults(ChartNo, SigmaNo, BreakIndex: Integer);
begin
  // Do nothing... not even inherited.
end;

function TPChart.AllowFreeze(SpcLine: TSPCLine): Boolean;
begin
  result := false;
  if SpcLine = slCenter then result := true;
end;

end.
