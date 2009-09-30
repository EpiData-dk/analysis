unit URunChart;

interface

uses SysUtils, UVectors, classes, ansDataTypes, UOutput, UCommands, Windows, UAnaToken,
     Chart, UChartArray, USPCBase;

type

  TRunChart = Class(TCustomSPCChart)
  private
    Mean: EpiFloat;
  protected
    procedure CalcMean; override;
    procedure CheckVarnames(); override;
    procedure CleanupOutput(OutputTable: TStatTable); override;
    procedure CreateCharts(ChartArray: TChartArray; const Dataframe: TEpiDataFrame);  override;
    function CreateOutputTable: TStatTable;  override;
    function ExcludeValueFunction(index: Integer; df: TEpiDataFrame): Extended; override;
    procedure ExecuteMeanFail(LoopIdx: Integer; LastIdx: Integer); override;
    procedure ExecuteMeanSuccess(LoopIdx: Integer; LastIdx: Integer); override;
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
    procedure PrepareVarnames(IncludeVarnames: TStrings;
      MissingVarnames: TStrings); override;
    procedure ResetMean; override;
    procedure SigmaResults(ChartNo: Integer; SigmaNo: Integer;
      BreakIndex: Integer); override;
  public
    constructor Create; override;
    destructor Destroy; override;
  end;

implementation

uses UCmdProcessor, UDebug, USPCUtils, Series, UGraph, GeneralUtils, Graphics,
 TeEngine, Math;

{ TRunChart }

procedure TRunChart.CalcMean;
begin
  Df.Sort(XVec.Name);
end;

procedure TRunChart.CheckVarnames;
begin
  if ((varnames.count <> 3) and (Cmd.ParamByName['XLABEL'] <> nil)) or
     ((varnames.count <> 2) and (Cmd.ParamByName['XLABEL'] = nil)) then
    dm.Error('Always %d variables for %s!', [2,'RunChart'], 34001);
end;

procedure TRunChart.CleanupOutput(OutputTable: TStatTable);
begin
  // Do nothing;
end;

constructor TRunChart.Create;
begin
  inherited;

end;

procedure TRunChart.CreateCharts(ChartArray: TChartArray;
  const Dataframe: TEpiDataFrame);
var
  Chart: TChart;
begin
  Chart := CreateStandardChart();
  Chart.LeftAxis.MinimumOffset := 8;
  Chart.LeftAxis.MaximumOffset := 8;
  Chart.Title.Caption := 'RunChart - ' + Dataframe.VectorByName[Varnames[0]].GetVariableLabel(Cmd.ParameterList);
  if Dataframe.VectorByName[Varnames[1]].GetVariableLabel(Cmd.ParameterList)[1] = '$' then
    Chart.BottomAxis.Title.Visible := false
  else
    Chart.BottomAxis.Title.Caption := Dataframe.VectorByName[Varnames[1]].GetVariableLabel(Cmd.ParameterList);
  Chart.LeftAxis.Title.Caption := 'Count';
  ChartArray.Add(Chart, 'IChart');
end;

function TRunChart.CreateOutputTable: TStatTable;
begin
  if Cmd.ParamExists['T'] or Cmd.ParamExists['T1'] then
    Result := dm.OutputList.NewTable(4, 1)
  else
    Result := dm.OutputList.NewTable(2, 1);
  Result.Cell[1,1] := 'Period:';
  Result.Cell[2,1] := 'Median:';
  if Cmd.ParamExists['T'] then
  begin
    Result.Cell[3,1] := 'Runs:';
    Result.Cell[4,1] := 'Limit:&nbsp;&nbsp;&nbsp;&nbsp;N';
  end;
end;

destructor TRunChart.Destroy;
begin

  inherited;
end;

function TRunChart.ExcludeValueFunction(index: Integer;
  df: TEpiDataFrame): Extended;
begin
  result := Df.VectorByName[Varnames[0]].AsFloat[Index];
end;

procedure TRunChart.ExecuteMeanFail(LoopIdx, LastIdx: Integer);
begin
  //
end;

procedure TRunChart.ExecuteMeanSuccess(LoopIdx, LastIdx: Integer);
begin
  //
end;

function TRunChart.GetCenter(LoopIndex, ChartNo: Integer): Extended;
begin
  result := Mean;
end;

function TRunChart.GetCenterText(ChartNo: integer): string;
begin
  Result := 'Median';
end;

function TRunChart.GetChartTypeText(ChartNo: integer): string;
begin
  Result := 'RunChart';
end;

function TRunChart.GetCtrlText(const Dataframe: TEpiDataFrame;
  const ChartNo: integer): string;
begin
  result := Dataframe.VectorByName[Varnames[0]].GetVariableLabel(Cmd.ParameterList);
end;

function TRunChart.GetCtrlVal(LoopIndex, ChartNo: Integer): Extended;
begin
  Result := YVec.AsFloat[LoopIndex];
end;

function TRunChart.GetLCLInfoTxt(ChartNo: Integer): String;
begin
  result := '';
end;

function TRunChart.GetSigma(LoopIndex, ChartNo: integer): Extended;
begin
  result := 0;
end;

function TRunChart.GetTimeVec(Dataframe: TEpiDataframe): TEpiVector;
begin
  result := Dataframe.VectorByName[Varnames[1]];
end;

function TRunChart.GetUCLInfoTxt(ChartNo: Integer): String;
begin
  result := '';
end;

function TRunChart.GetXVector: TEpiVector;
begin
  result := Df.VectorByName[Varnames[1]];
end;

function TRunChart.GetYVector: TEpiVector;
begin
  result := Df.VectorByName[Varnames[0]];
end;

function TRunChart.GetZVector: TEpiVector;
begin
  result := nil;
end;

function TRunChart.MakeOutputLine(ChartNo: Integer;
  OutputTable: TStatTable): Extended;
begin
  OutputTable.Cell[1, OutputTable.RowCount] := Format('%s - %s', [XVec.AsString[1], XVec.AsString[df.RowCount]]);
  OutputTable.Cell[2, OutputTable.RowCount] := Format('%.2f', [CenterVec[ChartNo].AsFloat[1]]);
  OutputTable.Cell[3, OutputTable.RowCount] := '';
  OutputTable.Cell[4, OutputTable.RowCount] := '';
end;

procedure TRunChart.PrepareVarnames(IncludeVarnames,
  MissingVarnames: TStrings);
begin
  IncludeVarnames.Add(Varnames[0]);
  IncludeVarnames.Add(Varnames[1]);
  if (Cmd.ParamExists['XLABEL']) then
    IncludeVarnames.Add(Cmd.ParamByName['XLABEL'].AsString);

  MissingVarnames.Assign(IncludeVarnames);
  MissingVarnames.Delete(0);

  IncludeVarnames.Add('$EXCLUDED');
end;

procedure TRunChart.ResetMean;
var
  Ix: Integer;
begin
  if frozen then exit;
  Df.Sort(YVec.Name);
  Ix := PercentileIndex(Df.RowCount, 0.5, Mean);
  if Mean = 0 then
    Mean := YVec.AsFloat[Ix]
  else
    Mean := YVec.AsFloat[Ix] + (YVec.AsFloat[Ix+1] - YVec.AsFloat[Ix])*(Mean);  // Bland Medical Statistics p 55
end;

procedure TRunChart.SigmaResults(ChartNo, SigmaNo, BreakIndex: Integer);
begin
  //
end;

end.
