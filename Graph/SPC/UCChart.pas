unit UCChart;

interface

uses SysUtils, UVectors, classes, ansDataTypes, UOutput, UCommands, Windows, UAnaToken,
     Chart, UChartArray, USPCBase;

type

  TCChart = class(TCustomSPCChart)
  private
    Mean: EpiFloat;
    Count: Integer;
    PerFactor: EpiFloat;
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
  public
    constructor Create; override;
    destructor Destroy; override;
  published

  end;



implementation

uses UCmdProcessor, UDebug, USPCUtils, Series, UGraph, GeneralUtils, Graphics,
 TeEngine, Math;

{ TCChart }


procedure TCChart.CalcMean;
begin
  Mean := Mean / Count;
end;

procedure TCChart.CheckVarnames;
begin
  if ((varnames.count <> 3) and (Cmd.ParamByName['XLABEL'] <> nil)) or
     ((varnames.count <> 2) and (Cmd.ParamByName['XLABEL'] = nil)) then
    dm.Error('Always %d variables for %s!', [2,'CChart'], 34001);
end;

procedure TCChart.CleanupOutput(OutputTable: TStatTable);
var
  i: integer;
begin
  if not Cmd.ParamExists['NOL'] then
  begin
    for i := 2 to OutputTable.RowCount do
      if Trim(OutputTable.Cell[3, i]) <> '' then
        break;

    if i > OutputTable.RowCount then
      OutputTable.DeleteColumn(3);
  end;
end;

constructor TCChart.Create;
begin
  inherited;

end;

procedure TCChart.CreateCharts(ChartArray: TChartArray;
  const Dataframe: TEpiDataFrame);
var
  Chart: TChart;
begin
  Chart := CreateStandardChart();
  Chart.LeftAxis.MinimumOffset := 8;
  Chart.LeftAxis.MaximumOffset := 8;
  Chart.Title.Caption := 'CChart - ' + Dataframe.VectorByName[Varnames[0]].GetVariableLabel(Cmd.ParameterList);
  if Dataframe.VectorByName[Varnames[1]].GetVariableLabel(Cmd.ParameterList)[1] = '$' then
    Chart.BottomAxis.Title.Visible := false
  else
    Chart.BottomAxis.Title.Caption := Dataframe.VectorByName[Varnames[1]].GetVariableLabel(Cmd.ParameterList);
  Chart.LeftAxis.Title.Caption := 'Count';
  ChartArray.Add(Chart, 'CChart');

  if Cmd.ParamExists['PER'] then
    PerFactor := Cmd.ParamByName['PER'].AsFloat
  else
    PerFactor := 1;
end;

function TCChart.CreateOutputTable: TStatTable;
begin
  Result := dm.OutputList.NewTable(4, 1);
  Result.TableType := sttNormal;
  Result.Cell[1,1] := 'Period:';
  Result.Cell[2,1] := 'Mean:';
  if not Cmd.ParamExists['NOL'] then
  begin
    Result.Cell[3,1] := 'LCL:';
    Result.Cell[4,1] := 'UCL:';
  end;
  if PerFactor > 1 then
    Result.Footer := Result.Footer + Format('Rate per %f', [PerFactor]);
end;

destructor TCChart.Destroy;
begin

  inherited;
end;

function TCChart.ExcludeValueFunction(index: Integer;
  df: TEpiDataFrame): Extended;
begin
  result := Df.VectorByName[Varnames[0]].AsFloat[index] * PerFactor;
end;

procedure TCChart.ExecuteMeanFail(LoopIdx, LastIdx: Integer);
begin
  //
end;

procedure TCChart.ExecuteMeanSuccess(LoopIdx, LastIdx: Integer);
begin
  Mean := Mean + YVec.AsFloat[LoopIdx];
  Inc(Count);
end;

function TCChart.GetCenter(LoopIndex, ChartNo: Integer): Extended;
begin
  Result := Mean * PerFactor;
end;

function TCChart.GetCenterText(ChartNo: integer): string;
begin
  Result := 'Mean';
end;

function TCChart.GetChartTypeText(ChartNo: integer): string;
begin
  Result := 'CChart';
end;

function TCChart.GetCtrlText(const Dataframe: TEpiDataFrame;
  const ChartNo: integer): string;
begin
  Result := 'CChart';
end;

function TCChart.GetCtrlVal(LoopIndex, ChartNo: Integer): Extended;
begin
  if YVec.IsMissing[LoopIndex] then
    Result := YVec.AsFloat[LoopIndex]
  else
    Result := YVec.AsFloat[LoopIndex] * PerFactor;
end;

function TCChart.GetLCLInfoTxt(ChartNo: Integer): String;
begin
  result := inherited GetLCLInfoTxt(ChartNo);
end;

function TCChart.GetSigma(LoopIndex, ChartNo: integer): Extended;
begin
  result := Sqrt(Mean) * PerFactor;
end;

function TCChart.GetTimeVec(Dataframe: TEpiDataframe): TEpiVector;
begin
  result := Dataframe.VectorByName[Varnames[1]];
end;

function TCChart.GetUCLInfoTxt(ChartNo: Integer): String;
begin
  result := inherited GetUCLInfoTxt(ChartNo);
end;

function TCChart.GetXVector: TEpiVector;
begin
  result := Df.VectorByName[Varnames[1]];
end;

function TCChart.GetYVector: TEpiVector;
begin
  result := Df.VectorByName[Varnames[0]];
end;

function TCChart.GetZVector: TEpiVector;
begin
  result := nil;
end;

function TCChart.MakeOutputLine(ChartNo: Integer;
  OutputTable: TStatTable): Extended;
begin
  OutputTable.Cell[1, OutputTable.RowCount] := Format('%s - %s', [LVec.AsString[1], LVec.AsString[df.RowCount]]);
  OutputTable.Cell[2, OutputTable.RowCount] := Format('%.2f', [CenterVec[ChartNo].AsFloat[1]]);
  if not Cmd.ParamExists['NOL'] then
  begin
    if Sigma3LCLVec[ChartNo].IsMissing[1] then
      OutputTable.Cell[3, OutputTable.RowCount] := ''
    else
      OutputTable.Cell[3, OutputTable.RowCount] := format('%.2f', [Sigma3LCLVec[ChartNo].AsFloat[1]]);
    OutputTable.Cell[4, OutputTable.RowCount] := Format('%.2f', [Sigma3UCLVec[ChartNo].AsFloat[1]]);
  end;
end;

procedure TCChart.PrepareVarnames(IncludeVarnames,
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

procedure TCChart.ResetMean;
begin
  Mean := 0;
  Count := 0;
end;

end.
