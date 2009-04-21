unit UIChart;

interface

uses SysUtils, UVectors, classes, ansDataTypes, UOutput, UCommands, Windows, UAnaToken,
     Chart, UChartArray, USPCBase;

type

  TIChart = class(TCustomSPCChart)
  private
    Mean: EpiFloat;
    MRBar: EpiFloat;
    Count: Integer;
  protected
    procedure CheckVarnames(); override;
    procedure CreateCharts(ChartArray: TChartArray; const Dataframe: TEpiDataFrame);  override;
    function CreateOutputTable: TStatTable;  override;
    function GetSigma(LoopIndex, ChartNo: integer): Extended; override;
    function GetTimeVec(Dataframe: TEpiDataframe): TEpiVector; override;
    function GetXVector: TEpiVector; override;
    function GetYVector: TEpiVector; override;
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
    function GetExcludedVector(ChartNo: Integer): TEpiVector; override;
  public
    constructor Create(); override;
    destructor Destroy(); override;
  end;

implementation

uses UCmdProcessor, UDebug, USPCUtils, Series, UGraph, GeneralUtils, Graphics,
 TeEngine, Math;

const
  UnitName = 'UIChart';

// ============================================================================
// Public methodes.
// ============================================================================
procedure TIChart.CalcMean;
begin
  if Frozen then exit;
  Mean := Mean / Count;
  MRBar := MRBar / (Count - 1);
end;

procedure TIChart.CheckVarnames();
begin
  if ((varnames.count <> 3) and (Cmd.ParamByName['XLABEL'] <> nil)) or
     ((varnames.count <> 2) and (Cmd.ParamByName['XLABEL'] = nil)) then
    dm.Error('Always %d variables for %s!', [2,'IChart'], 34001);
end;

constructor TIChart.Create();
begin
  inherited Create();;
  //
end;

procedure TIChart.CreateCharts(ChartArray: TChartArray; const Dataframe: TEpiDataFrame);
var
  Chart: TChart;
begin
  Chart := CreateStandardChart();
  Chart.LeftAxis.MinimumOffset := 8;
  Chart.LeftAxis.MaximumOffset := 8;
  Chart.Title.Caption := 'IChart - ' + Dataframe.VectorByName[Varnames[0]].GetVariableLabel(Cmd.ParameterList);
  if Dataframe.VectorByName[Varnames[1]].GetVariableLabel(Cmd.ParameterList)[1] = '$' then
    Chart.BottomAxis.Title.Visible := false
  else
    Chart.BottomAxis.Title.Caption := Dataframe.VectorByName[Varnames[1]].GetVariableLabel(Cmd.ParameterList);
  Chart.LeftAxis.Title.Caption := 'Measument';
  ChartArray.Add(Chart, 'IChart');

  if Cmd.ParamExists['MR'] then
  begin
//    Chart.BottomAxis.Labels := false;
    Chart.BottomAxis.Title.Visible := false;
    Chart.Foot.Visible := false;
    Chart := CreateStandardChart();
    Chart.LeftAxis.MinimumOffset := 8;
    Chart.LeftAxis.MaximumOffset := 8;
    Chart.LeftAxis.Title.Caption := 'Moving Range';
    if Dataframe.VectorByName[Varnames[1]].GetVariableLabel(Cmd.ParameterList)[1] = '$' then
      Chart.BottomAxis.Title.Visible := false
    else
      Chart.BottomAxis.Title.Caption := Dataframe.VectorByName[Varnames[1]].GetVariableLabel(Cmd.ParameterList);
    ChartArray.Add(Chart, 'Moving Range');
  end;
end;

function TIChart.CreateOutputTable: TStatTable;
begin
  Result := dm.OutputList.NewTable(5, 1);
  Result.Cell[1,1] := 'Period:';
  Result.Cell[2,1] := 'Chart:';
  Result.Cell[3,1] := 'Mean:';
  if not Cmd.ParamExists['NOL'] then
  begin
    Result.Cell[4,1] := 'LCL:';
    Result.Cell[5,1] := 'UCL:';
  end;
end;

destructor TIChart.Destroy();
begin
  inherited Destroy();
  //
end;

procedure TIChart.ExecuteMeanSuccess(LoopIdx, LastIdx: Integer);
begin
  if Frozen then exit;

  Mean := Mean + YVec.AsFloat[LoopIdx];
  Inc(Count);

  if LoopIdx = LastIdx then exit;
  MRBar := MRBar + Abs(YVec.AsFloat[LoopIdx] - YVec.AsFloat[LastIdx]);
  if (Length(CtrlVec) = 2) and (Assigned(CtrlVec[1])) then
  begin
    CtrlVec[1].AsFloat[LoopIdx] := Abs(YVec.AsFloat[LoopIdx] - YVec.AsFloat[LastIdx]);
    ExcludeVec[1].AsFloat[LoopIdx] := Abs(ExcludeVec[0].AsFloat[LoopIdx] - ExcludeVec[0].AsFloat[LoopIdx - 1]);
  end;
end;

procedure TIChart.ExecuteMeanFail(LoopIdx, LastIdx: Integer);
begin
  if Frozen then exit;

  if (Length(CtrlVec) = 2) and (Assigned(CtrlVec[1])) then
  begin
    CtrlVec[1].IsMissing[LoopIdx] := True;
    ExcludeVec[1].AsFloat[LoopIdx] := Abs(ExcludeVec[0].AsFloat[LoopIdx] - ExcludeVec[0].AsFloat[LoopIdx - 1]);
  end;
end;

function TIChart.GetCenter(LoopIndex, ChartNo: Integer): Extended;
begin
  if ChartNo = 0 then
    Result := Mean
  else
    Result := MRbar;
end;

function TIChart.GetSigma(LoopIndex, ChartNo: integer): Extended;
begin
  if ChartNo = 0 then
    Result := (2.66/3) * MRBar
  else
    Result := 0.7557 * MRBar; // Old style: 1.09; New value taken from D3_2 constants.
end;

function TIChart.GetTimeVec(Dataframe: TEpiDataframe): TEpiVector;
begin
  result := Dataframe.VectorByName[Varnames[1]];
end;

function TIChart.GetXVector: TEpiVector;
begin
  Result := df.VectorByName[Varnames[1]];
end;

function TIChart.GetYVector: TEpiVector;
begin
  Result := df.VectorByName[Varnames[0]];
end;

procedure TIChart.PrepareVarnames(IncludeVarnames,
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

procedure TIChart.ResetMean;
begin
  if Frozen then exit;
  Mean := 0;
  MRBar := 0;
  Count := 0;
end;

function TIChart.ExcludeValueFunction(index: Integer;
  df: TEpiDataFrame): Extended;
begin
  result := Df.Vectors[0].AsFloat[index];
end;

function TIChart.GetCtrlVal(LoopIndex, ChartNo: Integer): Extended;
begin
  if ChartNo = 0 then
    Result := YVec.AsFloat[LoopIndex]
  else
    result := CtrlVec[1].AsFloat[LoopIndex];
end;

function TIChart.MakeOutputLine(ChartNo: Integer; OutputTable: TStatTable): Extended;
begin
  if ChartNo = 0 then
  begin
    OutputTable.Cell[1, OutputTable.RowCount] := Format('%s - %s', [LVec.AsString[1], LVec.AsString[df.RowCount]]);
    OutputTable.Cell[2, OutputTable.RowCount] := 'IChart';
  end else begin
    OutputTable.Cell[1, OutputTable.RowCount] := '';
    OutputTable.Cell[2, OutputTable.RowCount] := 'MR';
  end;
  OutputTable.Cell[3, OutputTable.RowCount] := Format('%.2f', [CenterVec[ChartNo].AsFloat[1]]);
  if not Cmd.ParamExists['NOL'] then
  begin
    if Sigma3LCLVec[ChartNo].IsMissing[1] then
      OutputTable.Cell[4, OutputTable.RowCount] := ''
    else
      OutputTable.Cell[4, OutputTable.RowCount] := format('%.2f', [Sigma3LCLVec[ChartNo].AsFloat[1]]);
    OutputTable.Cell[5, OutputTable.RowCount] := format('%.2f', [Sigma3UCLVec[ChartNo].AsFloat[1]]);
  end;
end;

function TIChart.GetCenterText(ChartNo: integer): string;
begin
  if ChartNo = 0 then
    result := 'Mean'
  else
    result := 'MRBar';
end;

function TIChart.GetChartTypeText(ChartNo: integer): string;
begin
  if ChartNo = 0 then
    result := 'IChart'
  else
    result := 'MRChart';
end;

procedure TIChart.CleanupOutput(OutputTable: TStatTable);
var
  i: integer;
begin
  if not Cmd.ParamExists['NOL'] then
  begin
    for i := 2 to OutputTable.RowCount do
      if Trim(OutputTable.Cell[4, i]) <> '' then
        break;

    if i > OutputTable.RowCount then
      OutputTable.DeleteColumn(4);
  end;

  if not Cmd.ParamExists['MR'] then
    OutputTable.DeleteColumn(2);
end;

function TIChart.GetCtrlText(const Dataframe: TEpiDataFrame; const ChartNo: integer): string;
begin
  if ChartNo = 0 then
    result := Dataframe.VectorByName[Varnames[0]].GetVariableLabel(Cmd.ParameterList)
  else
    result := 'Moving Range';
end;

function TIChart.GetExcludedVector(ChartNo: Integer): TEpiVector;
begin
  if ChartNo = 0 then
    result := Df.FindVector('$EXCLUDED')
  else begin
    result := TEpiFloatVector.Create('$EXCLUDED', Df.RowCount);
    Df.Vectors.Add(Result);
  end;
end;

end.
