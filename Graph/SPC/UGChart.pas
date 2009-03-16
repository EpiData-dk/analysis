unit UGChart;

interface

uses SysUtils, UVectors, classes, ansDataTypes, UVectorOp, UOutput, UCommands, Windows, UAnaToken,
     Chart, UChartArray, USPCBase;

// ============================================================================
// Declaration of:
// Main methodes, functions and procedures.
// ============================================================================

type

  TGChart = class(TCustomSPCChart)
  private
    Mean: EpiFloat;
    Count: Integer;
  protected
    function AllowFreeze(SpcLine: TSPCLine): Boolean; override;
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
    constructor Create(); override;
    destructor Destroy(); override;
  end;

implementation

uses UCmdProcessor, UDebug, USPCUtils, Series, UGraph, GeneralUtils, Graphics, TeEngine;

const
  UnitName = 'UGChart';

// ============================================================================
// Public methodes.
// ============================================================================
constructor TGChart.Create();
begin
  inherited Create();;
  //
end;

destructor TGChart.Destroy();
begin
  inherited Destroy();
  //
end;

function TGChart.AllowFreeze(SpcLine: TSPCLine): Boolean;
begin
  Result := true;
end;

procedure TGChart.CalcMean;
begin
  if Frozen then exit;
  Mean := Mean / Count;
end;

procedure TGChart.CheckVarnames;
begin
    if ((varnames.count <> 3) and (Cmd.ParamExists['XLABEL'])) or
       ((varnames.Count <> 2) and (not Cmd.ParamExists['XLABEL'])) then
      dm.Error('Always %d variables for %s!', [2, 'DoGChart'], 34001);
end;

procedure TGChart.CleanupOutput(OutputTable: TStatTable);
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

procedure TGChart.CreateCharts(ChartArray: TChartArray;
  const Dataframe: TEpiDataFrame);
var
  Chart: TChart;
begin
  Chart := CreateStandardChart();
  Chart.LeftAxis.MinimumOffset := 8;
  Chart.LeftAxis.MaximumOffset := 8;
  Chart.Title.Caption := 'GChart - ' + Dataframe.VectorByName[Varnames[0]].GetVariableLabel(Cmd.ParameterList);
  ChartArray.Add(Chart, 'GChart');
end;

function TGChart.CreateOutputTable: TStatTable;
begin
  Result := dm.OutputList.NewTable(5, 1);
  Result.TableType := sttNormal;
  Result.Cell[1,1] := 'Period:';
  Result.Cell[2,1] := 'Rate:';
  Result.Cell[3,1] := 'Mean:';
  Result.Cell[4,1] := 'LCL:';
  Result.Cell[5,1] := 'UCL:';
end;

function TGChart.ExcludeValueFunction(index: Integer;
  df: TEpiDataFrame): Extended;
begin
  result := Df.Vectors[0].AsFloat[index];
end;

procedure TGChart.ExecuteMeanFail(LoopIdx, LastIdx: Integer);
begin
  CtrlVec[0].IsMissing[LoopIdx] := true;
end;

procedure TGChart.ExecuteMeanSuccess(LoopIdx, LastIdx: Integer);
begin
  if Assigned(CtrlVec[0]) then
    CtrlVec[0].AsFloat[LoopIdx] := 0;

  if LoopIdx = LastIdx then exit;

  if Assigned(CtrlVec[0]) then
     CtrlVec[0].AsFloat[LoopIdx] := YVec.AsFloat[LoopIdx] - YVec.AsFloat[LastIdx];

  if Frozen then exit;
  Mean := Mean + YVec.AsFloat[LoopIdx] - YVec.AsFloat[LastIdx];
  Inc(Count);
end;

function TGChart.GetCenter(LoopIndex, ChartNo: Integer): Extended;
begin
  Result := Mean;
end;

function TGChart.GetCenterText(ChartNo: integer): string;
begin
  Result := 'Mean';
end;

function TGChart.GetChartTypeText(ChartNo: integer): string;
begin
  Result := 'GChart';
end;

function TGChart.GetCtrlText(const Dataframe: TEpiDataFrame;
  const ChartNo: integer): string;
begin
  Result := 'GChart';
end;

function TGChart.GetCtrlVal(LoopIndex, ChartNo: Integer): Extended;
begin
  Result := CtrlVec[ChartNo].AsFloat[LoopIndex];
end;

function TGChart.GetLCLInfoTxt(ChartNo: Integer): String;
begin
  result := inherited GetLCLInfoTxt(ChartNo);
end;

function TGChart.GetSigma(LoopIndex, ChartNo: integer): Extended;
begin
  Result := Sqrt(Mean * (Mean + 1));
end;

function TGChart.GetTimeVec(Dataframe: TEpiDataframe): TEpiVector;
begin
  result := Dataframe.VectorByName[Varnames[1]];
end;

function TGChart.GetUCLInfoTxt(ChartNo: Integer): String;
begin
  result := inherited GetUCLInfoTxt(ChartNo);
end;

function TGChart.GetXVector: TEpiVector;
begin
  Result := df.VectorByName[VarNames[1]];
end;

function TGChart.GetYVector: TEpiVector;
begin
  Result := df.VectorByName[VarNames[0]];
end;

function TGChart.GetZVector: TEpiVector;
begin
  result := nil;
end;

function TGChart.MakeOutputLine(ChartNo: Integer;
  OutputTable: TStatTable): Extended;
begin
  OutputTable.Cell[1, OutputTable.RowCount] := Format('%s - %s', [LVec.AsString[1], LVec.AsString[df.RowCount]]);
  OutputTable.Cell[2, OutputTable.RowCount] := format('%.2f', [(1 / (Mean + 1))]);
  OutputTable.Cell[3, OutputTable.RowCount] := Format('%.2f', [Mean]);
  if Sigma3LCLVec[ChartNo].IsMissing[1] then
    OutputTable.Cell[4, OutputTable.RowCount] := ''
  else
    OutputTable.Cell[4, OutputTable.RowCount] := Format('%.2f', [Sigma3LCLVec[ChartNo].AsFloat[1]]);
  OutputTable.Cell[5, OutputTable.RowCount] := Format('%.2f', [Sigma3UCLVec[ChartNo].AsFloat[1]]);
end;

procedure TGChart.PrepareVarnames(IncludeVarnames,
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

procedure TGChart.ResetMean;
begin
  if Frozen then exit;
  Mean := 0;
  Count := 0;
end;
    
end.
