unit UUChart;

interface

uses SysUtils, UVectors, classes, ansDataTypes, UOutput, UCommands, Windows, UAnaToken,
     Chart, UChartArray, USPCBase;

type

  TUChart = class(TCustomSPCChart)
  private
    Mean: EpiFloat;
    Total: EpiFloat;
    PerFactor: EpiFloat;
  protected
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
    function AllowFreeze: Boolean; override;
  public
    constructor Create(); override;
    destructor Destroy(); override;
  end;

implementation

uses UCmdProcessor, UDebug, USPCUtils, Series, UGraph, GeneralUtils, Graphics,
 TeEngine, Math;

const
  UnitName = 'UGChart';

{ TUChart }

function TUChart.AllowFreeze: Boolean;
begin
  result := false;
end;

procedure TUChart.CalcMean;
begin
  Mean := Mean / Total;
end;

procedure TUChart.CheckVarnames;
begin
  if ((varnames.count <> 4) and (Cmd.ParamExists['XLABEL'])) or
     ((varnames.Count <> 3) and (not Cmd.ParamExists['XLABEL'])) then
    dm.Error('Always %d variables for %s!', [3, 'UChart'], 34001);
end;

procedure TUChart.CleanupOutput(OutputTable: TStatTable);
begin
  // No cleanup needed.
end;

constructor TUChart.Create;
begin
  inherited;

end;

procedure TUChart.CreateCharts(ChartArray: TChartArray;
  const Dataframe: TEpiDataFrame);
var
  Chart: TChart;
begin
  Chart := CreateStandardChart();
  Chart.LeftAxis.MinimumOffset := 8;
  Chart.LeftAxis.MaximumOffset := 8;
  Chart.Title.Caption := 'UChart - ' + Dataframe.VectorByName[Varnames[0]].GetVariableLabel(Cmd.ParameterList);
  ChartArray.Add(Chart, 'UChart');

  if Cmd.ParamExists['PER'] then
    PerFactor := Cmd.ParamByName['PER'].AsFloat
  else
    PerFactor := 1;
end;

function TUChart.CreateOutputTable: TStatTable;
begin
  Result := dm.OutputList.NewTable(2, 1);
  Result.TableType := sttNormal;
  Result.Cell[1,1] := 'Period:';
  Result.Cell[2,1] := 'Mean:';

  if PerFactor > 1 then
    Result.Footer := Result.Footer + Format('Rate per %f', [PerFactor]);
end;

destructor TUChart.Destroy;
begin

  inherited;
end;

function TUChart.ExcludeValueFunction(index: Integer;
  df: TEpiDataFrame): Extended;
begin
  result := PerFactor *
           (Df.VectorByName[Varnames[0]].AsFloat[index] /
            Df.VectorByName[Varnames[1]].AsFloat[index]);
end;

procedure TUChart.ExecuteMeanFail(LoopIdx, LastIdx: Integer);
begin
  CtrlVec[0].IsMissing[LoopIdx] := true;
end;

procedure TUChart.ExecuteMeanSuccess(LoopIdx, LastIdx: Integer);
begin
  if ZVec.AsFloat[LoopIdx] = 0 then
    dm.Error('Total must not be null (0)', [], 46000);
  CtrlVec[0].AsFloat[LoopIdx] :=
    YVec.AsFloat[LoopIdx] / ZVec.AsFloat[LoopIdx];
  Mean := Mean + YVec.AsFloat[LoopIdx];
  Total := Total + ZVec.AsFloat[LoopIdx];
end;

function TUChart.GetCenter(LoopIndex, ChartNo: Integer): Extended;
begin
  result := Mean * PerFactor;
end;

function TUChart.GetCenterText(ChartNo: integer): string;
begin
  Result := 'Mean';
end;

function TUChart.GetChartTypeText(ChartNo: integer): string;
begin
  Result := 'UChart';
end;

function TUChart.GetCtrlText(const Dataframe: TEpiDataFrame;
  const ChartNo: integer): string;
begin
  result := 'UChart';
end;

function TUChart.GetCtrlVal(LoopIndex, ChartNo: Integer): Extended;
begin
  if CtrlVec[0].IsMissing[LoopIndex] then
    Result := CtrlVec[0].AsFloat[LoopIndex]
  else
    Result := CtrlVec[0].AsFloat[LoopIndex] * PerFactor;
end;

function TUChart.GetLCLInfoTxt(ChartNo: Integer): String;
begin
  result := '';
end;

function TUChart.GetSigma(LoopIndex, ChartNo: integer): Extended;
begin
  result :=  Sqrt( Mean / ZVec.AsFloat[LoopIndex]) * PerFactor;
end;

function TUChart.GetTimeVec(Dataframe: TEpiDataframe): TEpiVector;
begin
  result := Dataframe.VectorByName[Varnames[2]];
end;

function TUChart.GetUCLInfoTxt(ChartNo: Integer): String;
begin
  result := '';
end;

function TUChart.GetXVector: TEpiVector;
begin
  result := Df.VectorByName[Varnames[2]];
end;

function TUChart.GetYVector: TEpiVector;
begin
  result := Df.VectorByName[Varnames[0]];
end;

function TUChart.GetZVector: TEpiVector;
begin
  result := Df.VectorByName[Varnames[1]];
end;

function TUChart.MakeOutputLine(ChartNo: Integer;
  OutputTable: TStatTable): Extended;
begin
  OutputTable.Cell[1, OutputTable.RowCount] := Format('%s - %s', [LVec.AsString[1], LVec.AsString[df.RowCount]]);
  OutputTable.Cell[2, OutputTable.RowCount] := Format('%.2f', [CenterVec[ChartNo].AsFloat[1]]);
end;

procedure TUChart.PrepareVarnames(IncludeVarnames,
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

procedure TUChart.ResetMean;
begin
  Mean := 0;
  Total := 0;
end;

procedure TUChart.SigmaResults(ChartNo, SigmaNo, BreakIndex: Integer);
begin
  // Do nothing... not even inherited.
end;

end.
