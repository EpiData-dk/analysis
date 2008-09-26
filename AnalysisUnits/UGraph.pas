unit UGraph;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, TeeEdit, TeeProcs, TeEngine, Chart, ComCtrls, StdCtrls, ExtCtrls,
  UVectors, TeeBoxplot, TeePng, TeeTools, TeeShape, StatChar, Series, UCommands,
  UEpiDatatypes, UOutput, UVariables, ansDatatypes, ActnList;

type
  TSeriesColorFunction = function(index: integer): TColor;

  TGraphForm = class(TForm)
    Panel1: TPanel;
    Edit1: TEdit;
    Button6: TButton;
    Button5: TButton;
    Button2: TButton;
    Button3: TButton;
    Button8: TButton;
    Button9: TButton;
    Panel2: TPanel;
    Panel3: TPanel;
    Button1: TButton;
    Button4: TButton;
    Button11: TButton;
    Button7: TButton;
    Button10: TButton;
    GraphPG: TPageControl;
    ChartEditor1: TChartEditor;
    ActionList1: TActionList;
    AcClose: TAction;
    procedure Button7Click(Sender: TObject);
    procedure GraphPGChange(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure Button10Click(Sender: TObject);
    procedure Edit1Change(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure Button8Click(Sender: TObject);
    procedure Button6Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button9Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button11Click(Sender: TObject);
    procedure AcCloseExecute(Sender: TObject);
  private
    { Private declarations }
    EditChanged: boolean;
    function GetActiveChart(): TChart;
    procedure Reset();
  public
    { Public declarations }
  end;

  TArrayVariant = array of variant;
  TExcludeFunction = function(index: integer; df: TEpiDataframe): EpiFloat;
  TGraphCallback = procedure(const chart: TChart);

  TGraph = class(TObject)
  private
    dummyform: TForm;
    GraphCallBack: TGraphCallback;

    // Axis labeling procedure...
    procedure BottomDateLabeling(Sender: TChartAxis; Series: TChartSeries; ValueIndex: Integer; Var LabelText: String);

    function CreateStandardChart(): TChart;
    procedure CommonChartOptions(Chart: TChart; Cmd: TCommand);
    procedure BlackAndWhiteChart(Chart: TChart);
    function AxisValue(Axis: TChartAxis; Imin, Imax: IValue; IsX: boolean): boolean;
    procedure AxisIncrement(Axis: TChartAxis; Value: extended);
    procedure AxisLines(Chart: TChart; Axis: string; Cmd: TCommand);
    function TextBox(Chart: TChart; cmd: TCommand): boolean;
    procedure ShowChart(Chart: TChart; Cmd: TCommand);
    function MultiVarTitle(Varlist: TEpiVectors; Parameters: TVarList): string;
    function FindBreak(Parameters: TVarList; XVector: TEpiVector): TArrayVariant;
    function ExcludeInDataframe(var dF: TEpiDataframe; Parameters: TVarList; ExcludeFunc: TExcludeFunction): boolean;
    procedure SPCTest(dataframe: TEpiDataframe; Const CountName: String;CmdID: Word;
                      Parameters: TVarlist; LowIndex, HighIndex: integer;
                      idx: integer; var output: TStatTable);
    procedure CalcXInc(Chart: TChart);
    procedure OutputNCases(const chart: TChart; N : EpiInt);

    function GraphTable(XVar: TEpiVector; YVars: TEpiVectors): TStatTable;
    // Put simple series functions here: (eg. barseries)
    function BarSeries(DataVector, LabelVector: TEpiVector; ColorFunc: TSeriesColorFunction): TChartSeries; overload;
    function BarSeries(RangeVector, DataVector, LabelVector: TEpiVector; ColorFunc: TSeriesColorFunction): TChartSeries; overload;
    //function HistogramSeries(DataVector, LabelVector: TEpiVector; ColorFunc: TSeriesColorFunction): TChartSeries;
    function HistogramSeries(XDataVector, YDataVector, LabelVector: TEpiVector; ColorFunc: TSeriesColorFunction): TChartSeries;

    function BoxSeries(DataVector: TEpiVector): TBoxSeries;
    function ScatterSeries(XVector, YVector, LabelVector: TEpiVector): TPointSeries;
    function PointSeries(XVector, YVector, LabelVector: TEpiVector): TPointSeries;
    function LineSeries(XVector, YVector, LabelVector: TEpiVector): TLineSeries;
    function PieSeries(XVector, LabelVector: TEpiVector): TPieSeries;

  public
    destructor Destroy(); override;

    // SIMPLE GRAPH FOR USE IN MORE COMPLEX GRAPH_OUTPUTS
    function DoBars(Dataframe: TEpiDataframe; Varnames: TStrings; CmdID: Word; Parameters: TVarList): TChart;
    function DoHistogram(Dataframe: TEpiDataframe; Varnames: TStrings; CmdID: Word; Parameters: TVarList): TChart;
    function DoBoxPlot(Dataframe: TEpiDataframe; Varnames: TStrings; CmdID: Word; Parameters: TVarList;
                       var OutputTable: TStatTable): TChart;
    function DoScatter(Dataframe: TEpiDataframe; Varnames: TStrings; CmdID: Word; Parameters: TVarList): TChart;
    function DoDotPlot(Dataframe: TEpiDataframe; Varnames: TStrings; CmdID: Word; Parameters: TVarList): TChart;
    function DoPie(Dataframe: TEpiDataframe; Varnames: TStrings; CmdID: Word; Parameters: TVarList): TChart;
    function DoEpiCurve(Dataframe: TEpiDataframe; Varnames: TStrings; CmdID: Word; Parameters: TVarList;
                        var OutputTable: TStatTable; var footnote:string): TChart;
    function DoCDFPlot(Dataframe: TEpiDataframe; Varnames: TStrings; CmdID: Word; Parameters: TVarList;
                       var OutputTable: TStatTable): TChart;
    function DoCIPlot(Dataframe: TEpiDataframe; Varnames: TStrings; CmdID: Word; Parameters: TVarList;
                        var OutputTable: TStatTable): TChart;
    function DoIchart(Dataframe: TEpiDataframe; Varnames: TStrings; CmdID: Word; Parameters: TVarList;
                      var SPCDataframe: TEpiDataframe; var OutputTable: TStatTable; var footnote:string): TChart;
    function DoPchart(Dataframe: TEpiDataframe; Varnames: TStrings; CmdID: Word; Parameters: TVarList;
                      var SPCDataframe: TEpiDataframe; var OutputTable: TStatTable; var footnote:string): TChart;
    function DoPareto(Dataframe: TEpiDataframe; Varnames: TStrings; CmdID: Word; Parameters: TVarList): TChart;
    function DoLifeTable(Dataframe: TEpiDataframe; Varnames: TStrings; CmdID: Word; Parameters: TVarList): TChart;

    // COMPLEX/MULTI GRAPHS - SHOULD PREFERABLY USE SIMPLE GRAPHS FOR CONSTRUCTION.
    function DoXChart(Dataframe: TEpiDataframe; Varnames: TStrings; Cmd: TCommand): boolean;

    // EXTRA FUNCTIONALITY.
    procedure ShowCharts(Charts: array of TChart; Cmd: TCommand);
    function SaveChart(Chart: TChart; Cmd: TCommand): string;
    procedure DoGraphs(Dataframe: TEpiDataFrame; Varnames: TStrings; Cmd: TCommand);
  end;

var
  OGraph: TGraph;

implementation

{$R *.dfm}
uses
  UAggregate, SMUtils, UCmdProcessor, Math, UAnaToken, UDebug, pngimage,
  cFileUtils,  uDateUtils, UTables,UStatFunctions, TeCanvas, CandleCh, UTranslation;

var
  GraphForm: TGraphForm;

const
  UnitName = 'UGraph';

{****************************
*  TSeriesColorFunctions    *
*****************************}

function GetGraphColour(index: integer): TColor;
var
  opt: TEpiOption;
const
  colorlist: array[0..9] of TColor =
  (clRed, clBlue, clBlack,clGreen, clYellow, clWhite, clSkyBlue, clFuchsia,  clGray , clAqua);
  //Red,  Blue,   Black,   Green,  Yellow,    White,   SkyBlue,  Fuchsia,    Gray ,   Aqua;
begin
  result := colorlist[0];
  if dm.GetOptionValue('GRAPH COLOUR',opt) then
  try
    if length(opt.Value) < 20 then
      dm.SetOptionValue('GRAPH COLOUR', opt.value + copy('01234567890123456789',1,20-length(opt.value)));
    index := StrToInt(opt.Value[(index mod 20)+1]);
    result := colorlist[index];
  except
    result := colorlist[1];
  end;
end;

function GetGraphPointerStyle(index: integer): TSeriesPointerStyle;
var
  opt: TEpiOption;
const
  stylelist: array[0..9] of TSeriesPointerStyle =
  (psCircle, psTriangle, psDownTriangle, psLeftTriangle, psRightTriangle, psRectangle, psSmallDot,
  psDiagCross, psStar,psCross);

begin
  result := stylelist[0];
  if dm.GetOptionValue('GRAPH SYMBOL',opt) then
  begin
      try
        if length(opt.Value) < 20 then
           dm.SetOptionValue('GRAPH SYMBOL', opt.value + copy('01234567890123456789',1,20-length(opt.value)));
        index := StrToInt(opt.Value[(index mod 20)+1]);
        result := stylelist[index];
      except
        result := stylelist[1];
      end;
  end;
end;

function GetGraphBrushStyle(index: integer): TBrushStyle;
var
  opt: TEpiOption;
const
  brushlist: array[0..1] of TBrushStyle =
    (bsClear, bsSolid);
begin
  result := brushlist[0];
  if dm.GetOptionValue('GRAPH SYMBOL FILLED',opt) then
  begin
      try
        if length(opt.Value) < 20 then
           dm.SetOptionValue('GRAPH SYMBOL FILLED', opt.value + copy('01010101010101010101',1,20-length(opt.value)));
        index := StrToInt(opt.Value[(index mod 20)+1]);
        result := brushlist[Min(1, index)];
      except
        result := bsSolid;
      end;
  end;
end;

function GetGraphTextColour(index: integer): TColor;
var
  opt: TEpiOption;
  val: integer;
const
  colorlist: array[0..9] of TColor =
  (clRed, clBlue, clBlack, clGreen, clWhite, clYellow, clSkyBlue, clFuchsia,  clGray , clAqua);
begin
  result := colorlist[0];
  if dm.GetOptionValue('GRAPH COLOUR TEXT',opt) then
  try
    if length(opt.Value) < 9 then
      dm.SetOptionValue('GRAPH COLOUR TEXT', '213333333');
    val := StrToInt(opt.Value[index])-1;
    result := colorlist[val];
  except
    result := 3; //black
  end;
end;

function GetHisColor(index: integer): TColor;
begin
  result := clTeeColor;
end;

function R2(const AValue : extended ; const ADigit : TRoundToRange):extended ;
var
  X : extended;
  i : integer ;
begin
  X := 1.0 ;
  for i := 1 to Abs(ADigit) do X := X * 10 ;
  if ADigit<0
    then Result := Round(AValue * X) / X
    else Result := Round(AValue / X) * X;
end;

{********************
*      TGraph       *
*********************}

destructor TGraph.Destroy();
begin
  if Assigned(dummyform) then FreeAndNil(dummyform);
end;

procedure TGraph.DoGraphs(Dataframe: TEpiDataFrame; Varnames: TStrings; Cmd: TCommand);
var
  Chart: TChart;
  Opt:   TEpiOption;
  footnote:  string;
  xtab: TStatTable;
  vectorlist: TEpiVectors;
  vectors: TStringList;
  DummyFrame: TEpiDataframe;
const
  procname = 'DoGraphs';
  procversion = '1.0.0.0';
begin
  ODebug.Add(UnitName + ':' + procname + ' - ' + procversion, 1);
  GraphCallBack := nil;
  dummyform := nil;
  Chart := nil;
  footnote := '';

  try
    case cmd.CommandID of
      opHistogram,
      opShortHistogram:      chart := DoHistogram(Dataframe, Varnames, Cmd.CommandID, Cmd.ParameterList);
      opBar:                 chart := DoBars(Dataframe, Varnames, Cmd.CommandID, Cmd.ParameterList);
      opBox, opBoxPlot:      chart := DoBoxPlot(Dataframe, Varnames, Cmd.CommandID, Cmd.ParameterList, xtab);
      opLine,
      opScatter,
      opShortScatter:        chart := DoScatter(Dataframe, Varnames, Cmd.CommandID, Cmd.ParameterList);
      opPie:                 chart := DoPie(Dataframe, Varnames, Cmd.CommandID, Cmd.ParameterList);
      opEpiCurve:            chart := DoEpiCurve(Dataframe, Varnames, Cmd.CommandID, Cmd.ParameterList,
                                                 xtab,footnote);
      opCDFPlot:             chart := DoCDFPlot(Dataframe, Varnames, Cmd.CommandID, Cmd.ParameterList, xtab);
      opCIPlot:              chart := DoCIPlot(Dataframe, Varnames, Cmd.CommandID, Cmd.ParameterList, xtab);
      opDotPlot:             chart := DoDotPlot(Dataframe, Varnames, Cmd.CommandID, Cmd.ParameterList);

      // TODO : Graph table works, but not optimized. Think of a better generic solution for graphtables.
      opIChart,
      opRunChart:            chart := DoIChart(Dataframe, Varnames, Cmd.CommandID, Cmd.ParameterList,
                                               DummyFrame, xtab, footnote);
      opPChart:              chart := DoPchart(Dataframe, Varnames, Cmd.CommandID, Cmd.ParameterList,
                                               DummyFrame, xtab, footnote);
      opPareto:              chart := DoPareto(Dataframe, Varnames, Cmd.CommandID, Cmd.ParameterList);
      opLifeTable,
      opShortLifeTable:      chart := DoLifeTable(DataFrame, Varnames, Cmd.CommandID, Cmd.ParameterList);

    else
      dm.Error('Command not in implemented in DoGraph', [], 113001);
    end;
    if (Chart = nil) then raise Exception.Create('TChart not initialized.');
    if Assigned(xtab) and (not cmd.ParamExists['Q']) then
    begin
      xtab.TableType := sttNormal;
      dm.CodeMaker.OutputTable(xtab,footnote);
      dm.Sendoutput;
    end;
    CommonChartOptions(Chart, Cmd);

    if (Cmd.ParambyName['N'] <> nil) then
      OutputNCases(Chart, dataframe.RowCount);  // add n= at this level CommonChartOptions no access to dataframe

    if Assigned(GraphCallBack) then
      GraphCallBack(Chart);

    if (cmd.ParamExists['EDIT']) then ShowChart(chart, cmd);

    footnote := SaveChart(chart, cmd);
    if (not (cmd.ParamExists['Q'] or Cmd.ParamExists['NG'])) and
       (AnsiCompareText(ExtractFileExt(footnote),'.png')=0) then
    begin
      xtab := dm.OutputList.NewTable(1,1);
      xtab.TableType := sttGraph;
      xtab.Cell[1,1] := '<img src="'+ footnote +'" ALT="' + footnote + '">';
      if ((dm.GetOptionValue('GRAPH FILENAME SHOW', Opt) and (Opt.Value = 'ON'))) then
        dm.CodeMaker.OutputTable(xtab,footnote)
      else
        dm.CodeMaker.OutputTable(xtab,'');

      if (Cmd.ParamExists['TAB']) and (Cmd.CommandID in [opPchart,opIChart,opXChart,opRunChart]) then
      begin
        vectors := TStringList.Create;
        vectors.Add(varnames[1]);
        if Cmd.CommandID = opPchart then
          vectors.Add(varnames[2]);
        vectorlist := dataframe.GetVectorListByName(vectors);
        dm.CodeMaker.OutputTable(GraphTable(Dataframe.VectorByName[varnames[0]], vectorlist), 'GraphSubTables');
      end;
      dm.Sendoutput;
    end;
  finally
    if Assigned(DummyForm) then FreeAndNil(DummyForm);
    if Assigned(DummyFrame) then FreeAndNil(DummyFrame);
  end;
end;

function TGraph.CreateStandardChart: TChart;
var
  Opt: TEpiOption;
const
  procname = 'CreateStandardChart';
  procversion = '1.0.0.0';
begin
  ODebug.Add(UnitName + ':' + procname + ' - ' + procversion, 1);
  if assigned(dummyform) then FreeAndNil(dummyform);
  if dummyform = nil then
  begin
    Dummyform := TForm.CreateNew(Application);
    dm.GetOptionValue('GRAPH SIZE Y', opt);
    Dummyform.Height := StrToInt(opt.Value);
    dm.GetOptionValue('GRAPH SIZE X', opt);
    Dummyform.Width := StrToInt(opt.Value);
    dummyform.Name := 'Dummyform';
  end;
  result := TChart.Create(dummyform);
  result.Parent := dummyform;
  result.Name := 'StandardChart';
  with result do
  begin
    // General for the form
    Align := alClient;
    AllowPanning := pmNone;
    View3D := False;
    Zoom.Allow := False;
    Color := clWhite;
    TabOrder := 0;
    AutoSize := True;
    BevelOuter := bvNone;

    // Backwall
    BackWall.Brush.Color := clWhite;
    BackWall.Brush.Style := bsClear;
    BackWall.Color := clWindow;
    BackWall.Pen.Visible := False;

    // Legend
    Legend.LegendStyle := lsSeries;
    Legend.TopPos := 5;
    Legend.Symbol.Width := 20;
    Legend.Symbol.WidthUnits := lcspixels;
    Legend.Visible := False;

    // Margins
    MarginBottom := 5;

    // Titles, footers, etc.
    SubFoot.Visible := False;
    Title.Font.Height := -16;

    Frame.Visible := False;

    // Axis
    BottomAxis.Grid.Visible := False;
    BottomAxis.MinorTickCount := 0;
    BottomAxis.TickOnLabelsOnly := false;
    BottomAxis.MinimumOffset := 1;
    BottomAxis.MaximumOffset := 5;         // add a small fraction at left end of x axis

    LeftAxis.Grid.Visible := False;
    LeftAxis.MinorTickCount := 0;
    LeftAxis.TickOnLabelsOnly := false;
    Leftaxis.MaximumOffset := 5;         // add a small fraction at top of left axis (to see e.g. top of bars)

    RightAxis.Grid.Visible := False;
    RightAxis.MinorTickCount := 0;
    RightAxis.TickOnLabelsOnly := false;
    Rightaxis.MaximumOffset := 5;         // add a small fraction at top of left axis (to see e.g. top of bars)
  end;
  result.Draw;
end;

procedure TGraph.BottomDateLabeling(Sender: TChartAxis; Series: TChartSeries; ValueIndex: Integer; Var LabelText: String);
var
  val: integer;
begin
  if Sender = Sender.ParentChart.BottomAxis then
    if TryStrToInt(LabelText, val) then
      LabelText := EpiDateToStr(StrToInt(LabelText), dfDMY);
end;

procedure TGraph.CalcXInc(Chart: TChart);
var
  increment, min, max: double;
  factor: integer;
  Axis: TChartAxis;
begin
  Axis := Chart.BottomAxis;
  if Axis.Automatic then
  begin
    // Force calculation of min and max!
    Axis.AdjustMaxMin;
    min := Axis.Minimum;
    max := Axis.Maximum;
    Axis.Automatic := false;
    Axis.SetMinMax(min, max);
  end;
  Axis.LabelStyle := talValue;
  Axis.LabelsSeparation := 1;
  increment := Axis.CalcIncrement;
  factor := 1;
  if (increment < 1) then
  begin
    factor := 0;
    while (increment < 1) do
    begin
      increment := increment * 10;
      dec(factor);
    end;
  end else begin
    factor := 0;
    while (increment > 10) do
    begin
      increment := increment / 10;
      inc(factor);
    end;
  end;
  if (increment <= 1) then
    Axis.Increment := 1 * IntPower(10, factor)
  else if (increment <= 2) then
    Axis.Increment := 2 * IntPower(10, factor)
  else
    Axis.Increment := 5 * IntPower(10, factor);
end;


procedure Tgraph.OutputNCases(const chart: TChart; N : EpiInt);
var
  tool: TAnnotationTool;
begin
  tool := TAnnotationTool.Create(chart);
  chart.Tools.Add(tool);
  tool.Text := 'N=' + inttostr(N);
  tool.Shape.CustomPosition := true;
  tool.Shape.Left := 15;
  tool.Shape.Top := 15;
  tool.Shape.Shadow.VertSize := 0;
  tool.Shape.Shadow.HorizSize := 0;
  tool.Shape.Frame.Visible := false;
  dm.AddResult('$Count', EpiTyInteger, n, 5, 0);
end;

function TGRaph.GraphTable(XVar: TEpiVector; YVars: TEpiVectors): TStatTable;
var
  cols, i, j: integer;
const
  procname = 'GraphTable';
  procversion = '1.0.0.0';
begin
  ODebug.IncIndent;
  ODebug.Add(UnitName + ':' + procname + ' - ' + procversion, 1);
  cols := YVars.DataFrame.RowCount;
  result := dm.CodeMaker.Output.NewTable(cols+1, YVars.Count+1);
  result.Cell[1,1] := XVar.Name;
  for i := 2 to YVars.Count+1 do
    result.Cell[1,i] := YVars[i-2].Name;
  for i := 2 to cols+1 do
  begin
    result.Cell[i,1] := XVar.AsString[i-1];
    for j := 2 to YVars.Count+1 do
      result.Cell[i,j] := YVars[j-2].AsString[i-1];
  end;
  ODebug.DecIndent;
end;

function TGraph.BarSeries(DataVector, LabelVector: TEpiVector; ColorFunc: TSeriesColorFunction): TChartSeries;
var
  i: integer;
  vec: TEpiIntVector;
const
  procname = 'BarSeriesA';
  procversion = '1.0.0.0';
begin
  ODebug.Add(UnitName + ':' + procname + ' - ' + procversion, 1);

  vec := TEpiIntVector.Create('$S', DataVector.Length);
  for i := 1 to DataVector.Length do
    vec.AsInteger[i] := i;
  result := BarSeries(vec, Datavector, LabelVector, ColorFunc);
end;

function TGraph.BarSeries(RangeVector, DataVector, LabelVector: TEpiVector; ColorFunc: TSeriesColorFunction): TChartSeries;
var
  i: integer;
const
  procname = 'BarSeriesB';
  procversion = '1.0.0.0';
begin
  ODebug.Add(UnitName + ':' + procname + ' - ' + procversion, 1);
  result := TBarSeries.Create(nil);
//  result := THistogramSeries.Create(nil);
  for i := 1 to DataVector.Length do
  begin
    if (RangeVector.IsMissing[i]) then
    begin
      result.AddXY(RangeVector.AsFloat[i-1]+1, DataVector.AsFloat[i],
                   LabelVector.GetValueLabel(LabelVector.AsString[i]),
                   ColorFunc(i-1))
    end else
      result.AddXY(RangeVector.AsFloat[i], DataVector.AsFloat[i],
                   LabelVector.GetValueLabel(LabelVector.AsString[i]),
                   ColorFunc(i-1));
  end;
end;

function TGraph.HistogramSeries(XDataVector,YDataVector, LabelVector: TEpiVector; ColorFunc: TSeriesColorFunction): TChartSeries;
var
  i: integer;
const
  procname = 'HistogramSeries';
  procversion = '1.0.0.0';
begin
  ODebug.Add(UnitName + ':' + procname + ' - ' + procversion, 1);
  result := TAreaSeries.create(nil);
//  result := TAreaSeries.Create(nil);
  //result.Stairs := True;

  for i := 1 to XDataVector.Length do
  begin
    result.addxy(XDataVector.Asfloat[i],YDataVector.AsFloat[i],
               LabelVector.GetValueLabel(LabelVector.AsString[i]),
               ColorFunc(i-1));
  end;
end;

{
function TGraph.HistogramSeries(DataVector, LabelVector: TEpiVector; ColorFunc: TSeriesColorFunction): TChartSeries;
var
  i: integer;
const
  procname = 'HistogramSeries';
  procversion = '1.0.0.0';
begin
  ODebug.Add(UnitName + ':' + procname + ' - ' + procversion, 1);
  result := THistogramSeries.create(nil);
  for i := 1 to DataVector.Length do
  begin
    result.add(DataVector.AsFloat[i],
               LabelVector.GetValueLabel(LabelVector.AsString[i]),
               ColorFunc(i-1));
  end;
end;
}
function TGraph.BoxSeries(DataVector: TEpiVector): TBoxSeries;
var
  i: integer;
const
  procname = 'BoxSeries';
  procversion = '1.0.0.0';
begin
  ODebug.Add(UnitName + ':' + procname + ' - ' + procversion, 1);
  result := TBoxSeries.Create(nil);
  for i := 1 to DataVector.Length do
    result.AddY(DataVector.AsFloat[i]);
end;

function TGraph.ScatterSeries(XVector, YVector, LabelVector: TEpiVector): TPointSeries;
var
  i: cardinal;
  dummy: Double;
const
  procname = 'ScatterSeries';
  procversion = '1.0.0.0';
begin
  ODebug.Add(UnitName + ':' + procname + ' - ' + procversion, 1);
  result := TPointSeries.Create(nil);
  // Find a y-coord worth using!
  i := 1;
  while (i<=XVector.Length) and (YVector.IsMissing[i]) do inc(i);
  dummy := YVector.AsFloat[i];

  for i := 1 to XVector.Length do
    if YVector.IsMissing[i] then
      result.AddNullXY(XVector.AsFloat[i], dummy)
    else
      result.AddXY(XVector.AsFloat[i], YVector.AsFloat[i], LabelVector.AsString[i]);
end;

function TGraph.PointSeries(XVector, YVector, LabelVector: TEpiVector): TPointSeries;
var
  i: cardinal;
const
  procname = 'PointSeries';
  procversion = '1.0.0.0';
begin
  ODebug.Add(UnitName + ':' + procname + ' - ' + procversion, 1);
  result := TPointSeries.Create(nil);

  for i := 1 to XVector.Length do
    if not YVector.IsMissing[i] then
      result.AddXY(XVector.AsFloat[i], YVector.AsFloat[i], LabelVector.AsString[i]);
end;

function TGraph.LineSeries(XVector, YVector, LabelVector: TEpiVector): TLineSeries;
var
  i: cardinal;
  dummy: Double;
const
  procname = 'LineSeries';
  procversion = '1.0.0.0';
begin
  ODebug.IncIndent;
  ODebug.Add(UnitName + ':' + procname + ' - ' + procversion, 1);
  result := TLineSeries.Create(nil);
  if YVector <> nil then
  begin
    // Find a y-coord worth using!
    i := 1;
    while (i<=XVector.Length) and (YVector.IsMissing[i]) do inc(i);
    dummy := YVector.AsFloat[i];
    for i := 1 to XVector.Length do
      if YVector.IsMissing[i] then
        continue
      else
        result.AddXY(XVector.AsFloat[i], YVector.AsFloat[i], LabelVector.AsString[i]);
  end else begin
    for i := 1 to XVector.Length do
      result.Add(XVector.AsFloat[i],
                 LabelVector.GetValueLabel(LabelVector.AsString[i]),
                 clTeeColor);
  end;
  ODebug.DecIndent;
end;

function TGraph.PieSeries(XVector, LabelVector: TEpiVector): TPieSeries;
var
  i: integer;
const
  procname = 'PieSeries';
  procversion = '1.0.0.0';
begin
  ODebug.Add(UnitName + ':' + procname + ' - ' + procversion, 1);
  result := TPieSeries.Create(nil);
  Result.Circled := true;
  for i := 1 to XVector.Length do
    result.Add(XVector.AsFloat[i],
               LabelVector.GetValueLabel(LabelVector.AsString[i]),
               clTeeColor);
end;



procedure TGraph.ShowCharts(Charts: array of TChart; Cmd: TCommand);
var
  Chart: TChart;
  Page:  TTabSheet;
  i:     integer;
const
  procname = 'ShowCharts';
  procversion = '1.0.0.0';
begin
  ODebug.Add(UnitName + ':' + procname + ' - ' + procversion, 1);

  // TODO : Show multiple charts on form
  if System.Length(Charts) = 0 then exit;
  GraphForm := TGraphForm.Create(nil);
  OTranslator.TranslateForm(GraphForm);

  for i := 0 to high(charts) do
  begin
    Page := TTabSheet.Create(GraphForm.GraphPG);
    Page.PageControl := GraphForm.GraphPG;
    Page.Caption := 'Graph ' + inttostr(i+1);
    Chart := Charts[i];
    Chart.Parent := Page;
    Chart.Align := alClient;  
  end;
  // Show the graph...
  GraphForm.ShowModal;
  for i := 0 to high(charts) do
    Charts[i].Parent := dummyform;   
  FreeAndNil(GraphForm);
end;

procedure TGraph.ShowChart(Chart: TChart; Cmd: TCommand);
var
  Charts: array of TChart;
const
  procname = 'ShowCharts';
  procversion = '1.0.0.0';
begin
  ODebug.Add(UnitName + ':' + procname + ' - ' + procversion, 1);
  SetLength(Charts, 1);
  Charts[0] := Chart;
  ShowCharts(Charts, cmd);
end;


{
 SaveChart saves the chart given conditions specified in cmd.
 It returns a string with the full pathname for the file.
}
function TGraph.SaveChart(Chart: TChart; Cmd: TCommand): string;
var
  s, ext: string;
  // save: boolean;
  opt: TEpiOption;
  day,month,year,hh,mm,ss,ms: Word;
  xsize, ysize: integer;
  rect: TRect;
  bitmap: TBitmap;
  pngobj: TPNGObject;
const
  procname = 'SaveChart';
  procversion = '1.0.0.0';
begin
  ODebug.Add(UnitName + ':' + procname + ' - ' + procversion, 1);

  if (dm.GetOptionValue('GRAPH SAVETYPE', Opt)) then ext := '.' + AnsiUpperCase(Opt.Value);
  DecodeTime(Time, hh, mm, ss, ms);
  DecodeDate(date,year,month,day);
  s := 'Graph' + IntToStr(year);

  // if graphs are not saved in current folder or logfile folder is different from
  // current folder then graphs will not be visible
  if dm.CodeMaker.LogFileName <> '' then
    if AnsiUppercase(ExtractFilePath(dm.CodeMaker.LogFileName)) <> AnsiUppercase(dm.CurrentDir) then
      dm.sysinfo('Graphs saved in: ' + dm.currentdir + '<br> Logfile saved in ' + ExtractFilePath(dm.CodeMaker.LogFileName));

  if month < 10 then s := s+ '0' + IntToStr(month) else s := s + IntToStr(month);
  if day < 10 then s := s+ '0' + IntToStr(day) else s := s + IntToStr(day);
  if hh < 10 then s := s+ '0' + IntToStr(hh) else s := s + IntToStr(hh);
  if mm < 10 then s := s+ '0' + IntToStr(mm) else s := s + IntToStr(mm);
  if ss < 10 then s := s+ '0' + IntToStr(ss) else s := s + IntToStr(ss);
  if ms < 10 then s := s+ '0' + IntToStr(ms) else s := s + IntToStr(ms);
  s := s + ext;
  if (Cmd.ParamByName['SAVE'] <> nil) then
  begin
    s := Cmd.ParamByName['SAVE'].AsString;
    if ExtractFileExt(Cmd.ParamByName['SAVE'].AsString) <> '' then
      ext := AnsiUppercase(ExtractFileExt(Cmd.ParamByName['SAVE'].AsString))
    else
      s := s + ext;
  end;
  s := ExpandFileName(s);
  if not((ext='.BMP') or (ext='.WMF') or (ext='.PNG')) then
    dm.Error('Unable to save using file format: %s', [ext], 113002);
  if (FileExists(s)) and (Cmd.ParamByName['REPLACE'] = nil) then
    dm.Error('File %s exist. Erase file or <br>use option /Replace', [s], 113003);
  if (dm.GetOptionValue('GRAPH SIZE X', Opt)) then xsize := StrToInt(Opt.Value);
  if (dm.GetOptionValue('GRAPH SIZE Y', Opt)) then ysize := StrToInt(Opt.Value);
  // Size Options override globally defined values. 
  if Assigned(Cmd.ParamByName['SIZEX']) then xsize := StrToInt(Cmd.ParamByName['SIZEX'].AsString);
  if Assigned(Cmd.ParamByName['SIZEY']) then ysize := StrToInt(Cmd.ParamByName['SIZEY'].AsString);
  Rect.Left := 0;
  Rect.Right := xsize;
  Rect.Bottom := ysize;
  Rect.Top := 0;
  if ext = '.PNG' then
  begin
    bitmap := Chart.TeeCreateBitmap(Chart.BackColor, Rect);
    pngobj := TPNGObject.Create;
    pngobj.Assign(BitMap);
    pngobj.SaveToFile(s);
    FreeAndNil(BitMap);
    FreeAndNil(PngObj);
  end;
  if ext = '.BMP' then Chart.SaveToBitmapFile(s);
  if ext = '.WMF' then Chart.SaveToMetafile(s);

  if (dm.GetOptionValue('GRAPH CLIPBOARD', Opt) and (Opt.Value = 'ON')) then
    Chart.CopyToClipboardMetafile(true, Rect);

  result := s;
end;

function TGraph.DoBars(Dataframe: TEpiDataframe; Varnames: TStrings; CmdID: Word; Parameters: TVarList): TChart;
var
  DF: TEpiDataframe;
  agglist: TAggrList;
  series: TChartSeries;
  Weighted, Grouped, Expand: boolean;
  i, k, total: integer;
  //last: variant;
  vec, nvec, zvec, tvec: TEpiVector;
  WeightName, GroupName: String;
const
  procname = 'DoBars';
  procversion = '1.0.1.1';

  function GetColour(index: integer): TColor;
  begin
    if Grouped then
      result := GetGraphColour(k)
    else
      result := GetGraphColour(index);
  end;

begin
  ODebug.IncIndent;
  ODebug.Add(UnitName + ':' + procname + ' - ' + procversion, 1);

  try
    // Aggregate dataframe!
    agglist := TAggrList.Create();

    //allow weights:
    if (Parameters.VarByName['W'] <> nil) then
    begin
      WeightName := (Parameters.VarByName['W'].AsString);
      Weighted := true;
      dm.AddResult('$WeightVar', EpiTyString, WeightName, 0, 0);
      Varnames.Delete(Varnames.IndexOf(WeightName));
    end;
    dm.AddResult('$weighted', EpiTyBoolean, Weighted, 0, 0);
    if not Weighted then
      agglist.Insert(0, TAggrCount.Create('$S', '', acAll))
    else
      agglist.Insert(0, TAggrSum.Create('$S', WeightName));

    // Allow groups:
    if (Parameters.VarByName['BY'] <> nil) then
    begin
      GroupName := (Parameters.VarByName['BY'].AsString);
      Grouped := true;
      if (Dataframe.VectorByName[Varnames[0]].DataType = EpiTyString) then
        dm.Error('Cannot use /BY with string variables', [], 113004);
    end;

    total := Dataframe.RowCount;                                        // TODO : Change to real CMD.
    df := OAggregate.AggregateDataframe(Dataframe, TStringList(varnames), agglist, TCommand.Create(0, Parameters));
    Expand := (Parameters.VarByName['XALL'] <> nil);
    if Grouped then
      Varnames.Delete(Varnames.IndexOf(GroupName));

    if Expand then
    begin
      for i := df.RowCount downto 1 do
        if (not df.VectorByName[Varnames[0]].IsMissing[i]) then break;
      df := df.ExpandDataframe(varnames, df.VectorByName[Varnames[0]].Value[1],
                               df.VectorByName[Varnames[0]].Value[i]);
    end;

    if Grouped then
    begin
      df.Sort(GroupName + ',' + varnames.CommaText);
      zvec := df.VectorByName[GroupName];
    end else
      df.Sort(Varnames.CommaText);

    vec := df.VectorByName[Varnames[0]];
    nvec := df.VectorByName['$S'];
    if Expand then
      for i := 1 to df.RowCount do
        if nvec.IsMissing[i] then nvec.AsInteger[i] := 0;

    // Create posible percent vector.
    if assigned(Parameters.VarbyName['PCT']) then
    begin
      tvec := TEpiFloatVector.Create('$P', nvec.Length);
      for i := 0 to nvec.Length do
        tvec.AsFloat[i] := (nvec.AsInteger[i] / total) * 100;
      nvec := tvec;
    end;

    // Create the graph and set customizable settings.
    result := CreateStandardChart();
    if (Parameters.VarByName['TI'] <> nil) then
      result.Title.Text.Add(Parameters.VarByName['TI'].AsString)
    else
      result.Title.Text.Add(vec.GetVariableLabel(Parameters));
    if Grouped then
      result.Legend.Title.Text.Add(' ' + df.VectorByName[GroupName].GetVariableLabel(Parameters))
    else
      result.Legend.Title.Text.Add(' ' + vec.GetVariableLabel(Parameters)); //{GroupName});
    result.BottomAxis.Title.Caption := ' ' + vec.GetVariableLabel(Parameters);
    result.BottomAxis.LabelStyle := talAuto;

    // Create the bar series.
    k := 0;
    series := TBarSeries.Create(nil);
    for i := 1 to vec.Length do
    begin
      if Expand or Grouped or (parameters.VarbyName['XMIN']<>nil) or
         (parameters.VarbyName['XINC']<>nil) then
      begin
        if (vec.IsMissing[i]) then
          series.AddXY(vec.AsFloat[i-1]+1, nvec.AsFloat[i],
                       vec.GetValueLabel(vec.AsString[i], Parameters),
                       GetColour(i-1))
        else
          series.AddXY(vec.AsFloat[i], nvec.AsFloat[i],
                       vec.GetValueLabel(vec.AsString[i], Parameters),
                       GetColour(i-1));
      end else begin
          series.Add(nvec.AsFloat[i],
                       vec.GetValueLabel(vec.AsString[i], Parameters),
                       GetColour(i-1));
      end;
      if Grouped and (i<vec.length) and (zvec.compare(i, i+1)<>0) then
      begin
        TBarSeries(series).BarWidthPercent := 75;
        series.marks.Visible := False;
        Series.ParentChart := result;
        Series.Color := GetColour(i-1);
        series.Title := zvec.GetValueLabel(zvec.AsString[i], Parameters);
        if (Parameters.VarbyName['YVALUE'] <> nil) then
        with series do begin
          Marks.Visible := true;
          Marks.Style := smsValue;
          Marks.Color := clWhite;
        end;
        inc(k);
        series := TBarSeries.Create(nil);
      end;
    end;
    // set special options for bars
    TBarSeries(series).BarWidthPercent := 75;
    series.marks.Visible := False;
    if grouped then
    begin
      series.Title := zvec.GetValueLabel(zvec.AsString[vec.Length], Parameters);
      Series.Color := GetColour(vec.Length);
    end;
    Series.ParentChart := result;
    if (Parameters.VarbyName['YVALUE'] <> nil) then
    with series do begin
      Marks.Visible := true;
      Marks.Style := smsValue;
      Marks.Color := clWhite;
    end;
    if Assigned(Parameters.VarbyName['PCT']) then
      result.LeftAxis.Title.Caption := 'Percent'
    else
      result.LeftAxis.Title.Caption := 'Count';
  finally
    if Assigned(df) then FreeAndNil(df);
    if Assigned(agglist) then FreeAndNil(agglist);
    if Assigned(tvec) then FreeAndNil(tvec);
    ODebug.DecIndent;
  end;
end;


function TGraph.DoHistogram(Dataframe: TEpiDataframe; Varnames: TStrings; CmdID: Word; Parameters: TVarList): TChart;
var
  DF: TEpiDataframe;
  agglist: TAggrList;
  series: TAreaSeries;
  {XOnly,} Grouped: boolean;
  i, k, total: integer;
  // last: Variant;
  vec, nvec, zvec, tvec: TEpiVector;
  GroupName: string;
const
  procname = 'DoHistogram';
  procversion = '1.0.0.0';

  function GetColour(index: integer): TColor;
  begin
    if Grouped then
      result := GetGraphColour(k)
    else
      result := GetGraphColour(index);
  end;

begin
  ODebug.IncIndent;
  ODebug.Add(UnitName + ':' + procname + ' - ' + procversion, 1);

  try
    // Allow groups:
    if (Parameters.VarByName['BY'] <> nil) then
    begin
      GroupName := (Parameters.VarByName['BY'].AsString);
      Grouped := true;
    end;

    total := Dataframe.RowCount;

    // Aggregate dataframe!
    agglist := TAggrList.Create();
    agglist.Add(TAggrCount.Create('$S', Varnames[0], acAll));
    df := OAggregate.AggregateDataframe(Dataframe, TStringList(Varnames), agglist, TCommand.Create(0, Parameters));

    if Grouped then
      Varnames.Delete(Varnames.IndexOf(GroupName));

    if df.VectorByName[Varnames[0]].DataType <> EpiTyDate then
    begin
      for i := df.RowCount downto 1 do
        if (not df.VectorByName[Varnames[0]].IsMissing[i]) then break;
      df := df.ExpandDataframe(varnames, df.VectorByName[Varnames[0]].Value[1],
                             df.VectorByName[Varnames[0]].Value[i]);
    end;

    if Grouped then
    begin
      df.Sort(GroupName + ',' + varnames.CommaText);
      zvec := df.VectorByName[GroupName];
    end else
      df.Sort(Varnames.CommaText);

    vec := df.VectorByName[Varnames[0]];
    nvec := df.VectorByName['$S'];

    for i := 1 to df.RowCount do
      if nvec.IsMissing[i] then nvec.AsInteger[i] := 0;

    // Create posible percent vector.
    if assigned(Parameters.VarbyName['PCT']) then
    begin
      tvec := TEpiFloatVector.Create('$P', nvec.Length);
      for i := 0 to nvec.Length do
        tvec.AsFloat[i] := (nvec.AsInteger[i] / total) * 100;
      nvec := tvec;
    end;

    // Create the graph and set customizable settings.
    result := CreateStandardChart();
    if (Parameters.VarByName['TI'] <> nil) then
      result.Title.Text.Add(Parameters.VarByName['TI'].AsString)
    else
      result.Title.Text.Add(vec.GetVariableLabel(Parameters));
    result.BottomAxis.Title.Caption := ' ' + vec.GetVariableLabel(Parameters);
    result.BottomAxis.LabelStyle := talAuto;

    // Create the histogram series.
    // series := Thistogramseries.create(nil);
    series := TAreaSeries.Create(nil);
    series.Name := 'Areaseries';
    k := 0;
    for i := 1 to vec.Length do
    begin
      if (vec.IsMissing[i]) then
        series.AddXY(vec.AsFloat[i-1]+1, nvec.AsFloat[i],
                     vec.GetValueLabel(vec.AsString[i], Parameters),
                     GetHisColor(i-1))
      else
        series.AddXY(vec.AsFloat[i], nvec.AsFloat[i],
                     vec.GetValueLabel(vec.AsString[i], Parameters),
                     GetHisColor(i-1));
      if Grouped and (i<vec.length) and (zvec.compare(i, i+1)<>0) then
      begin
        Series.Stairs := True;
        series.marks.Visible := False;
        Series.ParentChart := result;
        Series.Color := GetColour(i-1);
        series.Title := zvec.GetValueLabel(zvec.AsString[i], Parameters);
        if (Parameters.VarbyName['YVALUE'] <> nil) then
        with series do begin
          Marks.Visible := true;
          Marks.Style := smsValue;
          Marks.Color := clWhite;
        end;
        inc(k);
        series := TAreaSeries.Create(nil);
      end;
    end;
     //add one last point to see the last bar:
     //TODO: +1 should be one unit in current x variable   but does not work properly:
    try
      dec(i);
      if (not vec.IsMissing[i]) and (i>1) then
        series.AddXY(vec.AsFloat[i] + (vec.AsFloat[i]-vec.asfloat[i-1]), nvec.AsFloat[i],'',
                     GetHisColor(i-1));
    except
      dm.info('report problem with histogram (drawing last bar): %d', [i], 213001);
    end;
    Series.Stairs := True;
    Series.Marks.Visible := False;
    result.LeftAxis.AutomaticMinimum := false;
    result.LeftAxis.Minimum := 0;
    if grouped then
    begin
      series.Title := zvec.GetValueLabel(zvec.AsString[vec.Length], Parameters);
      Series.Color := GetColour(vec.Length);
    end;
    series.ParentChart := result;
    if (Parameters.VarbyName['YVALUE'] <> nil) then
    with series do begin
      Marks.Visible := true;
      Marks.Style := smsValue;
      Marks.Color := clWhite;
    end;
    if Assigned(Parameters.VarbyName['PCT']) then
      result.LeftAxis.Title.Caption := 'Percent'
    else
      result.LeftAxis.Title.Caption := 'Count';
  finally
    if assigned(df) then FreeAndNil(df);
    if Assigned(agglist) then FreeAndNil(agglist);
    if Assigned(tvec) then FreeAndNil(tvec);
  end;
  CalcXInc(Result);
  ODebug.DecIndent;
end;

function TGraph.DoBoxPlot(Dataframe: TEpiDataframe; Varnames: TStrings; CmdID: Word; Parameters: TVarList;
                          var OutputTable: TStatTable): TChart;
var
  Series: TBoxSeries;
  Vec, ByVec: TEpiVector;
  GroupVar: string;
  i, j, k: integer;
  agdf, df: TEpiDataframe;
  AggL: TAggrList;
  ByVars, LVars : TStrings;
const
  procname = 'DoBoxPlot';
  procversion = '1.0.0.0';

  function CreateTable(df: TEpiDataframe; Caption: string): TStatTable;
  begin
    Result := dm.CodeMaker.Output.NewTable(8,1);
    REsult.TableType := sttNormal;
    Result.Caption := 'Box Plot' + Caption;
    Result.Cell[1,1] := '';
    Result.Cell[2,1] := 'N';
    Result.Cell[3,1] := 'Min';
    Result.Cell[4,1] := 'P10';
    Result.Cell[5,1] := 'P25';
    Result.Cell[6,1] := 'Med';
    Result.Cell[7,1] := 'P75';
    Result.Cell[8,1] := 'P90';
    Result.Cell[9,1] := 'Max';
  end;


  procedure AddToTable(dataframe: TEpiDataFrame; VariableName: string);
  var
    i,j: integer;
  begin
    OutputTable.AddRow;
    OutputTable.Cell[1, OutputTable.RowCount] := VariableName;
    if DataFrame.RowCount > 1 then
      for j := 2 to OutputTable.ColCount do
        OutputTable.Cell[j, OutputTable.RowCount] := '';
    for i := 1 to dataframe.RowCount do
    begin
      if DataFrame.RowCount > 1 then
      begin
        OutputTable.AddRow;
        OutputTable.Cell[1, OutputTable.RowCount] := dataframe.Vectors[0].GetValueLabel(dataframe.Vectors[0].AsString[i], Parameters);
      end;
      for j := 2 to dataframe.VectorCount do
      begin
        OutputTable.Cell[j, OutputTable.RowCount] := dataframe.Vectors[j-1].AsString[i];
      end;
    end;
  end;

  procedure FinishBox(df: TEpiDataFrame; BoxNo, index: integer);
  begin
    with Series do
    begin
      Color := 0;
      LinePen.Width := 3;
      MildOut.VertSize := 5   ;
      MildOut.HorizSize := 5   ;
      MildOut.Style := psCircle   ;
      MildOut.Brush.Style := bsClear;
      MildOut.Visible := False;
      ExtrOut.Visible := False;
      UseCustomValues := False;
      Box.Color:= GetGraphColour(BoxNo);
      //Box.HorizSize := 25;
      Position := BoxNo;
      UseCustomValues := true;

      // always:
      Quartile1 := df.VectorByName['$25'].AsFloat[index+1];
      Median := df.VectorByName['$Med'].AsFloat[index+1];
      Quartile3 := df.VectorByName['$75'].AsFloat[index+1];
      OuterFence1 := df.VectorByName['$MIN'].AsFloat[index+1];
      OuterFence3 := df.VectorByName['$MAX'].AsFloat[index+1];

      if Assigned(Parameters.VarByName['P1090']) then
      begin   // use 10 and 90 percentile
        InnerFence1 := df.VectorByName['$10'].AsFloat[index+1];
        AdjacentPoint1 := df.VectorByName['$10'].AsFloat[index+1];
        AdjacentPoint3 := df.VectorByName['$90'].AsFloat[index+1];
        InnerFence3 := df.VectorByName['$90'].AsFloat[index+1];
      end
      else if Assigned(Parameters.VarByName['R']) then
      begin  // use complete range
        InnerFence1 := df.VectorByName['$MIN'].AsFloat[index+1];
        AdjacentPoint1 := df.VectorByName['$MIN'].AsFloat[index+1];
        AdjacentPoint3 := df.VectorByName['$MAX'].AsFloat[index+1];
        InnerFence3 := df.VectorByName['$MAX'].AsFloat[index+1];
      end
      else
      begin   // default  values, should be:
              // We need to find the correct boxplot value
                 // from EQUAL to or next observation above (p25 - 1.5*(p75-p25))
                 // to EQUAL to or observation before value (p75 + 1.5*(p75-p25))
                // find the adjacentpoints from sample values, built in function:
        ReCalcStats;
        InnerFence1 := AdjacentPoint1;
        InnerFence3 := AdjacentPoint3;
      end;

      if (Parameters.VarByName['OUT'] <> nil) then
      begin
        MildOut.Visible := True;
        ExtrOut.Visible := True;
      end;
    end;
  end;

begin
  ODebug.IncIndent();
  ODebug.Add(UnitName, self.ClassName, procname, procversion, 1);
  df := nil;
  ByVars := nil;

  // Create the graph and set customizable settings.
  result := CreateStandardChart();
  try
    if (Parameters.VarByName['TI'] <> nil) then
      result.Title.Text.Add(Parameters.VarByName['TI'].AsString);

    result.BottomAxis.MinimumOffset := 10;
    result.BottomAxis.MaximumOffset := 10;
    result.BottomAxis.Ticks.Visible := false;
    result.BottomAxis.Labels := true;
    result.MarginLeft := 10;
    result.MarginRight := 5;

    if Parameters.VarExists['BY'] then
    begin
      GroupVar := Parameters.VarbyName['BY'].AsString;
      Varnames.Delete(Varnames.IndexOf(GroupVar));
    end else begin
      GroupVar := '$BY';
      ByVec := TEpiIntVector.Create(GroupVar, Dataframe.RowCount);
      for i := 1 to Dataframe.RowCount do
        ByVec.AsInteger[i] := 1;
      Dataframe.Vectors.Add(ByVec);
    end;

    OutputTable := CreateTable(Dataframe,'');
    if Varnames.Count = 1 then
      Result.Title.Caption := DataFrame.VectorByName[Varnames[0]].GetVariableLabel(Parameters);

    ByVars := TStringList.Create;
    LVars := TStringList.Create;
    for i := 0 to Varnames.Count -1 do
    begin
      ByVars.Clear();
      ByVars.Add(Varnames[i]);
      ByVars.Add(GroupVar);
      LVars.Clear();
      LVars.Add(Varnames[i]);
      df := dataframe.PrepareDataframe(ByVars, LVars);

      ByVars.Delete(0);
      AggL := TAggrList.Create();
      Aggl.Add(TAggrCount.Create('$N', Varnames[i], acAll));
      Aggl.Add(TAggrMinMax.Create('$MIN', Varnames[i], true));
      Aggl.Add(TAggrPercentile.Create('$10', Varnames[i], ap10));
      Aggl.Add(TAggrPercentile.Create('$25', Varnames[i], ap25));
      Aggl.Add(TAggrPercentile.Create('$Med', Varnames[i], ap50));
      Aggl.Add(TAggrPercentile.Create('$75', Varnames[i], ap75));
      Aggl.Add(TAggrPercentile.Create('$90', Varnames[i], ap90));
      Aggl.Add(TAggrMinMax.Create('$MAX', Varnames[i], false));

      agdf := OAggregate.AggregateDataframe(df, TStringList(ByVars), AggL, TCommand.Create(0, Parameters));
      vec := df.VectorByName[Varnames[i]];
      ByVec := df.VectorByName[GroupVar];

      AddToTable(agdf, Vec.GetVariableLabel(Parameters));

      if Parameters.VarExists['BY'] then
      begin
        if Varnames.Count = 1 then
          result.BottomAxis.Title.Caption := ByVec.GetVariableLabel(Parameters)
        else
          result.BottomAxis.Title.Caption := result.BottomAxis.Title.Caption + Vec.GetVariableLabel(Parameters);
        if i < Varnames.Count -1 then
          result.BottomAxis.Title.Caption := result.BottomAxis.Title.Caption + ' | ';
      end;

      k := 0;
      Series := TBoxSeries.Create(Result);
      for j := 1 to df.RowCount-1 do
      begin
        Series.AddY(vec.AsFloat[j]);
        if (ByVec.compare(j, j+1) <> 0) then
        begin
          Series.ParentChart := result;
          Series.Title := vec.GetVariableLabel(Parameters);
          Series.xLabel[(i*agdf.RowCount)+k] := ByVec.GetValueLabel(ByVec.AsString[j], Parameters);
          FinishBox(agdf, (i*agdf.RowCount)+k, k);
          inc(k);
          Series := TBoxSeries.Create(Result);
        end;
      end;
      Series.AddY(vec.AsFloat[j]);
      Series.ParentChart := result;
      Series.Title := vec.GetVariableLabel(Parameters);
      if Parameters.VarExists['BY'] then
        Series.xLabel[(i*agdf.RowCount)+k] := ByVec.GetValueLabel(ByVec.AsString[j], Parameters)
      else
        Series.xLabel[(i*agdf.RowCount)+k] := vec.GetVariableLabel(Parameters);
      FinishBox(agdf, (i*agdf.RowCount)+k, k);
      
      FreeAndNil(df);
      FreeAndNil(agdf);
      FreeAndNil(AggL);
    end;
    if Result.SeriesList.Count = 2 then
    begin
      result.BottomAxis.MinimumOffset := 75;
      result.BottomAxis.MaximumOffset := 75;
    end;
    if Parameters.VarExists['NT'] then
      FreeAndNil(OutputTable);
  finally
    ODebug.DecIndent();
    if Assigned(ByVars) then FreeAndNil(ByVars);
    if Assigned(LVars) then FreeAndNil(LVars);
    if Assigned(AggL) then FreeAndNil(AggL);
  end;
end;

procedure SetScatter(Series: TCustomSeries; index: integer);
begin
  with Series do
  begin
    Pointer.Visible :=  true;
    pointer.VertSize := 4;
    pointer.HorizSize := 4;
    Stairs := false;
    Linepen.Visible:= True;
    Linepen.Width := 2;
    Color := GetGraphColour(index);
    Pointer.Brush.Color := GetGraphColour(index);
    pointer.Style := GetGraphPointerStyle(index);
    pointer.Brush.Style := bsSolid;
  end;
end;

procedure SetDotPlot(Series: TCustomSeries; index: integer);
begin
  with Series do
  begin
    pointer.VertSize := 2;
    pointer.HorizSize := 2;
    Stairs := false;
    Color := GetGraphColour(index);
    Pointer.Brush.Color := GetGraphColour(index);
    case (index mod 6) of
      0: pointer.Style := psCircle;
      1: begin
           pointer.Style := psCircle;
           pointer.Brush.Style := bsSolid;
         end;
      2: pointer.Style := psTriangle;
      3: begin
           pointer.Style := psTriangle;
           pointer.Brush.Style := bsSolid;
         end;
      4: pointer.Style := psRectangle;
      5: begin
           pointer.Style := psRectangle;
           pointer.Brush.Style := bsSolid;
         end;
    end;
  end;
end;

function TGraph.DoDotPlot(Dataframe: TEpiDataframe; Varnames: TStrings; CmdID: Word; Parameters: TVarList): TChart;
var
  i,j,k: integer;
  Xvalue, minx, maxx, di  : Double;
  series: TCustomSeries;
  xvec, yvec, zvec, lvec: TEpiVector;
  PVarDesc: TAnaVariableDescriptor;
  varlist : Tstringlist;
  newvalue : Boolean;
const
  procname = 'DoDotPlot';
  procversion = '1.0.0.0';
begin
  ODebug.Add(UnitName + ':' + procname + ' - ' + procversion, 1);
  if varnames.Count < 1 then dm.Error('Minimum 1 variables for %s!', ['DoDotPlot'], 113005);

  if Parameters.VarbyName['DI'] <> nil then di := (Parameters.VarbyName['DI'].AsFloat)
    else di := 0.015;

  // Create the graph and set customizable settings.
  result := CreateStandardChart();

  result.Title.Text.Add(MultiVarTitle(Dataframe.GetVectorListByName(Varnames), Parameters));
  varlist := TStringList.Create();

  // create a cheat group vector as part of the dataframe (for sorting)
  if varnames.Count = 1 then
  begin  // create a cheat group vector as part of the dataframe (for sorting)
    PVarDesc := TAnaVariableDescriptor.CreateLocal('$X1',EpiTyInteger,4,0,'%4.0f');
    dataframe.NewVector(PVarDesc);
    xvec := dataframe.VectorByName['$X1'];
    for j:=1 to dataframe.RowCount do
      xvec.asfloat[j] := 1.0;
    varlist.Add('$X1');  // the group variable
  end else begin
   varlist.Add(varnames[1]);  // the group variable
   xvec := dataframe.VectorByName[Varnames[1]];
  end;

  // the variable to graph
  yvec := dataframe.VectorByName[Varnames[0]];
  result.LeftAxis.Title.Caption := Yvec.GetVariableLabel(parameters);

  // the pseudo X to use for graphing as x coordinate
  PVarDesc := TAnaVariableDescriptor.CreateLocal('$X',EpiTyFloat,14,6);
  dataframe.NewVector(PVarDesc);
  zvec := dataframe.VectorByName['$X'];

  // The label vector
  lvec := TEpiStringVector.Create('$Label', dataframe.RowCount,25);
  for j:=1 to dataframe.RowCount do
      lvec.asString[j] := ' ';

  // sort observations based on group variable and within group on first variable:
  varlist.Add(varnames[0]);  // the values
  dataframe.Sort(varlist.CommaText);

  i := 1;
  minx := 0.90;
  xvalue := minx + 0.1   ;  // xvec.asfloat[1];  // 0.10
  if dataframe.RowCount > 1000 then xvalue := xvalue + 0.5;
  //if varnames.count = 1 then  xvalue := 2.0 ;

  maxx := xvalue;
  zvec.asfloat[1] := xvalue;

  if (varnames.count > 1) then
  if xvec.DataType in [EpiTyString,EpiTyUppercase] then
    lvec.asstring[1] := xvec.AsString[1]
  else
    lvec.asstring[1] := xvec.GetValueLabel(inttostr(xvec.Asinteger[1]));
   //dm.info(lvec.asstring[1]);
  for j:=2 to dataframe.RowCount do
  begin
    Newvalue := False;
    If (xvec.DataType in [EpiTyString,EpiTyUppercase, EpityBoolean]) then
      NeWvalue := (xvec.AsString[j] <> xvec.AsString[j-1])
    else {EpiTyFloat :}
      Newvalue := (xvec.AsFloat[j] <> xvec.AsFloat[j-1]);

    if Newvalue then
    begin
      inc(i);  // number of groups
      xvalue := maxx + 0.75;
      if dataframe.RowCount > 1000 then xvalue := xvalue + 1;
      case xvec.DataType of
        EpiTyString,
        EpiTyUppercase  : lvec.asstring[j] := xvec.AsString[j];
        EpiTyInteger    : if xvec.IsMissing[j] then
                            lvec.IsMissing[j] := true
                          else
                            lvec.asstring[j] := xvec.GetValueLabel(inttostr(xvec.Asinteger[j]));
      else
        lvec.asstring[j] := xvec.AsString[j];
      end;
    end;

    if ((yvec.AsFloat[j] = yvec.AsFloat[j-1]) and (xvec.AsFloat[j] = xvec.AsFloat[j-1])) then
      zvec.AsFloat[j] := zvec.AsFloat[j-1]+ di               // 0.015
    else
      zvec.AsFloat[j] := xvalue ;
    if zvec.asfloat[j] > maxx then maxx := zvec.asfloat[j];
        // if zvec.asfloat[j] < minx then minx := zvec.asfloat[j];
  end; //j loop

  dm.AddResult('$Groups', EpiTyInteger, i, 5, 0);

  if Parameters.VarbyName['C'] <> nil  then
  begin
      // scale to midpoint:           i j k
      j := 1;
      while (j <= dataframe.RowCount) do
      begin
        i := j;   // current first record with this value
        k := 1;   // counter, how many with same value
        while ((i+k) < dataframe.RowCount) and  (yvec.AsFloat[i+k-1] = yvec.AsFloat[i+k])
         do  inc(k);
        if k > 1 then
          for j:=i to (i+k-1) do
            zvec.AsFloat[j] := zvec.AsFloat[j] - (((k-1)*di)/2);   // 0.015
        j := i+k;
      end;
   end;     // end scaling

  for j:=1 to dataframe.RowCount do
    if (minx+0.20) > zvec.AsFloat[j] then
      minx := zvec.AsFloat[j] - 0.20;

  series := ScatterSeries(zvec, yvec, lvec);
  SetDotPlot(TPointSeries(Series), 1);

  //result.BottomAxis.StartPosition := 20.0;
  result.BottomAxis.EndPosition:=75;
  result.BottomAxis.LabelsSeparation := 0;
  //if varnames.count = 1 then maxx := max(maxx,xvalue) + 1.0 ;//; else maxx := max(maxx,xvalue) + 0.05;
  result.BottomAxis.SetMinMax(minx,maxx+0.25);
  result.BottomAxis.Labels := True;
  result.Axes.Bottom.Ticks.Visible := false; // turn off misleading tickmarks on x-axis
  if varnames.count = 1 then  result.MarginLeft := 10;
  result.bottomaxis.istartpos := -15;
  series.Title := yvec.GetVariableLabel(Parameters);
  series.ParentChart := result;
end;


function TGraph.DoScatter(Dataframe: TEpiDataframe; Varnames: TStrings; CmdID: Word; Parameters: TVarList): TChart;
var
  i, j, k: integer;
  series: TCustomSeries;
  xvec, yvec, zvec, lvec: TEpiVector;
  dummy: double;
  groupvar: string;
const
  procname = 'DoScatter';
  procversion = '1.0.0.0';


  procedure LocalScatter(index: integer);
  // set colour and style
  var
    pColor, pStyle, pBrush: integer;
  begin
    if Parameters.VarbyName['BYC'] <> nil then
    begin
      pColor := index;
      pStyle := 0;
      pBrush := 1;
    end else if Parameters.VarbyName['BYS'] <> nil then
    begin
      pColor := 0;
      pStyle := index;
      pBrush := 0;
    end else begin
      pColor := index;// div 2;
      pStyle := index;// div 2;
      pBrush := index;
    end;
    with series do
    begin
      //dm.info(format(' index: %d farve: %d pointer: %d ',[index,pcolor,pstyle]));
      Color := GetGraphColour(pColor);
      Pointer.Brush.Color := GetGraphColour(pColor);
      pointer.Style := GetGraphPointerStyle(pStyle);
      pointer.Brush.Style := GetGraphBrushStyle(pBrush);
    end;
  end;

begin
  ODebug.Add(UnitName + ':' + procname + ' - ' + procversion, 1);
  if varnames.Count < 2 then dm.Error('Minimum 2 variables for %s!', ['DoScatter'], 113006);

  // Create the graph and set customizable settings.
  result := CreateStandardChart();
  

  xvec := Dataframe.VectorByName[varnames[0]];

  Result.BottomAxis.Title.Caption := XVec.GetVariableLabel(Parameters);


  // Uncomment when XLAB needed in scatter!
{  if (Parameters.VarByName['XLAB'] = nil) then }
    lvec := xvec;
{  else begin
    lvec := Dataframe.VectorByName[Parameters.VarByName['XLABEL'].AsString];
    varnames.Delete(varnames.IndexOf(Parameters.VarByName['XLABEL'].AsString));
  end;     }
  // Create the scatter (point) series.
  if Parameters.VarExists['BY'] then
  begin
    k := 0;
    groupvar := Parameters.VarbyName['BY'].AsString;
    varnames.Delete(varnames.IndexOf(groupvar));
    result.Title.Text.Add(MultiVarTitle(Dataframe.GetVectorListByName(varnames), Parameters));
    result.Legend.Title.Text.Add(' ' + dataframe.VectorByName[GroupVar].GetVariableLabel(Parameters));
    //result.Legend.Title.Text.Add('Grouped by: ' + groupvar);
    dataframe.Sort(groupvar+','+varnames[0]);
    zvec := dataframe.VectorByName[groupvar];
    // Find a y-coord worth using!
    yvec := dataframe.VectorByName[Varnames[1]];
    i := 1;
    while (i<=xvec.Length) and (yvec.IsMissing[i]) do inc(i);
    dummy := yvec.AsFloat[i];
    if Varnames.Count = 2 then
      Result.LeftAxis.Title.Caption := dataframe.VectorByName[Varnames[1]].GetVariableLabel(Parameters);
    for j := 1 to varnames.Count -1 do
    begin
      yvec := dataframe.VectorByName[Varnames[j]];
      if cmdId in [opScatter, opShortScatter] then
        series := TPointSeries.Create(nil)
      else
        series := TLineSeries.Create(nil);
      SetScatter(Series, k);
      LocalScatter(k);
      inc(k);
      if (yvec.IsMissing[1]) then
        TChartSeries(Series).AddNullXY(xvec.AsFloat[1], dummy, lvec.AsString[1])
      else
        TChartSeries(Series).AddXY(xvec.AsFloat[1], yvec.AsFloat[1], lvec.AsString[1]);
      for i := 2 to dataframe.RowCount do
      begin
        if zvec.compare(i-1,i) <> 0 then
        begin
          series.ParentChart := result;
          series.Title := {yvec.Name + ' ' + }zvec.GetValueLabel(zvec.AsString[i-1], Parameters);
          if (Parameters.VarbyName['YVALUE'] <> nil) then
          with series do begin
            Marks.Visible := true;
            Marks.Style := smsValue;
            Marks.Color := clWhite;
          end;
          if cmdId in [opScatter, opShortScatter] then
            series := TPointSeries.Create(nil)
          else
            series := TLineSeries.Create(nil);
          SetScatter(Series, k);
          LocalScatter(k);
          inc(k);
        end;
        if (yvec.IsMissing[i]) then
          TChartSeries(Series).AddNullXY(xvec.AsFloat[i], dummy, lvec.AsString[i])
        else
          TChartSeries(Series).AddXY(xvec.AsFloat[i], yvec.AsFloat[i], lvec.AsString[i]);
      end;
      series.ParentChart := result;
      series.Title := {yvec.Name + ' ' +} zvec.GetValueLabel(zvec.AsString[i-1], Parameters);
      if (Parameters.VarbyName['YVALUE'] <> nil) then
      with series do begin
        Marks.Visible := true;
        Marks.Style := smsValue;
        Marks.Color := clWhite;
      end;
    end;
    result.BottomAxis.LabelStyle := talValue;
  end else begin
    result.Title.Text.Add(MultiVarTitle(Dataframe.GetVectorListByName(Varnames), Parameters));
    Dataframe.Sort(varnames[0]);
    if Varnames.Count = 2 then
      Result.LeftAxis.Title.Caption := dataframe.VectorByName[Varnames[1]].GetVariableLabel(Parameters);
    for i := 1 to varnames.Count -1 do
    begin
      yvec := dataframe.VectorByName[Varnames[i]];
      if cmdId in [opScatter, opShortScatter] then
        series := ScatterSeries(xvec, yvec, lvec)
      else
        series := LineSeries(xvec, yvec, lvec);
      SetScatter(Series, i-1);
      LocalScatter(i-1);
      series.ParentChart := result;
      series.Title := yvec.GetVariableLabel(Parameters);
      if (Parameters.VarbyName['YVALUE'] <> nil) then
      with series do begin
        Marks.Visible := true;
        Marks.Style := smsValue;
        Marks.Color := clWhite;
      end;
    end;
  end;
  
  CalcXInc(Result);
  // Since CalcXInc always set LabelStyle to talValue!
  if (xvec.DataType = EpiTyDate) then
  begin
    result.BottomAxis.AxisValuesFormat := '##';
    result.OnGetAxisLabel := BottomDateLabeling;
  end;
end;

function TGraph.DoPie(Dataframe: TEpiDataframe; Varnames: TStrings; CmdID: Word; Parameters: TVarList): TChart;
var
  xvec, lvec: TEpiVector;
  df: TEpiDataFrame;
  series: TChartSeries;
  agglist: TAggrList;
const
  procname = 'DoPie';
  procversion = '1.0.0.0';
begin
  ODebug.IncIndent;
  ODebug.Add(UnitName + ':' + procname + ' - ' + procversion, 1);
  if varnames.Count > 1 then Exception.Create('Maximum 1 variables for DoPie!');

  try
    // Create the graph and set customizable settings.
    result := CreateStandardChart();
    result.Title.Text.Add(MultiVarTitle(Dataframe.GetVectorListByName(Varnames), Parameters));

    // Aggregate dataframe!
    agglist := TAggrList.Create();
    agglist.Add(TAggrCount.Create('N', Varnames[0], acAll));
    df := OAggregate.AggregateDataframe(Dataframe, TStringList(varnames), agglist, TCommand.Create(0, Parameters));
    xvec := df.VectorByName['N'];
    lvec := df.VectorByName[varnames[0]];
    series := PieSeries(xvec, lvec);
    series.ParentChart := result;
  finally
    if Assigned(df) then FreeAndNil(df);
    if Assigned(agglist) then FreeAndNil(agglist);
    ODebug.DecIndent;
  end;
end;

procedure OutputYIncrease(const chart: TChart);
var
  ttool: TAnnotationTool;
begin
  if chart.LeftAxis.increment > 1 then
  begin
    ttool := TAnnotationTool.Create(nil);
    ttool.Text := FloatToStr(chart.LeftAxis.increment) + ' cases/box';
    ttool.ParentChart := chart;
    ttool.Shape.CustomPosition := true;
    ttool.Shape.Left :=10 ; // (chart.Width - ttool.Width) div 2;
    ttool.Shape.Top := 15;
  end
end;

function TGraph.DoEpiCurve(Dataframe: TEpiDataframe; Varnames: TStrings;
                           CmdID: Word; Parameters: TVarList;
                           var OutputTable: TStatTable; var footnote:string): TChart;
var
  i, j, aBegin, aEnd, total,
  maxval, scount, noncase, casemis: integer;
  vals: array of EpiFloat;
  cases, missings: array of integer;
  mins, maxs: array of string;
  min, max : integer;
  xvec, yvec, zvec, mvec: TEpiVector;
  df, df2: TEpiDataframe;
  agglist: TAggrList;
  series: array of TBarSeries;
  varlist: TStringList;
  title, mints,maxts: string;


  function CreateBarSeries(parent: TChart): TBarSeries;
  begin
    //series := TAreaSeries.Create(nil);
    //Series.Stairs := True;
    result := TBarSeries.Create(nil);
    result.StackGroup := 0;
    result.MultiBar := mbStacked;
    result.BarWidthPercent := 100;
    result.Marks.Visible := false;
    result.TickLines.Visible:=True;
    if result.color = clBlack then
      result.TickLines.Color := clWhite
    else
      result.TickLines.Color := clBlack;
  end;

const
  procname = 'DoEpiCurve';
  procversion = '1.0.0.0';
begin
  ODebug.IncIndent;
  ODebug.Add(UnitName + ':' + procname + ' - ' + procversion, 1);
  if varnames.Count < 2 then Exception.Create('Minimum 2 variables');
  if varnames.Count > 3 then Exception.Create('Maximum 3 variables');

  try
    // Finding overall max value of first variable.
    yvec := dataframe.VectorByName[varnames[0]];
    xvec := dataframe.VectorByName[varnames[1]];
    maxval := -MaxInt;
    aBegin := MaxInt;
    aEnd := -MaxInt;
    for i := 1 to dataframe.RowCount do
    begin
      if xvec.IsMissing[i] then
        continue;
      if maxval < yvec.AsInteger[i] then
        maxval := yvec.AsInteger[i];
      if aBegin > xvec.AsInteger[i] then
        aBegin := xvec.AsInteger[i];
      if aEnd < xvec.AsInteger[i] then
        aEnd := xvec.AsInteger[i];
    end;

    // Only records with maxvalue
    for i := 1 to dataframe.RowCount do
      if yvec.Value[i] <> maxval then
        dataframe.Selected[i] := false;
    if dataframe.SelectedRowCount = 0 then Exception.Create('No Data');

    if varnames.count < 3 then
      title := 'EpiCurve: ' + yvec.GetVariableLabel(Parameters)
    else
      title := 'EpiCurve: ' + yvec.GetVariableLabel(Parameters) +
               ' by ' + dataframe.VectorByName[varnames[2]].GetVariableLabel(Parameters);

    if Parameters.VarbyName['TI'] <> nil then
      title := Parameters.VarbyName['TI'].Value;

    footnote := yvec.GetVariableLabel(Parameters) + ' = ' + yvec.GetValueLabel(VarToStr(maxval), Parameters);

    Varnames.Delete(0);
    df := dataframe.prepareDataframe(varnames, nil);
    agglist := TAggrList.Create();
    agglist.Add(TAggrCount.Create('$S', Varnames[0], acAll));
    df := OAggregate.AggregateDataframe(df, TStringList(varnames), agglist, TCommand.Create(0, Parameters));
    varlist := TStringList.Create();
    varlist.Add(varnames[0]);
    df2 := df.ExpandDataframe(varlist,aBegin-1,aEnd+1);
    xvec := df2.VectorByName[varnames[0]];      // date variable
    yvec := df2.VectorByName['$S'];
    if varnames.Count = 2 then
    begin
      zvec := df2.VectorByName[varnames[1]];
      for i := 0 to zvec.Length do
        if zvec.AsInteger[i] < 0 then
          dm.Error('By variable cannot contain negative values', [], 113007);
    end else begin
      zvec := TEpiIntVector.Create('$T', df2.RowCount);
      for i := 1 to df2.RowCount do
        zvec.AsInteger[i] := 1;
    end;
    df2.Sort(varlist.CommaText);

    // Create the graph and set customizable settings.
    result := CreateStandardChart();
    result.Title.Text.Add(title);
    result.BottomAxis.Title.Caption := xvec.GetVariableLabel(parameters);
    result.LeftAxis.Title.Caption := 'Count';

    if Parameters.VarbyName['YINC'] = nil then
       Parameters.AddVar('YINC', 1);

    if Parameters.VarbyName['I'] <> Nil then GraphCallBack := OutputYIncrease;

    // This loop resets all missing data made by expanding the dataframe
    // and ONLY creates the appropriate number of barseries for the graph.
    scount := 1;
    setlength(series, scount);
    setlength(vals, scount);
    setlength(cases, scount);
    setlength(missings, scount);
    setlength(mins, scount);
    setlength(maxs, scount);
    // noncaseday := 0;  //dayonset nonmissing
    min := NA_Int;
    max := 0; //dates overall
    for i := 1 to df2.RowCount do
    begin
      if (not yvec.IsMissing[i]) then
      begin
        if ((scount - 1) < zvec.AsInteger[i]) then
        begin
          scount := (zvec.AsInteger[i] + 1);
          setlength(series, scount);
          setlength(vals, scount);
          setlength(cases, scount);
          setlength(missings, scount);
          setlength(mins, scount);
          setlength(maxs, scount);
        end;
        if not Assigned(series[zvec.AsInteger[i]]) then
        begin
          series[zvec.AsInteger[i]] := CreateBarSeries(result);
          series[zvec.AsInteger[i]].Title :=
          zvec.GetValueLabel(zvec.AsString[i]);
          vals[zvec.AsInteger[i]] := NA_FLOAT;
          cases[zvec.AsInteger[i]] := 0;
          missings[zvec.AsInteger[i]] := 0;
          mins[zvec.AsInteger[i]] := xvec.AsString[i];
          if xvec.AsInteger[i] < min then mints := xvec.AsString[i];
        end;
      end;
    end;
    // end that loop

    // To get the order of the series correct when displaying legend.
    j := 0;
    for i:=0 to scount-1 do
      if assigned(series[i]) then
      begin
        series[i].Color := GetGraphColour(j);
        series[i].ParentChart := result;
        inc(j);
      end;

    // Insert the data!
    i := 1;
    while (i <= df2.RowCount) do
    begin
      while (i <= df2.RowCount) and (xvec.AsInteger[i] = xvec.AsInteger[i+1]) do
      begin
        if not yvec.IsMissing[i] then
        begin
          if xvec.IsMissing[i] then
            missings[zvec.AsInteger[i]] := yvec.AsInteger[i]
          else begin
            vals[zvec.AsInteger[i]] := yvec.AsFloat[i];
            cases[zvec.AsInteger[i]] := cases[zvec.AsInteger[i]] + yvec.AsInteger[i];
            maxs[zvec.AsInteger[i]] := xvec.AsString[i];
            if xvec.AsInteger[i] > max then maxts := xvec.AsString[i];
          end;
        end;
        inc(i);
      end;

      if (i <= df2.RowCount) and (not yvec.IsMissing[i]) then
      begin
        if xvec.IsMissing[i] then
          missings[zvec.AsInteger[i]] := yvec.AsInteger[i]
        else begin
          vals[zvec.AsInteger[i]] := yvec.AsFloat[i];
          cases[zvec.AsInteger[i]] := cases[zvec.AsInteger[i]] + yvec.AsInteger[i];
          maxs[zvec.AsInteger[i]] := xvec.AsString[i];
          if xvec.AsInteger[i] > max then maxts := xvec.AsString[i];
        end;
      end;

      for j:=0 to scount-1 do
      begin
        if assigned(series[j]) then
        begin
          if (xvec.IsMissing[i]) then
          begin
            vals[j] := NA_FLOAT;
            continue;
          end;
          if (vals[j] = NA_FLOAT) then
            series[j].AddNullXY(xvec.AsFloat[i], 0, xvec.AsString[i])
          else
            series[j].AddXY(xvec.AsFloat[i], vals[j], xvec.AsString[i]);
          vals[j] := NA_FLOAT;
        end;
      end;
      inc(i);
    end; // end data

    if (xvec is TEpiDateVector) then
    begin
      result.BottomAxis.AxisValuesFormat := '##';
      result.OnGetAxisLabel := BottomDateLabeling;
    end;

    // Result variables.
    total := 0;
    casemis := 0;
    for i:=0 to scount-1 do
      if assigned(series[i]) then
      begin
        dm.AddResult('$case' + inttostr(i), EpiTyInteger, cases[i], 5, 0);
        inc(total, cases[i]);
        inc(casemis, missings[i]);
      end;

    dm.AddResult('$cases', EpiTyInteger, (total+casemis), 5, 0);
    dm.AddResult('$casesMis', EpiTyInteger, (casemis), 5, 0);

    // Number of records = Max number of non-cases:
    noncase := dataframe.rowcount;
    dm.AddResult('$NonCases', EpiTyInteger, (noncase-total-casemis), 5, 0);

    // output table
    if Parameters.VarbyName['NT'] = Nil then
    begin
      OutputTable := dm.CodeMaker.Output.NewTable(6);  // extend to number of groups in var3
      OutputTable.Cell[1,1] := '  ';
      OutputTable.Cell[2,1] := 'Total<br>N';
      OutputTable.Cell[3,1] := 'Cases<br>n';
      OutputTable.Cell[4,1] := '<u>'+YVec.GetVariableLabel(Parameters) + '</u>'+ '<br><small>Missing</small>';
      OutputTable.Cell[5,1] := ' <br>Min';
      OutputTable.Cell[6,1] := ' <br>Max';

      OutputTable.Caption := title;
      for j:=0 to scount-1 do
        if assigned(series[j]) then
        begin
          OutputTable.AddRow;
          OutputTable.Cell[1,OutputTable.RowCount] := zvec.GetValueLabel(inttostr(j), Parameters);
          OutputTable.Cell[2,OutputTable.RowCount] := inttostr(cases[j]+missings[j]);
          OutputTable.Cell[3,OutputTable.RowCount] := inttostr(cases[j]);
          OutputTable.Cell[4,OutputTable.RowCount] := inttostr(missings[j]);
          OutputTable.Cell[5,OutputTable.RowCount] := mins[j];
          OutputTable.Cell[6,OutputTable.RowCount] := maxs[j];
        end;
    end; // show table

    // add line of totals:
    OutputTable.AddRow;
    OutputTable.Cell[1,OutputTable.RowCount] := 'Total';
    OutputTable.Cell[2,OutputTable.RowCount] := inttostr(total+casemis);
    OutputTable.Cell[3,OutputTable.RowCount] := inttostr(total);
    OutputTable.Cell[4,OutputTable.RowCount] := inttostr(casemis);
    OutputTable.Cell[5,OutputTable.RowCount] := mints;
    OutputTable.Cell[6,OutputTable.RowCount] := maxts;

    footnote :=  'Outcome: ' + footnote + '<br>N<sub>non-case</sub>= ' + inttostr(noncase-total-casemis)
       {     +' .<br>' + Varnames[0] + ' : Valid N<sub>case</sub>= ' + inttostr(total) + '    Missing N<sub>case</sub>= ' + inttostr(casemis)};

{ Attempt to find non-cases for dayonset . CANNOT since these are filtered in the prepare dataframe !!
   if noncaseday > 0 then footnote := footnote + '<br> '+ inttostr(noncaseday)
                                            + ' non-missing values for non-cases in ' + df.VectorByName[varnames[0]].Name;
}
    if Parameters.VarbyName['YINC'] <> nil then
      if Parameters.VarbyName['YINC'].Value <> 1 then
        footnote:= footnote + '<br>Cases/Y tick: ' + Parameters.VarbyName['YINC'].Value;

   if Parameters.VarbyName['NT'] <> Nil then  // just show the footnote
       dm.info(footnote, [], 0);

  finally
    if Assigned(df) then FreeAndNil(df);
//    if Assigned(df2) then FreeAndNil(df2);
    if Assigned(varlist) then FreeAndNil(varlist);
    if Assigned(agglist) then FreeAndNil(agglist);
    setlength(vals, 0);
    setlength(series, 0);
  end;
  ODebug.DecIndent;
end;

function TGraph.DoCIPlot(Dataframe: TEpiDataframe; Varnames: TStrings; CmdID: Word; Parameters: TVarList;
 var OutputTable: TStatTable): TChart;

 var
  df: TEpiDataframe;
  agglist: TAggrList;
  i, j, c, sum, numerator, showvalue: integer;
  Low, High, Mid: EpiFloat;
  Series: TCandleSeries;
  ColorLine: TColorLineTool;
  sumtab: TSumTable;
  footnote, s: string;

const
  procname = 'DoCIPlot';
  procversion = '1.0.0.0';

  function addline(Chart: TChart; X: Epifloat; axis : integer = 0):Boolean;
  begin
    try
      colorline := TColorLineTool.Create(Chart);
      colorLine.Value := x;
      if axis = 1 then
      begin
          colorLine.Axis := Chart.LeftAxis;
          colorline.Pen.Color :=  GetGraphColour(0);
      end else
      begin
          ColorLine.Pen.Style := psDot;
          colorLine.Axis := Chart.BottomAxis;
      end;
      result := True;
    except
      result := False;
    end;
  end;

  function CreateTable(var Table : TStatTable; title, variable, Outcome : string): Boolean;
  var
    opt : TEpiOption;
  begin
      Table := dm.CodeMaker.Output.NewTable(7);  // extend to number of groups in var3
      Table.Caption := title;
      Table.Cell[1,1] := 'variable';
      Table.Cell[2,1] := 'stratum';
      Table.Cell[3,1] := 'Total N';
      Table.Cell[4,1] := 'n<u><sub><small>' + variable +'=' + outcome + '</small></u></sub>';
      Table.Cell[5,1] := '%';
      if dm.GetOptionValue('TABLE CI HEADER', Opt) then
        Table.Cell[6,1] := '<font class=ci>' + Opt.value  + '</font>'
      else
        Table.Cell[6,1] := '<font class=ci>(95% CI)</font>';
  end;

  function AddToTable(var Table : TStatTable; Varlabel,valuelabel: string; numerator, sum : Integer;
                      mid, low, high : EpiFloat; Parameters: TVarList): boolean;
  var j : integer;
      found : Boolean;
      opt : TEpiOption;
      cifmt,efmt: String;
  begin
    if (Parameters.VarByName['E1'] <> nil) then efmt := '%8.1f'
    else if (Parameters.VarByName['E0']<> nil) then efmt := '%8.0f'
    else if (Parameters.VarByName['E3']<> nil) then efmt := '%8.3f'
    else if (Parameters.VarByName['E4']<> nil) then efmt := '%8.4f'
    else efmt := '%4.1f';

    // confidence interval formats:
    if dm.GetOptionValue('TABLE CI FORMAT', Opt) then
      cifmt := Opt.value else cifmt := '() - ';       // (lv - uv)

    Table.AddRow;
    found := False;
    for j := 1 to table.RowCount do
      if (pos(Varlabel,Table.Cell[1,j]) = 1) and not Found then found := True;

    if found then  Table.Cell[1,Table.RowCount] := ' '
         else Table.Cell[1,Table.RowCount] := Varlabel;
    Table.Cell[2,Table.RowCount] := valuelabel;
    Table.Cell[3,Table.RowCount] := inttostr(sum);
    Table.Cell[4,Table.RowCount] := inttostr(numerator);
    Table.Cell[5,Table.RowCount] := format(efmt,[100*(numerator/sum)]);
    Table.Cell[6,Table.RowCount] := format(efmt,[low]);
    Table.Cell[7,Table.RowCount] := format(efmt,[high]);
  end;

begin
  ODebug.IncIndent;
  ODebug.Add(UnitName + ':' + procname + ' - ' + procversion, 1);
  sumtab := nil;
  df := nil;
  agglist := nil;

  try
    result := CreateStandardChart();

    agglist := TAggrList.Create();
    agglist.Insert(0, TAggrCount.Create('$S', '', acAll));
    df := OAggregate.AggregateDataframe(dataframe, TStringList(varnames), agglist, TCommand.Create(0, Parameters));

    if not (Parameters.VarExists['NM']) then
      Parameters.AddVar('M', '');
    OTables.Cmd := TCommand.Create(CmdID, Parameters);

    sumtab := OTables.CreateFVTable(df, Varnames);

    c := 0;
    // There will always be at lease one table.
    if (Parameters.VarExists['O']) then
    begin
      showvalue := Parameters.VarByName['O'].AsInteger;
      for j := 1 to sumtab[1].ColumnCount do
        if sumtab[1].ColHeaderValue[j] = showvalue then
          break;
      showvalue := j;
    end else
      Showvalue := sumtab[1].ColumnCount;

    if showvalue > sumtab[1].ColumnCount then
      dm.Error('Observation value not in data set', [], 113013);
    Sum := 0;
    for i := 1 to Sumtab.TableCount do
    begin
      if Sumtab[i].Total > Sum then
      begin
        Sum := Sumtab[i].Total;
        Numerator := Sumtab[i].ColTotal[ShowValue]; 
      end;
    end;
    s := sumtab[1].ColHeaderLabel[Showvalue];
    footnote :='Proportion of ' + dataframe.VectorByName[Varnames[0]].GetVariableLabel(Parameters) +
               ' = ' + s;

    if Parameters.VarExists['TI'] then
      result.Title.Caption := Parameters.VarbyName['TI'].AsString
    else
      result.title.Caption := footnote;

    Series := TCandleSeries.Create(nil);
    Series.UpCloseColor := GetGraphColour(c);
    Series.DownCloseColor := GetGraphColour(c);
    Series.Color := GetGraphColour(c);
    Series.ParentChart := result;
    Series.Title := 'Proportion';

    footnote := '&nbsp;&nbsp;Crude: Proportion of ' +
            dataframe.VectorByName[Varnames[0]].GetVariableLabel(Parameters) + ' = ' +
            s + ' among all.';

    if Parameters.VarbyName['NT'] = Nil then
    begin
      CreateTable(Outputtable, footnote, dataframe.VectorByName[Varnames[0]].GetVariableLabel(Parameters), s);
      OutputTable.Footer := footnote;
    end;

    result.bottomaxis.Title.Caption := '';
    EpiProportionCI(Numerator, Sum, High, Low);
    Mid := Numerator/ Sum;
    if not (Parameters.VarExists['NOTOT']) then
    begin
      if ((100.0*mid+0.15) > 100.0) then
        Series.AddOHLC(c+0.5,(mid*100.0)-0.15, Low*100.0, High*100.0,(mid*100.0))
      else
        Series.AddOHLC(c,(mid*100.0)-0.15, Low*100.0, High*100.0,(mid*100.0)+0.15);
      Series.xLabel[c]:= 'Total';

      // add vertical lines ?
      if (Parameters.VarByName['NL'] = nil) then
      begin
        addline(result,0.75);
        addline(result,-0.75);
        result.BottomAxis.IStartPos := -10;
      end;

      result.bottomaxis.Title.Caption := df.VectorByName[Varnames[0]].name + ' | ';

      if Parameters.VarbyName['NT'] = Nil then
        AddToTable(OutPutTable, dataframe.VectorByName[Varnames[0]].GetVariableLabel(Parameters), 'Total',
                   numerator, sum, mid*100, low*100, high*100, parameters);
      inc(c);
    end;

    //add crude line CI
    if (Parameters.VarByName['NOCI'] = nil) then
    begin
      addline(result, Low*100, 1);
      addline(result, High*100, 1);
    end;

    result.LeftAxis.Title.Caption := 'Percents (%)';

    for i := 1 to sumtab.TableCount do
    begin
      Sum := 0;

      for j := 1 to Sumtab[i].RowCount do
      begin
        Numerator := SumTab[i].Cell[ShowValue, j].N;
        Sum := SumTab[i].RowTotal[j];

        EpiProportionCI(Numerator, Sum, High, Low);
        Mid := Numerator / Sum;
        if ((100.0*mid+0.15) > 100.0) then
          Series.AddOHLC(c,(mid*100.0)-0.15, Low*100.0, High*100.0,(mid*100.0))
        else
          Series.AddOHLC(c,(mid*100.0)-0.15, Low*100.0, High*100.0,(mid*100.0)+0.15);
        Series.xLabel[c]:= SumTab[i].RowHeaderLabel[j];
        if Parameters.VarbyName['NT'] = Nil then
          AddToTable(OutPutTable, SumTab[i].RowHeader, Series.xLabel[c], numerator, sum, mid*100, low*100, high*100, parameters);
        inc(c);
      end;
      result.bottomaxis.Title.Caption := result.bottomaxis.Title.Caption + SumTab[i].RowHeader;
      if i < sumtab.TableCount then
        result.bottomaxis.Title.Caption := result.bottomaxis.Title.Caption  + ' | ';
      if (Parameters.VarByName['NL'] = nil) then addline(result,c-0.5);
    end;
    if Parameters.VarbyName['NT'] <> Nil then dm.info(footnote, [], 0);
  finally
    if Assigned(sumtab) then FreeAndNil(sumtab);
    if assigned(agglist) then FreeAndNil(agglist);
    if Assigned(df) then FreeAndNil(df);
  end;
end;

// draw a cdf and possibly probit plot:
function TGraph.DoCDFPlot(Dataframe: TEpiDataframe; Varnames: TStrings;
                           CmdID: Word; Parameters: TVarList; var OutputTable: TStatTable): TChart;
var
  Total, i, j, k, RowNo: EpiInt;
  xvec, yvec, zvec : TEpiVector;
  df, tdf: TEpiDataframe;
  agglist: TAggrList;
  series:  TCustomSeries;
  varlist: TStrings;
  title, s: string;
  tool: TAnnotationTool;
  PVarDesc: TAnaVariableDescriptor;
const
  procname = 'DoCDFPlot';
  procversion = '1.0.0.0';
begin
  ODebug.IncIndent;
  ODebug.Add(UnitName + ':' + procname + ' - ' + procversion, 1);
  try
    if (Parameters.VarbyName['AGG'] <> nil) and
       (not (dataframe.VectorByName[varnames[0]].DataType in [EpiTyInteger, EpiTyDate])) then
      dm.info('%s: is not integer/date, cannot aggregate', [varnames[0]], 213003);

    if (Parameters.VarbyName['AGG'] <> nil) and
       (dataframe.VectorByName[varnames[0]].DataType in [EpiTyInteger, EpiTyDate]) then
    begin
      agglist := TAggrList.Create();
      agglist.Add(TAggrCount.Create('$S', Varnames[0], acAll));
      df := OAggregate.AggregateDataframe(dataframe, TStringList(varnames), agglist, TCommand.Create(0, Parameters));
      varnames.Move(varnames.Count-1, 0);
      df.Sort(Varnames.CommaText);
      PVarDesc := TAnaVariableDescriptor.CreateLocal('$T',EpiTyFloat,14,6);
      df.NewVector(PVarDesc);
    end else begin
      df := dataframe.PrepareDataframe(varnames, nil, []);
      varnames.Move(varnames.Count-1, 0);
      df.Sort(varnames.CommaText);
      PVarDesc := TAnaVariableDescriptor.CreateLocal('$S',EpiTyInteger,14,0);
      df.NewVector(PVarDesc);
      for i:=1 to df.RowCount do
        df.VectorByName['$S'].asfloat[i] := 1;
      PVarDesc := TAnaVariableDescriptor.CreateLocal('$T',EpiTyFloat,14,6);
      df.NewVector(PVarDesc);
    end;

    // Reverse prior swap to get correct order of varnames.
    varnames.Move(varnames.Count-1, 0);

    result := CreateStandardChart();
    result.Title.Text.Add(dataframe.VectorByName[varnames[0]].GetVariableLabel(Parameters));
    result.BottomAxis.Title.Caption := dataframe.VectorByName[varnames[0]].GetVariableLabel(Parameters);

    varlist := TStringList.Create();
    varlist.Assign(varnames);
    varlist.Add('$S');
    varlist.Add('$T');

    if not Assigned(OutputTable) then
      OutputTable := DM.CodeMaker.Output.NewTable(1,1);
    Outputtable.TableType := sttNormal;

    RowNo := 1;
    k := 0;
    title := '';
    while (RowNo <= df.RowCount) do
    begin
      if Varnames.Count > 1 then
      begin
        // Deselect all.
        for i := 1 to df.RowCount do
          df.Selected[i] := false;
        // Reselect only those wanted.
        for i := rowno to df.RowCount-1 do
          if df.VectorByName[Varnames[1]].compare(i,i+1) <> 0 then
            break
          else
            df.Selected[i] := true;
      end;
      tdf := df.PrepareDataframe(varlist, nil, []);
      inc(RowNo, tdf.RowCount+1);
      
      // From here on:
      // - xvec is the vector for x-axisis.
      // - yvec is the count of x-values (1 of not aggregated)
      // - zvec is the accumulated percentage of yvec (seen so far).
      xvec := tdf.FindVector(varnames[0]);
      yvec := tdf.FindVector('$S');
      zvec := tdf.FindVector('$T');

      total := 0;
      for i:=1 to yvec.Length do
        inc(total, yvec.asInteger[i]);
      if total > 0 then
        zvec.asfloat[1] := (yvec.AsInteger[1]/total)*100
      else
        dm.error('Total = 0', [], 0);
      j := 0;
      for i := 2 to tdf.RowCount-1 do
      begin
       j := j + yvec.AsInteger[i-1];
       zvec.AsFloat[i] := ((j + yvec.AsInteger[i])/total)*100.0 ; // Cum Pct
      end;
      zvec.AsFloat[zvec.Length]:= 100.0;

    // probit instead of pct ?
      if Parameters.VarbyName['P'] <> nil then
      begin
        //convert cumulative pct to probits (halfway values):
        zvec.AsFloat[1] := (zvec.AsFloat[1]/2);
        for i:=2 to tdf.RowCount do
          zvec.AsFloat[i] := zvec.AsFloat[i-1] + (zvec.AsFloat[i]-zvec.AsFloat[i-1])/2.0;
        zvec.AsFloat[1] := NA_FLOAT;
        for i:=2 to tdf.RowCount do
          zvec.AsFloat[i] := pnormalinv(1-zvec.AsFloat[i]/100) ;            //convert probit to fractiles:
      end;
      series := ScatterSeries(xvec, zvec, xvec);
      SetScatter(TPointSeries(Series),k);
      series.ParentChart := result;
      if varnames.Count > 1 then
        s := tdf.FindVector(varnames[1]).GetValueLabel(tdf.FindVector(varnames[1]).AsString[1], Parameters)
      else
        s := tdf.FindVector(varnames[0]).GetVariableLabel(Parameters);
      series.title := s;
      OutputTable.AddRow();
      OutputTable.Cell[1, OutputTable.RowCount] := s + format(': Range: %7.2f - %7.2f ',[xvec.asfloat[1],xvec.asfloat[xvec.Length]]);
      inc(k);
      FreeAndNil(tdf);
    end;
    if (Parameters.VarbyName['AGG'] <> nil) and
       (dataframe.VectorByName[varnames[0]].DataType in [EpiTyInteger, EpiTyDate]) then
    begin
      OutputTable.AddRow();
      OutputTable.Cell[1, OutputTable.RowCount] := 'Aggregate plot.';
    end;
    result.LeftAxis.Title.Caption := 'Cummulative percent (%)';
    if (Parameters.VarbyName['P'] <> nil) then
    begin
      result.LeftAxis.Title.Caption := 'Z (Probit)';
      OutputTable.AddRow();
      OutputTable.Cell[1, OutputTable.RowCount] := ' Normal Probability Plot for Midpoints X=(X<sub>n-1</sub>+(X<sub>n</sub>-X<sub>n-1</sub>)/2).' +
                            ' Plotted at X<sub>2</sub>-X<sub>n</sub>.';
    end;
  finally
    if Assigned(df) then FreeAndNil(df);
    if Assigned(varlist) then FreeAndNil(varlist);
    if Assigned(agglist) then FreeAndNil(agglist);
    if Assigned(PvarDesc) then FreeAndNil(PvarDesc);
  end;
  CalcXInc(result);
  ODebug.DecIndent;
end;


function TGraph.ExcludeInDataframe(var dF: TEpiDataframe; Parameters: TVarList; ExcludeFunc: TExcludeFunction): boolean;
var
  ExclVector: TEpiVector;
  ExclVar: TAnaVariableDescriptor;
  ExpParams: array of integer;
  ExvValue: EpiFloat;
  ExvZero : Boolean;
  i: integer;


  function InExpList(no: integer; Params: array of integer): boolean;
  var
    i: integer;
  begin
    result := false;
    for i := 0 to high(params) do
      if params[i] = no then
        result := true;
  end;

begin
  result := false;

  // Checking for EXP option(s).
  SetLength(ExpParams, 0);
  i := 0;
  while (Parameters.VarByName['EXP'] <> nil) do
  begin
    SetLength(ExpParams, i+1);
    ExpParams[i] := Parameters.VarByName['EXP'].AsInteger;
    Parameters.RemoveVar(Parameters.VarByName['EXP']);
    inc(i);
  end;

  // Checking for EXV option(s).
  ExvValue := MaxExtended;
  if (Parameters.VarByName['EXV'] <> nil) then
    ExvValue := Parameters.VarByName['EXV'].AsFloat;

  if (Parameters.VarByName['EXZ'] <> nil) then
    ExvZero := True else ExvZero := False;

  ExclVar := TAnaVariableDescriptor.CreateLocal('Excluded', df.Vectors[1].DataType, 0, df.Vectors[1].FieldDataDecimals);
  df.NewVector(ExclVar);
  ExclVector := df.VectorByName[ExclVar.Name];

  for i := 1 to df.RowCount do
  begin
    begin
      case df.Vectors[1].DataType of
        EpiTyInteger: begin
                        ExclVector.AsInteger[i] := Floor(R2(ExcludeFunc(i, df),0));
                        if (ExcludeFunc(i, df) >= ExvValue) or
                           //(df.Vectors[2].AsFloat[i] >= ExvValue) or
                           (InExpList(i, ExpParams) or ((ExvZero) and (df.Vectors[1].AsInteger[i] = 0 )) ) then
                        begin
                          df.Vectors[1].AsInteger[i] := NA_INT;
                          result := true;
                        end;
                      end;
        EpiTyFloat:   begin
                        ExclVector.AsFloat[i] := ExcludeFunc(i, df); //df.Vectors[2].AsFloat[i];
                        if (ExcludeFunc(i, df) >= ExvValue) or
                           //(df.Vectors[2].AsFloat[i] >= ExvValue) or
                           (InExpList(i, ExpParams) or ((ExvZero) and (df.Vectors[1].AsFloat[i] = 0.0 )) ) then
                        begin
                          df.Vectors[1].AsFloat[i] := NA_FLOAT;
                          result := true;
                        end;
                      end;
        EpiTyDate:    begin
                        ExclVector.AsDate[i] := Floor(R2(ExcludeFunc(i, df),0)); //(df.Vectors[2].AsDate[i]);
                        if (ExcludeFunc(i, df) >= ExvValue) or
                           //(df.Vectors[2].AsFloat[i] >= ExvValue) or
                           (InExpList(i, ExpParams)) then
                        begin
                          df.Vectors[1].AsDate[i] := NA_DATE;
                          result := true;
                        end;
                      end;
        EpiTyString, EpiTyUppercase,
        EpiTyByte:    dm.Error('Cannot exclude on a string or byte field', [], 113008);
      end; // case
    end;
  end;
end;

procedure TGraph.SPCTest(dataframe: TEpiDataframe; Const CountName: String; CmdID: Word;
                         Parameters: TVarlist; LowIndex, HighIndex: integer;
                         idx: integer; var output: TStatTable);
var
  i, j, z, CountT2, CountT3,
  T1err, T2err, T3err,
  Test3Limit: integer;
  Test2Limit: Integer;
  CtrlVec, V: TEpiVector;
  Last: EpiFloat;
  Trend: Boolean;
  s, t, Mtext: string;

const
  RunArLow: array[0..16] of integer = (4, 4, 5, 5, 6, 6, 6, 7, 7, 8, 8, 9, 9, 9,10,10,10);
  RunArHi: array[0..16] of integer = (11,12,12,13,13,14,15,15,16,16,17,17,18,19,19,20,21);


  // The SPC Test runs through the dataset applied and performs 3 seperate tests.
  // In case a situation is recognized, we insert the anomaly found into its own
  // seperately created vector.
begin
  if (Parameters.VarByName['MTEXT'] <> nil) then Mtext := Parameters.VarByName['MTEXT'].AsString;

  Test2Limit := 8;
  Test3Limit := 6;

  if (Parameters.VarByName['XLABEL'] <> nil) then
    V := dataframe.VectorByName[Parameters.VarByName['XLABEL'].AsString]
  else
    V := dataframe.Vectors[1];

  if (Parameters.VarbyName['NT'] =  nil) and (Parameters.VarbyName['Q'] = nil) then
    output.Cell[1,idx+2] := format('%s - %s', [V.AsString[LowIndex], V.AsString[HighIndex]]);

  V := dataframe.VectorByName[CountName];

  T1err :=0; T2err :=0; T3err :=0; CountT2 :=1; CountT3 :=0;

  if (cmdID = opIChart) then
    z := 5
  else
    z := 4;

  // Test1: (PChart and IChart)
  // Test for values in the Count vector to be outside the limits of
  // to other vectors - here 'UCL' and 'LCL'
  Last := V.AsFloat[1];
  if (cmdID <> opRunChart) then
  begin
    CtrlVec := Dataframe.FindVector('TEST1');
    if CtrlVec = nil then
    begin
      CtrlVec := TEpiFloatVector.Create('TEST1', Dataframe.RowCount);
      dataframe.Vectors.Add(CtrlVec);
    end;
    for i:=LowIndex to HighIndex do
    begin
      if ((not V.IsMissing[i]) and
         (((V.AsFloat[i] < dataframe.VectorByName['LCL'].AsFloat[i]) and (not dataframe.VectorByName['LCL'].IsMissing[i]))
         or
         ((V.AsFloat[i] > dataframe.VectorByName['UCL'].AsFloat[i]) and (not dataframe.VectorByName['UCL'].IsMissing[i])))) then
      begin
        //situation;
        CtrlVec.AsFloat[i] := V.AsFloat[i];
        inc(T1err);
      end;
    end;
  end else
  // Test1: (RunChart)
  // Count the number of runs in the graph. A Run is defined as the
  // number of series of points consecutive on the same side of the
  // median. Extension dec. 5th JL: values on the median are ignored
  begin
    inc(T1Err);
    // avoid points starting on the median:
    j := LowIndex;
    while V.AsFloat[j] = dataframe.VectorByName[Mtext].AsFloat[1] do inc(j);
    last := V.AsFloat[j] ;
    // now start counting runs from point j:
    for i:=j to HighIndex do
    begin
      if (V.IsMissing[i]) then continue;
      if (V.AsFloat[i] = dataframe.VectorByName[Mtext].AsFloat[i]) then continue;  //point on median line
      if (V.AsFloat[i] < dataframe.VectorByName[Mtext].AsFloat[i]) and
         (Last < dataframe.VectorByName[Mtext].AsFloat[i]) then continue
      else
      if (V.AsFloat[i] > dataframe.VectorByName[Mtext].AsFloat[i]) and
         (Last > dataframe.VectorByName[Mtext].AsFloat[i]) then continue;
      if (V.AsFloat[i] = dataframe.VectorByName[Mtext].AsFloat[i]) then
      begin
        last := dataframe.VectorByName[Mtext].AsFloat[i];
        continue;
      end;
      Inc(T1err);
      Last := V.AsFloat[i];
    end;
  end;
  if (Parameters.VarbyName['NT'] =  nil) and (Parameters.VarbyName['Q'] = nil) then
       output.Cell[z-1,idx+2] := inttostr(T1err);

  if CmdID = opRunChart then
  begin
    if ((HighIndex-LowIndex)+1<14) or ((HighIndex-LowIndex)+1>30) then t:= 'na'
    else t := IntToStr(RunArLow[(HighIndex-LowIndex)+1-14]);
    dm.AddResult('$SPC'+ inttostr(idx+1) + 'Runs', EpiTyInteger, t1err, 0, 0);
    dm.AddResult('$SPC'+ inttostr(idx+1) + 'RunEL', EpiTyInteger, t, 0, 0);
    s := t;
    if ((HighIndex-LowIndex)+1<14) or ((HighIndex-LowIndex)+1>30) then t:= 'na'
       else t := IntToStr(RunArHi[(HighIndex-LowIndex)+1-14]);
    if (s <> 'na') then s := '('+ s + '-' + t + ')';
    dm.AddResult('$SPC'+ inttostr(idx+1) + 'RunEH', EpiTyInteger, t, 0, 0);
    if (Parameters.VarbyName['NT'] =  nil) and (Parameters.VarbyName['Q'] = nil) then
      Output.Cell[z,idx+2] := s;
    // Test fail or not ?
    if (HighIndex-LowIndex>13) and (HighIndex-LowIndex<31) then
      if (T1err > RunArHi[(HighIndex-LowIndex)+1-14]) or (T1err < RunArLow[(HighIndex-LowIndex)+1-14]) then
        T1Err := 1
      else
        T1Err := 0;
    if (s <> 'na') and (Parameters.VarbyName['NT'] =  nil) and (Parameters.VarbyName['Q'] = nil) then
      output.Cell[z+1,idx+2] := IntToStr(T1Err);
    inc(z,2);
  end;

  // Test2:
  // Test for consecusive values, from the Count vector, on the same side
  // of a line. Usually either Mean or Median!
  CtrlVec := Dataframe.FindVector('TEST2');
  if CtrlVec = nil then
  begin
    CtrlVec := TEpiFloatVector.Create('TEST2', Dataframe.RowCount);
    dataframe.Vectors.Add(CtrlVec);
  end;
  Last := dataframe.VectorByName[Mtext].AsFloat[LowIndex];
  j := LowIndex; //1;
  for i:=LowIndex to HighIndex do
  begin
    if (V.IsMissing[i]) then continue;
    if (V.AsFloat[i] < dataframe.VectorByName[Mtext].AsFloat[i]) and
       (Last < dataframe.VectorByName[Mtext].AsFloat[i]) then inc(CountT2)
    else
    if (V.AsFloat[i] > dataframe.VectorByName[Mtext].AsFloat[i]) and
       (Last > dataframe.VectorByName[Mtext].AsFloat[i]) then inc(CountT2)
    else
    if (V.AsFloat[i] = dataframe.VectorByName[MText].AsFloat[i]) then
      continue
    else
    begin
      // Now we are no longer on the same side of median/mean... maybe we have
      // a situation.
      if CountT2 >= Test2Limit then
      begin
        //Situation;
        while j <= i-1 do
        begin
          if not (V.AsFloat[j] = dataframe.VectorByName[MText].AsFloat[i-1]) then
            CtrlVec.AsFloat[j] := V.AsFloat[j];
          inc(j);
        end;
        inc(T2Err);
      end;
      CountT2 := 1;
      j := i;
    end;
    Last := V.AsFloat[i];
  end;
  // It might have been that this series was the last in the dataset.
  if CountT2 >= Test2Limit then
  begin
    while j <= i-1 do
    begin
      if not (V.AsFloat[j] = dataframe.VectorByName[MText].AsFloat[i-1]) then
        CtrlVec.AsFloat[j] := V.AsFloat[j];
      inc(j);
    end;
    inc(T2Err);
  end;

  if (Parameters.VarbyName['NT'] =  nil) and (Parameters.VarbyName['Q'] = nil) then
    Output.Cell[z, idx+2] := inttostr(T2err);

  // Test3:
  // A trend test, to see if the Count Vector has a series of either rising
  // or falling values.
  CtrlVec := Dataframe.FindVector('TEST3');
  if CtrlVec = nil then
  begin
    CtrlVec := TEpiFloatVector.Create('TEST3', Dataframe.RowCount);
    dataframe.Vectors.Add(CtrlVec);
  end;
  Last := V.AsFloat[LowIndex];
  Trend := (V.AsFloat[LowIndex] < V.AsFloat[LowIndex+1]);
  j := LowIndex-1; //0;
  for i:=LowIndex+1 to HighIndex do
  begin
    if (V.IsMissing[i]) then continue;
    if (Last > V.AsFloat[i]) and Trend then dec(CountT3)
    else
    if (Last < V.AsFloat[i]) and (not Trend) then inc(CountT3)
    else
    if (V.AsFloat[i] = Last) then continue
    else begin
      if abs(CountT3) >= Test3Limit then
      begin
        //Situation;
        while (j<=i-1) do
        begin
          CtrlVec.AsFloat[j] := V.AsFloat[j];
          inc(j);
        end;
        inc(T3Err);
      end;
      if (Last > V.AsFloat[i]) then CountT3 := -2 else CountT3 := 2;
      j := i-1;
    end;
    Trend := (Last > V.AsFloat[i]);
    Last := V.AsFloat[i];
  end;
  // It might have been that this series was the last in the dataset.
  if abs(CountT3) >= Test3Limit then
  begin
    //Situation;
    while (j<=i-1) do
    begin
      CtrlVec.AsFloat[j] := V.AsFloat[j];
      inc(j);
    end;
    inc(T3Err);
  end;

  if (Parameters.VarbyName['NT'] =  nil) and (Parameters.VarbyName['Q'] = nil) then
    Output.Cell[z+1, idx+2] := inttostr(T3err);

  s := '$spc'+inttostr(idx+1);
  dm.AddResult(s+'test1', EpiTyInteger, T1err, 0, 0);
  dm.AddResult(s+'test2', EpiTyInteger, T2err, 0, 0);
  dm.AddResult(s+'test3', EpiTyInteger, T3err, 0, 0);
end;

function ExcludeFuncIChart(index: integer; df: TEpiDataframe): EpiFloat;
begin
  result := df.Vectors[1].AsFloat[index];
end;

function TGraph.DoIchart(Dataframe: TEpiDataframe; Varnames: TStrings; CmdID: Word; Parameters: TVarList;
                  var SPCDataframe: TEpiDataframe; var OutputTable: TStatTable; var footnote:string): TChart;
var
  Breaks: TArrayVariant;
  excluded: boolean;
  mtext, s, t: string;
  xvec, yvec, lvec,
  tvec1, tvec2: TEpiVector;
  i, j, k, l, rc,
  ix, offset: integer;
  indexlow, indexhigh: cardinal;
  mean, sum, E, w: EpiFloat;
  lowval, highval: variant;

  SExclude, SMean, SUcl, SLcl, SYvec: TLineSeries;
  STest1, STest2, STest3: TPointSeries;

const
  procname = 'DoIchart';
  procversion = '1.0.0.0';
begin
// THIS IS HOW THE CALCULATION WORKS:
(*
means lege
gen mean = $mean1

sort tid
* nu beregnes moving ranges
gen mr = abs(lege - lege(recnumber-1))
if recnumber = 1 then mr = 0

* find summen af mr
means mr
define E ####.##### global
e = 2.66*($total1/($obs1-1))
gen ucl = mean+e
gen lcl= mean-e
set echo=on
*echo Mean = @mean Upper Control Limit = @ucl  Lower Control limit = @lcl
ichart tid lege mean ucl
*)
  ODebug.Add(UnitName + ':' + procname + ' - ' + procversion, 1);

  if ((varnames.count <> 3) and (Parameters.VarByName['XLABEL'] <> nil)) or
     ((varnames.count <> 2) and (Parameters.VarByName['XLABEL'] = nil)) then
    dm.Error('Always %d variables for %s!', [2,'DoIchart'], 113009);

  // Create the graph and set customizable settings.
  result := CreateStandardChart();
  SPCDataframe := dataframe.prepareDataframe(varnames, nil);
  Breaks := FindBreak(Parameters, SPCDataframe.VectorByName[varnames[0]]);
  SPCDataframe.Sort(Varnames[0]);

  mtext := 'Mean';
  if cmdID = opRunChart then mtext := 'Median';
  Parameters.AddVar('MTEXT', mtext);

  if cmdID = opIChart then
    dm.AddResult('$spctype', EpiTyString, 'IChart', 0, 0)
  else
    dm.AddResult('$spctype', EpiTyString, 'RunChart', 0, 0);
  dm.AddResult('$spccenter', EpiTyString, mtext, 0, 0);
  dm.AddResult('$spcbreak', EpiTyInteger, high(breaks)+1, 0, 0);

  xvec := SPCDataframe.VectorByName[varnames[0]];
  yvec := SPCDataframe.VectorByName[varnames[1]];
  excluded := ExcludeInDataframe(SPCDataframe, Parameters, ExcludeFuncIChart);

  if (Parameters.VarByName['XLABEL'] <> nil) then
    lvec := SPCDataframe.VectorByName[Parameters.VarByName['XLABEL'].AsString]
  else
    lvec := xvec;

  if CmdID = opRunChart then
  begin
    result.Title.Text.Add('RunChart - ' + yvec.GetVariableLabel(Parameters));
    if (Parameters.VarbyName['NT'] = nil) and (Parameters.VarbyName['T'] <> nil) and (Parameters.VarbyName['Q'] = nil) then
      OutputTable := dm.OutputList.NewTable(7,Length(breaks)+2);
  end else begin
    result.Title.Text.Add('IChart - ' + yvec.GetVariableLabel(Parameters));
    if  (Parameters.VarbyName['NT'] = nil) and (Parameters.VarbyName['T'] <> nil) and (Parameters.VarbyName['Q'] = nil) then
      OutputTable := dm.OutputList.NewTable(6,Length(breaks)+2)
  end;
  if (Parameters.VarByName['TI'] <> nil) then
  begin
    result.Title.Text.Clear;
    result.Title.Text.Add(Parameters.VarByName['TI'].AsString)
  end;

  if  (Parameters.VarbyName['NT'] = nil) and (Parameters.VarbyName['T'] <> nil ) and (Parameters.VarbyName['Q'] = nil) then
  begin
    OutputTable.TableType := sttNormal;
    i := 5;
    OutputTable.Cell[1,1] := 'Period:';
    OutputTable.Cell[2,1] := 'Centervalue:';
    if CmdID = opIchart then
    begin
      OutputTable.Cell[3,1] :=  'Limits:';
      OutputTable.Cell[4,1] :=  'Test 1:'
    end else begin {opRunChart}
      OutputTable.Cell[3,1] := 'Runs:';
      OutputTable.Cell[4,1] := 'Limit:';
      OutputTable.Cell[5,1] := 'Test 1:';
      i := 6;
    end;
    OutputTable.Cell[i,1] := 'Test 2:';
    OutputTable.Cell[i+1,1] :=  'Test 3:';
  end;

  if excluded then
  begin
    SExclude := TLineSeries.Create(result);
    SExclude.Title := 'Excluded';
    SExclude.LinePen.Width := 2;
    SExclude.Color := clBlue;
    SExclude.LinePen.Style := psDot;
    SExclude.Pointer.Visible := true;
    SExclude.Pointer.Style := psCircle;
    SExclude.Pointer.Color := clWhite;
    SExclude.Pointer.Brush.Style := bsClear;
    SExclude.ParentChart := result;
  end;
  SMean := TLineSeries.Create(result);
  SMean.Title := mtext;
  SMean.LinePen.Width := 2;
  SMean.Color := clGreen;
  SMean.ParentChart := result;
  SUcl := TLineSeries.Create(result);
  SUcl.Title := 'UCL';
  SUcl.LinePen.Width := 2;
  SUcl.Color := clRed;
  SUcl.ParentChart := result;
  SLcl := TLineSeries.Create(result);
  SLcl.Title := 'LCL';
  SLcl.LinePen.Width := 2;
  SLcl.Color := clRed;

  SYvec := TLineSeries.Create(result);
  SYvec.Title := yvec.GetVariableLabel(Parameters);
  SYvec.LinePen.Width := 2;
  SYvec.Color := clBlue;
  SYvec.Pointer.Visible := true;
  SYvec.Pointer.Style := psCircle;
  SYvec.Pointer.Brush.Style := bsSolid;
  if (Parameters.VarbyName['YVALUE'] <> nil) then
  with SYvec do begin
    Marks.Visible := true;
    Marks.Style := smsValue;
    Marks.Color := clWhite;
  end;

  // Create vectors:
  SPCDataframe.Vectors.Add(TEpiFloatVector.Create(mtext, dataframe.RowCount));
  // add limits for ichart:
  if CmdID <> opRunChart then
  begin
    SPCDataframe.Vectors.Add(TEpiFloatVector.Create('LCL', dataframe.RowCount));
    SPCDataframe.Vectors.Add(TEpiFloatVector.Create('UCL', dataframe.RowCount));
  end;

  t := '';
  for i := 0 to high(breaks)+1 do
  begin

    // Find current period.
    if i=0 then lowval := xvec.AsInteger[1] -1 else lowval := breaks[i-1];
    if i=high(breaks)+1 then highval := xvec.AsInteger[xvec.length]+1 else highval := breaks[i];

    // deselect all records:
    for j := 1 to SPCDataframe.RowCount do
      SPCDataframe.Selected[j] := false;

    // now select all in current breakperiod:
    for j := 1 to SPCDataframe.RowCount do
      if (xvec.AsInteger[j] >= lowval) and (xvec.AsInteger[j] < highval) then
        SPCDataframe.Selected[j] := true;

    // Not enough data for calculating LCL, UCL, etc.
    rc := SPCDataframe.SelectedRowCount;
    if rc<2 then
      dm.Error('Period too short: %d <br> (see breaks ? - Date as time variable ?)', [rc], 113010);

    SPCDataframe.Sort(xvec.Name);

    for j := 1 to dataframe.RowCount do
      if SPCDataframe.Selected[j] then break;
    indexlow := j;
    for j := dataframe.RowCount downto 1 do
      if SPCDataframe.Selected[j] then break;
    indexhigh := j;

    // Means (of the second variable)
    mean := 0;
    k := 0;
    for j := indexlow to indexhigh do
      if not yvec.IsMissing[j] then
      begin
        mean := mean + yvec.AsFloat[j];
        inc(k);
      end;
    mean := mean / k;
    if cmdID = opRunChart then
    begin
      // Sort by 2nd var to find median.
      // This must be done only on selected data!
      SPCDataframe.Sort(yvec.Name);

      {dm.info(format('sorted: %d',[i]));
      for j := indexlow to indexhigh do
            dm.info(yvec.Asstring[j]);
      }

      w:= (k+1)/2;
      ix := max(trunc((k+1)/2),1);
      offset := 0;
      j := 0;
      while (j < ix) do
      begin
        if not yvec.IsMissing[offset+indexlow] then
          inc(j);
        inc(offset);
      end;
      if w = ix then
        mean := yvec.AsFloat[offset+indexlow-1]
      else
        mean := yvec.AsFloat[offset+indexlow-1] +
          (yvec.AsFloat[offset+indexlow]-yvec.AsFloat[offset+indexlow-1])*(w-ix);  // Bland Medical Statistics p 55
    end;
    SPCDataframe.Sort(xvec.Name);
    tvec1 := SPCDataframe.VectorByName[mtext];
    for j := indexlow to indexhigh do
      tvec1.AsFloat[j] := mean;
    dm.AddResult('$spc'+inttostr(i+1)+'center', EpiTyFloat, mean, 0, 15);
    if (Assigned(OutputTable)) then
      OutputTable.Cell[2,i+2] := format('%.2f', [Mean]);

    sum := 0;
    offset := 1;
    for j := indexlow+1 to indexhigh do
      if (not yvec.IsMissing[j]) then
      begin
        if (not yvec.IsMissing[offset]) then
        begin
          sum := sum + Abs(yvec.AsFloat[j] - yvec.AsFloat[offset]);
        end;
        offset := j;
      end;

    // Gen ucl, lcl (generate ucl, lcl)
    if CmdID <> opRunChart then
    begin
      E := 2.66 * (sum / (k - 1));
      tvec1 := SPCDataframe.VectorByName['UCL'];
      tvec1.FieldDataDecimals := 3;
      tvec2 := SPCDataframe.VectorByName['LCL'];
      tvec2.FieldDataDecimals := 3;
      for j := indexlow to indexhigh do
      begin
        tvec1.AsFloat[j] := mean + E;
        tvec2.AsFloat[j] := mean - E;
        if ((mean - E) < 0) and
           (Parameters.VarByName['NEGLCL'] = nil) then
           tvec2.AsFloat[j] := NA_FLOAT;
      end;
    end; // end excl. runchart

      // Only for IChart.
    if cmdID = opIChart then
    begin
      s := '$spc' + inttostr(i+1);
      dm.AddResult(s+'UCL', EpiTyFloat,(mean + E) , 0, 15);
      if (tvec2.AsFloat[indexlow] <> NA_FLOAT) then dm.AddResult(s+'LCL', EpiTyFloat, (mean - E), 0, 15);
      if (Assigned(OutputTable)) then
      begin
        s := 'UCL: ' + format('%.2f', [Mean+E]);
        if (tvec2.AsFloat[indexlow] <> NA_FLOAT) then
          s := 'LCL: ' + format('%.2f', [Mean-E]) + ' ' + s;
        OutputTable.Cell[3,i+2] := s;
      end;
    end;

    if i > 0 then t := t + ' | ';
    t := t + format(Mtext + ': %.2f', [Mean]);
    if (cmdID = opIChart) then
    begin
      t := t + format(' UCL: %.2f', [tvec1.AsFloat[indexlow]]);
      if not (tvec2.IsMissing[indexlow]) then t := t + format(' LCL: %.2f', [tvec2.AsFloat[indexlow]]);
    end;

    for j := indexlow to indexhigh do
    begin
      if excluded then SExclude.AddXY(xvec.AsFloat[j], SPCDataframe.VectorByName['Excluded'].AsFloat[j], lvec.AsString[j]);
      if not yvec.IsMissing[j] then SYvec.AddXY(xvec.AsFloat[j], yvec.AsFloat[j], lvec.AsString[j]);
      SMean.AddXY(xvec.AsFloat[j], mean, lvec.AsString[j]);

      if (cmdID <> opRunChart) then
        begin
          SUcl.AddXY(xvec.AsFloat[j], tvec1.AsFloat[j], lvec.AsString[j]);
          if not tvec2.IsMissing[j] then SLcl.AddXY(xvec.AsFloat[j], tvec2.AsFloat[j], lvec.AsString[j]);
        end;
    end;
    dec(j);
    if excluded then SExclude.AddNullXY(xvec.AsFloat[j], mean, lvec.AsString[j]);
    SYvec.AddNullXY(xvec.AsFloat[j], mean, lvec.AsString[j]);
    SMean.AddNullXY(xvec.AsFloat[j], mean, lvec.AsString[j]);
    if (cmdID <> opRunChart) then
      begin
      SUcl.AddNullXY(xvec.AsFloat[j], mean, lvec.AsString[j]);
      if not tvec2.IsMissing[j] then SLcl.AddNullXY(xvec.AsFloat[j], mean, lvec.AsString[j]);
    end;
    if Assigned(Parameters.VarbyName['T']) then
      SPCTest(SPCDataframe, Varnames[1], CmdID, Parameters, indexlow, indexhigh, i, OutputTable);
  end;     // end breakpoint

  if (Parameters.VarbyName['T'] <> nil) then
  begin
    if CmdID = opIchart then
    begin
      STest1 := PointSeries(xvec, SPCDataframe.VectorByName['TEST1'], xvec );
      STest1.Pointer.Visible :=  true;
      STest1.Pointer.HorizSize := 7;
      STest1.Pointer.VertSize := 7;
      STest1.Pointer.Brush.Style := bsClear;
      STest1.Pointer.Style := psCircle;
      STest1.Color := clWhite;
      STest1.ShowInLegend := false;
      STest1.ParentChart := result;
    end;
    STest2 :=  PointSeries(xvec, SPCDataframe.VectorByName['TEST2'], xvec );
    STest2.Pointer.Visible :=  true;
    STest2.Pointer.HorizSize := 6;
    STest2.Pointer.VertSize := 6;
    STest2.Pointer.Brush.Style := bsClear;
    STest2.Pointer.Style := psRectangle;
    STest2.Color := clWhite;
    STest2.ShowInLegend := false;
    STest2.ParentChart := result;
    STest3 :=  PointSeries(xvec, SPCDataframe.VectorByName['TEST3'], xvec );
    STest3.Pointer.Visible :=  true;
    STest3.Pointer.HorizSize := 8;
    STest3.Pointer.VertSize := 8;
    STest3.Pointer.Brush.Style := bsClear;
    STest3.Pointer.Style := psDiamond;
    STest3.Color := clWhite;
    STest3.ShowInLegend := false;
    STest3.ParentChart := result;
  end;

  if (cmdID <> opRunChart) then
      if not tvec2.IsMissing[1] then SLcl.ParentChart := result;
  SYvec.ParentChart := result;

  if not Assigned(Parameters.VarByName['NOCI']) then
  begin
    Result.SubFoot.Text.Add(t);
    Result.SubFoot.Visible := true;
  end;
  result.BottomAxis.Title.Caption := xvec.GetVariableLabel(Parameters);
end;

function ExcludeFuncPChart (index: integer; df: TEpiDataframe): EpiFloat;
begin
  result := (df.Vectors[1].AsFloat[index] / df.Vectors[2].AsFloat[index]) * 100;
end;

function TGraph.DoPchart(Dataframe: TEpiDataframe; Varnames: TStrings; CmdID: Word; Parameters: TVarList;
                  var SPCDataframe: TEpiDataframe; var OutputTable: TStatTable; var footnote:string): TChart;
var
  Breaks: TArrayVariant;
  excluded: boolean;
  mtext, s, t: string;
  xvec, yvec, zvec, lvec,
  tvec1, tvec2, tvec3: TEpiVector;
  i, j, k, l, rc,
  ix, offset: integer;
  indexlow, indexhigh: cardinal;
  mean, sum, E, w, T1, T2: EpiFloat;
  lowval, highval: variant;

  SExclude, SMean, SUcl, SLcl, Sp: TLineSeries;
  STest1, STest2, STest3: TPointSeries;

const
  procname = 'DoPchart';
  procversion = '1.0.0.0';
begin
// THIS IS HOW THE CALCULATION WORKS:
// Varnames:                  0    1     2
// The call will be: PChart time count total
// Example:          PChart tid  lege  dagtotal
(*
define p ####.#####
p=lege/dagtotal
means lege
gen lege1 = $total1

means dagtotal
gen pbar=lege1/$total1


*gen mean = $mean1
define ucl #####.#####
define lcl #####.#####
ucl = pbar + 3*(sqrt((pbar*(1-pbar))/dagtotal))
lcl = pbar - 3*(sqrt((pbar*(1-pbar))/dagtotal))

lcl = lcl*100
ucl = ucl * 100
p = p*100
pbar = pbar*100

pchart tid p pbar ucl lcl
*)
  ODebug.Add(UnitName + ':' + procname + ' - ' + procversion, 1);

  if ((varnames.count <> 4) and (Parameters.VarByName['XLABEL'] <> nil)) or
     ((varnames.Count <> 3) and (Parameters.VarByName['XLABEL'] = nil)) then
    dm.Error('Always %d variables for %s!', [3, 'DoPChart'], 113009);

  // Create the graph and set customizable settings.
  result := CreateStandardChart();
  SPCDataframe := dataframe.prepareDataframe(varnames, nil);
  Breaks := FindBreak(Parameters, SPCDataframe.VectorByName[varnames[0]]);
  SPCDataframe.Sort(Varnames[0]);

  mtext := 'Mean';
  Parameters.AddVar('MTEXT', mtext);
  dm.AddResult('$spctype', EpiTyString, 'PChart', 0, 0);
  dm.AddResult('$spccenter', EpiTyString, mtext, 0, 0);
  dm.AddResult('$spcbreak', EpiTyInteger, high(breaks)+1, 0, 0);

  xvec := SPCDataframe.VectorByName[varnames[0]];
  yvec := SPCDataframe.VectorByName[varnames[1]];
  zvec := SPCDataframe.VectorByName[varnames[2]];
  excluded := ExcludeInDataframe(SPCDataframe, Parameters, ExcludeFuncPChart);

  if (Parameters.VarByName['XLABEL'] <> nil) then
    lvec := SPCDataframe.VectorByName[Parameters.VarByName['XLABEL'].AsString]
  else
    lvec := xvec;

  if (Parameters.VarByName['TI'] <> nil) then
    result.Title.Text.Add(Parameters.VarByName['TI'].AsString)
  else
    result.Title.Text.Add('PChart - ' + yvec.GetVariableLabel(Parameters) +
                          ' / ' + zvec.GetVariableLabel(Parameters));

  if (Parameters.VarbyName['T'] <> nil) and (Parameters.VarbyName['NT'] = nil) and (Parameters.VarbyName['Q'] = nil) then
  begin
    OutputTable := dm.OutputList.NewTable(5, Length(breaks)+2);
    OutputTable.TableType := sttNormal;
    OutputTable.Cell[1,1] := 'Period:';
    OutputTable.Cell[2,1] := 'Centervalue:';
    OutputTable.Cell[3,1] := 'Test 1:';
    OutputTable.Cell[4,1] := 'Test 2:';
    OutputTable.Cell[5,1] := 'Test 3:';
  end;

  if excluded then
  begin
    SExclude := TLineSeries.Create(result);
    SExclude.Title := 'Excluded';
    SExclude.LinePen.Width := 2;
    SExclude.Color := clBlue;
    SExclude.LinePen.Style := psDot;
    SExclude.Pointer.Visible := true;
    SExclude.Pointer.Style := psCircle;
    SExclude.Pointer.Color := clWhite;
    SExclude.Pointer.Brush.Style := bsClear;
    SExclude.ParentChart := result;
  end;
  SMean := TLineSeries.Create(result);
  SMean.Title := mtext;
  SMean.LinePen.Width := 2;
  SMean.Color := clGreen;
  SMean.ParentChart := result;
  SUcl := TLineSeries.Create(result);
  SUcl.Title := 'UCL';
  SUcl.LinePen.Width := 2;
  SUcl.Color := clRed;
  SUcl.Stairs := true;
  SUcl.ParentChart := result;
  SLcl := TLineSeries.Create(result);
  SLcl.Title := 'LCL';
  SLcl.LinePen.Width := 2;
  SLcl.Stairs := true;
  SLcl.Color := clRed;

  SP := TLineSeries.Create(result);
  SP.Title := 'P';
  SP.LinePen.Width := 2;
  SP.Color := clBlue;
  SP.Pointer.Visible := true;
  SP.Pointer.Style := psCircle;
  SP.Pointer.Brush.Style := bsSolid;
  if (Parameters.VarbyName['YVALUE'] <> nil) then
  with SP do begin
    Marks.Visible := true;
    Marks.Style := smsValue;
    Marks.Color := clWhite;
  end;

  // Create vectors:
  SPCDataframe.Vectors.Add(TEpiFloatVector.Create(mtext, dataframe.RowCount));
  SPCDataframe.Vectors.Add(TEpiFloatVector.Create('P', dataframe.RowCount));
  SPCDataframe.Vectors.Add(TEpiFloatVector.Create('LCL', dataframe.RowCount));
  SPCDataframe.Vectors.Add(TEpiFloatVector.Create('UCL', dataframe.RowCount));

  t := '';
  for i := 0 to high(breaks)+1 do
  begin
    // Find current period.
    if i=0 then lowval := xvec.AsInteger[1] -1 else lowval := breaks[i-1];
    if i=high(breaks)+1 then highval := xvec.AsInteger[xvec.length]+1 else highval := breaks[i];
    for j := 1 to SPCDataframe.RowCount do
      if (xvec.AsInteger[j] >= lowval) and (xvec.AsInteger[j] < highval) then
        SPCDataframe.Selected[j] := true
      else
        SPCDataframe.Selected[j] := false;

    // Not enough data for calculating LCL, UCL, etc.
    rc := SPCDataframe.SelectedRowCount;
    if rc<2 then
      dm.Error('SPC Period too short (breaks ?)', [], 113011 );

    SPCDataframe.Sort(xvec.Name);

    for j := 1 to dataframe.RowCount do
      if SPCDataframe.Selected[j] then break;
    indexlow := j;
    for j := dataframe.RowCount downto 1 do
      if SPCDataframe.Selected[j] then break;
    indexhigh := j;

    yvec := SPCDataframe.VectorByName[varnames[1]];

    // Generate P
    tvec1 := SPCDataframe.VectorByName['P'];
    tvec2 := SPCDataframe.VectorByName['excluded'];
    for j := indexlow to indexhigh do
    begin
      if yvec.IsMissing[j] then
      begin
        tvec1.AsFloat[j] := NA_FLOAT;
        continue;
      end else
        tvec1.AsFloat[j] := (yvec.AsFloat[j] / zvec.AsFloat[j]) * 100;
      if yvec.AsFloat[j] > zvec.AsFloat[j] then
        dm.Error('Record no. %d has a ''count'' value greater than ''total''', [j], 113012);
    end;

    // Generate mean, ucl, lcl
    // Notice this is NOT the same as mean(sum(Pi)/n), where Pi= CountVec.AsFloat[i]/TotalVec.AsFloat[i]
    // PBar is assumed as the overall percentage taken as sum of outcome divided by all observations
     // Means (of the percentages)
    mean := 0;
    k := 0;
    for j := indexlow to indexhigh do
      begin
        if ((yvec.IsMissing[j]) or (zvec.Ismissing[j])) then Continue;
        mean := mean + yvec.AsFloat[j]/zvec.AsFloat[j];
        inc(k);
      end;
    mean := mean / k;
    tvec1 := SPCDataframe.VectorByName['UCL'];
    tvec2 := SPCDataframe.VectorByName['LCL'];
    tvec3 := SPCDataframe.VectorByName[mtext];
    for j := indexlow to indexhigh do
    begin
      if yvec.IsMissing[j] then continue;
      tvec3.AsFloat[j] := mean * 100;
      if zvec.AsFloat[j] > 0 then
       begin
         tvec1.AsFloat[j] := (mean + 3*(sqrt((mean * (1 - mean)) / zvec.AsFloat[j]))) * 100;
         tvec2.AsFloat[j] := (mean - 3*(sqrt((mean * (1 - mean)) / zvec.AsFloat[j]))) * 100;
       end;
      if (tvec2.AsFloat[j] < 0) and (Parameters.VarbyName['NEGLCL'] = nil)
        then tvec2.AsFloat[j] := NA_FLOAT;
    end;

    s := '$spc' + inttostr(i+1);
    dm.AddResult(s+'center', EpiTyFloat,(mean*100), 0, 15);
    if (Assigned(OutputTable)) then
      OutputTable.Cell[2,i+2] := format('%.2f', [(mean*100)]);

    if i > 0 then t := t + ' | ';
      t := t + format(Mtext + ': %.2f', [(mean*100)]);

    yvec := SPCDataframe.VectorByName['P'];
    for j := indexlow to indexhigh do
    begin
      if excluded then SExclude.AddXY(xvec.AsFloat[j], SPCDataframe.VectorByName['Excluded'].AsFloat[j], lvec.AsString[j]);
      if not yvec.IsMissing[j] then SP.AddXY(xvec.AsFloat[j], yvec.AsFloat[j], lvec.AsString[j]);
      SMean.AddXY(xvec.AsFloat[j], (mean*100), lvec.AsString[j]);
      if not tvec1.IsMissing[j] then SUcl.AddXY(xvec.AsFloat[j]-0.5, tvec1.AsFloat[j], lvec.AsString[j]);
      if not tvec2.IsMissing[j] then SLcl.AddXY(xvec.AsFloat[j]-0.5, tvec2.AsFloat[j], lvec.AsString[j]);
    end;
    dec(j);
    if excluded then SExclude.AddNullXY(xvec.AsFloat[j], (mean*100), lvec.AsString[j]);

    // add last ucl + lcl once more to get the level correct
    if not tvec1.IsMissing[j] then SUcl.AddXY(xvec.AsFloat[j]+0.5, tvec1.AsFloat[j], ' ');
    if not tvec2.IsMissing[j] then SLcl.AddXY(xvec.AsFloat[j]+0.5, tvec2.AsFloat[j], ' ');
    SMean.AddXY(xvec.AsFloat[j]+0.5, (mean*100), ' ' );

    SP.AddNullXY(xvec.AsFloat[j]+0.5, (mean*100), lvec.AsString[j]);
    SMean.AddNullXY(xvec.AsFloat[j]+0.5, (mean*100), lvec.AsString[j]);
    SUcl.AddNullXY(xvec.AsFloat[j]+0.5, (mean*100), lvec.AsString[j]);
    SLcl.AddNullXY(xvec.AsFloat[j]+0.5, (mean*100), lvec.AsString[j]);

    if (Parameters.VarbyName['T'] <> nil) then
      SPCTest(SPCDataframe, 'P', CmdID, Parameters, indexlow, indexhigh, i, OutputTable);
  end;

  if (Parameters.VarbyName['T'] <> nil) then
  begin
    STest1 := PointSeries(xvec, SPCDataframe.VectorByName['TEST1'], xvec );
    STest1.Pointer.Visible :=  true;
    STest1.Pointer.HorizSize := 7;
    STest1.Pointer.VertSize := 7;
    STest1.Pointer.Brush.Style := bsClear;
    STest1.Pointer.Style := psCircle;
    STest1.Color := clWhite;
    STest1.ShowInLegend := false;
    STest1.ParentChart := result;
    STest2 :=  PointSeries(xvec, SPCDataframe.VectorByName['TEST2'], xvec );
    STest2.Pointer.Visible :=  true;
    STest2.Pointer.HorizSize := 6;
    STest2.Pointer.VertSize := 6;
    STest2.Pointer.Brush.Style := bsClear;
    STest2.Pointer.Style := psRectangle;
    STest2.Color := clWhite;
    STest2.ShowInLegend := false;
    STest2.ParentChart := result;
    STest3 :=  PointSeries(xvec, SPCDataframe.VectorByName['TEST3'], xvec );
    STest3.Pointer.Visible :=  true;
    STest3.Pointer.HorizSize := 8;
    STest3.Pointer.VertSize := 8;
    STest3.Pointer.Brush.Style := bsClear;
    STest3.Pointer.Style := psDiamond;
    STest3.Color := clWhite;
    STest3.ShowInLegend := false;
    STest3.ParentChart := result;
  end;
  if not tvec2.IsMissing[1] then SLcl.ParentChart := result;
  SP.ParentChart := result;

  if not Assigned(Parameters.VarByName['NCVI']) then
  begin
    Result.SubFoot.Text.Add(t);
    Result.SubFoot.Visible := true;
  end;
  result.BottomAxis.Title.Caption := xvec.GetVariableLabel(Parameters);
end;

function TGraph.DoXChart(Dataframe: TEpiDataframe; Varnames: TStrings; Cmd: TCommand): boolean;
var
  IDf: TEpiDataframe;
  Tab: TStatTable;
  Chart: TChart;
  Charts: Array of TChart;
  path1,path2 : string;
  opt: TEpiOption;
  footnote: string;
begin
  SetLength(Charts, 2);
  Chart := DoIchart(Dataframe, varnames, opIChart, cmd.ParameterList, IDf, Tab,footnote);
  CommonChartOptions(Chart, cmd);

  // testing xchart specific options: top graph
  chart.BottomAxis.Visible:=false;
  //chart.Title.Caption := 'Experimenting with double graphs - ichart top - runchart bottom';
  chart.Foot.Visible := false;
  path1 := SaveChart(chart, cmd);
  Charts[0] := chart;
  Chart := DoIchart(Dataframe, varnames, opRunChart, cmd.ParameterList, IDf, Tab, footnote);
  CommonChartOptions(Chart, cmd);
  // testing xchart specific options: bottoma graph
  chart.Title.Visible:=false;

  path2 := SaveChart(chart, cmd);
  Charts[1] := chart;
  if {(dm.GetOptionValue('GRAPH EDIT', Opt) and (Opt.Value = 'ON')) or}
     (cmd.ParamByName['EDIT'] <> nil) then ShowCharts(charts, cmd);

  if {(dm.GetOptionValue('GRAPH SHOW', Opt) and (Opt.Value = 'ON')) and}
      (cmd.ParamByName['Q'] = nil) and
     (AnsiCompareText(ExtractFileExt(path1),'.png')=0) then
  begin
    tab := dm.OutputList.NewTable(1,2);
    tab.TableType := sttGraph;
    tab.Cell[1,1] := '<img src="'+ path1 +'" ALT="' +path1 + '">';
    tab.Cell[1,2] := '<img src="'+ path2 +'" ALT="' +path2 + '">';
    if ((dm.GetOptionValue('GRAPH FILENAME SHOW', Opt) and (Opt.Value = 'ON'))) then
      dm.CodeMaker.OutputTable(tab,path1 + '<br>' + path2)
    else
      dm.CodeMaker.OutputTable(tab,'');
  end;

   dm.Sendoutput;

  if Dummyform <> nil then FreeAndNil(DummyForm);
end;

function TGraph.DoLifeTable(Dataframe: TEpiDataframe; Varnames: TStrings; CmdID: Word; Parameters: TVarList): TChart;
var
  XVec, TVec, YVec, ZVec,
  CILO, CIHI: TEpiVector;
  i, j: integer;
  LineSeries: TLineSeries;
  CandleSeries: TCandleSeries;


  procedure NewSeries();
  begin
    inc(j);
    LineSeries := TLineSeries.Create(Result);
    LineSeries.Title := ZVec.GetValueLabel(ZVec.AsString[i+1], Parameters);
    LineSeries.Stairs := true;
    LineSeries.Color := GetGraphColour(j);
    LineSeries.AddXY(0, 1);
    if (not Parameters.VarExists['NOCI']) then
    begin
      CandleSeries := TCandleSeries.Create(Result);
      CandleSeries.Title := 'CI for ' + ZVec.GetValueLabel(ZVec.AsString[i+1], Parameters);
      CandleSeries.HighLowPen.Color := GetGraphColour(j);
      CandleSeries.ShowInLegend := false;
    end;
  end;

const
  procname = 'DoLifeTable';
  procversion = '1.0.0.0';
begin
  ODebug.IncIndent;
  ODebug.Add(UnitName + ':' + procname + ' - ' + procversion, 1);
  try
    result := CreateStandardChart();
    result.LeftAxis.Automatic := false;
    result.LeftAxis.Minimum := 0;
    result.LeftAxis.Maximum := 1;

    XVec := Dataframe.VectorByName['$INTERBEG'];
    TVec := DataFrame.VectorByName['$PRSURV'];
    YVec := Dataframe.VectorByName['$CMPRSURV'];

    if Parameters.VarExists['BY'] then
      ZVec := DataFrame.VectorByName[Parameters.VarByName['BY'].Value]
    else begin
      ZVec := TEpiIntVector.Create('$T', Dataframe.RowCount);
      for i := 1 to Dataframe.RowCount do
        zvec.AsInteger[i] := 1;
    end;

    result.Title.Caption := 'KM-Plot: ' + Varnames[Varnames.Count-1];
    if Parameters.VarExists['BY'] then
      result.Title.Caption := Result.Title.Caption + ' by ' + ZVec.GetVariableLabel(Parameters);

    if Parameters.VarExists['TI'] then
      result.Title.Caption := Parameters.VarbyName['TI'].AsString;

    Result.LeftAxis.Title.Caption := 'Survival';
    Result.BottomAxis.Title.Caption := 'Time ' ; // TODO: XVec.GetVariableLabel(Parameters);

    if (not Parameters.VarExists['NOCI']) then
    begin
      CILO := Dataframe.VectorByName['$CILO'];
      CIHI := Dataframe.VectorByName['$CIHI'];
    end;

    i := 0;
    j := -1;
    NewSeries();

    for i := 1 to DataFrame.RowCount do
    begin

      if TVec.AsFloat[i] < 1 then
      begin
        LineSeries.AddXY(XVec.AsInteger[i], YVec.AsFloat[i]);
        if (not Parameters.VarExists['NOCI']) then
          CandleSeries.AddOHLC(XVec.AsInteger[i], (YVec.AsFloat[i])-0.015, CIHI.AsFloat[i], CILO.AsFloat[i], (YVec.AsFloat[i])+0.015)
      end;

      if (i < DataFrame.RowCount) and (ZVec.compare(i, i+1) <> 0) then
      begin
        LineSeries.ParentChart := result;
        // Extra point to end line as horisontal line, if last point is marked "censored"
        LineSeries.AddXY(XVec.AsInteger[i], YVec.AsFloat[i]);
        if (not Parameters.VarExists['NOCI']) then
          CandleSeries.ParentChart := result;
        NewSeries();
      end;
    end;

    // Extra point to end line as horisontal line, if last point is marked "censored"
    LineSeries.AddXY(XVec.AsInteger[i-1], YVec.AsFloat[i-1]);
    LineSeries.ParentChart := result;
    if (not Parameters.VarExists['NOCI']) then
      CandleSeries.ParentChart := result;
  finally

  end;
end;

// draw a pareto plot
function TGraph.DoPareto(Dataframe: TEpiDataframe; Varnames: TStrings;
                           CmdID: Word; Parameters: TVarList): TChart;
var
  Total, i, j, Rsum, RowNo: EpiInt;
  xvec, yvec, labelvec: TEpiVector;
  df: TEpiDataframe;
  agglist: TAggrList;
  series:  TCustomSeries;
  barseries: TChartSeries;
  varlist: TStringList;
  title, WeightName: string;
  Weighted: boolean;
  tool: TAnnotationTool;
  PVarDesc: TAnaVariableDescriptor;
const
  procname = 'DoPareto';
  procversion = '1.0.0.0';
begin
  ODebug.IncIndent;
  ODebug.Add(UnitName + ':' + procname + ' - ' + procversion, 1);
  try
    xvec := dataframe.VectorByName[varnames[0]];
    RowNo := dataframe.SelectedRowCount;
    dataframe.Sort(Varnames[0]);
    total := 0;

    if (not dataframe.VectorByName[varnames[0]].DataType in [EpiTyInteger]) then
      dm.info('%s: illegal, must be integer variable', [varnames[0]], 213004);

    title := xvec.GetVariableLabel(Parameters);

    // step 1: Build dataframe
    if Parameters.VarbyName['W'] <> nil then
    begin
      WeightName := (Parameters.VarbyName['W'].AsString);
      Weighted := true;
    end;
    agglist := TAggrList.Create();
    if not Weighted then
      agglist.Insert(0, TAggrCount.Create('$S', Varnames[0], acAll))
    else
      agglist.Insert(0, TAggrSum.Create('$S', WeightName));
    if Weighted then
      varnames.Delete(varnames.IndexOf(WeightName));
    df := OAggregate.AggregateDataframe(dataframe, TStringList(varnames), agglist, TCommand.Create(0, Parameters));
    RowNo := df.RowCount;
    xvec := df.VectorByName[varnames[0]];
    if Parameters.VarbyName['XLABEL'] <> nil then
      labelvec := df.VectorByName[Parameters.VarbyName['XLABEL'].AsString]
    else
      labelvec := df.VectorByName[Varnames[0]];
    // Actual counts for each group in Yvec $S
    yvec := df.VectorByName['$S'];     // counts

    // get total number of observations:
    for i:=1 to RowNo do
      inc(total, yvec.asInteger[i] );
    if total = 0 then
      dm.error('No Data', [], 103005);

      // Sort on counts (not reversed)
    df.Sort(df.VectorByName['$S'].Name);

    // Create the graph and set customizable settings.
    result := CreateStandardChart();

    // add data to the graph:
    barseries := TBarSeries.Create(nil);
    series := TLineSeries.Create(nil);

    // use j subindex to add columns in reverse order:
    j := xvec.length+1;
    rsum := 0;
    for i :=  1 to xvec.Length do
    begin
      j := j-1;
      rsum := rsum + yvec.AsInteger[j];
      if (xvec.IsMissing[i]) then
        barseries.AddXY(j{xvec.AsFloat[i-1]}+1, yvec.AsFloat[i],
                        labelvec.AsString[i],
//                        labelvec.GetValueLabel(xvec.AsString[i], Parameters),
                        GetGraphColour(i-1))
      else
        barseries.AddXY(j{xvec.AsFloat[i]}, yvec.AsFloat[i],
                        labelvec.AsString[i],
//                        labelvec.GetValueLabel(xvec.AsString[i], Parameters),
                        GetGraphColour(i-1));
      // add cumulative series with correct percentage:
      series.AddXY(i,(rsum/total)*100,'');
    end;

    // set special options for bars
    TBarSeries(barseries).BarWidthPercent := 75;
    barseries.marks.Visible := False;
    barSeries.ParentChart := result;
    barSeries.Title := xvec.GetVariableLabel(Parameters);

    // Further specification of percentage line:
    SetScatter(TLineSeries(Series), 0);
    series.ParentChart := result;
    series.Title := 'Cum Pct';
    series.VertAxis := aRightAxis;
    result.RightAxis.setMinMax(0,100);
    result.RightAxis.Title.Caption := 'Percent';
    result.BottomAxis.Title.Caption := ' ' + xvec.GetVariableLabel(Parameters);
    result.BottomAxis.LabelStyle := talAuto;


    //titles
    if (Parameters.VarByName['TI'] <> nil) then
      result.Title.Text.Add(Parameters.VarByName['TI'].AsString)
    else
      result.Title.Text.Add('Pareto Chart: ' + title);
  finally
    if Assigned(df) then FreeAndNil(df);
    if Assigned(agglist) then FreeAndNil(agglist);
    if Assigned(PvarDesc) then FreeAndNil(PvarDesc);
    ODebug.DecIndent;
  end;
end;





function TGraph.AxisValue(Axis: TChartAxis; Imin, Imax: IValue; IsX: boolean): boolean;
var
  min, max: extended;
const
  procname = 'AxisValue';
  procversion = '1.0.0.0';
begin
  ODebug.Add(UnitName + ':' + procname + ' - ' + procversion, 1);
  // solution except for dates:
  result:=false;
  if (Imin = nil) or (Imax = nil) then exit;
  result := True;
  Axis.Automatic := false;
  // Force calculation of min and max!
  min := Axis.Minimum;
  max := Axis.Maximum;
  try
    min := strtofloat(Imin.AsString);
    max := strtofloat(Imax.AsString);
    Axis.SetMinMax(min, max);
    Axis.LabelStyle := talValue;
  except
    on E: exception do
    begin
      raise Exception.Create('Illegal value for axis: '+ e.Message );
    end;
  end;
end;

procedure TGraph.AxisIncrement(Axis: TChartAxis; Value: extended);
var
  min, max: double;
const
  procname = 'AxisIncrement';
  procversion = '1.0.0.0';
begin
  ODebug.Add(UnitName + ':' + procname + ' - ' + procversion, 1);
  if Axis.Automatic then
  begin
    // Force calculation of min and max!
    Axis.AdjustMaxMin;
    min := Axis.Minimum;
    max := Axis.Maximum;
    Axis.Automatic := false;
    Axis.SetMinMax(min, max);
  end;
  Axis.Increment := Value;
  Axis.LabelStyle := talValue;
  Axis.MaximumOffset := 5;
  Axis.LabelsSeparation := 0;
end;

procedure TGraph.AxisLines(Chart: TChart; Axis: string; Cmd: TCommand);
var
  ColorLine: TColorLineTool;
begin
  while (Cmd.ParamByName[Axis] <> nil) do
  begin
    ColorLine := TColorLineTool.Create(Chart);
    if (Axis = 'XLINE') or (Axis = 'XLINED') then
      ColorLine.Axis := Chart.BottomAxis
    else
      ColorLine.Axis := Chart.LeftAxis;
    if pos('D', Axis) > 0 then
      ColorLine.Pen.Style := psDot;
    ColorLine.Value := Cmd.ParamByName[Axis].AsFloat;
    Cmd.ParameterList.RemoveVar(Cmd.ParamByName[Axis]);
  end;
end;

function TGraph.MultiVarTitle(Varlist: TEpiVectors; Parameters: TVarList): string;
var
  s: string;
  i: integer;
const
  procname = 'MultiVarTitle';
  procversion = '1.0.0.0';
begin
  ODebug.IncIndent();
  ODebug.Add(UnitName, Self.ClassName, procname, procversion, 1);
  if (Parameters.VarByName['TI'] <> nil) then
    s := Parameters.VarByName['TI'].AsString
  else begin
    s := Varlist[0].GetVariableLabel(Parameters);
    for i := 1 to Varlist.Count - 1 do
    begin
      if (i = 1) then
        s := s + ' vs. ';
      if (i <> 1) then
        s := s + ' and ';
      s := s + Varlist[i].GetVariableLabel(Parameters);
    end;
  end;
  result := s;
  ODebug.DecIndent();
end;

function TGraph.FindBreak(Parameters: TVarList; XVector: TEpiVector): TArrayVariant;
var
  Params: TArrayVariant;
  i,j : integer;
  dfFormat: TEpiDateFormat;

  procedure ArraySort(var List: array of variant; L, R: Integer);
  var
     I, J, P: Integer;
     Temp: Variant;
  begin
     I:=L;
     J:=R;
     P:=(L + R) shr 1;
     repeat
       while List[I] < List[P] do Inc(I);
       while List[J] > List[P] do Dec(J);
       if I <= J then
       begin
         // Swapping values
          Temp := List[I];
          List[I] := List[J];
          List[J] := Temp;
          if P=I then P:=J
          else if P=J then P:=I;
          Inc(I);
          Dec(J);
       end;
     until I>J;
     if L<J then ArraySort(List, L,J);
     if I < R then ArraySort(List, I, R);
  end;

begin
  i := 0;
  while (Parameters.VarbyName['BREAK'] <> nil) do
  begin
    SetLength(Params, i+1);
    case XVector.DataType of
      EpiTyDate:
          begin
            dfFormat := DateFmtToEpiDateFmt(XVector.FieldDataFormat);
            EpiStrToDate(Parameters.VarbyName['BREAK'].AsString, j, dfFormat);
            Params[i] := j;
          end;
      EpiTyInteger:
          Params[i] := StrToInt(Parameters.VarbyName['BREAK'].Value);
    else
          Params[i] := Parameters.VarbyName['BREAK'].Value;
    end;
    Parameters.RemoveVar(Parameters.VarbyName['BREAK']);
    inc(i);
  end;
  while (Parameters.VarbyName['B'] <> nil) do
  begin
    SetLength(Params, i+1);
    case XVector.DataType of
      EpiTyDate:
          begin
            dfFormat := DateFmtToEpiDateFmt(XVector.FieldDataFormat);
            EpiStrToDate(Parameters.VarbyName['B'].AsString, j, dfFormat);
            Params[i] := j;
          end;
      EpiTyInteger:
          Params[i] := StrToInt(Parameters.VarbyName['B'].Value);
    else
          Params[i] := Parameters.VarbyName['B'].Value;
    end;
    Parameters.RemoveVar(Parameters.VarbyName['B']);
    inc(i);
  end;
  if i>0 then ArraySort(Params, 0, i-1);
  result := params;
end;

function TGraph.TextBox(Chart: TChart; cmd: TCommand): boolean;
var
  s, t: string;
  x, y, btyp: integer;
  Tool: TAnnotationTool;
const
  procname = 'AddTextBox';
  procversion = '1.0.0.0';
begin
  ODebug.Add(UnitName + ':' + procname + ' - ' + procversion, 1);
  // Format a text-box like this: /TEXT="xpos, ypos, text, boxtype"
  // eg.: /TEXT="20,20,my car is red,1"
  // Default: boxtype = 1
  result := true;
  try
    while (Cmd.ParamByName['TEXT'] <> nil) do
    begin
      s := Cmd.ParamByName['TEXT'].AsString;
      //Get xcoord.
      t := copy(s, 1, pos(',', s)-1);
      delete(s, 1, pos(',', s));
      x := strtoint(t);
      //Get ycoord
      t := copy(s, 1, pos(',', s)-1);
      delete(s, 1, pos(',', s));
      y := strtoint(t);
      //Get text
      t := copy(s, 1, pos(',', s)-1);
      delete(s, 1, pos(',', s));
      //Get boxtype
      if pos(',', s)>0 then raise exception.Create('Too many arguments');
      btyp := strtoint(s);
      //Design the box
      tool := TAnnotationTool.Create(Chart);
      Chart.Tools.Add(tool);
      tool.Text := t;
      tool.Shape.CustomPosition := true;
//      tool.Shape.Left := CalcXPosValue(x)// x;
//      tool.Shape.Top := CalcXPos(y) //y;
      tool.Shape.Left := x;
      tool.Shape.Top := y;
      tool.Shape.Shadow.VertSize := 0;
      tool.Shape.Shadow.HorizSize := 0;
      tool.Shape.Frame.Visible := false;
      if btyp>0 then
        tool.Shape.Frame.Visible := true;
      tool.Shape.Font.Color := GetGraphTextColour(9);
      //Delete the current box-info.
      Cmd.ParameterList.RemoveVar(Cmd.ParamByName['TEXT']);
    end;
  except
    on E: exception do
    begin
      result := false;
      raise Exception.Create('Cannot display textbox: '+ e.Message );
    end;
  end;
end;


procedure TGraph.CommonChartOptions(Chart: TChart; Cmd: TCommand);
var
  opt: TEpiOption;
  s : string;
const
  procname = 'CommonChartOptions';
  procversion = '1.0.0.0';
begin
  ODebug.Add(UnitName + ':' + procname + ' - ' + procversion, 1);
  if chart = nil then exit;

  // Set-options applied to graph before showing.
  dm.GetOptionValue('GRAPH FONT SIZE', opt);
  Chart.Title.Font.Size := Round(1.25 * StrToInt(opt.value));
  Chart.Title.Font.Color := GetGraphTextColour(1);
  Chart.SubTitle.Font.Size := Round(1.1 * StrToInt(opt.value));
  Chart.SubFoot.Font.Size := Round(0.80 * StrToInt(opt.value));
  Chart.Foot.Font.Size := Round(0.70 * StrToInt(opt.value));
  Chart.Foot.Font.Color := GetGraphTextColour(2);

  if (Cmd.ParamByName['SUB'] <> nil) then
    Chart.SubTitle.Text.Add(Cmd.ParamByName['SUB'].AsString)
  else
    Chart.SubTitle.Visible := false;

  //footnote
  dm.GetOptionValue('GRAPH FOOTNOTE', Opt);
  if (Cmd.ParamByName['FN'] <> nil) then s := cmd.ParamByName['FN'].AsString
      else s:= opt.value;

  if s <> '' then
    begin
    Chart.Foot.Text.Add(s);
    Chart.Foot.Alignment := taRightJustify;
    end
    else Chart.Foot.Visible := false;

  // Colouring options for axis, tickmarks, axis labels and axis text.

  Chart.BottomAxis.Axis.Color := GetGraphTextColour(3);
  if cmd.CommandID <> opDotPlot then
    Chart.BottomAxis.Ticks.Color := GetGraphTextColour(4)
    else Chart.BottomAxis.Ticks.Color := ClWhite;
  Chart.BottomAxis.LabelsFont.Color := GetGraphTextColour(5);
  Chart.BottomAxis.Title.Font.Color := GetGraphTextColour(5);

  Chart.LeftAxis.Axis.Color := GetGraphTextColour(6);
  Chart.LeftAxis.Ticks.Color := GetGraphTextColour(7);
  Chart.LeftAxis.LabelsFont.Color := GetGraphTextColour(8);
  Chart.LeftAxis.Title.Font.Color := GetGraphTextColour(8);

  // Options applied to the graph before showing.
  if (Cmd.ParamByName['TEXT'] <> nil) then TextBox(Chart, Cmd);
  if (cmd.ParamByName['LEGEND'] <> nil) then Chart.Legend.Visible := true
    else Chart.Legend.Visible := false;
  if (Cmd.ParamByName['FRAME'] <> nil) then Chart.Frame.Visible := true
    else Chart.Frame.Visible := false;
  if (Cmd.ParamByName['HGRID'] <> nil) then Chart.Axes.Left.Grid.Visible := true
    else Chart.Axes.Left.Grid.Visible := false;
  if (Cmd.ParamByName['VGRID'] <> nil) then Chart.Axes.Bottom.Grid.Visible := true
    else Chart.Axes.Bottom.Grid.Visible := false;
  if (Cmd.ParamByName['XHIDE'] <> nil) then Chart.Axes.Bottom.Visible := false ;
//    else Chart.Axes.Bottom.Visible := true;
  if (Cmd.ParamByName['YHIDE'] <> nil) then Chart.Axes.Left.Visible := false
    else Chart.Axes.Left.Visible := true;
  if (Cmd.ParamByName['XINV'] <> nil) then Chart.Axes.Bottom.Inverted := true
    else Chart.Axes.Bottom.Inverted := false;
  if (Cmd.ParamByName['YINV'] <> nil) then Chart.Axes.Left.Inverted := true
    else Chart.Axes.Left.Inverted := false;
  if (Cmd.ParamByName['XLOG'] <> nil) then Chart.Axes.Bottom.Logarithmic := true
    else Chart.Axes.Bottom.Logarithmic := false;
  if (Cmd.ParamByName['YLOG'] <> nil) then Chart.Axes.Left.Logarithmic := true
    else Chart.Axes.Left.Logarithmic := false;
  if (Cmd.ParamByName['NOXTICK'] <> nil) then Chart.Axes.Bottom.Ticks.Visible := false
    else Chart.Axes.Bottom.Ticks.Visible := true;
  if (Cmd.ParamByName['NOYTICK'] <> nil) then Chart.Axes.Left.Ticks.Visible := false
    else Chart.Axes.Left.Ticks.Visible := true;
  if (Cmd.ParamByName['YMAX'] <> nil) or (cmd.ParamByName['YMIN'] <> nil) then
    if not AxisValue(Chart.Axes.Left, Cmd.ParamByName['YMIN'], Cmd.ParamByName['YMAX'], false)
      then dm.info('Specify both Max and Min for %s axis', ['Y'], 213005);

  if (Cmd.ParamByName['XMAX'] <> nil) or (cmd.ParamByName['XMIN'] <> nil) then
    if not AxisValue(Chart.Axes.Bottom, Cmd.ParamByName['XMIN'], Cmd.ParamByName['XMAX'], true)
      then dm.info('Specify both Max and Min for %s axis', ['X'], 213005);

  if (Cmd.ParamByName['XINC'] <> nil) then
    AxisIncrement(Chart.Axes.Bottom, Cmd.ParamByName['XINC'].AsFloat);

  if (Cmd.ParamByName['YINC'] <> nil) then
    AxisIncrement(Chart.Axes.Left, Cmd.ParamByName['YINC'].AsFloat);

  AxisLines(Chart, 'XLINE', Cmd);
  AxisLines(Chart, 'YLINE', Cmd);
  AxisLines(Chart, 'XLINED', Cmd);
  AxisLines(Chart, 'YLINED', Cmd);

  if Cmd.ParamByName['XTEXT'] <> nil then
    Chart.BottomAxis.Title.Caption := CMd.ParamByName['XTEXT'].AsString;

  if Cmd.ParamByName['X90'] <> nil then
    Chart.BottomAxis.LabelsAngle := 90;

  if Cmd.ParamByName['X45'] <> nil then
    Chart.BottomAxis.LabelsAngle := 45;

  if Cmd.ParamByName['XA'] <> nil then
    Chart.BottomAxis.LabelsAlternate := True;

  if (Cmd.ParamByName['X45'] <> nil) or (Cmd.ParamByName['XA'] <> nil) then
    Chart.BottomAxis.LabelsSeparation := 0;

  if Cmd.ParamByName['YTEXT'] <> nil then
  begin
    if CMd.ParamByName['YTEXT'].AsString = '' then
      Chart.LeftAxis.Title.Visible := false
    else begin
      Chart.LeftAxis.Title.Caption := Cmd.ParamByName['YTEXT'].AsString;
      Chart.LeftAxis.Title.Angle := 90;
      // Chart.LeftAxis.LabelsSize := 18;
      //if (StrLen(PChar(Chart.LeftAxis.Title.Caption))*4)>Chart.LeftAxis.LabelsSize then
      //  Chart.LeftAxis.LabelsSize := StrLen(PChar(Chart.LeftAxis.Title.Font.Height))*4;    //        Chart.LeftAxis.LabelsSize := 20;
    end;
  end;

  if Assigned(Cmd.ParamByName['BW']) then
    BlackAndWhiteChart(Chart);
end;

procedure TGraph.BlackAndWhiteChart(Chart: TChart);
var
  i: integer;
  Series: TChartSeries;
const
  bstype: array[0..1] of TBrushStyle = (bsSolid, bsClear);
begin
  // Background.
  Chart.BackColor := clWhite;

  // Text - (titles, footers, etc.)
  Chart.Title.Brush.Color := clWhite;
  Chart.Title.Font.Color := clBlack;
  Chart.SubTitle.Brush.Color := clWhite;
  Chart.SubTitle.Font.Color := clBlack;
  Chart.Foot.Brush.Color := clWhite;
  Chart.Foot.Font.Color := clBlack;
  Chart.SubFoot.Brush.Color := clWhite;
  Chart.SubFoot.Font.Color := clBlack;

  // Axis
  Chart.BottomAxis.Axis.Color := clBlack;
  Chart.BottomAxis.Ticks.Color := clBlack;
  Chart.BottomAxis.LabelsFont.Color := clBlack;
  Chart.BottomAxis.Title.Brush.Color := clWhite;
  Chart.BottomAxis.Title.Font.Color := clBlack;
  Chart.LeftAxis.Axis.Color := clBlack;
  Chart.LeftAxis.Ticks.Color := clBlack;
  Chart.LeftAxis.LabelsFont.Color := clBlack;
  Chart.LeftAxis.Title.Brush.Color := clWhite;
  Chart.LeftAxis.Title.Font.Color := clBlack;

  // Series
  for i := 0 to Chart.SeriesCount -1 do
  begin
    Series := Chart.Series[i];
    Series.Color := clBlack;
    if (Series.Marks.Visible) then
    begin
      Series.Marks.Brush.Color := clWhite;
      Series.Marks.Font.Color := clBlack;
    end;
    if (Series is TBoxSeries) then with (Series as TBoxSeries) do
    begin
      Box.Color := clWhite;
      Color := clWhite;
      Box.Brush.Style := bsClear;
      Box.Brush.Color := clWhite;
    end else
    if (Series is TBarSeries) or (Series is TPieSeries) then with (Series) do
    begin
      ColorRange(XValues, XValues.First, XValues.Last, clBlack);
    end else
    if (Series is TPointSeries) then with (Series as TPointSeries) do
    begin
      Pointer.Color := clBlack;
      Pointer.Brush.Color := clBlack;
      if (i mod 2) = 0 then
        Pointer.Brush.Style := bsClear;
    end;
  end;
end;

{********************
*      TGraphForm   *
*********************}

procedure TGraphForm.Button7Click(Sender: TObject);
begin
  ChartEditor1.Execute;
end;

function TGraphForm.GetActiveChart: TChart;
var
  i: integer;
begin
  i := 0;
  result := nil;
  while i < GraphPG.ActivePage.ControlCount do
    if GraphPG.ActivePage.Controls[i] is TChart then
    begin
      result := (GraphPG.ActivePage.Controls[i] as TChart);
      break;
    end else
      inc(i);
end;

procedure TGraphForm.GraphPGChange(Sender: TObject);
begin
  ChartEditor1.Chart := GetActiveChart();
end;

procedure TGraphForm.FormShow(Sender: TObject);
begin
  GraphPGChange(nil);
  EditChanged := false;
end;

procedure TGraphForm.Button10Click(Sender: TObject);
begin
  close();
end;

procedure TGraphForm.Edit1Change(Sender: TObject);
begin
  EditChanged := true;
end;

procedure TGraphForm.Reset();
begin
  Edit1.Text := 'Write text here - click button';
  EditChanged := false;
end;

procedure TGraphForm.Button5Click(Sender: TObject);
begin
  if not EditChanged then exit;
  GetActiveChart().Title.Text.Clear;
  GetActiveChart().Title.Text.Add(Edit1.Text);
  GetActiveChart().Title.Visible := true;
  Reset();
end;

procedure TGraphForm.Button8Click(Sender: TObject);
begin
  if not EditChanged then exit;
  GetActiveChart().SubTitle.Text.Clear;
  GetActiveChart().SubTitle.Text.Add(Edit1.Text);
  GetActiveChart().SubTitle.Visible := true;
  Reset();
end;

procedure TGraphForm.Button6Click(Sender: TObject);
begin
  if not EditChanged then exit;
  GetActiveChart().Foot.Text.Clear;
  GetActiveChart().Foot.Text.Add(Edit1.Text);
  GetActiveChart().Foot.Visible := true;
  Reset();
end;

procedure TGraphForm.Button2Click(Sender: TObject);
begin
  GetActiveChart.BottomAxis.Grid.Visible := not GetActiveChart.BottomAxis.Grid.Visible;
  GetActiveChart.LeftAxis.Grid.Visible := not GetActiveChart.LeftAxis.Grid.Visible;
end;

procedure TGraphForm.Button3Click(Sender: TObject);
begin
  GetActiveChart.Legend.Visible := not GetActiveChart.Legend.Visible;
end;

procedure TGraphForm.Button9Click(Sender: TObject);
begin
  GetActiveChart.Frame.Visible := not GetActiveChart.frame.Visible;
end;

procedure TGraphForm.Button1Click(Sender: TObject);
begin
  GetActiveChart.CopyToClipboardMetafile(true);
end;

procedure TGraphForm.Button4Click(Sender: TObject);
begin
  dm.SD.Filter := '*.wmf|WFM File';
  if dm.SD.Execute then
  begin
    if ExtractFileExt(dm.SD.FileName) = '' then
      GetActiveChart.SaveToMetafileEnh(dm.SD.FileName + '.wmf')
    else
      GetActiveChart.SaveToMetafileEnh(dm.SD.FileName);
  end;
end;

procedure TGraphForm.Button11Click(Sender: TObject);
var
  opt: TEpiOption;
  pngobj: TPNGObject;
  Rect: TRect;
  bitmap: TBitmap;
begin
  dm.SD.Filter := '*.png|PNG File';
  if dm.SD.Execute then
  begin
    Rect.Left := 0;
    Rect.Top := 0;
    if (dm.GetOptionValue('GRAPH SIZE X', Opt)) then Rect.Right := StrToInt(Opt.Value);
    if (dm.GetOptionValue('GRAPH SIZE Y', Opt)) then Rect.Bottom := StrToInt(Opt.Value);
    bitmap := GetActiveChart.TeeCreateBitmap(GetActiveChart.BackColor, Rect);
    pngobj := TPNGObject.Create;
    pngobj.Assign(BitMap);
    if ExtractFileExt(dm.SD.FileName) = '' then
      pngobj.SaveToFile(dm.SD.FileName + '.png')
    else
      pngobj.SaveToFile(dm.SD.FileName);
    FreeAndNil(bitmap);
    FreeAndNil(pngobj);
  end;
end;

procedure TGraphForm.AcCloseExecute(Sender: TObject);
begin
  Close();  
end;

end.
