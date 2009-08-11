unit UGraph;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, USPCBase, Graphics, Controls, Forms,
  Dialogs, TeeEdit, TeeProcs, TeEngine, Chart, ComCtrls, StdCtrls, ExtCtrls,
  UVectors, TeeBoxplot, TeePng, TeeTools, TeeShape, StatChar, Series, UCommands,
  UEpiDatatypes, UOutput, UVariables, ansDatatypes, ActnList, UChartArray,
  UBaseChart;

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

  TGraphCallback = procedure(const chart: TChart);

  TGraph = class(TObject)
  private
    dummyform: TForm;
    GraphCallBack: TGraphCallback;
    fCmd: TCommand;

    // Axis labeling procedure...
    procedure BottomDateLabeling(Sender: TChartAxis; Series: TChartSeries; ValueIndex: Integer; Var LabelText: String);

    procedure InitializeBaseUnit(var AUnit: TCustomChart; AUnitClass: TCustomChartClass); overload;
    procedure InitializeBaseUnit(var AUnit: TCustomSPCChart; AUnitClass: TCustomSPCChartClass); overload;
    function CreateStandardChart(): TChart;
    function CreateChartArray(): TChartArray;
    procedure CommonChartOptions(Chart: TChart);
    procedure BlackAndWhiteChart(Chart: TChart);
    function AxisValue(Axis: TChartAxis; Imin, Imax: IValue; IsX: boolean): boolean;
    procedure AxisIncrement(Axis: TChartAxis; Value: extended);
    procedure AxisLines(Chart: TChart; Axis: string; Cmd: TCommand);
    function TextBox(Chart: TChart; cmd: TCommand): boolean;
    procedure ShowChart(Chart: TChart);
    function MultiVarTitle(Varlist: TEpiVectors; Parameters: TVarList): string;
    procedure CalcAxisInc(Axis: TChartAxis; CalcLabel: Boolean = false);
    procedure OutputNCases(const chart: TChart; N : EpiInt);

    function GraphTable(XVar: TEpiVector; YVars: TEpiVectors): TStatTable;
    // Put simple series functions here: (eg. barseries)
    function BarSeries(DataVector, LabelVector: TEpiVector; ColorFunc: TSeriesColorFunction): TChartSeries; overload;
    function BarSeries(RangeVector, DataVector, LabelVector: TEpiVector; ColorFunc: TSeriesColorFunction): TChartSeries; overload;
    //function HistogramSeries(DataVector, LabelVector: TEpiVector; ColorFunc: TSeriesColorFunction): TChartSeries;
    function HistogramSeries(XDataVector, YDataVector, LabelVector: TEpiVector; ColorFunc: TSeriesColorFunction): TChartSeries;

    function LineSeries(XVector, YVector, LabelVector: TEpiVector): TLineSeries;
    function BoxSeries(DataVector: TEpiVector): TBoxSeries;
    function ScatterSeries(XVector, YVector, LabelVector: TEpiVector): TPointSeries;
    function PieSeries(XVector, LabelVector: TEpiVector): TPieSeries;

    procedure ShowCharts(Charts: TChartArray);
    function SaveCharts(Charts: TChartArray): TStrings;
  public
    destructor Destroy(); override;
    constructor Create();

    function PointSeries(XVector, YVector, LabelVector: TEpiVector): TPointSeries;
    function LineSeries2(XVector, YVector, LabelVector: TEpiVector): TLineSeries;
    procedure AddLine(Axis: TChartAxis; Val: Epifloat; Color: TColor);

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
    function DoPareto(Dataframe: TEpiDataframe; Varnames: TStrings; CmdID: Word; Parameters: TVarList): TChart;
    function DoLifeTable(Dataframe: TEpiDataframe; Varnames: TStrings; CmdID: Word; Parameters: TVarList): TChart;

    // EXTRA FUNCTIONALITY.
    procedure DoGraphs(Dataframe: TEpiDataFrame; Varnames: TStrings; Cmd: TCommand);
    property Cmd: TCommand read fCmd;
  end;

var
  OGraph: TGraph;

implementation

{$R *.dfm}
uses
  UAggregate, SMUtils, UCmdProcessor, Math, UAnaToken, UDebug, pngimage, UFormats,
  cFileUtils,  uDateUtils, UTables, UStatFunctions, TeCanvas, CandleCh, UTranslation,
  UXBarS, UXBarR, USPCUtils, GeneralUtils, UIChart, UPChart, URunChart, UGChart, UUChart,
  UCChart, EpiDataUtils, UCmdTypes, UGraphUtils;

var
  GraphForm: TGraphForm;
  OXBarR: TXBarS;
  OXBarS: TXBarS;
  OIChart: TIChart;
  OPChart: TPChart;
  OGChart: TGChart;
  OUChart: TUChart;
  OCChart: TCChart;
  ORunChart: TRunChart;


const
  UnitName = 'UGraph';

{********************
*      TGraph       *
*********************}

destructor TGraph.Destroy();
begin
  if Assigned(ORunChart) then FreeAndNil(ORunChart);
  if Assigned(OCChart) then FreeAndNil(OCChart);
  if Assigned(OUChart) then FreeAndNil(OUChart);
  if Assigned(OGChart) then FreeAndNil(OGChart);
  if Assigned(OPChart) then FreeAndNil(OPChart);
  if Assigned(OIChart) then FreeAndNil(OIChart);
  if Assigned(OXBarS) then FreeAndNil(OXBarS);
  if Assigned(OXBarR) then FreeAndNil(OXBarR);
  if Assigned(OSPCUtils) then FreeAndNil(OSPCUtils);
  if Assigned(dummyform) then FreeAndNil(dummyform);
end;

constructor TGraph.Create();
begin
  OSPCUtils := TSPCUtils.Create();
  InitializeBaseUnit(TCustomSPCChart(OXBarR), TXBarR);
  InitializeBaseUnit(TCustomSPCChart(OXBarS), TXBarS);
  InitializeBaseUnit(TCustomSPCChart(OIChart), TIChart);
  InitializeBaseUnit(TCustomSPCChart(OPChart), TPChart);
  InitializeBaseUnit(TCustomSPCChart(OGChart), TGChart);
  InitializeBaseUnit(TCustomSPCChart(OUChart), TUChart);
  InitializeBaseUnit(TCustomSPCChart(OCChart), TCChart);
  InitializeBaseUnit(TCustomSPCChart(ORunChart), TRunChart);
end;

procedure TGraph.DoGraphs(Dataframe: TEpiDataFrame; Varnames: TStrings; Cmd: TCommand);
var
  Charts: TChartArray;
  Opt:   TEpiOption;
  footnote:  string;
  xtab: TStatTable;
  vectorlist: TEpiVectors;
  vectors: TStringList;
  DummyFrame: TEpiDataframe;
  FileNames: TStrings;
  i: integer;
const
  procname = 'DoGraphs';
  procversion = '1.0.0.0';
begin
  ODebug.Add(UnitName + ':' + procname + ' - ' + procversion, 1);
  GraphCallBack := nil;
  dummyform := nil;
  Charts := nil;
  footnote := '';
  fCmd := Cmd;

  try
    Charts := TChartArray.Create();
    case cmd.CommandID of
      // Single graphs
      opHistogram,
      opShortHistogram:      Charts.Add(DoHistogram(Dataframe, Varnames, Cmd.CommandID, Cmd.ParameterList), 'Histogram');
      opBar:                 Charts.Add(DoBars(Dataframe, Varnames, Cmd.CommandID, Cmd.ParameterList), 'Bar');
      opBox, opBoxPlot:      Charts.Add(DoBoxPlot(Dataframe, Varnames, Cmd.CommandID, Cmd.ParameterList, xtab), 'BoxPlot');
      opLine,
      opScatter,
      opShortScatter:        Charts.Add(DoScatter(Dataframe, Varnames, Cmd.CommandID, Cmd.ParameterList));
      opPie:                 Charts.Add(DoPie(Dataframe, Varnames, Cmd.CommandID, Cmd.ParameterList), 'Pie');
      opEpiCurve:            Charts.Add(DoEpiCurve(Dataframe, Varnames, Cmd.CommandID, Cmd.ParameterList, xtab, footnote), 'EpiCurve');
      opCDFPlot:             Charts.Add(DoCDFPlot(Dataframe, Varnames, Cmd.CommandID, Cmd.ParameterList, xtab), 'CDFPlot');
      opCIPlot:              Charts.Add(DoCIPlot(Dataframe, Varnames, Cmd.CommandID, Cmd.ParameterList, xtab), 'CIPlot');
      opDotPlot:             Charts.Add(DoDotPlot(Dataframe, Varnames, Cmd.CommandID, Cmd.ParameterList), 'Dot Plot');
      opPChart:              Charts := OPChart.DoSPCChart(Dataframe, Varnames, xTab);
      opPareto:              Charts.Add(DoPareto(Dataframe, Varnames, Cmd.CommandID, Cmd.ParameterList), 'Pareto');
      opLifeTable,
      opShortLifeTable:      Charts.Add(DoLifeTable(DataFrame, Varnames, Cmd.CommandID, Cmd.ParameterList), 'Survival Curve');

      opGChart:              Charts := OGChart.DoSPCChart(Dataframe, Varnames, xTab);
      opCChart:              Charts := OCChart.DoSPCChart(Dataframe, Varnames, xTab);
      opUChart:              Charts := OUChart.DoSPCChart(DataFrame, Varnames, xTab);
      opXBar:                begin
                               if cmd.ParamExists['RANGE'] then
                                 Charts := OXBarR.DoSPCChart(Dataframe, Varnames, xTab)
                               else
                                 Charts := OXBarS.DoSPCChart(Dataframe, Varnames, xTab);
                             end;  
      opIChart:              Charts := OIChart.DoSPCChart(Dataframe, Varnames, xTab);
      opRunChart:            Charts := ORunChart.DoSPCChart(Dataframe, Varnames, xTab);
    else
      dm.Error('Command not in implemented in DoGraph', [], 33001);
    end;
    if (Charts = nil) then raise Exception.Create('TChartArray not initialized.');
    if (Charts.Count = 0) then raise Exception.Create('No charts initialized.');
    if Assigned(xtab) and (not cmd.ParamExists['Q']) and (not Cmd.ParamExists['NT']) then
    begin
      xtab.TableType := sttNormal;
      dm.CodeMaker.OutputTable(xtab,footnote);
      dm.Sendoutput;
    end;

    for i := 0 to Charts.Count -1 do                           
      CommonChartOptions(Charts[i]);

    if (Cmd.ParambyName['N'] <> nil) then
      OutputNCases(Charts[0], dataframe.RowCount);  // add n= at this level CommonChartOptions no access to dataframe

    if Assigned(GraphCallBack) then
      GraphCallBack(Charts[0]);

    if (cmd.ParamExists['EDIT']) then
      ShowCharts(Charts);

    FileNames := SaveCharts(Charts);

    if (not (cmd.ParamExists['Q'] or Cmd.ParamExists['NG'])) and
       (AnsiCompareText(ExtractFileExt(FileNames[0]),'.png')=0) then
    begin
      xtab := dm.OutputList.NewTable(1, Charts.Count);
      xtab.TableType := sttGraph;
      for i := 0 to Charts.Count - 1 do
      begin
        xtab.Cell[1,i+1] := '<img src="'+ FileNames[i] +'" ALT="' + FileNames[i] + '">';
        if ((dm.GetOptionValue('GRAPH FILENAME SHOW', Opt) and (Opt.Value = 'ON'))) then
          xtab.Footer := xtab.Footer + FileNames[i]
      end;

      dm.CodeMaker.OutputTable(xtab,'');

      if (Cmd.ParamExists['TAB']) and (Cmd.CommandID in [opPchart,opIChart,opRunChart]) then
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
    if Assigned(FileNames) then FreeAndNil(FileNames);
    fCmd := nil;
  end;
end;

procedure TGraph.AddLine(Axis: TChartAxis; Val: Epifloat; Color: TColor);
var
  ColorLine: TColorLineTool;
begin
  try
    colorline := TColorLineTool.Create(Axis.ParentChart);
    colorLine.Value := Val;
    colorLine.Axis  := Axis;
    colorline.Pen.Color := Color;
    if Axis.ParentChart.BottomAxis = Axis then
      ColorLine.Pen.Style := psDot;
  except
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
  
  if Not Assigned(dummyform) then
  begin
    Dummyform := TForm.CreateNew(Application);
    Dummyform.Height := 300; //StrToInt(opt.Value);
    Dummyform.Width := 400; //StrToInt(opt.Value);
    dummyform.Name := 'Dummyform';
  end;
  result := TChart.Create(dummyform);
  result.Parent := dummyform;
  result.Name := 'StandardChart' + IntToStr(Dummyform.ComponentCount);

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
    Title.Font.Height := -16;
    dm.GetOptionValue('GRAPH FOOTNOTE', Opt);
    Foot.Text.Add(Opt.Value);
    Foot.Alignment := taRightJustify;

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

function TGraph.CreateChartArray(): TChartArray;
begin
  result := TChartArray.Create();
end;

procedure TGraph.InitializeBaseUnit(var AUnit: TCustomSPCChart; AUnitClass: TCustomSPCChartClass);
begin
  AUnit := AUnitClass.Create();
  AUnit.OnCreateStandardChart := CreateStandardChart;
  AUnit.OnCreateChartArray := CreateChartArray;
end;

procedure TGraph.InitializeBaseUnit(var AUnit: TCustomChart; AUnitClass: TCustomChartClass);
begin
  AUnit := AUnitClass.Create();
  AUnit.OnCreateStandardChart := CreateStandardChart;
  AUnit.OnCreateChartArray := CreateChartArray;
end;

procedure TGraph.BottomDateLabeling(Sender: TChartAxis; Series: TChartSeries; ValueIndex: Integer; Var LabelText: String);
var
  val: integer;
begin
  if Sender = Sender.ParentChart.BottomAxis then
    if TryStrToInt(LabelText, val) then
      LabelText := EpiDateToStr(StrToInt(LabelText), dfDMY);
end;

procedure TGraph.CalcAxisInc(Axis: TChartAxis; CalcLabel: Boolean = false);
var
  increment, min, max: double;
  factor, w: integer;
begin
  Axis.AdjustMaxMin;
  if Axis.Automatic then
  begin
    // Force calculation of min and max!
    min := Axis.Minimum;
    max := Axis.Maximum;
    Axis.Automatic := false;
    Axis.SetMinMax(min, max);
  end;
  Axis.LabelStyle := talValue;
  Axis.LabelsSeparation := 10;
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

  if CalcLabel then
  begin
    w := Axis.LabelWidth(RoundTo(Axis.Minimum, -2));
    w := Math.Max(Axis.LabelWidth(RoundTo(Axis.Maximum, -2)), w);
    if (Axis.Minimum >= 0) and (Axis.Maximum <= 9) then
      w := Math.Max(Axis.LabelWidth(0.9), w);
    Inc(w, 5);
    Axis.LabelsSize := w;
  end;
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

// TODO -o Torsten : Redesign...
function TGraph.LineSeries2(XVector, YVector, LabelVector: TEpiVector): TLineSeries;
var
  i: cardinal;
  dummy: Double;
begin
  result := TLineSeries.Create(nil);

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



procedure TGraph.ShowCharts(Charts: TChartArray);
var
  Chart: TChart;
  Page:  TTabSheet;
  i:     integer;
const
  procname = 'ShowCharts';
  procversion = '1.0.0.0';
begin
  ODebug.Add(UnitName + ':' + procname + ' - ' + procversion, 1);

  if Charts.Count = 0 then exit;
  GraphForm := TGraphForm.Create(nil);
  OTranslator.TranslateForm(GraphForm);

  for i := 0 to Charts.Count - 1 do
  begin
    Page := TTabSheet.Create(GraphForm.GraphPG);
    Page.PageControl := GraphForm.GraphPG;
    Page.Caption := Charts.FormTitle[i];
    Chart := Charts[i];
    Chart.Parent := Page;
    Chart.Align := alClient;  
  end;
  // Show the graph...
  GraphForm.ShowModal;
  for i := 0 to Charts.Count - 1 do
    Charts[i].Parent := dummyform;   
  FreeAndNil(GraphForm);
end;

procedure TGraph.ShowChart(Chart: TChart);
var
  Charts: TChartArray;
const
  procname = 'ShowCharts';
  procversion = '1.0.0.0';
begin
  ODebug.Add(UnitName + ':' + procname + ' - ' + procversion, 1);
  Charts := nil;
  try
    Charts := TChartArray.Create();
    Charts.Add(Chart);
    ShowCharts(Charts);
  finally
    if Assigned(Charts) then FreeAndNil(Charts);
  end;
end;


{
 SaveChart saves the chart given conditions specified in cmd.
 It returns a string with the full pathname for the file.
}
function TGraph.SaveCharts(Charts: TChartArray): TStrings;
var
  s, ext: string;
  // save: boolean;
  opt: TEpiOption;
  day,month,year,hh,mm,ss,ms: Word;
  xsize, ysize, i: integer;
  rect: TRect;
  bitmap: TBitmap;
  pngobj: TPNGObject;
const
  procname = 'SaveChart';
  procversion = '1.0.0.0';
begin
  ODebug.IncIndent();
  ODebug.Add(UnitName + ':' + procname + ' - ' + procversion, 1);
  result := nil;
  try
    Result := TStringList.Create;
    if (dm.GetOptionValue('GRAPH SAVETYPE', Opt)) then ext := '.' + AnsiUpperCase(Opt.Value);

    for i := 0 to Charts.Count - 1 do
    begin
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
      if Cmd.ParamExists['SAVE'] then
      begin
        s := ExtractFilePath(Cmd.ParamByName['SAVE'].AsString);
        s := s + ExtractFileNameNoExt(Cmd.ParamByName['SAVE'].AsString);
        if i > 0 then
          s := s + '_' + IntToStr(i);
        if ExtractFileExt(Cmd.ParamByName['SAVE'].AsString) <> '' then
          ext := AnsiUppercase(ExtractFileExt(Cmd.ParamByName['SAVE'].AsString));
        s := s + ext;
      end;
      s := ExpandFileName(s);
      if not((ext='.BMP') or (ext='.WMF') or (ext='.PNG')) then
        dm.Error('Unable to save using file format: %s', [ext], 33002);
      if (FileExists(s)) and (Cmd.ParamByName['REPLACE'] = nil) then
        dm.Error('File %s exist. Erase file or <br>use option /Replace', [s], 33003);
      // Size Options override globally defined values. 
      if Assigned(Cmd.ParamByName['SIZEX']) then
        xsize := StrToInt(Cmd.ParamByName['SIZEX'].AsString)
      else
        xsize := 400;
      if Assigned(Cmd.ParamByName['SIZEY']) then
        ysize := StrToInt(Cmd.ParamByName['SIZEY'].AsString)
      else
        ysize := 300;
      Rect.Left := 0;
      Rect.Right := xsize;
      Rect.Bottom := ysize;
      Rect.Top := 0;
      if ext = '.PNG' then
      begin
        bitmap := Charts[i].TeeCreateBitmap(Charts[i].BackColor, Rect);
        pngobj := TPNGObject.Create;
        pngobj.Assign(BitMap);
        pngobj.SaveToFile(s);
        FreeAndNil(BitMap);
        FreeAndNil(PngObj);
      end;
      if ext = '.BMP' then Charts[i].SaveToBitmapFile(s);
      if ext = '.WMF' then Charts[i].SaveToMetafile(s);

      if (dm.GetOptionValue('GRAPH CLIPBOARD', Opt) and (Opt.Value = 'ON')) then
        Charts[i].CopyToClipboardMetafile(true, Rect);

      result.Add(s);
    end;
  finally
    ODebug.DecIndent();
  end;
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
      result := TGraphUtils.GetGraphColour(k)
    else
      result := TGraphUtils.GetGraphColour(index);
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
        dm.Error('Cannot use /BY with string variables', [], 33004);
    end;

    total := Dataframe.RowCount;                                  
    df := OAggregate.AggregateDataframe(Dataframe, TStringList(varnames), agglist, Cmd);
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
  i, Total, BinCount, Factor, XW: integer;
  agglist: TAggrList;
  DF: TEpiDataframe;
  Vec, NVec: TEpiVector;
  XWidth, Xmin, XMax: EpiFloat;
  Series: TBarSeries;
  
{
  series: TAreaSeries;
  Grouped: boolean;
  vec, nvec, zvec, tvec: TEpiVector;
  GroupName: string;}
const
  procname = 'DoHistogram';
  procversion = '1.0.0.0';

{  function GetColour(index: integer): TColor;
  begin
    if Grouped then
      result := TGraphUtils.GetGraphColour(k)
    else
      result := TGraphUtils.GetGraphColour(index);
  end;}

begin
  ODebug.IncIndent;
  ODebug.Add(UnitName, ClassName, Procname, procversion, 1);

  try
    result := CreateStandardChart;
    
    total := Dataframe.RowCount;

    if total > 500 then
      BinCount := Min(20 + (Total div 500), 35)
    else
      BinCount := Trunc(Sqrt(Total));

    if Cmd.ParamExists['BINS'] then
      BinCount := StrToIntDef(Cmd.ParamByName['BINS'].AsString, BinCount);

    // Aggregate dataframe!
    agglist := TAggrList.Create();
    agglist.Add(TAggrCount.Create('$S', Varnames[0], acAll));
    df := OAggregate.AggregateDataframe(Dataframe, TStringList(Varnames), agglist, TCommand.Create(0, Parameters));


    Vec := df.VectorByName[Varnames[0]];
    NVec := df.VectorByName['$S'];
    XMin := Vec.AsFloat[1];
    XMax := Vec.AsFloat[Vec.Length];

    XWidth := (XMax - XMin) / BinCount;

    Factor := 1;
    if Vec.DataType = EpiTyFloat then
    begin
      while XWidth < 10 do
      begin
        Factor := Factor * 10;
        XWidth := XWidth * 10;
      end;
    end;
    XW := Trunc(XWidth);

    Vec.AsFloat[1] := ((Trunc(Vec.AsFloat[1] * Factor) div XW) * XW) / Factor;
    for i := 2 to Df.RowCount do
    begin
     Vec.AsFloat[i] := ((Trunc(Vec.AsFloat[i] * Factor) div XW) * XW) / Factor;
     if Vec.compare(i-1, i) = 0 then
       NVec.AsInteger[i] := NVec.AsInteger[i-1] + NVec.AsInteger[i];
    end;

    Series := TBarSeries.Create(result);
    Series.ParentChart := result;
    for i := 1 to Df.RowCount - 1 do
    begin
      if Vec.compare(i, i + 1) <> 0 then
        series.AddXY(vec.AsFloat[i], nvec.AsFloat[i],
                     vec.GetValueLabel(vec.AsString[i], Parameters),
                     TGraphUtils.GetHisColor(i-1));
    end;
    i := Df.RowCount;
    series.AddXY(vec.AsFloat[i], nvec.AsFloat[i],
                 vec.GetValueLabel(vec.AsString[i], Parameters),
                 TGraphUtils.GetHisColor(i-1));




{    // Allow groups:
    if (Parameters.VarByName['BY'] <> nil) then
    begin
      GroupName := (Parameters.VarByName['BY'].AsString);
      Grouped := true;
    end;

    total := Dataframe.RowCount;


    if Grouped then
      Varnames.Delete(Varnames.IndexOf(GroupName));

//    if df.VectorByName[Varnames[0]].DataType <> EpiTyDate then
//    begin
      for i := df.RowCount downto 1 do
        if (not df.VectorByName[Varnames[0]].IsMissing[i]) then break;
      df := df.ExpandDataframe(varnames, df.VectorByName[Varnames[0]].AsFloat[1],
                             df.VectorByName[Varnames[0]].AsFloat[i]);
//    end;

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
    result.Title.Text.Add(vec.GetVariableLabel(Parameters));
    result.BottomAxis.Title.Caption := ' ' + vec.GetVariableLabel(Parameters);

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
                     TGraphUtils.GetHisColor(i-1))
      else
        series.AddXY(vec.AsFloat[i], nvec.AsFloat[i],
                     vec.GetValueLabel(vec.AsString[i], Parameters),
                     TGraphUtils.GetHisColor(i-1));
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
                     TGraphUtils.GetHisColor(i-1));
    except
      dm.info('report problem with histogram (drawing last bar): %d', [i], 33009);
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
    CalcAxisInc(Result.BottomAxis);
    result.BottomAxis.LabelStyle := talAuto;       }
  finally

    ODebug.DecIndent;
  end;
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
    Result := dm.CodeMaker.Output.NewTable(9,1);
    REsult.TableType := sttNormal;
    Result.Caption := 'Box Plot' + Caption;
    Result.Cell[1,1] := '';
    Result.Cell[2,1] := 'N';
    Result.Cell[3,1] := 'Min';
    Result.Cell[4,1] := 'P<sub>10</sub>';
    Result.Cell[5,1] := 'P<sub>25</sub>';
    Result.Cell[6,1] := 'Median';
    Result.Cell[7,1] := 'P<sub>75</sub>';
    Result.Cell[8,1] := 'P<sub>90</sub>';
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
      Box.Color:= TGraphUtils.GetGraphColour(BoxNo);
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
        if BoxNo = 1 then dm.info('Whiskers at 10-90 percentile');
      end
      else if Assigned(Parameters.VarByName['R']) then
      begin  // use complete range
        InnerFence1 := df.VectorByName['$MIN'].AsFloat[index+1];
        AdjacentPoint1 := df.VectorByName['$MIN'].AsFloat[index+1];
        AdjacentPoint3 := df.VectorByName['$MAX'].AsFloat[index+1];
        InnerFence3 := df.VectorByName['$MAX'].AsFloat[index+1];
        if BoxNo = 1 then dm.info('Whiskers at Range');
      end
      else
      begin   // default  values, should be:
              // We need to find the correct boxplot value
                 // from EQUAL to or next observation above (p25 - 1.5*(p75-p25))
                 // to EQUAL to or observation before value (p75 + 1.5*(p75-p25))
                // find the adjacentpoints from sample values, built in function:
        if BoxNo = 1 then dm.info('Whiskers at 1.5*InterQuartile Range');
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
    result.LeftAxis.MinimumOffset := 10;
    result.LeftAxis.MaximumOffset := 10;
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
    Color := TGraphUtils.GetGraphColour(index);
    Pointer.Brush.Color := TGraphUtils.GetGraphColour(index);
    pointer.Style := TGraphUtils.GetGraphPointerStyle(index);
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
    Color := TGraphUtils.GetGraphColour(index);
    Pointer.Brush.Color := TGraphUtils.GetGraphColour(index);
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
  if varnames.Count < 1 then dm.Error('Minimum 1 variables for %s!', ['DoDotPlot'], 33005);

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
  //result.BottomAxis.EndPosition:=75;
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
      Color := TGraphUtils.GetGraphColour(pColor);
      Pointer.Brush.Color := TGraphUtils.GetGraphColour(pColor);
      pointer.Style := TGraphUtils.GetGraphPointerStyle(pStyle);
      pointer.Brush.Style := TGraphUtils.GetGraphBrushStyle(pBrush);
    end;
  end;

begin
  ODebug.Add(UnitName + ':' + procname + ' - ' + procversion, 1);
  if varnames.Count < 2 then dm.Error('Minimum 2 variables for %s!', ['DoScatter'], 33006);

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
  
  CalcAxisInc(Result.BottomAxis);
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

    if Cmd.ParamExists['O'] then
      maxval := StrToInt(Cmd.ParamByName['O'].AsString);

    // Only records with maxvalue
    for i := 1 to dataframe.RowCount do
      if yvec.Value[i] <> maxval then
        dataframe.Selected[i] := false;
    if dataframe.SelectedRowCount = 0 then dm.Error('No Data', [], 10000);

    if varnames.count < 3 then
      title := 'EpiCurve: ' + yvec.GetVariableLabel(Parameters)
    else
      title := 'EpiCurve: ' + yvec.GetVariableLabel(Parameters) +
               ' by ' + dataframe.VectorByName[varnames[2]].GetVariableLabel(Parameters);

    footnote := yvec.GetVariableLabel(Parameters) + ' = ' + yvec.GetValueLabel(VarToStr(maxval), Parameters);

    Varnames.Delete(0);
    df := dataframe.prepareDataframe(varnames, nil);
    if df.RowCount = 0 then dm.Error('No Data', [], 10000);
    
    agglist := TAggrList.Create();
    agglist.Add(TAggrCount.Create('$S', Varnames[0], acAll));
    df := OAggregate.AggregateDataframe(df, TStringList(varnames), agglist, TCommand.Create(0, Parameters));
//    df.SendToOutput(nil);
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
          dm.Error('By variable cannot contain negative values', [], 33007);
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
    
	  result.Legend.Title.Caption := zvec.GetVariableLabel(Parameters);
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
          series[zvec.AsInteger[i]].Title := zvec.GetValueLabel(zvec.AsString[i]);
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
        series[i].Color := TGraphUtils.GetGraphColour(j);
        series[i].ParentChart := result;
        inc(j);
      end;

    // Insert the data!
    i := 1;
    while (i <= df2.RowCount) do
    begin								// Why not .Compare(i, i+1)??
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
  sumtab: TSumTable;
  footnote, s: string;

const
  procname = 'DoCIPlot';
  procversion = '1.0.0.0';

  function CreateTable(var Table : TStatTable; title, variable, Outcome : string): Boolean;
  var
    opt : TEpiOption;
  begin
      Table := dm.CodeMaker.Output.NewTable(6);
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
    Table.Cell[6,Table.RowCount] := EpiCIFormat(0, low, high, efmt, cifmt, '', 0);
//    Table.Cell[7,Table.RowCount] := format(efmt,[high]);
  end;

begin
  ODebug.IncIndent;
  ODebug.Add(UnitName + ':' + procname + ' - ' + procversion, 1);
  sumtab := nil;
  df := nil;
  agglist := nil;

  try
    result := CreateStandardChart();
    result.LeftAxis.MinimumOffset := 10;
    result.LeftAxis.MaximumOffset := 10;


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
      dm.Error('Observation value not in data set', [], 33008);
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

    result.title.Caption := 'Proportion of ' + dataframe.VectorByName[Varnames[0]].GetVariableLabel(Parameters) +
                            ' = ' + s;

    Series := TCandleSeries.Create(nil);
    Series.UpCloseColor := TGraphUtils.GetGraphColour(c);
    Series.DownCloseColor := TGraphUtils.GetGraphColour(c);
    Series.Color := TGraphUtils.GetGraphColour(c);
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
        addline(result.BottomAxis, c+0.50, clBlack);
        addline(result.BottomAxis, c-0.50, clBlack);
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
      addline(result.LeftAxis, Low*100, TGraphUtils.GetGraphColour(0));
      addline(result.LeftAxis, High*100, TGraphUtils.GetGraphColour(0));
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
      if (Parameters.VarByName['NL'] = nil) then addline(result.BottomAxis, c-0.5, clBlack);
    end;
    if Parameters.VarbyName['NT'] <> Nil then dm.info(footnote, [], 0);
    Series.DateValues.DateTime := false;
    Result.BottomAxis.Labels := true;
    Result.BottomAxis.LabelStyle := talText;
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
      dm.info('%s: is not integer/date, cannot aggregate', [varnames[0]], 33010);

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
    result.LeftAxis.Title.Caption := 'Cumulative percent (%)';
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
  CalcAxisInc(Result.BottomAxis);
  ODebug.DecIndent;
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
    LineSeries.Color := TGraphUtils.GetGraphColour(j);
    LineSeries.AddXY(0, 1);
    if (not Parameters.VarExists['NOCI']) then
    begin
      CandleSeries := TCandleSeries.Create(Result);
      CandleSeries.Title := 'CI for ' + ZVec.GetValueLabel(ZVec.AsString[i+1], Parameters);
      CandleSeries.HighLowPen.Color := TGraphUtils.GetGraphColour(j);
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
    result.LeftAxis.Increment := 0.1; 

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
      dm.info('%s: illegal, must be integer variable', [varnames[0]], 33011);

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
	result.LeftAxis.Title.Caption := 'Count';

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
//                        labelvec.AsString[i],
                        labelvec.GetValueLabel(xvec.AsString[i], Parameters),
                        TGraphUtils.GetGraphColour(i-1))
      else
        barseries.AddXY(j{xvec.AsFloat[i]}, yvec.AsFloat[i],
//                        labelvec.AsString[i],
                        labelvec.GetValueLabel(xvec.AsString[i], Parameters),
                        TGraphUtils.GetGraphColour(i-1));
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
  s: string;
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
    s := Imin.AsString;
    if MibIsDate(s, ftInteger) then
      min := EpiStrToDatefmt(s, '%MDY')
    else
      min := strtofloat(s);
    s := Imax.AsString;
    if MibIsDate(s, ftInteger) then
      max := EpiStrToDatefmt(s, '%MDY')
    else
      max := strtofloat(s);
    Axis.SetMinMax(min, max);
//    Axis.LabelStyle := talValue;
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
  i: integer;
const
  procname = 'MultiVarTitle';
  procversion = '1.0.0.0';
begin
  ODebug.IncIndent();
  ODebug.Add(UnitName, Self.ClassName, procname, procversion, 1);
  result := Varlist[0].GetVariableLabel(Parameters);
  for i := 1 to Varlist.Count - 1 do
  begin
    if (i = 1) then
      result := result + ' vs. '
    else
      result := result + ' and ';
    result := result + Varlist[i].GetVariableLabel(Parameters);
  end;
  ODebug.DecIndent();
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
      tool.Shape.Font.Color := TGraphUtils.GetGraphTextColour(9);
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


procedure TGraph.CommonChartOptions(Chart: TChart);
var
  opt: TEpiOption;
  s : string;
  Dummy: Double;
  w, fs: integer;
  OptList: TStrings;

  procedure RemoveOption(OptName: string);
  begin
    if cmd.ParamExists[OptName] then
      Cmd.ParameterList.RemoveVar(Cmd.ParamByName[OptName]);
  end;

const
  procname = 'CommonChartOptions';
  procversion = '1.0.0.0';
begin
  ODebug.Add(UnitName + ':' + procname + ' - ' + procversion, 1);
  if chart = nil then exit;

  CalcAxisInc(Chart.LeftAxis, true);

  if Cmd.ParamExists['POSYTEXT'] then
    Chart.LeftAxis.LabelsSize := Cmd.ParamByName['POSYTEXT'].AsInteger;
  RemoveOption('POSYTEXT');

  // Set-options applied to graph before showing.
  dm.GetOptionValue('GRAPH FONT SIZE', opt);
  Fs := StrToInt(opt.Value);

  if Cmd.ParamExists['FONTSIZE'] then
    Fs := StrToIntDef(Cmd.ParamByName['FONTSIZE'].AsString, Fs);

  Chart.Title.Font.Size := Round(1.25 * Fs);
  Chart.Title.Font.Color := TGraphUtils.GetGraphTextColour(1);
  Chart.SubTitle.Font.Size := Round(1.1 * Fs);
  Chart.SubFoot.Font.Size := Round(0.80 * Fs);
  Chart.Foot.Font.Size := Round(0.70 * Fs);
  Chart.Foot.Font.Color := TGraphUtils.GetGraphTextColour(2);

  for w := 0 to Chart.AxesList.Count -1 do
  begin
    Chart.Axes[w].LabelsFont.Size := Max(Round(0.60 * Fs), 8);
    Chart.Axes[w].Title.Font.Size := Max(Round(0.70 * Fs), 8);
  end;

  if (Cmd.ParamExists['NOXLABEL']) then
    Chart.BottomAxis.Labels := false;
  RemoveOption('NOXLABEL');

  if (Cmd.ParamExists['NOYLABEL']) then
    Chart.LeftAxis.Labels := false;
  RemoveOption('NOYLABEL');

  if (Cmd.ParamExists['TI']) then
    Chart.Title.Caption := Cmd.ParamByName['TI'].AsString;
  RemoveOption('TI');

  if (Cmd.ParamExists['SUB']) then
    Chart.SubTitle.Text.Add(Cmd.ParamByName['SUB'].AsString)
  else
    Chart.SubTitle.Visible := false;
  RemoveOption('SUB');

  if Cmd.ParamExists['SUBFOOT'] then
    Chart.SubFoot.Text.Add(Cmd.ParamByName['SUBFOOT'].AsString)
  else
    Chart.SubFoot.Visible := false;

  //footnote
  dm.GetOptionValue('GRAPH FOOTNOTE', Opt);
  if (Cmd.ParamExists['FN']) then
  begin
    Chart.Foot.Text.Clear;
    Chart.Foot.Text.Add(cmd.ParamByName['FN'].AsString);
  end;
  RemoveOption('FN');

  // subfoot
  // TODO -o Torsten : Implement subfootnote

  // Colouring options for axis, tickmarks, axis labels and axis text.

  Chart.BottomAxis.Axis.Color := TGraphUtils.GetGraphTextColour(3);
  if cmd.CommandID <> opDotPlot then
    Chart.BottomAxis.Ticks.Color := TGraphUtils.GetGraphTextColour(4)
  else
    Chart.BottomAxis.Ticks.Color := ClWhite;
  Chart.BottomAxis.LabelsFont.Color := TGraphUtils.GetGraphTextColour(5);
  Chart.BottomAxis.Title.Font.Color := TGraphUtils.GetGraphTextColour(5);

  Chart.LeftAxis.Axis.Color := TGraphUtils.GetGraphTextColour(6);
  Chart.LeftAxis.Ticks.Color := TGraphUtils.GetGraphTextColour(7);
  Chart.LeftAxis.LabelsFont.Color := TGraphUtils.GetGraphTextColour(8);
  Chart.LeftAxis.Title.Font.Color := TGraphUtils.GetGraphTextColour(8);

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
      then dm.info('Specify both Max and Min for %s axis', ['Y'], 33012);
  RemoveOption('YMAX');
  RemoveOption('YMIN');

  if (Cmd.ParamByName['XMAX'] <> nil) or (cmd.ParamByName['XMIN'] <> nil) then
    if not AxisValue(Chart.Axes.Bottom, Cmd.ParamByName['XMIN'], Cmd.ParamByName['XMAX'], true)
      then dm.info('Specify both Max and Min for %s axis', ['X'], 33012);
  RemoveOption('XMAX');
  RemoveOption('XMIN');

  if (Cmd.ParamByName['XINC'] <> nil) then
    AxisIncrement(Chart.Axes.Bottom, Cmd.ParamByName['XINC'].AsFloat);
  RemoveOption('XINC');

  if (Cmd.ParamByName['YINC'] <> nil) then
    AxisIncrement(Chart.Axes.Left, Cmd.ParamByName['YINC'].AsFloat);
  RemoveOption('YINC');

  AxisLines(Chart, 'XLINE', Cmd);
  AxisLines(Chart, 'YLINE', Cmd);
  AxisLines(Chart, 'XLINED', Cmd);
  AxisLines(Chart, 'YLINED', Cmd);

  if Cmd.ParamByName['XTEXT'] <> nil then
  begin
    if Cmd.ParamByName['XTEXT'].AsString = '' then
      Chart.BottomAxis.Title.Visible := false
    else begin
      Chart.BottomAxis.Title.Caption := Cmd.ParamByName['XTEXT'].AsString;
      Chart.BottomAxis.Title.Visible := true
    end;
  end;
  RemoveOption('XTEXT');

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
    if Cmd.ParamByName['YTEXT'].AsString = '' then
      Chart.LeftAxis.Title.Visible := false
    else begin
      Chart.LeftAxis.Title.Caption := Cmd.ParamByName['YTEXT'].AsString;
      Chart.LeftAxis.Title.Angle := 90;
    end;
  end;
  RemoveOption('YTEXT');

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
//    if (dm.GetOptionValue('GRAPH SIZE X', Opt)) then Rect.Right := StrToInt(Opt.Value);
//    if (dm.GetOptionValue('GRAPH SIZE Y', Opt)) then Rect.Bottom := StrToInt(Opt.Value);
    Rect.Right := 400;
    Rect.Bottom := 300;
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
