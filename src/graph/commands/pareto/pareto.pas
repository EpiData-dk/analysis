unit pareto;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, chartcommandresult, executor, outputcreator,
  ast, epidatafiles, chartcommand, chartfactory, chartconfiguration,
  TAGraph, TAChartAxis, TATransformations,
  tables_types, tables;

type

  { TParetoChart }

  TParetoChart = class(TInterfacedObject, IChartCommand)
  private
    FChartFactory: IChartFactory;
    FExecutor: TExecutor;
    FOutputCreator: TOutputCreator;
  public
    RightAxisTransform,
    LeftAxisTransform:   TChartAxisTransformations;
    RightAxisAuto,
    LeftAxisAuto:        TAutoScaleAxisTransform;
    procedure Init(ChartFactory: IChartFactory; Executor: TExecutor; OutputCreator: TOutputCreator);
    function Execute(Command: TCustomGraphCommand): IChartCommandResult;
  end;

implementation

uses
  TASeries, TASources, TATypes, TAStyles, TAChartUtils, TALegend, TAChartAxisUtils,
  Graphics, charttitles, ast_types, epidatafilestypes,
  epicustombase, epifields_helper, options_utils, graph_utils, paretosource;

{ TParetoChart }

procedure TParetoChart.Init(ChartFactory: IChartFactory; Executor: TExecutor;
  OutputCreator: TOutputCreator);
begin
  FChartFactory := ChartFactory;
  FExecutor := Executor;
  FOutputCreator := OutputCreator;
end;

function TParetoChart.Execute(Command: TCustomGraphCommand): IChartCommandResult;
const
  dummyVarName = '_dummy4barchart';
  barTitle     = 'Count';
  lineTitle    = 'Cumulative %';
var
  {Chart}
  Chart:               TChart;
  ChartConfiguration:  IChartConfiguration;
  BarSource:           TParetoBarSource;
  LabelSeries:         TListChartSource;
  BarSeries:           TBarSeries;
  LineSource:          TParetoLineSource;
  LineSeries:          TLineSeries;
  RightAxis:           TChartAxis;
  BarStyles,
  LineStyles  :        TChartStyles;
  bStyle,
  lStyle:              TChartStyle;
  sColor:              TColorMap;
  {Frequencies}
  DataFile:            TEpiDataFile;
  T:                   TTables;
  nilStratVariable:    TStringList;
  nilTablesRefMap:     TEpiReferenceMap;
  TablesAll:           TTwoWayTables;
  TableData:           TTwowayTable;
  {command}
  VarNames:            TStrings;
  DFVars:              TStrings;
  dummyVar:            TEpiField;
  WeightVar:           TEpiField;
  WeightVarName:       UTF8String;
  XVar:                TEpiField;
  XVarTitle:           UTF8String;
  Opt:                 TOption;

  ValueLabelOutput:    TEpiGetValueLabelType;
  VariableLabelOutput: TEpiGetVariableLabelType;
  i:                   Integer;
  sTitle:              UTF8String;
begin
  VariableLabelOutput := VariableLabelTypeFromOptionList(Command.Options, FExecutor.SetOptions);
  ValueLabelOutput    := ValueLabelTypeFromOptionList(Command.Options, FExecutor.SetOptions);
  sColor              := ChartColorsFromOptions(Command.Options, FExecutor.SetOptions);
  VarNames            := Command.VariableList.GetIdentsAsList;
  DFVars              := Command.VariableList.GetIdentsAsList;
  nilStratVariable    := TStringList.Create;
  WeightVarName       := '';
  if (Command.HasOption(['w'],Opt)) then
    begin
      WeightVarName := Opt.Expr.AsIdent;
      DFVars.Add(WeightVarName);
    end;
  DataFile      := FExecutor.PrepareDatafile(DFVars, DFVars);
  Chart         := FChartFactory.NewChart();
  dummyVar      := DataFile.NewField(ftInteger);
  dummyVar.Name := dummyVarName;
  Varnames.Add(dummyVarName);
// Note: for now, only a single variable is allowed
//       for !by, set up loop to do one chart per stratum;
//       will need to sort the table output for each stratum,
//       with TableData.SortByCol(i,true) (don't need dummy var)
  T := TTables.Create(FExecutor, FOutputCreator);
  TablesAll := T.CalcTables(Datafile, VarNames,
                nilStratVariable, WeightVarName, Command.Options, nilTablesRefMap);
  TableData := TablesAll.UnstratifiedTable;
  TableData.SortByColTotal(true);

  BarSource     := TParetoBarSource.Create(Chart);
  LineSource    := TParetoLineSource.Create(Chart);
  LabelSeries   := TListChartSource.Create(Chart);
  BarSource.SetSource(TableData);
  LineSource.SetSource(TableData);
  for i := 0 to TableData.ColCount - 1 do
    LabelSeries.Add(i.ToDouble, 0, TableData.ColVariable.GetValueLabelFormatted(i, ValueLabelOutput));

  BarStyles := TChartStyles.Create(Chart);
  bStyle    := BarStyles.Add;
  with bStyle do
    begin
      Brush.Style := bsSolid;
      Brush.Color := sColor[0];
      Text        := barTitle;
    end;
  BarSeries := TBarSeries.Create(Chart);
  with BarSeries do
    begin
      Source          := BarSource;
      Stacked         := false;
      BarWidthPercent := 80;
      AxisIndexY      := 0;
      Styles          := BarStyles;
      Legend.Multiplicity:=lmStyle;
      Legend.GroupIndex := 0;
    end;

  LineStyles := TChartStyles.Create(Chart);
  lStyle     := LineStyles.Add;
  with lStyle do
    begin
      Pen.Style   := psSolid;
      Pen.Color   := sColor[1];
      Pen.Width   := 4;
      Text        := lineTitle;
    end;
  LineSeries := TLineSeries.Create(Chart);
  with LineSeries do
    begin
      Source     := LineSource;
      AxisIndexY := 2;
      Styles     := LineStyles;
      Legend.Multiplicity:=lmStyle;
      Legend.GroupIndex := 1;
    end;

  // Add series to the chart; line will be on top of bars
  Chart.AddSeries(LineSeries);
  Chart.AddSeries(BarSeries);
  with Chart.Legend do
    begin
      Visible       := true;
      UseSidebar    := true;
      Frame.Visible := false;
      Alignment     := laTopCenter;
      ColumnCount   := 2;
    end;

  XVar := Datafile.Fields.FieldByName[Varnames[0]];
  XVarTitle := XVar.GetVariableLabel(VariableLabelOutput);
  sTitle := 'Pareto Chart for ' + XVarTitle;
  if (WeightVarName <> '') then
    begin
      WeightVar := Datafile.Fields.FieldByName[WeightVarName];
      sTitle += ' weighted (' + WeightVar.GetVariableLabel(VariableLabelOutput)+ ')';
    end;
  VariableLabelOutput := VariableLabelTypeFromOptionList(Command.Options, FExecutor.SetOptions, sovStatistics);
  ChartConfiguration := FChartFactory.NewChartConfiguration();
  ChartConfiguration.GetTitleConfiguration()
    .SetTitle(sTitle)
    .SetFootnote('')
    .SetXAxisTitle(XVarTitle)
    .SetYAxisTitle(barTitle);

  ChartConfiguration.GetAxesConfiguration()
    .GetXAxisConfiguration()
    .SetShowAxisMarksAsDates(XVar.FieldType in DateFieldTypes);

  // set up left and right axis transformations to make them independent
  LeftAxisTransform     := TChartAxisTransformations.Create(Chart);
  LeftAxisAuto          := TAutoScaleAxisTransform.Create(Chart);
  LeftAxisAuto.Enabled  := true;
  LeftAxisTransform.List.Add(LeftAxisAuto);
  RightAxisTransform    := TChartAxisTransformations.Create(Chart);
  RightAxisAuto         := TAutoScaleAxisTransform.Create(Chart);
  RightAxisAuto.Enabled := true;
  RightAxisTransform.List.Add(RightAxisAuto);

  Chart.Frame.Visible := false;

  with Chart.BottomAxis do
    begin
      Marks.Source := LabelSeries;
      Marks.Style  := smsLabel;
      Grid.Style   := psClear;
      Margin       := 0;
    end;

  with Chart.LeftAxis do
    begin
      Transformations := LeftAxisTransform;
      Grid.Style      := psClear;
      Margin          := 0;
      Range.Min       := 0;
      Range.UseMin    := true;
      Range.Max       := TableData.Cell[0,0].N;
      Range.UseMax    := true;
    end;

  RightAxis := Chart.AxisList.Add;
  with RightAxis do
    begin
      Alignment                   := calRight;
      Title.Caption               := lineTitle;
      Title.Visible               := true;
      Transformations             := RightAxisTransform;
      Grid.Style                  := psClear;
      Title.LabelFont.Orientation := 900;
      Range.Min                   := 0;
      Range.UseMin                := true;
      Range.Max                   := 100;
      Range.UseMax                := true;
    end;

  Result := FChartFactory.NewGraphCommandResult();
  Result.AddChart(Chart, ChartConfiguration);
  VarNames.Free;
  DFVars.Free;
  T.Free;
  TablesAll.Free;
  XVar.Free;
  WeightVar.Free;
  Datafile.Free;
end;

initialization
  RegisterChartCommand(stPareto, TParetoChart);

end.
