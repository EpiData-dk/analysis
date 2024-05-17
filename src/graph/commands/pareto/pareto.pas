unit pareto;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, chartcommandresult, executor, outputcreator,
  ast, epidatafiles, chartcommand, chartfactory, chartconfiguration,
  TAGraph, TAChartAxis, TATransformations,
  epifields_helper, tables_types, tables,
  chart_options;

type

  { TParetoChart }

  TParetoChart = class(TInterfacedObject, IChartCommand)
  private
    FChartFactory:        IChartFactory;
    FExecutor:            TExecutor;
    FOutputCreator:       TOutputCreator;
    FValueLabelOutput:    TEpiGetValueLabelType;
    FVariableLabelOutput: TEpiGetVariableLabelType;
    FXVarTitle,
    FWVarTitle:           UTF8String;
    FXDate:               Boolean;
    FColor:               TColorMap;
  protected
    procedure DoOneChart(TableData: TTwoWayTable; StratumValue, StratumLabel: UTF8String;
          ChartResult: IChartCommandResult);
  public
    procedure Init(ChartFactory: IChartFactory; Executor: TExecutor; OutputCreator: TOutputCreator);
    function Execute(Command: TCustomGraphCommand): IChartCommandResult;
  end;

implementation

uses
  TASeries, TASources, TATypes, TAStyles, TAChartUtils, TALegend, TAChartAxisUtils,
  Graphics, charttitles, ast_types,
  epicustombase, options_utils, epidatafilestypes, paretosource;

{ TParetoChart }

procedure TParetoChart.Init(ChartFactory: IChartFactory; Executor: TExecutor;
  OutputCreator: TOutputCreator);
begin
  FChartFactory  := ChartFactory;
  FExecutor      := Executor;
  FOutputCreator := OutputCreator;
end;

function TParetoChart.Execute(Command: TCustomGraphCommand): IChartCommandResult;
const
  dummyVarName = '_dummy4chart';
var
  {Frequencies}
  DataFile:            TEpiDataFile;
  T:                   TTables;
  StratifyVarnames:    TStringList;
  nilTablesRefMap:     TEpiReferenceMap;
  TablesAll:           TTwoWayTables;
  TableData:           TTwowayTable;
  {command}
  VarNames:            TStrings;
  DFVars:              TStrings;
  dummyVar:            TEpiField;
  StratVar:            TEpiField;
  WeightVar:           TEpiField;
  WeightVarName:       UTF8String;
  msg:                 UTF8String;
  XVar:                TEpiField;
  Opt:                 TOption;
  byVar:               Boolean;
  i:                   Integer;
begin
  FVariableLabelOutput := VariableLabelTypeFromOptionList(Command.Options, FExecutor.SetOptions);
  FValueLabelOutput    := ValueLabelTypeFromOptionList(Command.Options, FExecutor.SetOptions);
  FColor               := ChartColorsFromOptions(Command.Options, FExecutor.SetOptions, msg);
  if (msg <> '') then
    begin
      FExecutor.Error(msg);
      exit;
    end;
  VarNames             := Command.VariableList.GetIdentsAsList;
  DFVars               := Command.VariableList.GetIdentsAsList;

  StratifyVarnames := TStringList.Create;
  WeightVarName    := '';
  for Opt in Command.Options do
    case Opt.Ident of
      'by':
        if (StratifyVarnames.Count = 0) then
          begin
            DFVars.Add(Opt.Expr.AsIdent);
            StratifyVarnames.Add(Opt.Expr.AsIdent);
          end
        else
          FExecutor.Error('Multiple !by options are not allowed. ' + Opt.Expr.AsIdent + ' ignored.');
      'w':
        if (WeightVarName = '') then
          begin
            WeightVarName := Opt.Expr.AsIdent;
            DFVars.Add(WeightVarName);
          end
        else
          FExecutor.Error('Multiple !w options are not allowed. ' + Opt.Expr.AsIdent + ' ignored.');
    end;

  byVar := (StratifyVarnames.Count = 1);

  Result := FChartFactory.NewGraphCommandResult();

  DataFile := FExecutor.PrepareDatafile(DFVars, DFVars);
  if (DataFile.Size < 1) then
    FExecutor.Error('No data')
  else
    begin // create charts
      if (WeightVarName <> '') then
        begin
          WeightVar  := Datafile.Fields.FieldByName[WeightVarName];
          FWVarTitle += ' weighted (' + WeightVar.GetVariableLabel(FVariableLabelOutput)+ ')';
          WeightVar.Free;
        end
      else
        FWVarTitle := '';
      dummyVar      := DataFile.NewField(ftInteger);
      dummyVar.Name := dummyVarName;
      Varnames.Add(dummyVarName);
      T := TTables.Create(FExecutor, FOutputCreator);
      TablesAll := T.CalcTables(Datafile, VarNames,
                    StratifyVarnames, WeightVarName, Command.Options, nilTablesRefMap);
      XVar := Datafile.Fields.FieldByName[Varnames[0]];
      FXVarTitle := XVar.GetVariableLabel(FVariableLabelOutput);
      FXDate := XVar.FieldType in DateFieldTypes;

      if (byVar) then
        begin
          for i := 0 to TablesAll.Count - 1 do
            begin
              TableData := TablesAll.Tables[i];
              TableData.SortByColTotal(true);
              StratVar := TablesAll.StratifyVariables[0];
              if (TableData.Total > 0) then
                DoOneChart(TableData,
                  StratVar.GetValueLabel(i, gvtValue),
                  StratVar.GetVariableLabel(FVariableLabelOutput)
                    + '=' + StratVar.GetValueLabel(i, FValueLabelOutput),
                  Result);
            end
        end
      else
        begin
          TableData := TablesAll.UnstratifiedTable;
          TableData.SortByColTotal(true);
          DoOneChart(TableData, '', '', Result);
        end;

      T.Free;
      TablesAll.Free;
      XVar.Free;
      dummyVar.Free;
    end;  // create charts

  StratifyVarNames.Free;
  VarNames.Free;
  DFVars.Free;
  Datafile.Free;
end;

procedure TParetoChart.DoOneChart(TableData: TTwoWayTable;
          StratumValue, StratumLabel: UTF8STring; ChartResult: IChartCommandResult);
const
  barTitle     = 'Count';
  lineTitle    = 'Cumulative %';
var
  i:                   Integer;
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
  sTitle:              UTF8String;
  RightAxisTransform,
  LeftAxisTransform:   TChartAxisTransformations;
  RightAxisAuto,
  LeftAxisAuto:        TAutoScaleAxisTransform;
  xisDate:             Boolean;
  xLabelStr:           String;
begin
  Chart         := FChartFactory.NewChart();
  BarSource     := TParetoBarSource.Create(Chart);
  LineSource    := TParetoLineSource.Create(Chart);
  LabelSeries   := TListChartSource.Create(Chart);
  BarSource.SetSource(TableData);
  LineSource.SetSource(TableData);

  xisDate := TableData.ColVariable.FieldType in DateFieldTypes;
  for i := 0 to TableData.ColCount - 1 do
    begin
      if (xisDate) then
        xLabelStr := DateToStr(TableData.ColVariable.AsDateTime[i])
      else
        xLabelStr := TableData.ColVariable.GetValueLabelFormatted(i, FValueLabelOutput);
      LabelSeries.Add(i.ToDouble, 0, xLabelStr);
    end;

  BarStyles := TChartStyles.Create(Chart);
  bStyle    := BarStyles.Add;
  with bStyle do
    begin
      Brush.Style := bsSolid;
      Brush.Color := FColor[0];
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
      Pen.Color   := FColor[1];
      Pen.Width   := 8;
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

  sTitle := 'Pareto Chart for ' + FXVarTitle + FWVarTitle;
  if (StratumLabel <> '') then
    sTitle += LineEnding + StratumLabel;
  ChartConfiguration := FChartFactory.NewChartConfiguration();
  ChartConfiguration.GetTitleConfiguration()
    .SetTitle(sTitle)
    .SetFootnote('')
    .SetXAxisTitle(FXVarTitle)
    .SetYAxisTitle(barTitle)
    .SetY2AxisTitle(lineTitle)
    .SetStratumValue(StratumValue);

  ChartConfiguration.GetAxesConfiguration()
    .GetXAxisConfiguration();
  // axis labels are already set, regardless of data type
  // don't reformat dates via XAxisConfiguration

  // set up left and right axis transformations to make them independent
  LeftAxisTransform     := TChartAxisTransformations.Create(nil);
  LeftAxisAuto          := TAutoScaleAxisTransform.Create(nil);
  LeftAxisAuto.Enabled  := true;
  LeftAxisTransform.List.Add(LeftAxisAuto);
  RightAxisTransform    := TChartAxisTransformations.Create(nil);
  RightAxisAuto         := TAutoScaleAxisTransform.Create(nil);
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
    end;

  ChartResult.AddChart(Chart, ChartConfiguration);
end;

initialization
  RegisterChartCommand(stPareto, TParetoChart);

end.
