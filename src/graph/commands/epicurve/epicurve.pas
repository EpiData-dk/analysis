unit epicurve;

{$mode objfpc}{$H+}

// TODO: see histogram
// TODO: allow option !nobox (equivalent to histogram, so just invoke it instead)
interface

uses
  Classes, SysUtils, chartcommandresult, executor, outputcreator, epifields_helper,
  ast, epidatafiles, epidatafilestypes, epicustombase, chartcommand, chartfactory, chartconfiguration,
  TAGraph, TASources, TALegend, tables_types, tables, freq, histogramsource;

type

  floatArray = array of Double;
  freqArray  = array of array of Double;
  { TEpicurveChart }

  TEpicurveChart = class(TInterfacedObject, IChartCommand)
  private
    FChartFactory: IChartFactory;
    FExecutor: TExecutor;
    FOutputCreator: TOutputCreator;
  public
    procedure Init(ChartFactory: IChartFactory; Executor: TExecutor; OutputCreator: TOutputCreator);
    function Execute(Command: TCustomGraphCommand): IChartCommandResult;
  end;

implementation

uses
  TASeries, TATypes, TAStyles, Graphics, charttitles, ast_types,
  options_utils;

{ TEpicurveChart }

procedure TEpicurveChart.Init(ChartFactory: IChartFactory; Executor: TExecutor;
  OutputCreator: TOutputCreator);
begin
  FChartFactory  := ChartFactory;
  FExecutor      := Executor;
  FOutputCreator := OutputCreator;
end;

function TEpicurveChart.Execute(Command: TCustomGraphCommand): IChartCommandResult;
var
  {Chart}
  FChart:              TChart;
  ChartConfiguration:  IChartConfiguration;
  Titles:              IChartTitleConfiguration;
  DataFile:            TEpiDataFile;
  BarSeries:           TBarSeries;
  SeriesStyles:        TChartStyles;
  HistogramSource:     THistogramSource;
  LegendSeries:        TBarSeries;
  LegendStyles:        TChartStyles;
  LegendSource:        TListChartSource;
  aStyle:              TChartStyle;
  // for now, use default colours from classic analysis
  // TODO: put in graph options
  sColor:              array of TColor = (clBlue, clRed, clBlack, clGreen, clYellow, clWhite, clSkyBlue, clFuchsia, clGray, clAqua);
  {Frequencies}
  T:                   TTables;
  Statistics:          TTableStatistics;
  StratVariable:       TStringList;
  TablesRefMap:        TEpiReferenceMap;
  TableData:           TTwowayTable;
  F:                   TFreqCommand;
  FreqData:            TFreqDatafile;
  {command}
  VarNames:            TStrings;
  XVar:                TEpiField;
  WeightVarName:       UTF8String;
  AllVariables:        TStrings;
  {formatting}
  ValueLabelOutput:    TEpiGetValueLabelType;
  VariableLabelOutput: TEpiGetVariableLabelType;
  ByVarName:           UTF8String;
  box1, box, colour:   Integer;
  i:                   Integer;
  sTitle:              UTF8String;
begin
  VariableLabelOutput := VariableLabelTypeFromOptionList(Command.Options, FExecutor.SetOptions);
  ValueLabelOutput    := ValueLabelTypeFromOptionList(Command.Options, FExecutor.SetOptions);

  // Get Variable names
  VarNames := Command.VariableList.GetIdentsAsList;
  AllVariables := Command.VariableList.GetIdentsAsList;
  StratVariable := TStringList.Create;

  // weight variable not allowed for epicurve
  WeightVarName := '';

  // Get the data and fields.
  DataFile := FExecutor.PrepareDatafile(AllVariables, AllVariables);
  XVar := Datafile.Fields.FieldByName[VarNames[0]];

  // Create the chart
  FChart := FChartFactory.NewChart();
  BarSeries := TBarSeries.Create(FChart);
  HistogramSource := THistogramSource.Create(FChart);
  HistogramSource.Reset;

// add series for the time variable
// method depends on stratification or not
  if (Varnames.Count > 1) then
    begin
  // with stratification
  // Note: this does NOT call CalcTables with stratification
      T := TTables.Create(FExecutor, FOutputCreator);
      TableData  := T.CalcTables(Datafile, VarNames,
                    StratVariable, WeightVarName, Command.Options, TablesRefMap, Statistics).UnstratifiedTable;
      HistogramSource.boxes := true;
      HistogramSource.FromTable(TableData);
      T.Free;
      ByVarName := Datafile.Fields.FieldByName[VarNames[1]].GetVariableLabel(VariableLabelOutput);
    end
  else
    begin
      F := TFreqCommand.Create(FExecutor, FOutputCreator);
      FreqData := F.CalcFreq(Datafile, VarNames[0],TablesRefMap);
      HistogramSource.boxes := true;
      HistogramSource.FromFreq(FreqData);
      F.Free;
    end;

  SeriesStyles := TChartStyles.Create(FChart);
  BarSeries.Source := HistogramSource;
  BarSeries.Stacked := true;
  BarSeries.BarWidthPercent := 100;
  BarSeries.ShowInLegend := false;
  BarSeries.Styles := SeriesStyles;
  FChart.AddSeries(BarSeries);

  // for stratified graph, set colours and legend
  if (Varnames.Count > 1) then
    begin
      // set up Legend series
      LegendSource := TListChartSource.Create(FChart);
      LegendSeries := TBarSeries.Create(FChart);
      LegendSeries.Source := LegendSource;
      LegendStyles := TChartStyles.Create(FChart);
      FChart.AddSeries(LegendSeries);

      colour := 0;
      box1 := 0;
      for i := 0 to TableData.RowCount - 1 do
        begin
          // individual box styles
          for box := box1 to box1 + HistogramSource.RowHeight[i] - 1 do
            begin
              aStyle := SeriesStyles.Add;
              aStyle.Brush.Color:=sColor[colour];
              aStyle.Pen.Color := clSilver;     // will work with any box colours
            end;
          LegendSource.Add(HistogramSource.X0.ToDouble + i, 0);
          aStyle := LegendStyles.Add;
          aStyle.Text := TableData.RowVariable.GetValueLabelFormatted(i, ValueLabelOutput);
          aStyle.Brush.Color:=sColor[colour];
          if (colour = length(sColor)) then colour := 0;     // if more strata than colours, recycle the colours
          colour += 1;
          box1 += HistogramSource.RowHeight[i];
        end;

      LegendSeries.Stacked := true;
      LegendSeries.Styles := LegendStyles;
      LegendSeries.Legend.Multiplicity:=lmStyle;
      LegendSeries.Legend.GroupIndex  := 0;

      FChart.Legend.Visible        := true;
      FChart.Legend.UseSidebar     := true;
      FChart.Legend.Frame.Visible  := false;
      FChart.Legend.GroupTitles.Add(ByVarName);
    end  // stratified
  else
    begin
      aStyle := SeriesStyles.Add;
      aStyle.Brush.Color := sColor[0];
      aStyle.Pen.Color := clSilver;
    end;

  // Create the titles
  sTitle := 'Count by ' + XVar.GetVariableLabel(VariableLabelOutput);
  if (Varnames.Count > 1) then
    sTitle += ' by ' + ByVarName;
  ChartConfiguration := FChartFactory.NewChartConfiguration();
  Titles := ChartConfiguration.GetTitleConfiguration()
    .SetTitle(sTitle)
    .SetFootnote('')
    .SetXAxisTitle(XVar.GetVariableLabel(VariableLabelOutput))
    .SetYAxisTitle('Count');

  ChartConfiguration.GetAxesConfiguration()
    .GetXAxisConfiguration()
    .SetShowAxisMarksAsDates(XVar.FieldType in DateFieldTypes);
  ChartConfiguration.GetAxesConfiguration()
    .GetYAxisConfiguration();


  with FChart do
    begin
      BottomAxis.Marks.Source := HistogramSource.AddAxisScales(FChart);
      BottomAxis.Grid.Style   := psClear;
      BottomAxis.Margin       := 0;
      LeftAxis.Grid.Style     := psClear;
      LeftAxis.Margin         := 0;
      Frame.Visible           := false;
    end;
  // Create the command result
  Result := FChartFactory.NewGraphCommandResult();
  Result.AddChart(FChart, ChartConfiguration);
  XVar := nil;
  AllVariables.Free;
  Datafile.Free;
end;

initialization
  RegisterChartCommand(stEpicurve, TEpicurveChart);

end.
