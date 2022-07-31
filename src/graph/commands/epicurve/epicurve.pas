unit epicurve;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, chartcommandresult, executor, outputcreator, epifields_helper,
  ast, epidatafiles, epidatafilestypes, epicustombase, chartcommand, chartfactory, chartconfiguration,
  TAGraph, TASources, TACustomSource, TALegend, tables_types, tables, freq, histogramsource;

type

  floatArray = array of Double;
  freqArray  = array of array of Double;
  { TEpicurveChart }

  TEpicurveChart = class(TInterfacedObject, IChartCommand)
  private
    FChartFactory: IChartFactory;
    FChart: TChart;
    FExecutor: TExecutor;
    FOutputCreator: TOutputCreator;
    FValueLabelOutput:    TEpiGetValueLabelType;
    FVariableLabelOutput: TEpiGetVariableLabelType;
    FHistogramSource: THistogramSource;
    FByVarName: UTF8String;
  public
    procedure Init(ChartFactory: IChartFactory; Executor: TExecutor; OutputCreator: TOutputCreator);
    function Execute(Command: TCustomGraphCommand): IChartCommandResult;
  end;

implementation

uses
  TASeries, TATypes, TAStyles, Graphics, charttitles, ast_types,
  options_utils, math;

{ TEpicurveChart }

procedure TEpicurveChart.Init(ChartFactory: IChartFactory; Executor: TExecutor;
  OutputCreator: TOutputCreator);
begin
  FChartFactory := ChartFactory;
  FExecutor := Executor;
  FOutputCreator := OutputCreator;
end;

function TEpicurveChart.Execute(Command: TCustomGraphCommand): IChartCommandResult;
var
  {Charts}
  ChartConfiguration: IChartConfiguration;
  Titles:             IChartTitleConfiguration;
  DataFile:           TEpiDataFile;
  BarSeries:          TBarSeries;
  SeriesStyles:       TChartStyles;
  aStyle:             TChartStyle;
  sColor:             array of TColor = (clRed, clBlue, clGreen, clYellow, clGray);
  {Frequencies}
  T:                  TTables;
  Statistics:         TTableStatistics;
  StratVariable:      TStringList;
  TablesRefMap:       TEpiReferenceMap;
  TableData:          TTwowayTable;
  F:                  TFreqCommand;
  FreqData:           TFreqDatafile;
  {command}
  VarNames:           TStrings;
  XVar:               TEpiField;
  WeightVarName:      UTF8String;
  AllVariables:       TStrings;
  Opt:                TOption;

  i,k:                Integer;
  sTitle:             UTF8String;
begin
  FVariableLabelOutput := VariableLabelTypeFromOptionList(Command.Options, FExecutor.SetOptions);
  FValueLabelOutput    := ValueLabelTypeFromOptionList(Command.Options, FExecutor.SetOptions);

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
  FHistogramSource := THistogramSource.Create(FChart);
  FHistogramSource.Reset;

// add series for the time variable
// method depends on stratification or not
  if (Varnames.Count > 1) then
    begin
  // with stratification
  // Note: this does NOT call CalcTables with stratification
      T := TTables.Create(FExecutor, FOutputCreator);
      TableData  := T.CalcTables(Datafile, VarNames,
                    StratVariable, WeightVarName, Command.Options, TablesRefMap, Statistics).UnstratifiedTable;
      FHistogramSource.FromTable(TableData);
      T.Free;
      FByVarName := Datafile.Fields.FieldByName[VarNames[1]].GetVariableLabel(FVariableLabelOutput);
    end
  else
    begin
      F := TFreqCommand.Create(FExecutor, FOutputCreator);
      FreqData := F.CalcFreq(Datafile, VarNames[0],TablesRefMap);
      FHistogramSource.boxes := true;
      FHistogramSource.FromFreq(FreqData);
      F.Free;
    end;

  BarSeries.Source := FHistogramSource;
  BarSeries.Stacked := true;
  BarSeries.BarWidthPercent := 100;
  SeriesStyles := TChartStyles.Create(FChart);
  FChart.AddSeries(BarSeries);
  if (Varnames.Count > 1) then
    begin
      k := 0;
      for i := 0 to TableData.RowCount - 1 do
        begin
          aStyle := SeriesStyles.Add;
          aStyle.Text := TableData.RowVariable.GetValueLabelFormatted(i, FValueLabelOutput);
          if (k = length(sColor)) then k := 0;     // if more strata than colours, recycle the colours
          aStyle.Brush.Color:=sColor[k];
          k += 1;
        end;

      BarSeries.Styles := SeriesStyles;
      BarSeries.Legend.Multiplicity:=lmStyle;
      BarSeries.Legend.GroupIndex  := 0;

      FChart.Legend.Visible        := true;
      FChart.Legend.UseSidebar     := true;
      FChart.Legend.Frame.Visible  := false;
      FChart.Legend.GroupTitles.Add(FByVarName);
    end;

  // Create the titles
  sTitle := 'Count by ' + XVar.GetVariableLabel(FVariableLabelOutput);
  if (Varnames.Count > 1) then
    sTitle += ' by ' + FByVarName;
  ChartConfiguration := FChartFactory.NewChartConfiguration();
  Titles := ChartConfiguration.GetTitleConfiguration()
    .SetTitle(sTitle)
    .SetFootnote('')
    .SetXAxisTitle(XVar.GetVariableLabel(FVariableLabelOutput))
    .SetYAxisTitle('Count');

  ChartConfiguration.GetAxesConfiguration()
    .GetXAxisConfiguration()
    .SetShowAxisMarksAsDates(XVar.FieldType in DateFieldTypes);
  ChartConfiguration.GetAxesConfiguration()
    .GetYAxisConfiguration();


  with FChart do
    begin
      BottomAxis.Marks.Source := FHistogramSource.AddAxisScales(FChart);
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
