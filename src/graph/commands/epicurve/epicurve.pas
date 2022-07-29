unit epicurve;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, chartcommandresult, executor, outputcreator, epifields_helper,
  ast, epidatafiles, epidatafilestypes, epicustombase, chartcommand, chartfactory, chartconfiguration,
  TAGraph, TASources, TACustomSource, TALegend, tables_types, tables, freq, epicurvesource;

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
    FEpicurveSource: TEpicurveSource;
    FByVarName: UTF8String;
    FxAxisSource, FyAxisSource: TListChartSource;
    procedure doAddAxisScales(X0, Xn, Ymax: Integer);
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

procedure TEpicurveChart.doAddAxisScales(X0, Xn, Ymax: Integer);
var
  i: Integer;
  tick: Double;
begin
  FxAxisSource := TListChartSource.Create(FChart);
  FyAxisSource := TListChartSource.Create(FChart);
  for i := X0 to Xn do
    begin
      tick := i.ToDouble;
      FxAxisSource.Add(tick, tick);
    end;
  tick := 0;
  for i := 0 to (yMax div 5) do
    begin
      FyAxisSource.Add(tick, tick);
      tick += 5;
    end;
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
  EpicurveTable:      TTwowayTable;
  F:                  TFreqCommand;
  EpicurveFreq:       TFreqDatafile;
  {command}
  VarNames:           TStrings;
  XVar:               TEpiField;
  WeightVarName:      UTF8String;
  AllVariables:       TStrings;
  Opt:                TOption;

  i,k:                  Integer;
  s:string;
begin
  FVariableLabelOutput := VariableLabelTypeFromOptionList(Command.Options, FExecutor.SetOptions);
  FValueLabelOutput    := ValueLabelTypeFromOptionList(Command.Options, FExecutor.SetOptions);

  // Get Variable names
  VarNames := Command.VariableList.GetIdentsAsList;
  AllVariables := Command.VariableList.GetIdentsAsList;
  StratVariable := TStringList.Create;

  // check for weight variable
  WeightVarName := '';
  if (Command.HasOption(['w'],Opt)) then
    begin
      WeightVarName := Opt.Expr.AsIdent;
      AllVariables.Add(WeightVarName);
    end;
  k := AllVariables.Count;
  s := AllVariables.Names[k-1];
  {// check for stratifying variable  For now, 2nd variable is for strata
  if (Command.HasOption(['by'],Opt)) then
    begin
      VarNames.Add(Opt.Expr.AsIdent);
      AllVariables.Add(Opt.Expr.AsIdent);
    end;
  }
  // Get the data and fields.
  DataFile := FExecutor.PrepareDatafile(AllVariables, AllVariables);
  XVar := Datafile.Fields.FieldByName[VarNames[0]];

  for i := 0 to k-1 do
    s:=Datafile.Fields.Field[i].Name;
  // Create the chart
  FChart := FChartFactory.NewChart();
  BarSeries := TBarSeries.Create(FChart);
  FEpicurveSource := TEpicurveSource.Create(FChart);
  FEpicurveSource.Reset;

// add series for the time variable
// method depends on stratification or not
  if (Varnames.Count > 1) then
    begin
  // with stratification
  // Note: this does NOT call CalcTables with stratification
      T := TTables.Create(FExecutor, FOutputCreator);
      EpicurveTable  := T.CalcTables(Datafile, VarNames,
                    StratVariable, WeightVarName, Command.Options, TablesRefMap, Statistics).UnstratifiedTable;
      FEpicurveSource.FromTable(EpicurveTable);
      T.Free;
      FByVarName := Datafile.Fields.FieldByName[VarNames[1]].GetVariableLabel(FVariableLabelOutput);
    end
  else
    begin
      F := TFreqCommand.Create(FExecutor, FOutputCreator);
      EpicurveFreq := F.CalcFreq(Datafile, VarNames[0],TablesRefMap);
      FEpicurveSource.boxes := true;;
      FEpicurveSource.FromFreq(EpicurveFreq);
      F.Free;
    end;

  BarSeries.Source := FEpicurveSource;
  BarSeries.Stacked := true;
  BarSeries.BarWidthPercent := 100;
  SeriesStyles := TChartStyles.Create(FChart);
  FChart.AddSeries(BarSeries);
  if (Varnames.Count > 1) then
    begin
      for i := 0 to EpicurveTable.RowCount - 1 do
        begin
          aStyle := SeriesStyles.Add;
          aStyle.Text := EpicurveTable.RowVariable.GetValueLabelFormatted(i, FValueLabelOutput);
          aStyle.Brush.Color:=sColor[i];
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
  ChartConfiguration := FChartFactory.NewChartConfiguration();
  Titles := ChartConfiguration.GetTitleConfiguration()
    .SetTitle('Count by ' + XVar.GetVariableLabel(FVariableLabelOutput))
    .SetFootnote('')
    .SetXAxisTitle(XVar.GetVariableLabel(FVariableLabelOutput))
    .SetYAxisTitle('Count');

  ChartConfiguration.GetAxesConfiguration()
    .GetXAxisConfiguration()
    .SetShowAxisMarksAsDates(XVar.FieldType in DateFieldTypes);
  ChartConfiguration.GetAxesConfiguration()
    .GetYAxisConfiguration();
  doAddAxisScales(FEpicurveSource.X0, FEpicurveSource.Xn, FEpicurveSource.maxCount);

  with FChart do
    begin
      BottomAxis.Marks.Source := FxAxisSource;
      BottomAxis.Grid.Style   := psClear;
      BottomAxis.Margin       := 0;
      LeftAxis.Grid.Style     := psClear;
      LeftAxis.Marks.Source   := FyAxisSource;
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
