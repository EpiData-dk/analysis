unit epicurve;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, chartcommandresult, executor, outputcreator,
  ast, epidatafiles, epidatafilestypes, epicustombase, chartcommand, chartfactory, chartconfiguration,
  TAGraph, tables_types, tables;

type

  floatArray = array of Double;
  { TEpicurveChart }

  TEpicurveChart = class(TInterfacedObject, IChartCommand)
  private
    FChartFactory: IChartFactory;
    FExecutor: TExecutor;
    FOutputCreator: TOutputCreator;
    function doBoxes(n: Integer): floatArray;
  public
    procedure Init(ChartFactory: IChartFactory; Executor: TExecutor; OutputCreator: TOutputCreator);
    function Execute(Command: TCustomGraphCommand): IChartCommandResult;
  end;

implementation

uses
  TASeries, TATypes, TASources, Graphics, charttitles, ast_types,
  epifields_helper, options_utils;

{ TEpicurveChart }

procedure TEpicurveChart.Init(ChartFactory: IChartFactory; Executor: TExecutor;
  OutputCreator: TOutputCreator);
begin
  FChartFactory := ChartFactory;
  FExecutor := Executor;
  FOutputCreator := OutputCreator;
end;

function TEpicurveChart.doBoxes(n: Integer): floatArray;
var
  i: Integer;
begin
  setLength(result, n);
  for i := 0 to n - 1 do
    result[i] := 1.0;
end;

function TEpicurveChart.Execute(Command: TCustomGraphCommand): IChartCommandResult;
var
  TimeSeries: TBarSeries;
  EpicurveSource: TListChartSource;
  VarNames: TStrings;
  Chart: TChart;
  Titles: IChartTitleConfiguration;
  DataFile: TEpiDataFile;
  XVar, ByVar: TEpiField;
  ChartConfiguration: IChartConfiguration;
  VariableLabelType: TEpiGetVariableLabelType;
  T: TTables;
  Freqs: TTwoWayTable;
  Statistics: TTableStatistics;
  TablesRefMap: TEpiReferenceMap;
  EpicurveTable: TTwowayTables;
  StratVariable: TStringList;
  WeightVarName: UTF8String;
  AllVariables: TStrings;
  Opt: TOption;
  Boxes: array of EpiFloat;
  ix, iy: Integer;
  ixLow, ixHigh: Integer;
begin
  // Get Variable names
  VarNames := Command.VariableList.GetIdentsAsList;
  AllVariables := VarNames;
  StratVariable := TStringList.Create;

  // check for stratifying variable
  if (Command.HasOption(['by'],Opt)) then
    begin
      StratVariable.Add(Opt.Expr.AsIdent);
      AllVariables.Add(Opt.Expr.AsIdent);
    end;
  // Get the data and fields.
  DataFile := FExecutor.PrepareDatafile(AllVariables, AllVariables);
  XVar := Datafile.Fields.FieldByName[VarNames[0]];
  WeightVarName := '';
  // Get frequencies by time variable
  T := TTables.Create(FExecutor, FOutputCreator);
  EpicurveTable  := T.CalcTables(Datafile, VarNames,
    StratVariable, WeightVarName, Command.Options, TablesRefMap, Statistics);
  Freqs := EpicurveTable.UnstratifiedTable;

  // Create the chart
  Chart := FChartFactory.NewChart();

  // Create the line/point series
  TimeSeries := TBarSeries.Create(Chart);
  EpicurveSource := TListChartSource.Create(Chart);
  iy := 0;
  ixLow := Freqs.RowVariable.AsInteger[0];
  ixHigh := Freqs.RowVariable.AsInteger[Freqs.RowCount - 1];
  for ix := ixLow to ixHigh do
    begin
      if (ix < Freqs.RowVariable.AsInteger[iy]) then
        EpicurveSource.Add(ix.ToDouble, 0.0)
      else
        begin
          EpicurveSource.AddXYList(ix.ToDouble, doBoxes(Freqs.RowTotal[iy] - 1));
          iy += 1;
        end;
    end;

  TimeSeries.Stacked := true;
  TimeSeries.Source := EpicurveSource;

  // Add series to the chart
  Chart.AddSeries(TimeSeries);

  // Create the titles
  ChartConfiguration := FChartFactory.NewChartConfiguration();
  VariableLabelType := VariableLabelTypeFromOptionList(Command.Options, FExecutor.SetOptions, sovStatistics);
  Titles := ChartConfiguration.GetTitleConfiguration()
    .SetTitle('Epidemic Curve for ' + XVar.GetVariableLabel(VariableLabelType))
    .SetFootnote('')
    .SetXAxisTitle(XVar.GetVariableLabel(VariableLabelType));

  ChartConfiguration.GetAxesConfiguration()
    .GetXAxisConfiguration()
    .SetShowAxisMarksAsDates(XVar.FieldType in DateFieldTypes);

  ChartConfiguration.GetAxesConfiguration()
    .GetYAxisConfiguration();

  // Create the command result
  Result := FChartFactory.NewGraphCommandResult();
  Result.AddChart(Chart, ChartConfiguration);
  AllVariables.Free;
end;

initialization
  RegisterChartCommand(stEpicurve, TEpicurveChart);

end.
