unit histogram;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, chartcommandresult, executor, outputcreator, epifields_helper,
  ast, epidatafiles, epidatafilestypes, epicustombase, chartcommand, chartfactory, chartconfiguration,
  TAGraph, TALegend, tables_types, tables, freq, histogramsource, histogramdata;

type

  { THistogramChart }

  THistogramChart = class(TInterfacedObject, IChartCommand)
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

{ THistogramChart }

procedure THistogramChart.Init(ChartFactory: IChartFactory; Executor: TExecutor;
  OutputCreator: TOutputCreator);
begin
  FChartFactory := ChartFactory;
  FExecutor := Executor;
  FOutputCreator := OutputCreator;
end;

function THistogramChart.Execute(Command: TCustomGraphCommand): IChartCommandResult;
var
  {Chart}
  Chart:               TChart;
  ChartConfiguration:  IChartConfiguration;
  Titles:              IChartTitleConfiguration;
  DataFile:            TEpiDataFile;
  HistogramSource:     THistogramSource;
  HistogramData:       THistogram;
  BarSeries:           TBarSeries;
  SeriesStyles:        TChartStyles;
  aStyle:              TChartStyle;
  // for now, use default colours from classic analysis
  // TODO: put in graph options
  sColor:              TColorMap;
  {Frequencies}
  T:                   TTables;
  nilStatistics:       TTableStatistics;
  StratVariable:       TStringList;
  nilTablesRefMap:     TEpiReferenceMap;
  TableData:           TTwowayTable;
  F:                   TFreqCommand;
  FreqData:            TFreqDatafile;
  {command}
  VarNames:            TStrings;
  DFVars:              TStrings;
  XVar:                TEpiField;
  WeightVarName:       UTF8String;
  Opt:                 TOption;

  ValueLabelOutput:    TEpiGetValueLabelType;
  VariableLabelOutput: TEpiGetVariableLabelType;
  ReverseStrata:       Boolean;
  ByVarName:           UTF8String;
  i, colour:           Integer;
  sTitle:              UTF8String;
  yPct:                Boolean;
  yType:               UTF8String;

begin
  VariableLabelOutput := VariableLabelTypeFromOptionList(Command.Options, FExecutor.SetOptions);
  ValueLabelOutput    := ValueLabelTypeFromOptionList(Command.Options, FExecutor.SetOptions);
  sColor              := ChartColorsFromOptions(Command.Options, FExecutor.SetOptions);
  VarNames := Command.VariableList.GetIdentsAsList;
  DFVars := Command.VariableList.GetIdentsAsList;
  StratVariable := TStringList.Create;
  ReverseStrata := Command.HasOption('sd', Opt);
  if (ReverseStrata) then
    begin
      Command.Options.Remove(Opt); // don't pass this to TABLES!
      if (Varnames.Count = 1) then
        FOutputCreator.DoInfoShort('!sd ignored with a single variable');
    end;

   WeightVarName := '';
  if (Command.HasOption(['w'],Opt)) then
    begin
      WeightVarName := Opt.Expr.AsIdent;
      DFVars.Add(WeightVarName);
    end;

  yPct := Command.HasOption('pct');

  DataFile := FExecutor.PrepareDatafile(DFVars, DFVars);
  XVar := Datafile.Fields.FieldByName[VarNames[0]];
  Chart := FChartFactory.NewChart();
  HistogramData := THistogram.Create(FExecutor, Command);
  if (Command.HasOption('interval', Opt)) then
    HistogramData.Interval := Opt.Expr.AsInteger;
  HistogramData.PctCalc := yPct;
  if (Varnames.Count > 1) then
    begin
   // Note: this does NOT call CalcTables with stratification
      T := TTables.Create(FExecutor, FOutputCreator);
      TableData  := T.CalcTables(Datafile, VarNames,
                    StratVariable, WeightVarName, Command.Options, nilTablesRefMap, nilStatistics).UnstratifiedTable;
      if (ReverseStrata) then
        TableData.SortByRowLabel(true);
      HistogramData.Fill(TableData);
      T.Free;
      ByVarName := Datafile.Fields.FieldByName[VarNames[1]].GetVariableLabel(VariableLabelOutput);
    end
  else
    begin
      F := TFreqCommand.Create(FExecutor, FOutputCreator);
      FreqData := F.CalcFreq(Datafile, VarNames[0], nilTablesRefMap);
      HistogramData.Fill(FreqData);
      F.Free;
    end;

  HistogramSource := THistogramSource.Create(Chart);
  HistogramSource.Histogram := HistogramData;
  BarSeries := TBarSeries.Create(Chart);
  BarSeries.Source := HistogramSource;
  BarSeries.Stacked := Command.HasOption('stack');
  BarSeries.BarWidthPercent := 100;
  SeriesStyles := TChartStyles.Create(Chart);
  if (Varnames.Count > 1) then
    begin
      colour := 0;
      for i := 0 to TableData.RowCount - 1 do
        begin
          if (colour = length(sColor)) then
            colour := 0;
          aStyle := SeriesStyles.Add;
          aStyle.Text := TableData.RowVariable.GetValueLabelFormatted(i, ValueLabelOutput);
          aStyle.Brush.Color:=sColor[colour];
          aStyle.Pen.Color := clSilver;
          colour += 1;
        end;
      BarSeries.Legend.Multiplicity:=lmStyle;
      BarSeries.Legend.GroupIndex  := 0;

      Chart.Legend.Visible        := true;
      Chart.Legend.UseSidebar     := true;
      Chart.Legend.Frame.Visible  := false;
      Chart.Legend.GroupTitles.Add(ByVarName);
    end  // stratified
  else
    begin
      aStyle := SeriesStyles.Add;
      aStyle.Brush.Color := sColor[0];
      aStyle.Pen.Color := clSilver;
    end;
    BarSeries.Styles := SeriesStyles;

  Chart.AddSeries(BarSeries);

  if (yPct) then
    yType := 'Percent of Total'
  else
    yType := 'Count';
  sTitle := yType + ' by ' + XVar.GetVariableLabel(VariableLabelOutput);
  if (Varnames.Count > 1) then
    sTitle += ' by ' + ByVarName;
  if (WeightVarName <> '') then
    sTitle += ' weighted (' + WeightVarName + ')';

  ChartConfiguration := FChartFactory.NewChartConfiguration();
  Titles := ChartConfiguration.GetTitleConfiguration()
    .SetTitle(sTitle)
    .SetFootnote('')
    .SetXAxisTitle(XVar.GetVariableLabel(VariableLabelOutput))
    .SetYAxisTitle(yType);

  ChartConfiguration.GetAxesConfiguration()
    .GetXAxisConfiguration()
    .SetShowAxisMarksAsDates(XVar.FieldType in DateFieldTypes);
  ChartConfiguration.GetAxesConfiguration()
    .GetYAxisConfiguration();

  with Chart do
    begin
      BottomAxis.Marks.Source := HistogramSource.AddAxisScales(Chart);
      BottomAxis.Grid.Style   := psClear;
      BottomAxis.Margin       := 0;
      LeftAxis.Grid.Style     := psClear;
      LeftAxis.Margin         := 0;
      Frame.Visible           := false;
    end;

  Result := FChartFactory.NewGraphCommandResult();
  Result.AddChart(Chart, ChartConfiguration);
  XVar := nil;
  VarNames.Free;
  Datafile.Free;
end;

initialization
  RegisterChartCommand(stHistogram, THistogramChart);

end.
