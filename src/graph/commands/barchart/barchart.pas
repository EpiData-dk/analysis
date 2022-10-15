unit barchart;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, chartcommandresult, TAGraph, executor, outputcreator,
  ast, epidatafiles, chartcommand, chartfactory, chartconfiguration,
  tables_types, tables, freq;

type

  { TBarChart }

  TBarChart = class(TInterfacedObject, IChartCommand)
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
  TASeries, TASources, TATypes, TAStyles, TAChartUtils, TALegend,
  Graphics, charttitles, ast_types, epidatafilestypes,
  epicustombase, epifields_helper, options_utils, barchartsource;

{ TBarChart }

procedure TBarChart.Init(ChartFactory: IChartFactory; Executor: TExecutor;
  OutputCreator: TOutputCreator);
begin
  FChartFactory := ChartFactory;
  FExecutor := Executor;
  FOutputCreator := OutputCreator;
end;

function TBarChart.Execute(Command: TCustomGraphCommand): IChartCommandResult;
var
  {Chart}
  Chart:               TChart;
  ChartConfiguration:  IChartConfiguration;
  Titles:              IChartTitleConfiguration;
  BarSource:           TBarSource;
  LabelSeries:         TListChartSource;
  BarSeries:           TBarSeries;
  SeriesStyles:        TChartStyles;
  aStyle:              TChartStyle;
  // TODO: put in graph options
  sColor:              TColorMap;
  {Frequencies}
  DataFile:            TEpiDataFile;
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
  dummyVar:            TEpiField;
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
  VarNames            := Command.VariableList.GetIdentsAsList;
  DFVars              := Command.VariableList.GetIdentsAsList; // variable order may change when adding weight var
  StratVariable       := TStringList.Create;
  ReverseStrata       := Command.HasOption('sd', Opt);
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
  BarSource := TBarSource.Create(Chart);
  BarSource.Pct := yPct;
  LabelSeries := TListChartSource.Create(Chart);
  if (Varnames.Count = 1) then
// add dummy variable to use Tables with one variable plus weight variable
    begin
      dummyVar := DataFile.NewField(ftInteger);
      dummyVar.Name := '_dummy4barchart';    // for random string:  IntToHex(Random(Int64($7fffffffffffffff)), 16);
      Varnames.Add('_dummy4barchart');
    end;
// Note: this does NOT call CalcTables with stratification
  T := TTables.Create(FExecutor, FOutputCreator);
  TableData  := T.CalcTables(Datafile, VarNames,
                StratVariable, WeightVarName, Command.Options, nilTablesRefMap, nilStatistics).UnstratifiedTable;
  if (ReverseStrata) then
    TableData.SortByRowLabel(true);
  BarSource.SetSource(TableData, ValueLabelOutput);
  ByVarName := Datafile.Fields.FieldByName[VarNames[1]].GetVariableLabel(VariableLabelOutput);
  for i := 0 to TableData.ColCount - 1 do
    LabelSeries.Add(i.ToDouble, 0, TableData.ColVariable.GetValueLabelFormatted(i, ValueLabelOutput));
// if dummy var was created, remove it now
  if (Varnames.IndexOf('_dummy4barchart') > -1) then
    Varnames.Delete(Varnames.IndexOf('_dummy4barchart'));

  BarSeries := TBarSeries.Create(Chart);
  BarSeries.Source := BarSource;
  BarSeries.Stacked := Command.HasOption('stack');
  BarSeries.BarWidthPercent := 80;
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

  // Add series to the chart
  Chart.AddSeries(BarSeries);

  if (yPct) then
    yType := 'Percent'
  else
    yType := 'Count';
  sTitle := yType + ' by ' + XVar.GetVariableLabel(VariableLabelOutput);
  if (Varnames.Count > 1) then
    sTitle += ' by ' + ByVarName;
  if (WeightVarName <> '') then
    sTitle += ' weighted (' + WeightVarName + ')';

  ChartConfiguration := FChartFactory.NewChartConfiguration();
  VariableLabelOutput := VariableLabelTypeFromOptionList(Command.Options, FExecutor.SetOptions, sovStatistics);
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
      BottomAxis.Marks.Source := LabelSeries;
      BottomAxis.Marks.Style  := smsLabel;
      BottomAxis.Grid.Style   := psClear;
      BottomAxis.Margin       := 0;
      LeftAxis.Grid.Style     := psClear;
      LeftAxis.Margin         := 0;
      Frame.Visible           := false;
    end;

  // Create the command result
  Result := FChartFactory.NewGraphCommandResult();
  Result.AddChart(Chart, ChartConfiguration);
end;

initialization
  RegisterChartCommand(stBarchart, TBarChart);

end.
