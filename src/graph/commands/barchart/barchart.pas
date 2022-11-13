unit barchart;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, chartcommandresult, TAGraph, executor, outputcreator,
  ast, epidatafiles, chartcommand, chartfactory, chartconfiguration,
  tables_types, tables;

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
  epicustombase, epifields_helper, options_utils, graph_utils, barchartsource;

{ TBarChart }

procedure TBarChart.Init(ChartFactory: IChartFactory; Executor: TExecutor;
  OutputCreator: TOutputCreator);
begin
  FChartFactory := ChartFactory;
  FExecutor := Executor;
  FOutputCreator := OutputCreator;
end;

function TBarChart.Execute(Command: TCustomGraphCommand): IChartCommandResult;
const
  dummyVarName = '_dummy4barchart';
var
  {Chart}
  Chart:               TChart;
  ChartConfiguration:  IChartConfiguration;
  BarSource:           TBarSource;
  LabelSeries:         TListChartSource;
  BarSeries:           TBarSeries;
  SeriesStyles:        TChartStyles;
  aStyle:              TChartStyle;
  sColor:              TColorMap;
  {Frequencies}
  DataFile:            TEpiDataFile;
  T:                   TTables;
  StratVariable:       TStringList;
  nilTablesRefMap:     TEpiReferenceMap;
  TablesAll:           TTwoWayTables;
  TableData:           TTwoWayTable;
  {command}
  VarNames:            TStrings;
  DFVars:              TStrings;
  XVar:                TEpiField;
  dummyVar:            TEpiField;
  WeightVarName:       UTF8String;
  cOptions:            TOptionList;
  Opt:                 TOption;

  ValueLabelOutput:    TEpiGetValueLabelType;
  VariableLabelOutput: TEpiGetVariableLabelType;
  ReverseStrata:       Boolean;
  ByVarName:           UTF8String;
  i, colourNum:           Integer;
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
  cOptions            := TOptionList.Create;
  for Opt in Command.Options do
    cOptions.Add(Opt);
  ReverseStrata       := cOptions.HasOption('sd', Opt);
  if (ReverseStrata) then
    begin
      cOptions.Remove(Opt);
      if (Varnames.Count = 1) then
        FOutputCreator.DoInfoShort('!sd ignored with a single variable');
    end;

  WeightVarName := '';
  if (cOptions.HasOption('w',Opt)) then
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
      dummyVar.Name := dummyVarName;
      Varnames.Add(dummyVarName);
    end;
// Note: this does NOT call CalcTables with stratification
  T := TTables.Create(FExecutor, FOutputCreator);
  TablesAll  := T.CalcTables(Datafile, VarNames,
                StratVariable, WeightVarName, cOptions, nilTablesRefMap);
  TableData := TablesAll.UnstratifiedTable;
  if (ReverseStrata) then
    TableData.SortByRowLabel(true);
  BarSource.SetSource(TableData, ValueLabelOutput);

  ByVarName := Datafile.Fields.FieldByName[VarNames[1]].GetVariableLabel(VariableLabelOutput);
  for i := 0 to TableData.ColCount - 1 do
    LabelSeries.Add(i.ToDouble, 0, TableData.ColVariable.GetValueLabelFormatted(i, ValueLabelOutput));

  if (Varnames.IndexOf(dummyVarName) > -1) then
    Varnames.Delete(Varnames.IndexOf(dummyVarName));

  BarSeries := TBarSeries.Create(Chart);
  BarSeries.Source := BarSource;
  BarSeries.Stacked := cOptions.HasOption('stack');
  BarSeries.BarWidthPercent := 80;
  SeriesStyles := TChartStyles.Create(Chart);
  if (Varnames.Count > 1) then
    begin
      colourNum := 0;
      for i := 0 to TableData.RowCount - 1 do
        begin
          if (colourNum = length(sColor)) then
            colourNum := 0;
          aStyle := SeriesStyles.Add;
          aStyle.Text := TableData.RowVariable.GetValueLabelFormatted(i, ValueLabelOutput);
          aStyle.Brush.Color:=sColor[colourNum];
          aStyle.Pen.Color := clSilver;
          colourNum += 1;
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

  VariableLabelOutput := VariableLabelTypeFromOptionList(cOptions, FExecutor.SetOptions, sovStatistics);
  ChartConfiguration := FChartFactory.NewChartConfiguration();
  ChartConfiguration.GetTitleConfiguration()
    .SetTitle(sTitle)
    .SetFootnote('')
    .SetXAxisTitle(XVar.GetVariableLabel(VariableLabelOutput))
    .SetYAxisTitle(yType);

  ChartConfiguration.GetAxesConfiguration()
    .GetXAxisConfiguration()
    .SetShowAxisMarksAsDates(XVar.FieldType in DateFieldTypes);

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
  VarNames.Free;
  TablesAll.Free;
  T.Free;
  DataFile.Free;
  StratVariable.Free;
end;

initialization
  RegisterChartCommand(stBarchart, TBarChart);

end.
