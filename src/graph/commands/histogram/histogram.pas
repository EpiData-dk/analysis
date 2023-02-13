unit histogram;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, chartcommandresult, executor, outputcreator, epifields_helper,
  ast, epidatafiles, epidatafilestypes, epicustombase, chartcommand, chartfactory, chartconfiguration,
  TAGraph, TALegend, tables_types, tables, histogramsource, histogramdata;

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
  TASeries, TATypes, TAStyles, TAChartUtils, Graphics, charttitles, ast_types,
  options_utils, chart_options;

{ THistogramChart }

procedure THistogramChart.Init(ChartFactory: IChartFactory; Executor: TExecutor;
  OutputCreator: TOutputCreator);
begin
  FChartFactory := ChartFactory;
  FExecutor := Executor;
  FOutputCreator := OutputCreator;
end;

function THistogramChart.Execute(Command: TCustomGraphCommand): IChartCommandResult;
const
  dummyVarName = '_dummy4barchart';
var
  {Chart}
  Chart:               TChart;
  ChartConfiguration:  IChartConfiguration;
  DataFile:            TEpiDataFile;
  HistogramSource:     THistogramSource;
  HistogramData:       THistogram;
  BarSeries:           TBarSeries;
  SeriesStyles:        TChartStyles;
  aStyle:              TChartStyle;
  // TODO: put in graph options
  sColors:             TColorMap;
  {Frequencies}
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
  i, colourNum:        Integer;
  sTitle:              UTF8String;
  yPct:                Boolean;
  yType:               UTF8String;
begin
  VariableLabelOutput := VariableLabelTypeFromOptionList(Command.Options, FExecutor.SetOptions);
  ValueLabelOutput    := ValueLabelTypeFromOptionList(Command.Options, FExecutor.SetOptions);
  sColors             := ChartColorsFromOptions(Command.Options, FExecutor);
  VarNames            := Command.VariableList.GetIdentsAsList;
  DFVars              := Command.VariableList.GetIdentsAsList;
  StratVariable       := TStringList.Create;
  cOptions            := TOptionList.Create;
  for Opt in Command.Options do
    cOptions.Add(Opt);
  ReverseStrata       := cOptions.HasOption('sd', Opt);
  if (ReverseStrata) then
    begin
      cOptions.Remove(Opt); // don't pass this to TABLES!
      if (Varnames.Count = 1) then
        FOutputCreator.DoInfoShort('!sd ignored with a single variable');
    end;

  WeightVarName := '';
  if (cOptions.HasOption('w',Opt)) then
    begin
      WeightVarName := Opt.Expr.AsIdent;
      DFVars.Add(WeightVarName);
    end;

  yPct := cOptions.HasOption('pct');

  Result := FChartFactory.NewGraphCommandResult();

  DataFile := FExecutor.PrepareDatafile(DFVars, DFVars);
  if (DataFile.Size < 1) then
    FExecutor.Error('No data')
  else
    begin  // create chart
      Chart := FChartFactory.NewChart();
      XVar := Datafile.Fields.FieldByName[VarNames[0]];
      HistogramData := THistogram.Create(FExecutor, Command);
      if (cOptions.HasOption('interval', Opt)) then
        HistogramData.Interval := Opt.Expr.AsInteger;
      HistogramData.PctCalc := yPct;
      if (Varnames.Count = 1) then
    // add dummy variable to use Tables with one variable
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
      HistogramData.Fill(TableData);

      ByVarName := Datafile.Fields.FieldByName[VarNames[1]].GetVariableLabel(VariableLabelOutput);
      // if dummy var was created, remove it now
      if (Varnames.IndexOf(dummyVarName) > -1) then
        Varnames.Delete(Varnames.IndexOf(dummyVarName));

      HistogramSource := THistogramSource.Create(Chart);
      HistogramSource.Histogram := HistogramData;
      BarSeries := TBarSeries.Create(Chart);
      BarSeries.Source := HistogramSource;
      BarSeries.Stacked := cOptions.HasOption('stack');
      BarSeries.BarWidthPercent := 100;
      SeriesStyles := TChartStyles.Create(Chart);
      if (Varnames.Count > 1) then
        begin
          colourNum := 0;
          for i := 0 to TableData.RowCount - 1 do
            begin
              if (colourNum = length(sColors)) then
                colourNum := 0;
              aStyle := SeriesStyles.Add;
              aStyle.Text := TableData.RowVariable.GetValueLabelFormatted(i, ValueLabelOutput);
              aStyle.Brush.Color:=sColors[colourNum];
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
          aStyle.Brush.Color := sColors[0];
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
          BottomAxis.Marks.Source := HistogramSource.AddXAxisScales(Chart);
          BottomAxis.Marks.Style  := smsValue;
          BottomAxis.Grid.Style   := psClear;
          BottomAxis.Margin       := 0;
          BottomAxis.Intervals.MinLength := 20;
          LeftAxis.Marks.Source   := HistogramSource.AddYAxisScales(Chart);
          LeftAxis.Grid.Style     := psClear;
          LeftAxis.Margin         := 0;
          LeftAxis.Intervals.MinLength := 20;
          LeftAxis.Intervals.MaxLength := 1000; // no interpolation of ticks
          Frame.Visible           := false;
        end;

      Result.AddChart(Chart, ChartConfiguration);
      XVar := nil;
      TablesAll.Free;
      T.Free;
    end;  // create chart

VarNames.Free;
DataFile.Free;
StratVariable.Free;
cOptions.Free;
end;

initialization
  RegisterChartCommand(stHistogram, THistogramChart);

end.
