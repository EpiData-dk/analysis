unit epicurve;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, chartcommandresult, executor, outputcreator, epifields_helper,
  ast, epidatafiles, epidatafilestypes, epicustombase, chartcommand, chartfactory, chartconfiguration,
  TAGraph, TASources, TALegend, tables_types, tables, histogramsource, histogramdata;

type

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
  options_utils, chart_options;

{ TEpicurveChart }

procedure TEpicurveChart.Init(ChartFactory: IChartFactory; Executor: TExecutor;
  OutputCreator: TOutputCreator);
begin
  FChartFactory  := ChartFactory;
  FExecutor      := Executor;
  FOutputCreator := OutputCreator;
end;

function TEpicurveChart.Execute(Command: TCustomGraphCommand): IChartCommandResult;
const
  dummyVarName = '_dummy4barchart';
var
  {Chart}
  Chart:               TChart;
  ChartConfiguration:  IChartConfiguration;
  DataFile:            TEpiDataFile;
  LegendSeries:        TBarSeries;
  LegendStyles:        TChartStyles;
  LegendSource:        TListChartSource;
  HistogramSource:     THistogramSource;
  HistogramData:       THistogram;
  BarSeries:           TBarSeries;
  SeriesStyles:        TChartStyles;
  aStyle:              TChartStyle;
  sColors:             TColorMap;
  {Frequencies}
  T:                   TTables;
  StratVariable:       TStringList;
  nilTablesRefMap:     TEpiReferenceMap;
  TablesAll:           TTwoWayTables;
  TableData:           TTwoWayTable;
  {command}
  VarNames:            TStrings;
  XVar:                TEpiField;
  dummyVar:            TEpiField;
  WeightVarName:       UTF8String;
  cOptions:            TOptionList;
  Opt:                 TOption;

  ValueLabelOutput:    TEpiGetValueLabelType;
  VariableLabelOutput: TEpiGetVariableLabelType;
  ReverseStrata:       Boolean;
  ByVarName:           UTF8String;
  box1, box:           Integer;
  i, colourNum:        Integer;
  sTitle:              UTF8String;

begin
  VariableLabelOutput := VariableLabelTypeFromOptionList(Command.Options, FExecutor.SetOptions);
  ValueLabelOutput    := ValueLabelTypeFromOptionList(Command.Options, FExecutor.SetOptions);
  sColors             := ChartColorsFromOptions(Command.Options, FExecutor);
  VarNames            := Command.VariableList.GetIdentsAsList;
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

  // weight variable not allowed for epicurve
  WeightVarName := '';

  Result := FChartFactory.NewGraphCommandResult();

  DataFile := FExecutor.PrepareDatafile(VarNames, VarNames);
  if (DataFile.Size < 1) then
    FExecutor.Error('No data')
  else
    begin  // create chart
      Chart := FChartFactory.NewChart();
      XVar := Datafile.Fields.FieldByName[VarNames[0]];
      HistogramData := THistogram.Create(FExecutor, Command);
      if (cOptions.HasOption('interval', Opt)) then
        HistogramData.Interval := Opt.Expr.AsInteger;
      if (Varnames.Count = 1) then
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
      HistogramData.HistogramToEpicurve;

      ByVarName := Datafile.Fields.FieldByName[VarNames[1]].GetVariableLabel(VariableLabelOutput);

      if (Varnames.IndexOf(dummyVarName) > -1) then
        Varnames.Delete(Varnames.IndexOf(dummyVarName));

      HistogramSource := THistogramSource.Create(Chart);
      HistogramSource.Histogram := HistogramData;

      BarSeries := TBarSeries.Create(Chart);
      BarSeries.Source := HistogramSource;
      BarSeries.Stacked := true;
      BarSeries.BarWidthPercent := 100;
      BarSeries.ShowInLegend := false;
      SeriesStyles := TChartStyles.Create(Chart);

      if (Varnames.Count > 1) then
        begin
          LegendSource := TListChartSource.Create(Chart);
          LegendSeries := TBarSeries.Create(Chart);
          LegendSeries.Source := LegendSource;
          LegendStyles := TChartStyles.Create(Chart);
          LegendSeries.ShowInLegend := true;
          LegendSeries.Styles := LegendStyles;
          LegendSeries.Legend.Multiplicity:=lmStyle;
          LegendSeries.Legend.GroupIndex  := 0;
          Chart.AddSeries(LegendSeries);

          colourNum := 0;
          box1 := 0;
          for i := 0 to TableData.RowCount - 1 do
            begin
              // individual box styles
              if (colourNum = length(sColors)) then
                colourNum := 0;     // if more strata than colours, recycle the colours
              for box := box1 to box1 + HistogramData.MaxCount[i] - 1 do
                begin
                  aStyle := SeriesStyles.Add;
                  aStyle.Brush.Color:=sColors[colourNum];
                  aStyle.Pen.Color := clSilver;     // will work with any box colours
                end;
              LegendSource.Add(HistogramData.Base.ToDouble, 0);
              aStyle := LegendStyles.Add;
              aStyle.Text := TableData.RowVariable.GetValueLabelFormatted(i, ValueLabelOutput);
              aStyle.Brush.Color:=sColors[colourNum];
              colourNum += 1;
              box1 += HistogramData.MaxCount[i];
            end;

          Chart.Legend.Visible        := true;
          Chart.Legend.UseSidebar     := true;
          Chart.Legend.Frame.Visible  := false;
          Chart.Legend.GroupTitles.Add(ByVarName);
        end
      else
        begin
          aStyle := SeriesStyles.Add;
          aStyle.Brush.Color := sColors[0];
          aStyle.Pen.Color := clSilver;
        end;
      BarSeries.Styles := SeriesStyles;

      Chart.AddSeries(BarSeries);

       sTitle := 'Count by ' + XVar.GetVariableLabel(VariableLabelOutput);
      if (Varnames.Count > 1) then
        sTitle += ' by ' + ByVarName;
      ChartConfiguration := FChartFactory.NewChartConfiguration();
      ChartConfiguration.GetTitleConfiguration()
        .SetTitle(sTitle)
        .SetFootnote('')
        .SetXAxisTitle(XVar.GetVariableLabel(VariableLabelOutput))
        .SetYAxisTitle('Count');

      ChartConfiguration.GetAxesConfiguration()
        .GetXAxisConfiguration()
        .SetShowAxisMarksAsDates(XVar.FieldType in DateFieldTypes);

      with Chart do
        begin
          BottomAxis.Marks.Source := HistogramSource.AddAxisScales(Chart);
          BottomAxis.Grid.Style   := psClear;
          BottomAxis.Margin       := 0;
          LeftAxis.Grid.Style     := psClear;
          LeftAxis.Margin         := 0;
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
  RegisterChartCommand(stEpicurve, TEpicurveChart);

end.
