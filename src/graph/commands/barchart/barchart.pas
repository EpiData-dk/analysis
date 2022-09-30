unit barchart;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, chartcommandresult, TAGraph, executor, outputcreator,
  ast, epidatafiles, chartcommand, chartfactory, chartconfiguration,
  tables_types, tables, freq;

type

  {TODO: Completely revamp!!
    use barchartsource
    similar code to histogram to get variables, datafile and choose execfreq or exectables
  }
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
  // for now, use default colours from classic analysis
  // TODO: put in graph options
  sColor:              array of TColor = (clBlue, clRed, clBlack, clGreen, clYellow, clWhite, clSkyBlue, clFuchsia, clGray, clAqua);
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
  VarNames := Command.VariableList.GetIdentsAsList;
  DFVars   := Command.VariableList.GetIdentsAsList; // variable order may change when adding weight var
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
  BarSource := TBarSource.Create(Chart);
  BarSource.Pct := yPct;
  LabelSeries := TListChartSource.Create(Chart);
  if (Varnames.Count > 1) then
    begin
   // Note: this does NOT call CalcTables with stratification
      T := TTables.Create(FExecutor, FOutputCreator);
      TableData  := T.CalcTables(Datafile, VarNames,
                    StratVariable, WeightVarName, Command.Options, nilTablesRefMap, nilStatistics).UnstratifiedTable;
      if (ReverseStrata) then
        TableData.SortByRowLabel(true);
      BarSource.SetSource(TableData, ValueLabelOutput);
      ByVarName := Datafile.Fields.FieldByName[VarNames[1]].GetVariableLabel(VariableLabelOutput);
      for i := 0 to TableData.ColCount - 1 do
        LabelSeries.Add(i.ToDouble, 0, TableData.ColVariable.GetValueLabelFormatted(i, ValueLabelOutput)); //.AsString[i]);   // TODO: use value label for 3rd parameter!
    end
  else
    begin
      F := TFreqCommand.Create(FExecutor, FOutputCreator);
      FreqData := F.CalcFreq(Datafile, VarNames[0],nilTablesRefMap);
      BarSource.SetSource(FreqData);
      for i := 0 to FreqData.Count.Size - 1 do
        LabelSeries.Add(i.ToDouble, 0, FreqData.Categ.GetValueLabel(i, ValueLabelOutput)); // .AsString[i]);   // TODO: use value label for 3rd parameter!
    end;

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

  Chart.BottomAxis.Marks.Source := LabelSeries;
  Chart.BottomAxis.Marks.Style := smsLabel;

  // Create the command result
  Result := FChartFactory.NewGraphCommandResult();
  Result.AddChart(Chart, ChartConfiguration);
end;

initialization
  RegisterChartCommand(stBarchart, TBarChart);

end.
