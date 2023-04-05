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
  epicustombase, epifields_helper, options_utils, chart_options, barchartsource;

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
  BarSource:           array of TBarSource;

  BarSeries:           array of TBarSeries;
  aBarSeries:          TBarSeries;
  LabelSeries:         TListChartSource;
  SeriesStyles:        TChartStyles;
  aStyle:              TChartStyle;
  sColor:              TColorMap;
  {Frequencies}
  DataFile:            array of TEpiDataFile;
  T:                   TTables;
  StratVariable:       TStringList;
  nilTablesRefMap:     TEpiReferenceMap;
  TablesAll:           TTwoWayTables;
  TableData:           TTwoWayTable;
  {command}
  VarNames:            TStrings;
  DFVars:              TStrings;
  XVar,
  YVar,
  ByVar,
  dummyVar:            TEpiField;
  WeightVarName:       UTF8String;
  tabOptions:          TOptionList;
  Opt:                 TOption;

  ValueLabelOutput:    TEpiGetValueLabelType;
  VariableLabelOutput: TEpiGetVariableLabelType;
  ReverseStrata:       Boolean;
  ByVarName,
  xVarName,
  yVarName:            UTF8String;
  i, colourNum,
  nSeries,
  ixSeries,
  nVariables,
  ixVariable:          Integer;
  sTitle:              UTF8String;
  yPct,
  yCount,
  multiSeries,
  hasBy
  chartOK:             Boolean;
  yType:               UTF8String;
  msg:                 UTF8String;

  procedure setUpValues(aBarSource: TBarSource);
  var
    ix: Integer;
  begin
    aBarSource.Datafile := Datafile;
    aBarSource.SetYVariableName(YVarName);
    DataFile.SortRecords(XVar);
    for ix := 0 to Datafile.Size - 1 do
      LabelSeries.Add(ix.ToDouble, 0, XVar.GetValueLabelFormatted(ix, ValueLabelOutput));
  end;

  procedure setUpFrequencies(aBarSource: TBarSource);
  var
    tableVarNames: TStrings;
    ix: Integer;
  begin
    tableVarNames := TStringList.Create;
    tableVarNames.AddStrings(VarNames);
    if (hasBy) then
      tableVarNames.Add(ByVarName)
    else
      begin
        dummyVar := DataFile.NewField(ftInteger);
        dummyVar.Name := dummyVarName;
        tableVarNames.Add(dummyVarName);
      end;
    T := TTables.Create(FExecutor, FOutputCreator);
    TablesAll  := T.CalcTables(Datafile, tableVarNames,
                  StratVariable, WeightVarName, tabOptions, nilTablesRefMap);
    TableData := TablesAll.UnstratifiedTable;
    if (ReverseStrata) then
      TableData.SortByRowLabel(true);
    aBarSource.SetSource(TableData);
    aBarSource.Pct := yPct;

    for ix := 0 to TableData.ColCount - 1 do
      LabelSeries.Add(ix.ToDouble, 0, TableData.ColVariable.GetValueLabelFormatted(ix, ValueLabelOutput));

    tableVarNames.Free;
  end;
  {
  big refactor is needed
  need the following:
  IF only one variable
    step 1
      create dataset
      validate that there is data
    step 2
    allows !pct !by !stack
      call procedure that handles count data
        series styles are based on the by variable
        sort output of tables
        source has single y only
        free the dataset!
  IF multiple variables
    step 1
      create full dataset, excluding missing X, but including missing Ys
      validate that there is at least some data
      !pct and !by are invalid
    step 2
      call procedure to handle value data
        series styles are based on number of variables
        sort data by x variable
        copy data to a matrix of epifloat
        x-index is based on runner counting changes in x (skip missing)
        y-index is based on order of vars in VarNames
        matrix value is based on y variable value, with missing set to zero?
        create multi-y source based on the matrix
        free the dataset!
   }
begin
  VariableLabelOutput := VariableLabelTypeFromOptionList(Command.Options, FExecutor.SetOptions);
  ValueLabelOutput    := ValueLabelTypeFromOptionList(Command.Options, FExecutor.SetOptions);
  Result := FChartFactory.NewGraphCommandResult();
  sColor              := ChartColorsFromOptions(Command.Options, FExecutor.SetOptions, msg);
  if (msg <> '') then
    begin
      FExecutor.Error(msg);
      exit;
    end;
  VarNames := Command.VariableList.GetIdentsAsList;
  nVariables  := VarNames.Count;
  multiSeries := nVariables > 1;
  DFVars := TStringList.Create;
  DFVars.Add(VarNames[0]);
  StratVariable       := TStringList.Create;
  tabOptions          := TOptionList.Create;
  for Opt in Command.Options do
    if (Opt.Ident <> 'by') then
      tabOptions.Add(Opt);
  yPct := tabOptions.HasOption('pct');
  yCount := (multiSeries) and (not yPct);
  ReverseStrata       := tabOptions.HasOption('sd', Opt);
  if (ReverseStrata) then
    begin
      tabOptions.Remove(Opt);
      if (nSeries = 1) then
        FOutputCreator.DoInfoShort('!sd ignored with a single variable');
    end;
  WeightVarName := '';
  if (tabOptions.HasOption('w',Opt)) then
    begin
      if (multiSeries) then
        begin
          FExecutor.Error('Cannot use !w with more than one variable');
          exit;
        end;
      WeightVarName := Opt.Expr.AsIdent;
      DFVars.Add(WeightVarName);
    end;
  ByVarName := '';
  hasBy := Command.HasOption('by',Opt);
  if (hasBy) then
    begin
      if (multiSeries) then
        begin
          FExecutor.Error('Cannot use !by with more than one variable');
          exit;
        end;
      ByVarName := Opt.Expr.AsIdent;
      DFVars.Add(ByVarName);
    end;

{
 TODO: manage multiple variables, which will always be values and yield additional series on the same chart
 NOT!! DFVars := Command.VariableList.GetIdentsAsList; // variable order may change when adding weight var
 create DataFile for each pair of (XVar, YVar)
 if only one variable and by, DataFile has XVar and ByVar)
   DO NOT pass !by to Tables!!
 if only one variable and no by, DataFile has only XVar
}
if (multiSeries) then
  nVariables := nVariables - 1;
setLength(Datafile, nVariables);
setLength(BarSource, nVariables);
setLength(BarSeries, nVariables);
chartOK := false;
xVarName := VarNames[0];
XVar := Datafile.Fields.FieldByName[xVarName];
nSeries := 0;
colourNum := 0;
Chart := FChartFactory.NewChart();
for ixVariable := 0 to nVariables do
  begin
  if (multiSeries) then
  begin
    YVarName := VarNames[1];
    DFVars.Add(YVarName);
  end;
  DataFile := FExecutor.PrepareDatafile(DFVars, DFVars);
  if (DataFile.Size < 1) then
    FExecutor.Error('No Data for ' + xVarName + ' and ' + yVarName)
  else
  begin   // create chart series
    chartOK := true;
    BarSource[nSeries] := TBarSource.Create(Chart);
    LabelSeries := TListChartSource.Create(Chart);
    if (multiSeries) then
      setUpValues(BarSource[nSeries])
    else
      setUpFrequencies(BarSource[nSeries]);
    BarSource[nSeries].Sorted := true;

    BarSeries[nSeries] := TBarSeries.Create(Chart);
    with BarSeries[nSeries] do
      begin
        Source := BarSource[nSeries];
        Stacked := tabOptions.HasOption('stack');
        BarWidthPercent := 80;
      end;
    SeriesStyles := TChartStyles.Create(Chart);
    if (hasBy) then
      begin
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
        aBarSeries.Legend.Multiplicity:=lmStyle;
        aBarSeries.Legend.GroupIndex  := 0;

        Chart.Legend.Visible        := true;
        Chart.Legend.UseSidebar     := true;
        Chart.Legend.Frame.Visible  := false;
        Chart.Legend.GroupTitles.Add(ByVarName);
      end  // hasBy
    else
      begin
        if (colourNum = length(sColor)) then
          colourNum := 0;
        aStyle := SeriesStyles.Add;
        aStyle.Brush.Color := sColor[colourNum];
        aStyle.Pen.Color := clSilver;
        colourNum += 1;
      end;
      BarSeries[nSeries].Styles := SeriesStyles;

    // Add series to the chart
    Chart.AddSeries(BarSeries[nSeries]);
    nSeries += 1;
    end;
    end;

if (nSeries > 0) then
  begin
    if (yCount) then
      yType := 'Count'
    else if (yPct) then
      yType := 'Percent'
    else
      yType := YVarName;
    sTitle := yType + ' by ' + XVar.GetVariableLabel(VariableLabelOutput);
    if (hasBy) then
      sTitle += ' by ' + ByVarName;
    if (WeightVarName <> '') then
      sTitle += ' weighted (' + WeightVarName + ')';

    VariableLabelOutput := VariableLabelTypeFromOptionList(tabOptions, FExecutor.SetOptions, sovStatistics);
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
        LeftAxis.Intervals.NiceSteps:='.2|.5|.1';
        LeftAxis.Intervals.MinLength := 5;
        LeftAxis.Intervals.MaxLength := 200; // no interpolation of ticks at reasonable scale
        Frame.Visible           := false;
      end;
    Result.AddChart(Chart, ChartConfiguration);
    XVar := nil;
    if (yCount) then
    begin
      TablesAll.Free;
      T.Free;
      DataFile.Free;
    end;
  end;   // create chart

  VarNames.Free;
  tabOptions.Free;
  StratVariable.Free;
end;

initialization
  RegisterChartCommand(stBarchart, TBarChart);

end.
