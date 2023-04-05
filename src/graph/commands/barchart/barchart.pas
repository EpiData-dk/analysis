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
  yVarName:            UTF8String;
  i, colourNum:        Integer;
  sTitle:              UTF8String;
  yPct,
  yCount,
  hasBy:               Boolean;
  yType:               UTF8String;
  msg:                 UTF8String;

  procedure setUpValues;
  var ix: Integer;
  begin
    BarSource.Datafile := Datafile;
    BarSource.SetYVariableName(YVarName);
    DataFile.SortRecords(XVar);
    for ix := 0 to Datafile.Size - 1 do
      LabelSeries.Add(ix.ToDouble, 0, XVar.GetValueLabelFormatted(ix, ValueLabelOutput));
  end;

  procedure setUpFrequencies;
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
    ix := tableVarnames.Count;
    ix := Datafile.Fields.Count;
    T := TTables.Create(FExecutor, FOutputCreator);
    TablesAll  := T.CalcTables(Datafile, tableVarNames,
                  StratVariable, WeightVarName, tabOptions, nilTablesRefMap);
    TableData := TablesAll.UnstratifiedTable;
    if (ReverseStrata) then
      TableData.SortByRowLabel(true);
    BarSource.SetSource(TableData, ValueLabelOutput);
    BarSource.Pct := yPct;

    for ix := 0 to TableData.ColCount - 1 do
      LabelSeries.Add(ix.ToDouble, 0, TableData.ColVariable.GetValueLabelFormatted(ix, ValueLabelOutput));

    tableVarNames.Free;
  end;

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
  DFVars := TStringList.Create;
  DFVars.Add(VarNames[0]);
  StratVariable       := TStringList.Create;
  tabOptions          := TOptionList.Create;
  for Opt in Command.Options do
    if (Opt.Ident <> 'by') then
      tabOptions.Add(Opt);
  yPct := tabOptions.HasOption('pct');
  yCount := (VarNames.Count = 1) and (not yPct);
  ReverseStrata       := tabOptions.HasOption('sd', Opt);
  if (ReverseStrata) then
    begin
      tabOptions.Remove(Opt);
      if (Varnames.Count = 1) then
        FOutputCreator.DoInfoShort('!sd ignored with a single variable');
    end;
  WeightVarName := '';
  if (tabOptions.HasOption('w',Opt)) then
    begin
      if (Varnames.Count > 1) then
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
      if (Varnames.Count > 1) then
        begin
          FExecutor.Error('Cannot use !by with more than one variable');
          exit;
        end;
      ByVarName := Opt.Expr.AsIdent;
      DFVars.Add(ByVarName);
    end;

{
 TODO: manage multiple variables
 NOT!! DFVars := Command.VariableList.GetIdentsAsList; // variable order may change when adding weight var
 create DataFile for each pair of (XVar, YVar)
 if only one variable and by, DataFile has XVar and ByVar)
   DO NOT pass !by to Tables!!
 if only one variable and no by, DataFile has only XVar
}
  if (VarNames.Count > 1) then
  begin
    YVarName := VarNames[1];
    DFVars.Add(YVarName);
  end;
  DataFile := FExecutor.PrepareDatafile(DFVars, DFVars);
  if (DataFile.Size < 1) then
    FExecutor.Error('No Data')
  else
  begin   // create chart
    XVar := Datafile.Fields.FieldByName[VarNames[0]];
    Chart := FChartFactory.NewChart();
    BarSource := TBarSource.Create(Chart);
    LabelSeries := TListChartSource.Create(Chart);
    if (hasBy) then
      setUpValues
    else
      setUpFrequencies;
    BarSource.Sorted := true;

    BarSeries := TBarSeries.Create(Chart);
    BarSeries.Source := BarSource;
    BarSeries.Stacked := tabOptions.HasOption('stack');
    BarSeries.BarWidthPercent := 80;
    SeriesStyles := TChartStyles.Create(Chart);
    if (hasBy) then
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
