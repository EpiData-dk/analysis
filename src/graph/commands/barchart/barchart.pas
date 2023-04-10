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
//  aBarSeries:          TBarSeries;
  LabelSeries:         TListChartSource;
  SeriesStyles:        array of TChartStyles;
  aStyle:              TChartStyle;
  sColor:              TColorMap;
  {Frequencies}
  DataFile:            TEpiDataFile;
  T:                   TTables;
  TablesAll:           TTwoWayTables;
  TableData:           TTwoWayTable;
  {command}
  VarNames:            TStrings;
  DFVars:              TStrings;
  XVarOnly:            TStrings;
  XVar,
  dummyVar:            TEpiField;
  WeightVarName:       UTF8String;
  tabOptions:          TOptionList;
  Opt:                 TOption;

  ValueLabelOutput:    TEpiGetValueLabelType;
  VariableLabelOutput: TEpiGetVariableLabelType;
  ReverseStrata:       Boolean;
  ByVarName,
  xVarName:            UTF8String;
  i, colourNum,
  nSeries,
  ixSeries:            Integer;
  sTitle:              UTF8String;
  yPct,
  yCount,
  multiSeries,
  hasBy,
  chartOK:             Boolean;
  yType:               UTF8String;
  msg:                 UTF8String;

  procedure setUpValues(aBarSource: TBarSource; aVarName: UTF8String);
  var
    ix: Integer;
  begin
    aBarSource.SetValueSource(DataFile, XVarName, aVarName);
    if (LabelSeries.Count = 0) then
      for ix := 0 to Datafile.Size - 1 do
        LabelSeries.Add(ix.ToDouble, 0, XVar.GetValueLabelFormatted(ix, ValueLabelOutput));
  end;

  procedure setUpFrequencies(aBarSource: TBarSource);
  var
    StratVariable:   TStringList;
    nilTablesRefMap: TEpiReferenceMap;
    ix: Integer;
  begin
    if (not hasBy) then
      begin
        dummyVar := DataFile.NewField(ftInteger);
        dummyVar.Name := dummyVarName;
        DFVars.Add(dummyVarName);
      end;
    StratVariable := TStringList.Create;
    T := TTables.Create(FExecutor, FOutputCreator);
    TablesAll  := T.CalcTables(Datafile, DFVars,
                  StratVariable, WeightVarName, tabOptions, nilTablesRefMap);
    TableData  := TablesAll.UnstratifiedTable;
    if (ReverseStrata) then
      TableData.SortByRowLabel(true);
    aBarSource.SetCountSource(TableData, yPct);
    if (DFVars.IndexOf(dummyVarName) > -1) then
      DFVars.Delete(DFVars.IndexOf(dummyVarName));
    for ix := 0 to TableData.ColCount - 1 do
      LabelSeries.Add(ix.ToDouble, 0, TableData.ColVariable.GetValueLabelFormatted(ix, ValueLabelOutput));
    StratVariable.Free;
  end;

begin
  VariableLabelOutput := VariableLabelTypeFromOptionList(Command.Options, FExecutor.SetOptions);
  ValueLabelOutput    := ValueLabelTypeFromOptionList(Command.Options, FExecutor.SetOptions);
  Result              := FChartFactory.NewGraphCommandResult();
  sColor              := ChartColorsFromOptions(Command.Options, FExecutor.SetOptions, msg);
  if (msg <> '') then
    begin
      FExecutor.Error(msg);
      exit;
    end;
  chartOK             := false;
  hasBy               := false;
  VarNames            := Command.VariableList.GetIdentsAsList;
  xVarName            := VarNames[0];
  xVarOnly            := TStringList.Create;
  xVarOnly.Add(xVarName);
  nSeries             := VarNames.Count;
  multiSeries         := nSeries > 1;
  DFVars              := TStringList.Create;
  if (multiSeries) then
    // multiple variables - display y values at each x
    begin
      if (Command.HasOption('by')) then
        begin
          FExecutor.Error('Cannot use !by with more than one variable');
          exit;
        end;
      if (Command.HasOption('w')) then
        begin
          FExecutor.Error('Cannot use !w with more than one variable');
          exit;
        end;
      DFVars.AddStrings(VarNames);
      DataFile := FExecutor.PrepareDatafile(DFVars, xVarOnly);
      nSeries := nSeries - 1;
    end
  else
    // one variable - display count or percent
    begin
      DFVars.Add(xVarName);
      tabOptions       := TOptionList.Create;
      for Opt in Command.Options do
        if (Opt.Ident <> 'by') then
          tabOptions.Add(Opt);
      yPct             := tabOptions.HasOption('pct');
      yCount           := (not multiSeries) and (not yPct);
      ReverseStrata    := tabOptions.HasOption('sd', Opt);
      if (ReverseStrata) then
        begin
          tabOptions.Remove(Opt);
          if (nSeries = 1) then
            FOutputCreator.DoInfoShort('!sd ignored with no !by variable');
        end;
      WeightVarName := '';
      if (tabOptions.HasOption('w',Opt)) then
        begin
          WeightVarName := Opt.Expr.AsIdent;
          DFVars.Add(WeightVarName);
        end;
      ByVarName := '';
      hasBy := Command.HasOption('by', Opt);
      if (hasBy) then
        begin
          ByVarName := Opt.Expr.AsIdent;
          DFVars.Add(ByVarName);
        end;
      DataFile := FExecutor.PrepareDatafile(DFVars, DFVars);
    end;  // one variable
// is there any data?
  if (DataFile.Size < 1) then
    begin
      FExecutor.Error('No Data.');
      DataFile.Free;
      exit;
    end;

// set up source(s) and series
setLength(BarSource, nSeries);
setLength(BarSeries, nSeries);
setLength(SeriesStyles, nSeries);

Chart := FChartFactory.NewChart();
chartOK := true;
XVar := Datafile.Fields.FieldByName[xVarName];
LabelSeries := TListChartSource.Create(Chart);
colourNum := 0;
DataFile.SortRecords(XVar);
for ixSeries := 0 to nSeries - 1 do
  begin   // create chart series
    BarSource[ixSeries] := TBarSource.Create(Chart);
    if (multiSeries) then
      setUpValues(BarSource[ixSeries], VarNames[ixSeries + 1])
    else
      setUpFrequencies(BarSource[ixSeries]);
    BarSeries[ixSeries] := TBarSeries.Create(Chart);
    BarSource[ixSeries].Sorted := true;
    with BarSeries[ixSeries] do
      begin
        Source := BarSource[ixSeries];
        Stacked := Command.HasOption('stack');
        BarWidthPercent := 80;
        BarWidthStyle := bwPercentMin;
      end;
    SeriesStyles[ixSeries] := TChartStyles.Create(Chart);
    if (hasBy) then
      begin
        for i := 0 to TableData.RowCount - 1 do
          begin
            if (colourNum = length(sColor)) then
              colourNum := 0;
            aStyle := SeriesStyles[ixSeries].Add;
            aStyle.Text := TableData.RowVariable.GetValueLabelFormatted(i, ValueLabelOutput);
            aStyle.Brush.Color:=sColor[colourNum];
            aStyle.Pen.Color := clSilver;
            colourNum += 1;
          end;
        BarSeries[ixSeries].Legend.Multiplicity:=lmStyle;
        BarSeries[ixSeries].Legend.GroupIndex  := 0;

        Chart.Legend.Visible        := true;
        Chart.Legend.UseSidebar     := true;
        Chart.Legend.Frame.Visible  := false;
        Chart.Legend.GroupTitles.Add(ByVarName);
      end
    else
      begin
        if (colourNum = length(sColor)) then
          colourNum := 0;
        aStyle := SeriesStyles[ixSeries].Add;
        aStyle.Brush.Color := sColor[colourNum];
        aStyle.Pen.Color := clSilver;
        colourNum += 1;
      end;

    BarSeries[ixSeries].Styles := SeriesStyles[ixSeries];
    Chart.AddSeries(BarSeries[ixSeries]);
  end;

if (chartOK) then
  begin
    if (yCount) then
      yType := 'Count of '
    else if (yPct) then
      yType := 'Percent of '
    else
      yType := '';
    sTitle := yType + XVar.GetVariableLabel(VariableLabelOutput);
    if (hasBy) then
      sTitle += ' by ' + ByVarName;
    if (WeightVarName <> '') then
      sTitle += LineEnding + 'weighted (' + WeightVarName + ')';

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
  end;   // create chart

  if (not MultiSeries) then
  begin
    TablesAll.Free;
    T.Free;
  end;
  DataFile.Free;
  VarNames.Free;
  tabOptions.Free;
end;

initialization
  RegisterChartCommand(stBarchart, TBarChart);

end.
