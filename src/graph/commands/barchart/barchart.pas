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
  BarSeries:           TBarSeries;
  LabelSeries:         TListChartSource;
  SeriesStyles:        TChartStyles;
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
  SortFields:          TEpiFields;
  WeightVarName:       UTF8String;
//  tabOptions:          TOptionList;
  Opt:                 TOption;
  ValueLabelOutput:    TEpiGetValueLabelType;
  VariableLabelOutput: TEpiGetVariableLabelType;
  ByVarName,
  xVarName:            UTF8String;
  i, colourNum,
  nSeries:             Integer;
  sTitle,
  yTitle:              UTF8String;
  yPct,
  valueSeries,
  hasBy:               Boolean;
  msg:                 UTF8String;

  procedure setUpValues(aBarSource: TBarSource);
  var
    ix: Integer;
  begin
    SortFields := TEpiFields.Create(nil);
    SortFields.AddItem(XVar);
    DataFile.SortRecords(SortFields, Command.HasOption('sxd'));
    aBarSource.SetValueSource(DataFile, VarNames);
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
                  StratVariable, WeightVarName, Command.Options, nilTablesRefMap);
    TableData  := TablesAll.UnstratifiedTable;

    if (Command.HasOption('ssd')) then
      TableData.SortByRowLabel(true)
    else if (Command.HasOption('ssa')) then
      TableData.SortByRowLabel(false);
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
  hasBy               := false;
  VarNames            := Command.VariableList.GetIdentsAsList;
  xVarName            := VarNames[0];
  nSeries             := VarNames.Count;
  yTitle              := '';
  valueSeries         := nSeries > 1;
  DFVars              := TStringList.Create;
  if (valueSeries) then
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
      xVarOnly := TStringList.Create;
      xVarOnly.Add(xVarName);
      DataFile := FExecutor.PrepareDatafile(DFVars, xVarOnly);
      nSeries := nSeries - 1;
      xVarOnly.Free;
    end
  else
    // one variable - display count or percent
    begin
      DFVars.Add(xVarName);
      yPct := Command.HasOption('pct');
      if (yPct) then
        yTitle := 'Percent'
      else
        yTitle := 'Count';
      if (Command.HasOption('ssd')) and (not Command.HasOption('by')) then
        FOutputCreator.DoInfoShort('!sd ignored with no !by variable');
      WeightVarName := '';
      if (Command.HasOption('w',Opt)) then
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
     end;

  if (DataFile.Size < 1) then
    begin
      FExecutor.Error('No Data.');
      VarNames.Free;
      DataFile.Free;
      exit;
    end;
  Chart := FChartFactory.NewChart();
  XVar := Datafile.Fields.FieldByName[xVarName];
  LabelSeries := TListChartSource.Create(Chart);
  colourNum := 0;

  // create chart series
  BarSource := TBarSource.Create(Chart);
  if (valueSeries) then
    setUpValues(BarSource)
  else
    begin
      setUpFrequencies(BarSource);
      nSeries := TableData.RowCount;
    end;
  BarSeries := TBarSeries.Create(Chart);
  BarSource.Sorted := true;
  with BarSeries do
    begin
      Source := BarSource;
      Stacked := Command.HasOption('stack');
      BarWidthPercent := 80;
      BarWidthStyle := bwPercentMin;
    end;
  SeriesStyles := TChartStyles.Create(Chart);
  if (nSeries > 1) then
    begin
      for i := 0 to nSeries - 1 do
        begin
          if (colourNum = length(sColor)) then
            colourNum := 0;
          aStyle := SeriesStyles.Add;
          if (valueSeries) then
            aStyle.Text := Datafile.Field[i + 1].Name
          else
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
    end
  else
    begin
      if (colourNum = length(sColor)) then
        colourNum := 0;
      aStyle := SeriesStyles.Add;
      aStyle.Brush.Color := sColor[colourNum];
      aStyle.Pen.Color := clSilver;
      colourNum += 1;
    end;
  BarSeries.Styles := SeriesStyles;
  Chart.AddSeries(BarSeries);

  sTitle := yTitle;
  if (sTitle <> '') then
    sTitle += ' of ';
  sTitle += XVar.GetVariableLabel(VariableLabelOutput);
  if (hasBy) then
    sTitle += ' by ' + ByVarName;
  if (WeightVarName <> '') then
    sTitle += LineEnding + 'weighted (' + WeightVarName + ')';

  VariableLabelOutput := VariableLabelTypeFromOptionList(Command.Options, FExecutor.SetOptions, sovStatistics);
  ChartConfiguration := FChartFactory.NewChartConfiguration();
  ChartConfiguration.GetTitleConfiguration()
    .SetTitle(sTitle)
    .SetFootnote('Chart by EpiData')
    .SetXAxisTitle(XVar.GetVariableLabel(VariableLabelOutput))
    .SetYAxisTitle(yTitle);

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

  if (not valueSeries) then
    begin
      TablesAll.Free;
      T.Free;
    end;
  XVar := nil;
  DataFile.Free;
  VarNames.Free;
end;

initialization
  RegisterChartCommand(stBarchart, TBarChart);

end.
