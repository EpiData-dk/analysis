unit scatter;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, chartcommandresult, TAGraph, executor, outputcreator,
  ast, epidatafiles, chartcommand, chartfactory, chartconfiguration;

type

  { TScatterChart }

  TScatterChart = class(TInterfacedObject, IChartCommand)
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
  TASeries, TATypes, TAStyles, TASources, TALegend, Graphics,
  charttitles, ast_types, scattersource, epidatafilestypes,
  epifields_helper, options_utils, chart_options;

{ TScatterChart }

procedure TScatterChart.Init(ChartFactory: IChartFactory; Executor: TExecutor;
  OutputCreator: TOutputCreator);
begin
  FChartFactory := ChartFactory;
  FExecutor := Executor;
  FOutputCreator := OutputCreator;
end;

function TScatterChart.Execute(Command: TCustomGraphCommand): IChartCommandResult;
var
  LineSeries: array of TLineSeries;
  ScatterSource: array of TScatterSource;
  VarNames: TStrings;
  StratifyVarName: UTF8String;
  Chart: TChart;
  DF: TEpiDataFile;
  XVar, YVar, SVar: TEpiField;
  SortFields: TEpiFields;
  ChartConfiguration: IChartConfiguration;
  LineStyles: array of TChartStyles;
  lStyle:  TChartStyle;
  sColors: TColorMap;
  VariableLabelType: TEpiGetVariableLabelType;
  ValueLabelOutput:    TEpiGetValueLabelType;
  VariableLabelOutput: TEpiGetVariableLabelType;
  sPoints, sLine: Boolean;
  Opt: TOption;
  doStrata: Boolean;
  i, nStrata: Integer;
  sOffset: array of Integer;
  msg, sTitle: UTF8String;
begin
  sColors := ChartColorsFromOptions(Command.Options, FExecutor.SetOptions, msg);
  if (msg <> '') then
  begin
    FExecutor.Error(msg);
    exit;
  end;
  VariableLabelOutput := VariableLabelTypeFromOptionList(Command.Options, FExecutor.SetOptions);
  ValueLabelOutput    := ValueLabelTypeFromOptionList(Command.Options, FExecutor.SetOptions);
  // Get Variable names
  VarNames := Command.VariableList.GetIdentsAsList;
  // Create the charts
  Chart := FChartFactory.NewChart();
  Result := FChartFactory.NewGraphCommandResult();

  doStrata := Command.HasOption('by', Opt);
  if (doStrata) then
    begin
      StratifyVarName := Opt.Expr.AsIdent;
      VarNames.Add(StratifyVarName);
    end;

  // Get the data and fields.
  DF := FExecutor.PrepareDatafile(VarNames, VarNames);
  if (DF.Size < 1) then
    FExecutor.Error('No data')
  else
  begin  // create chart
    // Get other options
    sLine := Command.HasOption('l');
    sPoints := (not Command.HasOption('l')) or Command.HasOption('p');
    XVar := DF.Fields.FieldByName[VarNames[0]];
    YVar := DF.Fields.FieldByName[Varnames[1]];
    if (doStrata) then
      begin   // stratified
        SVar := DF.Fields.FieldByName[StratifyVarName];
        sTitle := LineEnding + 'by ' + SVar.GetVariableLabel(VariableLabelOutput);
        SortFields := TEpiFields.Create(nil);
        SortFields.AddItem(SVar);
        SortFields.AddItem(XVar);
        DF.SortRecords(SortFields);
        nStrata := 0;
        sOffset := [0];
        for i := 1 to SVar.Size - 1 do
          begin
            if (SVar.AsValue[i] <> SVar.AsValue[i-1]) then
              begin
                nStrata += 1;
                SetLength(sOffset,nStrata + 1);
                sOffset[nStrata] := i;
              end;
          end;
        SetLength(sOffset, nStrata + 2);
        sOffset[nStrata + 1] := DF.Size - 1;
      end
    else
      begin   // unstratified
        sTitle  := '';
        nStrata := 0;
        sOffset := [0,DF.Size - 1];
        DF.SortRecords(XVar);
      end;
    Varnames.Free;

    // Create one series and source for each stratum
    SetLength(ScatterSource, nStrata + 1);
    // - datasource is destroyed by the chart, so we let it handle the datafile destruction
    //   otherwise we would leak memory.
    SetLength(LineSeries,nStrata + 1);
    SetLength(LineStyles, nStrata + 1);

    for i := 0 to nStrata do
      begin  // create a series
        ScatterSource[i] := TScatterSource.Create(Chart);
        with ScatterSource[i] do
          begin
            Datafile := DF;
            Offset := sOffset[i];
            Size   := sOffset[i+1] - sOffset[i];
            XVariableName := XVar.Name;
            YVariableName := YVar.Name;
            Sorted := true;
          end;

        // style the series
        LineStyles[i] := TChartStyles.Create(Chart);
        lStyle     := LineStyles[i].Add;
        with lStyle do
          begin
            Pen.Style   := psSolid;
            Pen.Color   := sColors[i];
            Pen.Width   := 4;
            if (doStrata) then
              Text      := SVar.GetValueLabel(sOffset[i], ValueLabelOutput);
          end;

        // Create the line/point series
        LineSeries[i] := TLineSeries.Create(Chart);
        with LineSeries[i] do
          begin
            Source := ScatterSource[i];
            ShowPoints := sPoints;
            Styles := LineStyles[i];
            if (sPoints) then
            begin
              Pointer.Pen.Color := sColors[i];
              Pointer.Pen.Width := 2;
              Pointer.Style := psCircle;
              Linetype := ltNone;
            end;
            if (Sline) then
            begin
              LineType := ltFromPrevious;
//                LinePen.Color:= sColors[i];
            end;
            if (doStrata) then
              begin
                Legend.Multiplicity:=lmStyle;
                Legend.GroupIndex  := i;
              end;
          end;
        // Add series to the chart
        Chart.AddSeries(LineSeries[i]);
      end;

    // Create the titles
    ChartConfiguration := FChartFactory.NewChartConfiguration();
    ChartConfiguration.GetTitleConfiguration()
      .SetTitle(XVar.GetVariableLabel(VariableLabelType) + ' vs. ' + YVar.GetVariableLabel(VariableLabelType)
       + sTitle)
      .SetFootnote('')
      .SetXAxisTitle(XVar.GetVariableLabel(VariableLabelType))
      .SetYAxisTitle(YVar.GetVariableLabel(VariableLabelType));

    ChartConfiguration.GetAxesConfiguration()
      .GetXAxisConfiguration()
      .SetShowAxisMarksAsDates(XVar.FieldType in DateFieldTypes);

    ChartConfiguration.GetAxesConfiguration()
      .GetYAxisConfiguration()
      .SetShowAxisMarksAsDates(YVar.FieldType in DateFieldTypes);

    with Chart do
      begin
        BottomAxis.Grid.Style   := psClear;
        BottomAxis.Margin       := 0;
        LeftAxis.Grid.Style     := psDot;
        LeftAxis.Grid.Color     := clSilver;
        LeftAxis.Margin         := 0;
        Frame.Visible           := false;
      end;

    if (doStrata) then
        with Chart.Legend do
          begin
            Visible        := true;
            UseSidebar     := true;
            Frame.Visible  := false;
            GroupTitles.Add(SVar.GetVariableLabel(VariableLabelType));
          end;

    // Create the command result
    Result.AddChart(Chart, ChartConfiguration);
  end;   // create chart
end;

initialization
  RegisterChartCommand(stScatter, TScatterChart);

end.
