unit graphcommandexecutor;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fgl, Forms, ast, executor, chartcommandresult, graphform,
  charttitles, chartaxesconfiguration, outputcreator, TATextElements,
  TAChartAxisUtils, TAChartAxis, TATypes;

type

  { TGraphCommandExecutor }

  TGraphCommandExecutor = class
  private
    FExecutor: TExecutor;
    FOutputCreator: TOutputCreator;
    procedure SaveGraph(ST: TCustomGraphCommand; CommandResult: IChartCommandResult);
    procedure ShowDialog(ST: TCustomGraphCommand; CommandResult: IChartCommandResult);
    procedure UpdateChartTitles(ST: TCustomGraphCommand; CommandResult: IChartCommandResult);
    procedure ApplyChartTitle(ChartTitle: TChartTitle; MainCaption: UTF8String; Option: TOption);
    procedure ApplyAxisTitle(AxisTitle: TChartAxisTitle; MainCaption: UTF8String; Option: TOption);
    procedure UpdateChartAxes(ST: TCustomGraphCommand; CommandResult: IChartCommandResult);
    procedure ShowMarksAsDates(var AText: String; AMark: Double);
  public
    constructor Create(AExecutor: TExecutor; AOutputCreator: TOutputCreator);
    procedure Execute(ST: TCustomGraphCommand);
  end;

implementation

uses
  chartcommand, chartfactory, graphformfactory, savegraphaction, TAGraph, chartpair,
  LazFileUtils, ana_globals, ast_types, chart_options, Graphics, options_utils;

{ TGraphCommandExecutor }

procedure TGraphCommandExecutor.SaveGraph(ST: TCustomGraphCommand;
  CommandResult: IChartCommandResult);
var
  SaveAction: TSaveGraphAction;
  Opt: TOption;
  Chart: TChart;
begin
  SaveAction := TSaveGraphAction.Create(nil);

  try
    Chart := CommandResult.GetChartPairs().First.Chart;
    SaveAction.Chart := Chart;

    // This forces the SaveAction to free the chart, chartsource and objects
    // in the chartsource (see Scatter)
    SaveAction.InsertComponent(Chart);

    ST.HasOption(['export', 'S', 'E'], Opt);
    SaveAction.Filename := Opt.Expr.AsString;
    if (FileExistsUTF8(SaveAction.Filename)) and
       (not ST.HasOption('replace'))
    then
      begin
        FOutputCreator.DoError('File exists.' + LineEnding + 'Add !REPLACE or erase file:' + LineEnding + SaveAction.Filename);
        ST.ExecResult := csrFailed;
      end;

    if (ST.HasOption(['sizex', 'sx'], Opt)) then
      SaveAction.GraphSize.Width := Opt.Expr.AsInteger;

    if (ST.HasOption(['sizey', 'sy'], Opt)) then
      SaveAction.GraphSize.Height := Opt.Expr.AsInteger;

    if SaveAction.Execute then
      FOutputCreator.DoInfoAll('Graph saved as: ' + SaveAction.Filename)
    else
      FOutputCreator.DoError('Graph not saved!');
  except
    on E: EIncorrectGraphExtension do
      FOutputCreator.DoError('Graph not saved! ' + E.Message);
    on E: Exception do
      FOutputCreator.DoError('Graph not saved! ' + E.Message);
  end;

  SaveAction.Free;
end;

procedure TGraphCommandExecutor.ShowDialog(ST: TCustomGraphCommand;
  CommandResult: IChartCommandResult);
var
  GraphForm: IGraphForm;
  Form: TCustomForm;
begin
  GraphForm := TheGraphFormFactory.NewGraphForm();
  GraphForm.SetCommandResult(CommandResult);
  Form := GraphForm.GetForm;
  Form.Show;
end;

procedure TGraphCommandExecutor.ShowMarksAsDates(var AText: String;
  AMark: Double);
begin
  AText := DateToStr(AMark);
end;

procedure TGraphCommandExecutor.UpdateChartTitles(ST: TCustomGraphCommand;
  CommandResult: IChartCommandResult);
var
  ChartPairs: TChartPairList;
  Pair: TChartPair;
  Chart: TChart;
  Titles: IChartTitles;
  Opt: TOption;
  inc: Integer;
begin
  ChartPairs := CommandResult.GetChartPairs();

  for Pair in ChartPairs do
  begin
    Chart := Pair.Chart;
    Titles := Pair.Configuration.GetTitleConfiguration();

    ST.HasOption(['title', 'ti'], Opt);
    ApplyChartTitle(Chart.Title, Titles.GetTitle(), Opt);
    // main title may be larger
    inc := StrToInt(FExecutor.SetOptions.GetValue(ANA_SO_CHART_TITLE_SIZE_INCREMENT).Value);
    if (inc > 0) then
      Chart.Title.Font.Size := Chart.Title.Font.Size + inc;

    ST.HasOption(['footer', 'fn'], Opt);
    ApplyChartTitle(Chart.Foot, Titles.GetFootnote(), Opt);

    ST.HasOption(['xtitle', 'xt'], Opt);
    ApplyAxisTitle(Chart.BottomAxis.Title, Titles.GetXAxisTitle(), Opt);

    ST.HasOption(['ytitle', 'yt'], Opt);
    ApplyAxisTitle(Chart.LeftAxis.Title, Titles.GetYAxisTitle(), Opt);
  end;
end;

procedure TGraphCommandExecutor.ApplyChartTitle(ChartTitle: TChartTitle;
  MainCaption: UTF8String; Option: TOption);
var
  AFont: TFont;
begin
  if (Assigned(Option)) then
    MainCaption := Option.Expr.AsString;

  ChartTitle.Visible := MainCaption <> '';
  ChartTitle.Text.Clear;
  ChartTitle.Text.Add(MainCaption);
  AFont := FontFromSetOptions(
           ANA_SO_CHART_FONT_NAME,
           ANA_SO_CHART_FONT_SIZE,
           ANA_SO_CHART_FONT_COLOR,
           ANA_SO_CHART_FONT_STYLE,
           FExecutor.SetOptions
         );
  ChartTitle.Font.Assign(AFont);
end;

procedure TGraphCommandExecutor.ApplyAxisTitle(AxisTitle: TChartAxisTitle;
  MainCaption: UTF8String; Option: TOption);
begin
  if (Assigned(Option)) then
    MainCaption := Option.Expr.AsString;

  AxisTitle.Visible := MainCaption <> '';
  AxisTitle.Caption := MainCaption;
end;

procedure TGraphCommandExecutor.UpdateChartAxes(ST: TCustomGraphCommand;
  CommandResult: IChartCommandResult);
var
  ChartPairs: TChartPairList;
  Pair: TChartPair;
  Chart: TChart;
  Configuration: IChartAxesConfiguration;
  Opt: TOption;
  isDate: Boolean;
  minmax: double;
  validRange: boolean;
  rangeMsg: UTF8String;
  errorMsg: UTF8String;

  function setMinMax(out value: double; out msg: UTF8String): boolean;
  begin
    result := true;
    msg := '';
    if (isDate) then
      begin
        if (opt.Expr.ResultType = rtDate) then
          begin
            value := opt.Expr.AsDate;
            exit;
          end;
      end
    else
      begin
        if (opt.Expr.ResultType <> rtDate) then
          begin
            value := opt.Expr.AsFloat;
            exit;
          end;
      end;
    result := false;
    msg := ' Ignored invalid ' + opt.Ident + ': ' + opt.Expr.AsString;
  end;

begin
  ChartPairs := CommandResult.GetChartPairs();
  validRange := true;
  errorMsg := '';

  for Pair in ChartPairs do
    begin
      Chart := Pair.Chart;
      Configuration := Pair.Configuration.GetAxesConfiguration();

      with (Chart.BottomAxis) do
        begin
          isDate := Configuration.GetXAxisConfiguration().GetShowAxisMarksAsDates;
          if (isDate) then
            begin
              OnMarkToText := @ShowMarksAsDates;
              Marks.OverlapPolicy := opHideNeighbour;
              Marks.SetAdditionalAngle(Pi / 2);
            end;

          if (ST.HasOption('xmin', opt)) then
            if (setMinMax(minmax, rangeMsg))then
              begin
                Range.UseMin := true;
                Range.Min := minmax;
              end
            else
              ErrorMsg += rangeMsg + LineEnding;

          if (ST.HasOption('xmax', opt)) then
            if (setMinMax(minmax, rangeMsg))then
              begin
                Range.UseMax  := true;
                Range.Max := minmax;
              end
            else
              ErrorMsg += rangeMsg + LineEnding;
        end;

      with (Chart.LeftAxis) do
        begin
          isDate := Configuration.GetYAxisConfiguration().GetShowAxisMarksAsDates;
          if (isDate) then
            OnMarkToText := @ShowMarksAsDates;

          if (ST.HasOption('ymin', opt)) then
            if (setMinMax(minmax, rangeMsg))then
              begin
                Range.UseMin := true;
                Range.Min := minmax;
              end
            else
              ErrorMsg += rangeMsg + LineEnding;

          if (ST.HasOption('ymax', opt)) then
            if (setMinMax(minmax, rangeMsg))then
              begin
                Range.UseMax  := true;
                Range.Max := minmax;
              end
            else
              ErrorMsg += rangeMsg + LineEnding;
        end;
      end;
    if (errorMsg <> '') then
      FExecutor.Error(errorMsg);
end;

constructor TGraphCommandExecutor.Create(AExecutor: TExecutor; AOutputCreator: TOutputCreator);
begin
  FExecutor := AExecutor;
  FOutputCreator := AOutputCreator;
end;

procedure TGraphCommandExecutor.Execute(ST: TCustomGraphCommand);
var
  Command: IChartCommand;
  CommandResult: IChartCommandResult;
  Opt: TOption;
begin
  Command := GetChartCommand(ST.StatementType);
  Command.Init(TheChartFactory, FExecutor, FOutputCreator);
  CommandResult := Command.Execute(ST);

  if (CommandResult = nil) then exit;
  if (CommandResult.GetChartPairs().Count = 0) then exit;

  UpdateChartTitles(ST, CommandResult);
  UpdateChartAxes(ST, CommandResult);

  if (ST.HasOption(['export', 'S', 'E'], Opt)) then
    SaveGraph(ST, CommandResult)
  else
    ShowDialog(ST, CommandResult);
end;

end.
