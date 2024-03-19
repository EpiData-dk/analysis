unit graphcommandexecutor;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fgl, Forms, ast, executor, chartcommandresult, graphform,
  charttitles, chartaxesconfiguration, outputcreator, TATextElements,
  TAChartAxisUtils, TAChartAxis, TATypes, Graphics;

type

  { TGraphCommandExecutor }

  TGraphCommandExecutor = class
  private
    FExecutor: TExecutor;
    FOutputCreator: TOutputCreator;
    FFont: TFont;
    procedure SaveGraph(ST: TCustomGraphCommand; CommandResult: IChartCommandResult);
    procedure ShowDialog(ST: TCustomGraphCommand; CommandResult: IChartCommandResult);
    procedure UpdateChartTitles(ST: TCustomGraphCommand; CommandResult: IChartCommandResult);
    procedure ApplyChartTitle(ChartTitle: TChartTitle; MainCaption: UTF8String; Option: TOption);
    procedure ApplyAxisTitle(AxisTitle: TChartAxisTitle; MainCaption: UTF8String; Option: TOption);
    procedure UpdateChartAxes(ST: TCustomGraphCommand; CommandResult: IChartCommandResult);
    procedure ShowMarksAsDates(var AText: String; AMark: Double);
    function ApplyChartSaveName(Command: UTF8String; Option: TOption): UTF8String;
  public
    constructor Create(AExecutor: TExecutor; AOutputCreator: TOutputCreator);
    procedure Execute(ST: TCustomGraphCommand);
  end;

implementation

uses
  chartcommand, chartfactory, graphformfactory, savegraphaction, TAGraph, chartpair,
  LazFileUtils, ana_globals, ast_types, chart_options, options_utils;

{ TGraphCommandExecutor }

procedure TGraphCommandExecutor.SaveGraph(ST: TCustomGraphCommand;
  CommandResult: IChartCommandResult);
var
  SaveAction: TSaveGraphAction;
  Opt: TOption;
  Chart: TChart;
  ChartPairs: TChartPairList;
  Pair: TChartPair;
  aFileName: UTF8String;
  allNames: UTF8String;
  saveName: UTF8String;
  saveMultiple: UTF8String;
begin
  SaveAction := TSaveGraphAction.Create(nil);
  ST.HasOption(['export', 'S', 'E'], Opt);
  aFileName := Opt.Expr.AsString;
  allNames := '';
  saveMultiple := 'Graph';
  try
    ChartPairs := CommandResult.GetChartPairs();
    if (ChartPairs.Count > 1) then
      saveMultiple := 'Graphs';
    for Pair in Chartpairs do
      begin
        saveName := GetSaveChartFilename(aFileName, Pair.Configuration.GetTitleConfiguration().GetSaveName());
        SaveAction.AddChart(Pair.Chart, saveName);
        // This forces the SaveAction to free the chart, chartsource and objects
        // in the chartsource (see Scatter)
        SaveAction.InsertComponent(Pair.Chart);
        allNames += LineEnding + '    ' + checkName;
//        SaveAction.AddFilename(aFileName);
        if (FileExistsUTF8(saveName)) and
           (not ST.HasOption('replace')) then
          begin
            FOutputCreator.DoError('File exists: ' + saveName);
            ST.ExecResult := csrFailed;
          end;
      end;
    if (ST.ExecResult = csrFailed) then
      FOutputCreator.DoError('Add !REPLACE or erase file' + saveMultiple)
    else
      begin
        if (ST.HasOption(['sizex', 'sx'], Opt)) then
          SaveAction.GraphSize.Width := Opt.Expr.AsInteger;

        if (ST.HasOption(['sizey', 'sy'], Opt)) then
          SaveAction.GraphSize.Height := Opt.Expr.AsInteger;

        if SaveAction.Execute then
          FOutputCreator.DoInfoAll(saveMultiple + ' saved as: ' + allNames)
        else
          FOutputCreator.DoError(saveMultiple + 'not saved!');
      end;
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
  i, inc: Integer;
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

    ST.HasOption(['s', 'save', 'e', 'export'], Opt);
    Pair.Configuration.GetTitleConfiguration().SetSaveName(ApplyChartSaveName(ST.Command, Opt));

    ST.HasOption(['footer', 'fn'], Opt);
    ApplyChartTitle(Chart.Foot, Titles.GetFootnote(), Opt);

    ST.HasOption(['xtitle', 'xt'], Opt);
    ApplyAxisTitle(Chart.BottomAxis.Title, Titles.GetXAxisTitle(), Opt);

    ST.HasOption(['ytitle', 'yt'], Opt);
    ApplyAxisTitle(Chart.LeftAxis.Title, Titles.GetYAxisTitle(), Opt);

    // set font for all axes and legen
    for i := 0 to Chart.AxisList.Count - 1 do
      Chart.AxisList.Axes[i].Title.LabelFont := FFont;

    Chart.Legend.Font := FFont;
  end;
end;

procedure TGraphCommandExecutor.ApplyChartTitle(ChartTitle: TChartTitle;
  MainCaption: UTF8String; Option: TOption);
begin
  if (Assigned(Option)) then
    MainCaption := Option.Expr.AsString;

  ChartTitle.Visible := MainCaption <> '';
  ChartTitle.Text.Clear;
  ChartTitle.Text.Add(MainCaption);
  ChartTitle.Font.Assign(FFont);
end;

procedure TGraphCommandExecutor.ApplyAxisTitle(AxisTitle: TChartAxisTitle;
  MainCaption: UTF8String; Option: TOption);
begin
  if (Assigned(Option)) then
    MainCaption := Option.Expr.AsString;
  AxisTitle.LabelFont.Assign(FFont);
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

function TGraphCommandExecutor.ApplyChartSaveName(Command: UTF8String; Option: TOption): UTF8String;
var
  saveName: UTF8String;
begin
  if (assigned(Option)) then
    Result := Option.Expr.AsString
  else
    Result := Command;
end;

constructor TGraphCommandExecutor.Create(AExecutor: TExecutor; AOutputCreator: TOutputCreator);
var
  AFontName: UTF8String;
begin
  FExecutor := AExecutor;
  FOutputCreator := AOutputCreator;
  AFontName := ANA_SO_CHART_FONT_NAME;
  if (AFontName = '') then
    AFontName := ANA_SO_OUTPUT_FONT_NAME;
  // MAC requires a font name if saving to .svg
  {$IFDEF DARWIN}
  if (AFontName = '') then
    AFontName := 'Arial';
  {$ENDIF}
  FFont := FontFromSetOptions(
           AFontName,
           ANA_SO_CHART_FONT_SIZE,
           ANA_SO_CHART_FONT_COLOR,
           ANA_SO_CHART_FONT_STYLE,
           FExecutor.SetOptions
         );
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
