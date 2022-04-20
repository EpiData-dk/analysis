unit graphcommandexecutor;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fgl, Forms, ast, executor, chartcommandresult, graphform,
  charttitles, outputcreator, TATextElements, TAChartAxisUtils;

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
  public
    constructor Create(AExecutor: TExecutor; AOutputCreator: TOutputCreator);
    procedure Execute(ST: TCustomGraphCommand);
  end;

implementation

uses
  chartcommand, chartfactory, graphformfactory, savegraphaction, TAGraph;

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
    Chart := CommandResult.GetCharts().First;
    SaveAction.Chart := Chart;

    // This forces the SaveAction to free the chart, chartsource and objects
    // in the chartsource (see Scatter)
    SaveAction.InsertComponent(Chart);

    ST.HasOption(['export', 'S', 'E'], Opt);
    SaveAction.Filename := Opt.Expr.AsString;

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

procedure TGraphCommandExecutor.UpdateChartTitles(ST: TCustomGraphCommand;
  CommandResult: IChartCommandResult);
var
  Charts: TChartList;
  Chart: TChart;
  Titles: IChartTitles;
  Opt: TOption;
begin
  Charts := CommandResult.GetCharts();

  for Chart in Charts do
  begin
    Titles := CommandResult.GetChartTitles(Chart);

    ST.HasOption(['title', 't'], Opt);
    ApplyChartTitle(Chart.Title, Titles.GetTitle(), Opt);

    ST.HasOption(['footer', 'f'], Opt);
    ApplyChartTitle(Chart.Foot, Titles.GetFootnote(), Opt);

    ST.HasOption(['xtitle', 'xt'], Opt);
    ApplyAxisTitle(Chart.BottomAxis.Title, Titles.GetXAxisTitle(), Opt);

    ST.HasOption(['ytitle', 'yt'], Opt);
    ApplyAxisTitle(Chart.LeftAxis.Title, Titles.GetYAxisTitle(), Opt);
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
end;

procedure TGraphCommandExecutor.ApplyAxisTitle(AxisTitle: TChartAxisTitle;
  MainCaption: UTF8String; Option: TOption);
begin
  if (Assigned(Option)) then
    MainCaption := Option.Expr.AsString;

  AxisTitle.Visible := MainCaption <> '';
  AxisTitle.Caption := MainCaption;
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

  UpdateChartTitles(ST, CommandResult);

  if (ST.HasOption(['export', 'S', 'E'], Opt)) then
    SaveGraph(ST, CommandResult)
  else
    ShowDialog(ST, CommandResult);
end;

end.

