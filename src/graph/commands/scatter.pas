unit scatter;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, graphcommand, TAGraph, executor, outputcreator,
  ast, epidatafiles, TASources, TACustomSource;

type

  { TScatter }

  TScatter = class(IGraphCommand)
  private
    FChart: TChart;
    FDataFile: TEpiDataFile;
    FExecutor: TExecutor;
    FOutputCreator: TOutputCreator;
    FXVar: TEpiField;
    FYVar: TEpiField;
    procedure GetScatterDataItem(ASource: TUserDefinedChartSource;
      AIndex: Integer; var AItem: TChartDataItem);
  public
    destructor destroy; override;
    procedure Init(ChartFactory: IChartFactory; Executor: TExecutor; OutputCreator: TOutputCreator);
    function Execute(Command: TCustomGraphCommand): IGraphCommandResult;
    function GetObject(): TObject;
  end;

implementation

uses
  TASeries, TATypes, Graphics;

{ TScatter }

procedure TScatter.GetScatterDataItem(ASource: TUserDefinedChartSource;
  AIndex: Integer; var AItem: TChartDataItem);
begin
  AItem.X := FXVar.AsFloat[AIndex];
  AItem.Y := FYVar.AsFloat[AIndex];
end;

destructor TScatter.destroy;
begin
  FDataFile.Free;
  inherited destroy;
end;

procedure TScatter.Init(ChartFactory: IChartFactory; Executor: TExecutor;
  OutputCreator: TOutputCreator);
begin
  FChart := ChartFactory.NewChart();
  FChart.AllowZoom := false;

  FExecutor := Executor;
  FOutputCreator := OutputCreator;
end;

function TScatter.Execute(Command: TCustomGraphCommand): IGraphCommandResult;
var
  line: TLineSeries;
  ScatterSource: TUserDefinedChartSource;
  VarNames: TStrings;
begin
  VarNames := Command.VariableList.GetIdentsAsList;
  FDataFile := FExecutor.PrepareDatafile(VarNames, VarNames);

  line := TLineSeries.Create(FChart);
  line.ShowPoints := true;
  line.LineType := ltNone;
  line.Pointer.Brush.Style := bsClear;
  line.Pointer.Pen.Color := clBlack;
  line.Pointer.Style := psCircle;

  FXVar := FDataFile.Field[0];
  FYVar := FDataFile.Field[1];

  ScatterSource := TUserDefinedChartSource.Create(FChart);
  ScatterSource.OnGetChartDataItem := @GetScatterDataItem;
  ScatterSource.PointsNumber := FXVar.Size;
  line.Source := ScatterSource;

  FChart.AddSeries(line);

  // TODO : All axis text etc. should be moved out into a common system
  FChart.Title.Visible := true;
  FChart.Title.Text.Clear;
  FChart.Title.Text.Add(FXVar.Question.Text + ' vs. ' + FYVar.Question.Text);

  FChart.BottomAxis.Title.Visible := true;
  FChart.BottomAxis.Title.Caption := FXVar.Question.Text;
  FChart.LeftAxis.Title.Visible := true;
  FChart.LeftAxis.Title.Caption := FYVar.Question.Text;

  Result := nil;
end;

function TScatter.GetObject(): TObject;
begin
  Result := Self;
end;

end.

