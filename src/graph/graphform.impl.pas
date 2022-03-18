unit graphform.impl;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, graphform, chartfactory, graphcommandresult,
  ComCtrls, auto_position_form;

type

  { TGraphForm }

  TGraphForm = class(TCustomAutoPositionForm, IGraphForm)
  private
    FCommandResult: IGraphCommandResult;
    FPageControl: TPageControl;
  public
    constructor Create(AOwner: TComponent); override;
    procedure SetCommandResult(ACommandResult: IGraphCommandResult);
    function GetForm: TCustomForm;
  end;

implementation

uses
  Controls, TAGraph, charttitles;

{ TGraphForm }

constructor TGraphForm.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FPageControl := TPageControl.Create(Self);
  FPageControl.Parent := Self;
  FPageControl.Align := alClient;
end;

procedure TGraphForm.SetCommandResult(ACommandResult: IGraphCommandResult);
var
  Charts: TChartList;
  Chart: TChart;
  Sheet: TTabSheet;
  Titles: IChartTitles;
begin
  Charts := ACommandResult.GetCharts();

  for Chart in Charts do
    begin
      Sheet := FPageControl.AddTabSheet;
      Sheet.Caption := 'Graph: ' + IntToStr(FPageControl.PageCount);
      Chart.Parent := Sheet;
      Chart.Align := alClient;

      Titles := ACommandResult.GetChartTitles(Chart);

      Chart.Title.Visible := Titles.GetTitle() <> '';
      Chart.Title.Text.Clear;
      Chart.Title.Text.Add(Titles.GetTitle());

      Chart.Foot.Visible := Titles.GetFootnote() <> '';
      Chart.Foot.Text.Clear;
      Chart.Foot.Text.Add(Titles.GetFootnote());

      Chart.BottomAxis.Title.Visible := Titles.GetXAxisTitle() <> '';
      Chart.BottomAxis.Title.Caption := Titles.GetXAxisTitle();

      Chart.LeftAxis.Title.Visible := Titles.GetYAxisTitle() <> '';
      Chart.LeftAxis.Title.Caption := Titles.GetYAxisTitle();
    end;
end;

function TGraphForm.GetForm: TCustomForm;
begin
  Result := Self;
end;

end.

