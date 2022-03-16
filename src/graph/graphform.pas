unit graphform;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fgl, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  ComCtrls, chartfactory, TAGraph, TASources;

type

  { TGraphForm1 }

  TGraphForm1 = class(TForm, IGraphForm)
    PageControl1: TPageControl;
    Panel1: TPanel;
  public
    procedure SetCommandResult(ACommandResult: IGraphCommandResult);
    function GetForm: TCustomForm;
  end;

var
  GraphForm1: TGraphForm1;

implementation

{$R *.lfm}

{ TGraphForm1 }

procedure TGraphForm1.SetCommandResult(ACommandResult: IGraphCommandResult);
var
  Charts: TChartList;
  Chart: TChart;
  Sheet: TTabSheet;
  Titles: IChartTitles;
begin
  Charts := ACommandResult.GetCharts();

  for Chart in Charts do
    begin
      Sheet := PageControl1.AddTabSheet;
      Sheet.Caption := 'Graph: ' + IntToStr(PageControl1.PageCount);
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

function TGraphForm1.GetForm: TCustomForm;
begin
  result := self
end;

//function TGraphForm1.NewChart: TChart;
//var
//  Sheet: TTabSheet;
//begin
//  Sheet := PageControl1.AddTabSheet;
//  Sheet.Caption := 'Graph: ' + IntToStr(PageControl1.PageCount);
//
//  result := TChart.Create(self);
//  result.Parent := sheet;
//  result.Align := alClient;
//end;

end.

