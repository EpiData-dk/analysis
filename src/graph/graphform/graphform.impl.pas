unit graphform.impl;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, graphform, chartfactory, graphcommandresult,
  ComCtrls, auto_position_form, Dialogs, graphpopupmenu, savegraphaction;

type

  { TGraphForm }

  TGraphForm = class(TCustomAutoPositionForm, IGraphForm)
  private
    FCommandResult: IGraphCommandResult;
    FGraphPopupMenu: TGraphPopupMenu;
    FPageControl: TPageControl;
    FSaveGraphAction: TSaveGraphAction;
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
  // Remove this to support multiple charts on the form
  FPageControl.ShowTabs := false;

  FSaveGraphAction := TSaveGraphAction.Create(Self);

  FGraphPopupMenu := TGraphPopupMenu.Create(self);
  FGraphPopupMenu.SaveAsAction := FSaveGraphAction;

  PopupMenu := FGraphPopupMenu;
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
      Sheet.Caption := 'Chart: ' + IntToStr(FPageControl.PageCount);
      Chart.Parent := Sheet;
      Chart.Align := alClient;
      Sheet.InsertComponent(Chart);

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

  FGraphPopupMenu.Chart := Charts.First;
  FSaveGraphAction.Chart := Charts.First;
end;

function TGraphForm.GetForm: TCustomForm;
begin
  Result := Self;
end;

end.

