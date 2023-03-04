unit graphform.impl;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, graphform, chartfactory, chartcommandresult,
  ComCtrls, auto_position_form, Dialogs, graphpopupmenu, savegraphdialogaction;

type

  { TGraphForm }

  TGraphForm = class(TCustomAutoPositionForm, IGraphForm)
  private
    FCommandResult: IChartCommandResult;
    FGraphPopupMenu: TGraphPopupMenu;
    FPageControl: TPageControl;
    FSaveGraphAction: TSaveGraphDialogAction;
  public
    constructor Create(AOwner: TComponent); override;
    procedure SetCommandResult(ACommandResult: IChartCommandResult);
    function GetForm: TCustomForm;
  end;

implementation

uses
  Controls, TAGraph, charttitles, chartpair;

{ TGraphForm }

constructor TGraphForm.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FPageControl := TPageControl.Create(Self);
  FPageControl.Parent := Self;
  FPageControl.Align := alClient;
  // Remove this to support multiple charts on the form
  FPageControl.ShowTabs := false;

  FSaveGraphAction := TSaveGraphDialogAction.Create(Self);

  FGraphPopupMenu := TGraphPopupMenu.Create(self);
  FGraphPopupMenu.SaveAsAction := FSaveGraphAction;

  PopupMenu := FGraphPopupMenu;
end;

procedure TGraphForm.SetCommandResult(ACommandResult: IChartCommandResult);
var
  ChartPairs: TChartPairList;
  Pair: TChartPair;
  Sheet: TTabSheet;
  Titles: IChartTitles;
  Chart: TChart;
  i: Integer;
  s: UTF8String;
begin
  ChartPairs := ACommandResult.GetChartPairs();
  for Pair in ChartPairs do
    begin
      Chart := Pair.Chart;

      Sheet := FPageControl.AddTabSheet;
      Sheet.Caption := 'Chart: ' + IntToStr(FPageControl.PageCount);
      Chart.Parent := Sheet;
      Chart.Align := alClient;
      Sheet.InsertComponent(Chart);
// if graph command adds a second title line, line 2 is the tab caption
      s := Pair.Configuration.GetTitleConfiguration().GetTitle();
      i := Pos(LineEnding, s);
      if (i > 0) then
        Sheet.Caption := copy(s, i+1);
    end;
  FPageControl.ShowTabs := (ChartPairs.Count > 1);
  // TODO: Jamie/Torsten - accomodate multiple charts from one graph command
  //       saving just the one the user clicked on
  FSaveGraphAction.Chart := ChartPairs.First.Chart;
end;

function TGraphForm.GetForm: TCustomForm;
begin
  Result := Self;
end;

end.

