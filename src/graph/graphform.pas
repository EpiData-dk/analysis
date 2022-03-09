unit graphform;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, ComCtrls,
  graphcommand, TAGraph, TASources;

type

  { TGraphForm1 }

  TGraphForm1 = class(TForm, IGraphForm, IChartFactory)
    PageControl1: TPageControl;
    Panel1: TPanel;
  public
    function GetChartFactory: IChartFactory;
    function GetForm: TCustomForm;
    function NewChart: TChart;
  end;

var
  GraphForm1: TGraphForm1;

implementation

{$R *.lfm}

{ TGraphForm1 }

function TGraphForm1.GetChartFactory: IChartFactory;
begin
  result := self;
end;

function TGraphForm1.GetForm: TCustomForm;
begin
  result := self
end;

function TGraphForm1.NewChart: TChart;
var
  Sheet: TTabSheet;
begin
  Sheet := PageControl1.AddTabSheet;
  Sheet.Caption := 'Graph: ' + IntToStr(PageControl1.PageCount);

  result := TChart.Create(self);
  result.Parent := sheet;
  result.Align := alClient;
end;

end.

