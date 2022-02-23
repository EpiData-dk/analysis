unit graphform;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, graphcommand, TAGraph;

type

  { TGraphForm1 }

  TGraphForm1 = class(TForm, IGraphForm, IChartFactory)
  private

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
begin
  result := TChart.Create(self);
end;

end.

