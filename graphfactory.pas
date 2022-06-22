unit graphfactory;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, graphcommand;

type

  { TGraphFormFactory }

  TGraphFormFactory = class(TComponent)

  public
    constructor Create(AOwner: TComponent); override;
    function NewChartFactory(): IChartFactory;
    procedure ShowCharts(Factory: IChartFactory);
  end;

implementation

{ TGraphFormFactory }

constructor TGraphFormFactory.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
end;

function TGraphFormFactory.NewGraphForm(): TGraphForm1;
begin
  result := TGraphForm1.Create(self);
end;

end.

