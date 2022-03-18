unit graphformfactory.impl;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, graphform, graphformfactory, fgl;

type

  TGraphFormList = specialize TFPGList<IGraphForm>;

  { TGraphFormFactory }

  TGraphFormFactory = class(IGraphFormFactory)
  private
    FList: TGraphFormList;
  public
    constructor Create();
    function NewGraphForm(): IGraphForm;
    procedure CloseAllOpenForms();
  end;

implementation

uses
  graphform.impl;

{ TGraphFormFactory }

constructor TGraphFormFactory.Create();
begin
  FList := TGraphFormList.Create;
end;

function TGraphFormFactory.NewGraphForm(): IGraphForm;
begin
  result := TGraphForm.Create(nil);
  FList.Add(Result);
end;

procedure TGraphFormFactory.CloseAllOpenForms();
var
  i: Integer;
begin
  for i := 0 to FList.Count - 1 do
    FList[i].GetForm.Free;
  FList.Clear;
end;

end.

