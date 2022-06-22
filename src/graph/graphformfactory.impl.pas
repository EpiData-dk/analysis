unit graphformfactory.impl;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, graphform, graphformfactory, fgl, Forms;

type

  TGraphFormList = specialize TFPGList<IGraphForm>;

  { TGraphFormFactory }

  TGraphFormFactory = class(IGraphFormFactory)
  private
    FList: TGraphFormList;
    procedure BeforeFormDestructions(Sender: TObject);
  public
    constructor Create();
    function NewGraphForm(): IGraphForm;
    procedure CloseAllOpenForms();
    procedure CascadeWindows(LeadForm: TCustomForm);
  end;

implementation

uses
  graphform.impl;

{ TGraphFormFactory }

procedure TGraphFormFactory.BeforeFormDestructions(Sender: TObject);
var
  AGraphForm: IGraphForm;
begin
  if (not (Sender is IGraphForm)) then
     Exit;
  AGraphForm := (Sender as IGraphForm);

  FList.Remove(AGraphForm);
end;

constructor TGraphFormFactory.Create();
begin
  FList := TGraphFormList.Create;
end;

function TGraphFormFactory.NewGraphForm(): IGraphForm;
begin
  result := TGraphForm.Create(nil);
  result.GetForm.AddHandlerOnBeforeDestruction(@BeforeFormDestructions);
  FList.Add(Result);
end;

procedure TGraphFormFactory.CloseAllOpenForms();
var
  i: Integer;
  Form: TCustomForm;
begin
  for i := 0 to FList.Count - 1 do
    begin
      Form := FList[i].GetForm;
      Form.RemoveHandlerOnBeforeDestruction(@BeforeFormDestructions);
      Form.Close;
    end;
  FList.Clear;
end;

procedure TGraphFormFactory.CascadeWindows(LeadForm: TCustomForm);
var
  Runner, i: Integer;
  CurrentForm: TCustomForm;
begin
  Runner := 1;
  for i := 0 to FList.Count -1 do
    begin
      CurrentForm := FList[i].GetForm;
      if (CurrentForm = LeadForm) then continue;

      CurrentForm.top := LeadForm.Top + (30 * Runner);
      CurrentForm.Left := LeadForm.Left + (30 * Runner);

      CurrentForm.BringToFront;
      Inc(Runner);
    end;
end;

end.

