unit auto_position_form;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms;

type

  { TCustomAutoPositionForm }

  TCustomAutoPositionForm = class(TCustomForm)
  private
    FFirstShow: boolean;
  protected
    procedure DoShow; override;
    procedure DoDestroy; override;
    procedure DoClose(var CloseAction: TCloseAction); override;
  public
    constructor Create(AOwner: TComponent); override;
    class procedure RestoreDefaultPos(AForm: TCustomForm);
  end;

implementation

uses
  ana_procs;

{ TCustomAutoPositionForm }

procedure TCustomAutoPositionForm.DoShow;
begin
  inherited DoShow;

  FFirstShow := true;
  LoadFormPosition(Self, ClassName);
end;

procedure TCustomAutoPositionForm.DoDestroy;
begin
  // If the form has not been shown but only created, then all boundries will
  // have default values.
  if (FFirstShow) then
    SaveFormPosition(Self, ClassName);

  inherited DoDestroy;
end;

procedure TCustomAutoPositionForm.DoClose(var CloseAction: TCloseAction);
begin
  SaveFormPosition(Self, ClassName);

  inherited DoClose(CloseAction);
end;

constructor TCustomAutoPositionForm.Create(AOwner: TComponent);
begin
  inherited CreateNew(AOwner);

  FFirstShow := false;
  Position := poOwnerFormCenter;
end;

class procedure TCustomAutoPositionForm.RestoreDefaultPos(AForm: TCustomForm);
var
  W, H: integer;
begin
  DeleteFormPosition(ClassName);

  if (AForm is TCustomAutoPositionForm) then
    begin
      H := AForm.Monitor.Height div 2;
      W := AForm.Monitor.Width div 2;
      AForm.SetBounds(AForm.Left, AForm.Top, W, H);
      TCustomAutoPositionForm(AForm).MoveToDefaultPosition;
    end;
end;

end.

