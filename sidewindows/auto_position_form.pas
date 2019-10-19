unit auto_position_form;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms;

type

  { TCustomAutoPositionForm }

  TCustomAutoPositionForm = class(TCustomForm)
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

  LoadFormPosition(Self, ClassName);
end;

procedure TCustomAutoPositionForm.DoDestroy;
begin
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

  Position := poOwnerFormCenter;
end;

class procedure TCustomAutoPositionForm.RestoreDefaultPos(AForm: TCustomForm);
begin
  DeleteFormPosition(ClassName);

  if (AForm is TCustomAutoPositionForm) then
    TCustomAutoPositionForm(AForm).MoveToDefaultPosition;
end;

end.

