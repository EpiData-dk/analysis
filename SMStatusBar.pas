unit SMStatusBar;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ComCtrls,CommCtrl;

type
  TSMStatusBar = class(TStatusBar)
  private
    fMousePanel : TStatusPanel;
    function findPanelatPos(X, Y: Integer): TStatusPanel;
  protected
   procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
  public
    constructor Create(AOwner: TComponent); override;
    property MousePanel : TStatusPanel read fMousePanel;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('EpiData', [TSMStatusBar]);
end;

{ TSMStatusBar }

constructor TSMStatusBar.Create(AOwner: TComponent);
begin
  inherited;
  ControlStyle := ControlStyle + [csAcceptsControls];
end;



function  TSMStatusBar.findPanelatPos( X, Y: Integer):TStatusPanel;
var
 i : integer;
 PanelRect : Trect;
 pt: Tpoint;
begin
 Result:=nil;
 pt.x:=x;
 pt.y:=y;
 for i:=0 to panels.count-1 do
 begin
   if (sendMessage(HAndle, SB_GETRECT,i, integer(@PanelRect))<> 0) and
      ptInRect(PanelRect, Pt) then
      begin
           Result:= panels[i];
           break;
      end;
 end;
end;

procedure TSMStatusBar.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
    FmousePanel:=findPanelatPos(X,Y);
    inherited;
end;

end.
