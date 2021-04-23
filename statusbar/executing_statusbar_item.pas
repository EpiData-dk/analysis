unit executing_statusbar_item;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, analysis_statusbar, StdCtrls, epiv_custom_statusbar, ExtCtrls;

type

  { TExecutingStatusbarItem }

  TExecutingStatusbarItem = class(TAnalysisStatusbarItem)
  private
    FExecutingShape: TShape;
  protected
    procedure Update(Condition: TEpiVCustomStatusbarUpdateCondition); override;
  public
    class function Caption: string; override;
    class function Name: string; override;
  public
    constructor Create(AStatusBar: TEpiVCustomStatusBar); override;
    function GetPreferedWidth: Integer; override;
  end;


implementation

uses
  Controls, Graphics;

{ TExecutingStatusbarItem }

procedure TExecutingStatusbarItem.Update(
  Condition: TEpiVCustomStatusbarUpdateCondition);
begin
  if Executor.Executing then
    FExecutingShape.Brush.Color := clRed
  else
    FExecutingShape.Brush.Color := clLime;
end;

class function TExecutingStatusbarItem.Caption: string;
begin
  Result := 'Running';
end;

class function TExecutingStatusbarItem.Name: string;
begin
  result := 'executing';
end;

constructor TExecutingStatusbarItem.Create(AStatusBar: TEpiVCustomStatusBar);
begin
  inherited Create(AStatusBar);

  FExecutingShape := TShape.Create(AStatusBar);
  with FExecutingShape do
  begin
    Shape := stCircle;
    Parent := Panel;
    Brush.Color := clLime;

    AnchorParallel(akLeft,   2, Panel);
    AnchorParallel(akTop,    0, Panel);
    AnchorParallel(akBottom, 0, Panel);
    AnchorParallel(akRight,  2, Panel);
  end;
end;

function TExecutingStatusbarItem.GetPreferedWidth: Integer;
begin
  if not Panel.HandleAllocated then
    begin
      Result := inherited GetPreferedWidth;
      Exit;
    end;

  Result := 24;
end;

end.

