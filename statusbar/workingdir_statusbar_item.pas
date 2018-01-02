unit workingdir_statusbar_item;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, analysis_statusbar, StdCtrls, epiv_custom_statusbar;

type

  { TWorkingDirStatusbarItem }

  TWorkingDirStatusbarItem = class(TAnalysisStatusbarItem)
  private
    FWorkingdirLabel: TLabel;
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
  Controls, LazFileUtils;

{ TWorkingDirStatusbarItem }

procedure TWorkingDirStatusbarItem.Update(
  Condition: TEpiVCustomStatusbarUpdateCondition);
begin
  FWorkingdirLabel.Caption := GetCurrentDirUTF8;
end;

class function TWorkingDirStatusbarItem.Caption: string;
begin
  Result := 'Current Working Directory'
end;

class function TWorkingDirStatusbarItem.Name: string;
begin
  result := 'workingdir';
end;

constructor TWorkingDirStatusbarItem.Create(AStatusBar: TEpiVCustomStatusBar);
begin
  inherited Create(AStatusBar);

  FWorkingdirLabel := TLabel.Create(AStatusBar);
  FWorkingdirLabel.AnchorParallel(akLeft, 2, Panel);
  FWorkingdirLabel.AnchorVerticalCenterTo(Panel);
  FWorkingdirLabel.Caption := GetCurrentDirUTF8;
  FWorkingdirLabel.Parent := Panel;

  Resizable := true;
end;

function TWorkingDirStatusbarItem.GetPreferedWidth: Integer;
begin
  if not Panel.HandleAllocated then
    begin
      Result := inherited GetPreferedWidth;
      Exit;
    end;

  Result := FWorkingdirLabel.Width + 4;
end;

end.

