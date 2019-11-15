unit selected_count_statusbar_item;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, analysis_statusbar, StdCtrls, epiv_custom_statusbar;

type

  { TSelectedCountStatusbarItem }

  TSelectedCountStatusbarItem = class(TAnalysisStatusbarItem)
  private
    FRecordsLabel: TLabel;
    procedure DoUpdate;
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
  Controls;

{ TSelectedCountStatusbarItem }

procedure TSelectedCountStatusbarItem.DoUpdate;
begin
  if Assigned(Executor) and
     (not (csDestroying in Panel.ComponentState))
  then
    FRecordsLabel.Caption := IntToStr(Executor.SelectVector.Size);
end;

procedure TSelectedCountStatusbarItem.Update(
  Condition: TEpiVCustomStatusbarUpdateCondition);
begin
  case Condition of
    sucDefault,
    sucDocFile,
    sucDataFile:
      DoUpdate;

    sucSelection: ;
    sucSave: ;
    sucExample: ;
  end;
end;

class function TSelectedCountStatusbarItem.Caption: string;
begin
  Result := 'Counts during select';
end;

class function TSelectedCountStatusbarItem.Name: string;
begin
  result := 'selectcount';
end;

constructor TSelectedCountStatusbarItem.Create(AStatusBar: TEpiVCustomStatusBar
  );
begin
  inherited Create(AStatusBar);

  FRecordsLabel := TLabel.Create(Panel);
  FRecordsLabel.AnchorHorizontalCenterTo(Panel);
  FRecordsLabel.AnchorVerticalCenterTo(Panel);
  FRecordsLabel.Caption := '';
  FRecordsLabel.Parent := Panel;
end;

function TSelectedCountStatusbarItem.GetPreferedWidth: Integer;
begin
  if not Panel.HandleAllocated then
    begin
      Result := inherited GetPreferedWidth;
      Exit;
    end;

  Result := FRecordsLabel.Width + 4;
end;

end.

