unit analysis_statusbar;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, epiv_custom_statusbar, executor;

type

  { TAnalysisStatusbar }

  TAnalysisStatusbar = class(TEpiVCustomStatusBar)
  private
    FExecutor: TExecutor;
  protected
    procedure AddItem(AStatusBarItem: TEpiVCustomStatusBarItem); override;
  public
    constructor Create(TheOwner: TComponent; AExecutor: TExecutor);
    property Executor: TExecutor read FExecutor;
  end;

  { TAnalysisStatusbarItem }

  TAnalysisStatusbarItem = class(TEpiVCustomStatusBarItem)
  private
    function GetExecutor: TExecutor;
    function GetStatusbar: TAnalysisStatusbar;
  public
    property Statusbar: TAnalysisStatusbar read GetStatusbar;
    property Executor: TExecutor read GetExecutor;
  end;

implementation

uses
  workingdir_statusbar_item,
  epiv_statusbar_item_currentuser, epiv_statusbar_item_cycleno,
  epiv_statusbar_item_savetime, selected_count_statusbar_item,
  executing_statusbar_item, epiv_statusbar_item_progressbar,
  epiv_statusbar_item_saveicon;

{ TAnalysisStatusbarItem }

function TAnalysisStatusbarItem.GetExecutor: TExecutor;
begin
  if Assigned(Statusbar) then
    Result := Statusbar.Executor
  else
    Result := nil;
end;

function TAnalysisStatusbarItem.GetStatusbar: TAnalysisStatusbar;
begin
  result := TAnalysisStatusbar(inherited Statusbar);
end;

{ TAnalysisStatusbar }

procedure TAnalysisStatusbar.AddItem(AStatusBarItem: TEpiVCustomStatusBarItem);
begin
  inherited AddItem(AStatusBarItem);
end;

constructor TAnalysisStatusbar.Create(TheOwner: TComponent; AExecutor: TExecutor
  );
begin
  inherited Create(TheOwner);

  FExecutor := AExecutor;

  AddItem(TWorkingDirStatusbarItem.Create(Self));
  AddItem(TExecutingStatusbarItem.Create(Self));
  AddItem(TEpiVStatusBarItem_CurrentUser.Create(Self));
  AddItem(TEpiVStatusBarItem_CycleNo.Create(Self));
  AddItem(TSelectedCountStatusbarItem.Create(Self));
  AddItem(TEpiVStatusBarItem_SaveTimer.Create(Self));
  AddItem(TEpiVStatusBarItem_ProgressBar.Create(self));
//  AddItem(TEpiVStatusBarItem_SavingIcon.Create(Self));
end;

end.

