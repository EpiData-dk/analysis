unit stat_dialog_custom_view;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ExtCtrls, stat_dialog_contribution, Controls;

type

  { TCustomStatDialogView }

  TCustomStatDialogView = class(TPanel, IStatDialogView)
  private
    FOnModifiedListeners: TList;
  protected
    procedure DoModified();
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure EnterView(); virtual; abstract;
    function ExitView(): boolean; virtual; abstract;
    function GetControl(): TControl; virtual;
    function GetViewCaption(): UTF8String; virtual; abstract;
    function IsDefined(): boolean; virtual; abstract;
    procedure ResetView(); virtual; abstract;
    procedure AddOnModified(OnModified: IStatDiaglogViewModified); virtual;
    procedure RemoveOnModified(OnModified: IStatDiaglogViewModified); virtual;
  end;

implementation

{ TCustomStatDialogView }

procedure TCustomStatDialogView.DoModified();
var
  Item: Pointer;
begin
  for Item in FOnModifiedListeners do
    IStatDiaglogViewModified(Item).OnViewModified(Self);
end;

constructor TCustomStatDialogView.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);

  FOnModifiedListeners := TList.Create;
end;

destructor TCustomStatDialogView.Destroy;
begin
  FOnModifiedListeners.Free;
  inherited Destroy;
end;

function TCustomStatDialogView.GetControl(): TControl;
begin
  result := self;
end;

procedure TCustomStatDialogView.AddOnModified(
  OnModified: IStatDiaglogViewModified);
begin
  FOnModifiedListeners.Add(OnModified);
end;

procedure TCustomStatDialogView.RemoveOnModified(
  OnModified: IStatDiaglogViewModified);
begin
  FOnModifiedListeners.Remove(OnModified);
end;

end.

