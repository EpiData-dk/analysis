unit commandtree_form;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, auto_position_form, commandtree;

type

  TCommandTreeFormLineAction = procedure (Sender: TObject; Const LineText: UTF8String; ChangeFocus: boolean) of object;

  { TCommandTreeForm }

  TCommandTreeForm = class(TCustomAutoPositionForm)
  private
    FCommandTree: TCommandTree;
    FOnLineAction: TCommandTreeFormLineAction;
    procedure CommandTreeCommandDoubleClick(const CommandString: UTF8String);
    procedure CommandTreePressEnterKey(const CommandString: UTF8String);
  protected
    procedure DoLineAction(Const LineText: UTF8String; ChangeFocus: Boolean);
  public
     constructor Create(TheOwner: TComponent); override;
     property OnLineAction: TCommandTreeFormLineAction read FOnLineAction write FOnLineAction;
  end;

implementation

uses
  ana_procs;

{ TCommandTreeForm }

procedure TCommandTreeForm.CommandTreeCommandDoubleClick(
  const CommandString: UTF8String);
begin
  DoLineAction(CommandString, True);
end;

procedure TCommandTreeForm.CommandTreePressEnterKey(
  const CommandString: UTF8String);
begin
  DoLineAction(CommandString, false);
end;

procedure TCommandTreeForm.DoLineAction(const LineText: UTF8String;
  ChangeFocus: Boolean);
begin
  if (Assigned(OnLineAction)) then
    OnLineAction(Self, LineText, ChangeFocus);
end;

constructor TCommandTreeForm.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);

  Caption := 'Commands';

  FCommandTree := TCommandTree.Create(Self);
  FCommandTree.Visible := true;
  FCommandTree.Parent := self;
  FCommandTree.Align := alClient;
  FCommandTree.Tag := 1;
  FCommandTree.OnCommandDoubleClick := @CommandTreeCommandDoubleClick;
  FCommandTree.OnCommandPressEnterKey := @CommandTreePressEnterKey;
end;

end.

