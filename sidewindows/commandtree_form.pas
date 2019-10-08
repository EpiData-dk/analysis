unit commandtree_form;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, commandtree;

type

  TCommandTreeFormLineAction = procedure (Sender: TObject; Const LineText: UTF8String; ChangeFocus: boolean) of object;

  { TCommandTreeForm }

  TCommandTreeForm = class(TForm)
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
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

var
  CommandTreeForm: TCommandTreeForm;

implementation

{$R *.lfm}

uses
  ana_procs;

{ TCommandTreeForm }

procedure TCommandTreeForm.FormClose(Sender: TObject;
  var CloseAction: TCloseAction);
begin
  SaveFormPosition(Self, Self.Name);
end;

procedure TCommandTreeForm.FormDestroy(Sender: TObject);
begin
   SaveFormPosition(Self, Self.Name);
end;

procedure TCommandTreeForm.FormShow(Sender: TObject);
begin
  LoadFormPosition(Self, Self.Name);
end;

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

  FCommandTree := TCommandTree.Create(Self);
  FCommandTree.Visible := true;
  FCommandTree.Parent := self;
  FCommandTree.Align := alClient;
  FCommandTree.Tag := 1;
  FCommandTree.OnCommandDoubleClick := @CommandTreeCommandDoubleClick;
  FCommandTree.OnCommandPressEnterKey := @CommandTreePressEnterKey;

end;

end.

