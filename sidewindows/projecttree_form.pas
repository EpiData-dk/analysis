unit projecttree_form;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, epiv_projecttreeview_frame,
  epicustombase, executor, epidocument, auto_position_form;

type

  TProjectTreeFormLineAction = procedure (Sender: TObject; Const LineText: UTF8String) of object;

  { TProjectTreeForm }

  TProjectTreeForm = class(TCustomAutoPositionForm)
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    FExecutor: TExecutor;
    FOnLineAction: TProjectTreeFormLineAction;
    FProjectTree: TEpiVProjectTreeViewFrame;
    procedure ProjectGetText(Sender: TObject; const AObject: TEpiCustomBase;
      ObjectType: TEpiVTreeNodeObjectType; const StaticText: boolean;
      var NodeText: string);
    procedure ProjectTreeDoubleClick(Sender: TObject;
      const AObject: TEpiCustomBase; ObjectType: TEpiVTreeNodeObjectType);
    procedure ProjectTreeHint(Sender: TObject; const AObject: TEpiCustomBase;
      ObjectType: TEpiVTreeNodeObjectType; var HintText: string);
  protected
    procedure DoOnLineAction(Const LineText: UTF8String);
  public
    constructor Create(TheOwner: TComponent; Executor: TExecutor);
    procedure AddDocument(EpiDocument: TEpiDocument);
    procedure UpdateProjectTree;
    property OnLineAction: TProjectTreeFormLineAction read FOnLineAction write FOnLineAction;
  end;

implementation

uses
  epidatafilerelations, ana_procs;

{ TProjectTreeForm }

procedure TProjectTreeForm.ProjectTreeHint(Sender: TObject;
  const AObject: TEpiCustomBase; ObjectType: TEpiVTreeNodeObjectType;
  var HintText: string);
begin
  if (ObjectType <> otRelation) then
    Exit;

  HintText := TEpiMasterRelation(AObject).Datafile.Name;
end;

procedure TProjectTreeForm.DoOnLineAction(const LineText: UTF8String);
begin
  if (Assigned(OnLineAction)) then
    OnLineAction(Self, LineText);
end;

procedure TProjectTreeForm.ProjectTreeDoubleClick(Sender: TObject;
  const AObject: TEpiCustomBase; ObjectType: TEpiVTreeNodeObjectType);
begin
  if (ObjectType <> otRelation) then
    Exit;

  DoOnLineAction(TEpiMasterRelation(AObject).Datafile.Name);
end;

procedure TProjectTreeForm.FormClose(Sender: TObject;
  var CloseAction: TCloseAction);
begin
  SaveFormPosition(Self, Self.Name);
end;

procedure TProjectTreeForm.FormDestroy(Sender: TObject);
begin
  SaveFormPosition(Self, Self.Name);
end;

procedure TProjectTreeForm.FormShow(Sender: TObject);
begin
  LoadFormPosition(Self, Self.Name);
end;

procedure TProjectTreeForm.ProjectGetText(Sender: TObject;
  const AObject: TEpiCustomBase; ObjectType: TEpiVTreeNodeObjectType;
  const StaticText: boolean; var NodeText: string);
begin
  if (ObjectType <> otRelation) then exit;
  if (not StaticText) then exit;

  if (TEpiMasterRelation(AObject).Datafile = FExecutor.DataFile) then
    NodeText := NodeText + '*';
end;

constructor TProjectTreeForm.Create(TheOwner: TComponent; Executor: TExecutor);
begin
  inherited Create(TheOwner);

  Caption := 'Project Tree';

  FExecutor := Executor;

  FProjectTree := TEpiVProjectTreeViewFrame.Create(Self);
  FProjectTree.Visible := true;
  FProjectTree.Parent := self;
  FProjectTree.Align := alClient;
  FProjectTree.ShowProtected := true;
  FProjectTree.Tag := 0;
  FProjectTree.AllowSelectProject := false;
  FProjectTree.EditCaption := false;
  FProjectTree.EditStructure := false;
  FProjectTree.ShowCheckBoxes := false;
  FProjectTree.ShowRecordCount := true;
  FProjectTree.MinDocumentCount := 1;
  FProjectTree.MaxDocumentCount := 1;
  FProjectTree.ShowHint := true;
  FProjectTree.OnGetHint := @ProjectTreeHint;
  FProjectTree.OnTreeNodeDoubleClick := @ProjectTreeDoubleClick;
  FProjectTree.OnGetText := @ProjectGetText;
end;

procedure TProjectTreeForm.AddDocument(EpiDocument: TEpiDocument);
begin
  FProjectTree.AddDocument(EpiDocument);
end;

procedure TProjectTreeForm.UpdateProjectTree;
begin
  FProjectTree.UpdateTree;
end;

end.

