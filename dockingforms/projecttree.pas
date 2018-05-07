unit projecttree;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, epicustombase,
  epiv_projecttreeview_frame, executor, ast;

type

  { TProjectTreeForm2 }

  TProjectTreeForm2 = class(TForm)
    procedure FormCreate(Sender: TObject);
  private
    FExecutor: TExecutor;
    FProjectTree: TEpiVProjectTreeViewFrame;
    procedure AfterStatement(Statement: TCustomStatement);
    procedure ProjectTreeHint(Sender: TObject; const AObject: TEpiCustomBase;
      ObjectType: TEpiVTreeNodeObjectType; var HintText: string);
    procedure ProjectTreeDoubleClick(Sender: TObject;
      const AObject: TEpiCustomBase; ObjectType: TEpiVTreeNodeObjectType);
    procedure ProjectGetText(Sender: TObject; const AObject: TEpiCustomBase;
      ObjectType: TEpiVTreeNodeObjectType; const StaticText: boolean;
      var NodeText: string);
  public
    constructor Create(TheOwner: TComponent; AExecutor: TExecutor);
    procedure UpdateTree;
  end;

var
  ProjectTreeForm2: TProjectTreeForm2;

implementation

{$R *.lfm}

uses
  epidatafilerelations, ast_types;

{ TProjectTreeForm2 }

procedure TProjectTreeForm2.FormCreate(Sender: TObject);
begin
  FProjectTree := TEpiVProjectTreeViewFrame.Create(Self);
  FProjectTree.Visible := true;
  FProjectTree.Parent := Self;
  FProjectTree.Align := alClient;
  FProjectTree.ShowProtected := true;

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

procedure TProjectTreeForm2.ProjectTreeHint(Sender: TObject;
  const AObject: TEpiCustomBase; ObjectType: TEpiVTreeNodeObjectType;
  var HintText: string);
begin
  if (ObjectType <> otRelation) then
    Exit;

  HintText := TEpiMasterRelation(AObject).Datafile.Name;
end;

procedure TProjectTreeForm2.AfterStatement(Statement: TCustomStatement);
begin
  if (Statement.StatementType = stRead) and
     (Statement.ExecResult = csrSuccess)
  then
    FProjectTree.AddDocument(FExecutor.Document);
end;

procedure TProjectTreeForm2.ProjectTreeDoubleClick(Sender: TObject;
  const AObject: TEpiCustomBase; ObjectType: TEpiVTreeNodeObjectType);
begin
  if (ObjectType <> otRelation) then
    Exit;

//  FCmdEdit.Text := FCmdEdit.Text + ' ' + TEpiMasterRelation(AObject).Datafile.Name;
end;

procedure TProjectTreeForm2.ProjectGetText(Sender: TObject; const AObject: TEpiCustomBase;
  ObjectType: TEpiVTreeNodeObjectType; const StaticText: boolean;
  var NodeText: string);
begin
  if (ObjectType <> otRelation) then exit;
  if (not StaticText) then exit;

  if (TEpiMasterRelation(AObject).Datafile = FExecutor.DataFile) then
    NodeText := NodeText + '*';
end;

constructor TProjectTreeForm2.Create(TheOwner: TComponent; AExecutor: TExecutor);
begin
  inherited Create(TheOwner);
  FExecutor := AExecutor;
  FExecutor.AddOnAfterStatementHandler(@AfterStatement);
end;

procedure TProjectTreeForm2.UpdateTree;
begin
  FProjectTree.UpdateTree;
end;

end.

