unit varnames;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, VirtualTrees,
  epidatafiles, executor;

type

  { TVarNamesForm }

  TVarNamesForm = class(TForm)
    VarnamesList: TVirtualStringTree;
    procedure FormCreate(Sender: TObject);
  private
    FExecutor: TExecutor;
    procedure VarnamesListAfterGetMaxColumnWidth(Sender: TVTHeader;
      Column: TColumnIndex; var MaxWidth: Integer);
    procedure VarnamesListGetImageIndex(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
      var Ghosted: Boolean; var ImageIndex: Integer);
    procedure VarnamesListGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; TextType: TVSTTextType; var CellText: String);
    procedure VarnamesListKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure VarnamesListNodeDblClick(Sender: TBaseVirtualTree;
      const HitInfo: THitInfo);
  public
     constructor Create(TheOwner: TComponent; AExecutor: TExecutor);
     procedure UpdateVarnames;
     property Executor: TExecutor read FExecutor;
  end;

var
  VarNamesForm: TVarNamesForm;

implementation

{$R *.lfm}

uses
  epiv_datamodule, strutils, epimiscutils, LCLType, epifields_helper;

{ TVarNamesForm }

procedure TVarNamesForm.FormCreate(Sender: TObject);
begin
  with VarnamesList do
  begin
    Images := DM.Icons16;

    OnAfterGetMaxColumnWidth := @VarnamesListAfterGetMaxColumnWidth;
    OnGetText                := @VarnamesListGetText;
    OnKeyDown                := @VarnamesListKeyDown;
    OnNodeDblClick           := @VarnamesListNodeDblClick;
    OnGetImageIndex          := @VarnamesListGetImageIndex;
  end;
end;

procedure TVarNamesForm.VarnamesListAfterGetMaxColumnWidth(Sender: TVTHeader;
  Column: TColumnIndex; var MaxWidth: Integer);
var
  W: Integer;
begin
  W := VarnamesList.Canvas.GetTextWidth(Sender.Columns[Column].Text) +
       // apparently Margin and TextMargin are cummulative...
       (VarnamesList.TextMargin * 2) +
       (VarnamesList.Margin * 2);

  if (W > MaxWidth) then
    MaxWidth := W;
end;

procedure TVarNamesForm.VarnamesListGetImageIndex(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
  var Ghosted: Boolean; var ImageIndex: Integer);
var
  F: TEpiField;
begin
  if Column <> 1 then exit;
  Ghosted := false;

  F := Executor.SortedFields[Node^.Index];
  ImageIndex := DM.GetImageIndex(F.FieldType);
end;

procedure TVarNamesForm.VarnamesListGetText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: String);
var
  F: TEpiField;
begin
  F := Executor.SortedFields[Node^.Index];

  case Column of
    0: CellText := F.Name + IfThen(F.IsKeyfield, '*', '');
    1: CellText := EpiTypeNamesShort[F.FieldType];
    2: CellText := F.Question.Text;
  end;
end;

procedure TVarNamesForm.VarnamesListKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
var
  Nodes: TVTVirtualNodeEnumeration;
  Node: PVirtualNode;
  S: UTF8String;
  F: TEpiField;
begin
  if (Key <> VK_RETURN) then
    Exit;

  S := '';
  for Node in VarnamesList.SelectedNodes() do
    begin
      F := Executor.SortedFields[Node^.Index];
      S := S + ' ' + F.Name;
    end;

 {  FCmdEdit.Text := FCmdEdit.Text + S;
  if FCmdEdit.CanFocus then
    FCmdEdit.SetFocus;    }
end;

procedure TVarNamesForm.VarnamesListNodeDblClick(Sender: TBaseVirtualTree;
  const HitInfo: THitInfo);
var
  F: TEpiField;
begin
  F := Executor.SortedFields[HitInfo.HitNode^.Index];
//  FCmdEdit.Text := FCmdEdit.Text + ' ' + F.Name;
end;

constructor TVarNamesForm.Create(TheOwner: TComponent; AExecutor: TExecutor);
begin
  inherited Create(TheOwner);
  FExecutor := AExecutor;
end;

procedure TVarNamesForm.UpdateVarnames;
begin
  if (not Assigned(Executor.DataFile)) then
    VarnamesList.RootNodeCount := 0
  else
    VarnamesList.RootNodeCount := Executor.SortedFields.Count;

  VarnamesList.InvalidateChildren(nil, true);
  VarnamesList.Header.AutoFitColumns(false);
end;

end.

