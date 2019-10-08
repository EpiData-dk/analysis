unit varnames_form;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, VirtualTrees, epidatafiles;

type

  TVariablesFormGetFieldList = function (Sender: TObject): TEpiFields of object;
  TVariablesFormLineAction = procedure (Sender: TObject; Const LineText: UTF8String; ChangeFocus: boolean) of object;

  { TVariablesForm }

  TVariablesForm = class(TForm)
    VarnamesList: TVirtualStringTree;
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    FOnGetFieldList: TVariablesFormGetFieldList;
    function GetFieldList: TEpiFields;
    procedure FieldListMissingError;
    procedure DoLineAction(Const LineText: UTF8String; ChangeFocus: boolean);
  private
    FOnLineAction: TVariablesFormLineAction;
    procedure VarnamesListGetImageIndex(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
      var Ghosted: Boolean; var ImageIndex: Integer);
    procedure VarnamesListGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; TextType: TVSTTextType; var CellText: String);
    procedure VarnamesListKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure VarnamesListNodeDblClick(Sender: TBaseVirtualTree;
      const HitInfo: THitInfo);
    procedure VarnamesListAfterGetMaxColumnWidth(Sender: TVTHeader;
      Column: TColumnIndex; var MaxWidth: Integer);
  protected
    property FieldList: TEpiFields read GetFieldList;
  public
    constructor Create(TheOwner: TComponent);
    procedure UpdateVarNames;
    property OnGetFieldList: TVariablesFormGetFieldList read FOnGetFieldList write FOnGetFieldList;
    property OnLineAction: TVariablesFormLineAction read FOnLineAction write FOnLineAction ;
  end;

var
  VariablesForm: TVariablesForm;

implementation

{$R *.lfm}

uses
  epiv_datamodule, LCLType, strutils, epimiscutils, epifields_helper, ana_procs;

{ TVariablesForm }

procedure TVariablesForm.FormShow(Sender: TObject);
begin
  LoadFormPosition(Self, Self.Name);
end;

procedure TVariablesForm.FormClose(Sender: TObject;
  var CloseAction: TCloseAction);
begin
  SaveFormPosition(Self, Self.Name);
end;

procedure TVariablesForm.FormDestroy(Sender: TObject);
begin
  SaveFormPosition(Self, Self.Name);
end;

function TVariablesForm.GetFieldList: TEpiFields;
begin
  if Assigned(OnGetFieldList) then
    result := OnGetFieldList(Self)
  else

end;

procedure TVariablesForm.FieldListMissingError;
begin
  raise Exception.Create('OnGetFieldList not assigned!');
end;

procedure TVariablesForm.DoLineAction(const LineText: UTF8String;
  ChangeFocus: boolean);
begin
  if Assigned(FOnLineAction) then
    FOnLineAction(Self, LineText, ChangeFocus);
end;

procedure TVariablesForm.VarnamesListGetImageIndex(
  Sender: TBaseVirtualTree; Node: PVirtualNode; Kind: TVTImageKind;
  Column: TColumnIndex; var Ghosted: Boolean; var ImageIndex: Integer);
var
  F: TEpiField;
begin
  if Column <> 1 then exit;
  Ghosted := false;

  F := FieldList[Node^.Index];
  ImageIndex := DM.GetImageIndex(F.FieldType);
end;

procedure TVariablesForm.VarnamesListGetText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: String);
var
  F: TEpiField;
begin
  F := FieldList[Node^.Index];

  case Column of
    0: CellText := F.Name + IfThen(F.IsKeyfield, '*', '');
    1: CellText := EpiTypeNamesShort[F.FieldType];
    2: CellText := F.Question.Text;
  end;
end;

procedure TVariablesForm.VarnamesListKeyDown(Sender: TObject;
  var Key: Word; Shift: TShiftState);
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
      F := FieldList[Node^.Index];
      S := S + ' ' + F.Name;
    end;

  DoLineAction(S, True);
end;

procedure TVariablesForm.VarnamesListNodeDblClick(Sender: TBaseVirtualTree;
  const HitInfo: THitInfo);
var
  F: TEpiField;
begin
  F := FieldList[HitInfo.HitNode^.Index];
  DoLineAction(F.Name, False);
end;

procedure TVariablesForm.VarnamesListAfterGetMaxColumnWidth(
  Sender: TVTHeader; Column: TColumnIndex; var MaxWidth: Integer);
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

constructor TVariablesForm.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);

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

procedure TVariablesForm.UpdateVarNames;
begin
  if (not Assigned(FieldList)) then
    VarnamesList.RootNodeCount := 0
  else
    VarnamesList.RootNodeCount := FieldList.Count;
end;

end.

