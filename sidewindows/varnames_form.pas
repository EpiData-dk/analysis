unit varnames_form;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, VirtualTrees, epidatafiles,
  auto_position_form, epicustombase;

type

  TVariablesFormGetFieldList = function (Sender: TObject): TEpiFields of object;
  TVariablesFormLineAction = procedure (Sender: TObject; Const LineText: UTF8String; ChangeFocus: boolean) of object;

  { TVariablesForm }

  TVariablesForm = class(TCustomAutoPositionForm)
  private
    FVarnamesList: TVirtualStringTree;
    FOnGetFieldList: TVariablesFormGetFieldList;
    procedure DataFileChangeEvent(const Sender: TEpiCustomBase;
      const Initiator: TEpiCustomBase; EventGroup: TEpiEventGroup;
      EventType: Word; Data: Pointer);
    procedure UpdateTree;
    procedure DoLineAction(Const LineText: UTF8String; ChangeFocus: boolean);
  private
    FDataFile: TEpiDataFile;
    FOnLineAction: TVariablesFormLineAction;
    function GetFieldList: TEpiFields;
    procedure SetDataFile(AValue: TEpiDataFile);
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
    property DataFile: TEpiDataFile read FDataFile write SetDataFile;
    property OnLineAction: TVariablesFormLineAction read FOnLineAction write FOnLineAction ;
  end;

implementation

uses
  epiv_datamodule, LCLType, strutils, epimiscutils, epifields_helper, ana_procs;

{ TVariablesForm }

procedure TVariablesForm.DataFileChangeEvent(const Sender: TEpiCustomBase;
  const Initiator: TEpiCustomBase; EventGroup: TEpiEventGroup; EventType: Word;
  Data: Pointer);


  function IsAddDelItem: boolean;
  begin
    result := (Initiator is TEpiFields) and
              (EventGroup = eegCustomBase) and
              (TEpiCustomChangeEventType(EventType) in [ecceAddItem, ecceDelItem]);
  end;

  function IsNameChange: boolean;
  begin
    result := (Initiator is TEpiField) and
              (EventGroup = eegCustomBase) and
              (TEpiCustomChangeEventType(EventType) = ecceName);
  end;

  function IsLabelChange: boolean;
  begin
    result := (Initiator is TEpiTranslatedText) and
              (Initiator.Owner is TEpiField) and
              (EventGroup = eegCustomBase) and
              (TEpiCustomChangeEventType(EventType) = ecceText);
  end;

begin
  if (Initiator = FDataFile) and
     (EventGroup = eegCustomBase) and
     (TEpiCustomChangeEventType(EventType) = ecceDestroy)
  then
    begin
      DataFile := nil;
      Exit;
    end;

  if (IsAddDelItem) or
     (IsNameChange) or
     (IsLabelChange)
  then
    UpdateTree;
end;

procedure TVariablesForm.UpdateTree;
begin
  if (not Assigned(FieldList)) then
    FVarnamesList.RootNodeCount := 0
  else
    FVarnamesList.RootNodeCount := FieldList.Count;

  FVarnamesList.InvalidateChildren(nil, true);
  FVarnamesList.Header.AutoFitColumns(false);
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

procedure TVariablesForm.SetDataFile(AValue: TEpiDataFile);
begin
  if FDataFile = AValue then Exit;

  if (Assigned(FDataFile)) then
    FDataFile.UnRegisterOnChangeHook(@DataFileChangeEvent);

  FDataFile := AValue;

  if (Assigned(FDataFile)) then
    FDataFile.RegisterOnChangeHook(@DataFileChangeEvent, true);

  UpdateTree;

  FVarnamesList.Header.AutoFitColumns(false);
end;

function TVariablesForm.GetFieldList: TEpiFields;
begin
  Result := nil;

  if Assigned(FDataFile) then
    Result := FDataFile.Fields;
end;

procedure TVariablesForm.VarnamesListGetText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: String);
var
  F: TEpiField;
begin
  F := FieldList[Node^.Index];

  if (not Assigned(F.DataFile)) then
    Exit;

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
  for Node in FVarnamesList.SelectedNodes() do
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
  W := FVarnamesList.Canvas.GetTextWidth(Sender.Columns[Column].Text) +
       // apparently Margin and TextMargin are cummulative...
       (FVarnamesList.TextMargin * 2) +
       (FVarnamesList.Margin * 2);

  if (W > MaxWidth) then
    MaxWidth := W;
end;

constructor TVariablesForm.Create(TheOwner: TComponent);
var
  Column: TVirtualTreeColumn;
begin
  inherited Create(TheOwner);

  Caption := 'Variables';

  FVarnamesList := TVirtualStringTree.Create(Self);
  with FVarnamesList do
    begin
      Parent := Self;
      Align  := alClient;

      // Name
      Column := Header.Columns.Add;
      with Column do
        begin
          MinWidth := 10;
          Options := [coAllowClick, coEnabled, coParentBidiMode, coParentColor, coResizable, coShowDropMark, coVisible, coAutoSpring, coAllowFocus, coEditable];
          Position := 0;
          Text := 'Name';
        end;

      // Type
      Column := Header.Columns.Add;
      with Column do
        begin
          MinWidth := 10;
          Options := [coAllowClick, coDraggable, coEnabled, coParentBidiMode, coParentColor, coResizable, coShowDropMark, coVisible, coAutoSpring, coAllowFocus, coEditable];
          Position := 1;
          Text := 'Type';
        end;

      // Label
      Column := Header.Columns.Add;
      with Column do
        begin
          Options := [coAllowClick, coDraggable, coEnabled, coParentBidiMode, coParentColor, coResizable, coShowDropMark, coVisible, coAutoSpring, coAllowFocus, coEditable];
          Position := 2;
          Text := 'Label';
        end;

      Header.AutoSizeIndex := 2;
      Header.Options := [hoAutoResize, hoColumnResize, hoDblClickResize, hoDrag, hoShowImages, hoShowSortGlyphs, hoVisible, hoAutoSpring];
      TreeOptions.MiscOptions := [toFullRepaintOnResize, toGridExtensions, toInitOnSave, toToggleOnDblClick, toWheelPanning];
      TreeOptions.PaintOptions := [toShowButtons, toShowDropmark, toShowVertGridLines, toThemeAware, toUseBlendedImages, toUseBlendedSelection];
      TreeOptions.SelectionOptions := [toFullRowSelect, toMultiSelect];

      Images := DM.Icons16;

      OnAfterGetMaxColumnWidth := @VarnamesListAfterGetMaxColumnWidth;
      OnGetText                := @VarnamesListGetText;
      OnKeyDown                := @VarnamesListKeyDown;
      OnNodeDblClick           := @VarnamesListNodeDblClick;
      OnGetImageIndex          := @VarnamesListGetImageIndex;
    end;
end;

end.

