unit freq_variable_view;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, stat_dialog_custom_view, stat_dialog_contribution, ExtCtrls, freq_variable_model,
  Controls, epidatafiles, VirtualTrees;

type

  { TFreqVariableView }

  TFreqVariableView = class(TCustomStatDialogView)
  private
    FFirstView: boolean;
    FDataModel: TFreqVariableModel;
    FFields: TEpiFields;
    FVarnamesList: TVirtualStringTree;
    FOnModified: IStatDiaglogViewModified;
    procedure VarnamesHeaderClick(Sender: TVTHeader; HitInfo: TVTHeaderHitInfo);
    procedure VarnameListChecked(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure VarnamesListGetImageIndex(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
      var Ghosted: Boolean; var ImageIndex: Integer);
    procedure VarnamesListGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; TextType: TVSTTextType; var CellText: String);
    procedure VarnamesListInitNode(Sender: TBaseVirtualTree; ParentNode,
      Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
  private
    procedure UpdateTreeView();
    procedure UpdateSelectAllCheckbox();
    procedure CheckAllFields(Value: boolean);
  public
    constructor Create(TheOwner: TComponent); override;
    procedure EnterView(); override;
    function ExitView(): boolean; override;
    function GetViewCaption(): UTF8String; override;
    function IsDefined(): boolean; override;
    procedure ResetView(); override;
    procedure SetModel(DataModel: TFreqVariableModel);
  end;

implementation

uses
  epiv_datamodule, StrUtils, epifields_helper, epimiscutils;

{ TFreqVariableView }

procedure TFreqVariableView.UpdateTreeView();
begin
  FFields := FDataModel.GetListFields();

  FVarnamesList.RootNodeCount := FFields.Count;
  FVarnamesList.ReinitNode(FVarnamesList.RootNode, true);
  FVarnamesList.InvalidateChildren(nil, true);
  FVarnamesList.Header.Columns[0].CheckState := csUncheckedNormal;

  // Fails during create because this view has no parent yet and
  // the TreeView requires a handle when autofitting.
  if (HandleAllocated) then
    FVarnamesList.Header.AutoFitColumns(false);
end;

procedure TFreqVariableView.UpdateSelectAllCheckbox();
var
  CheckState: TCheckState;
begin
  case FDataModel.GetSelectState of
    vssNone:
      CheckState := csUncheckedNormal;
    vssMultiple:
      CheckState := csMixedNormal;
    vssAll:
      CheckState := csCheckedNormal;
  end;

  FVarnamesList.Header.Columns[0].CheckState := CheckState;
end;

procedure TFreqVariableView.CheckAllFields(Value: boolean);
var
  Node: PVirtualNode;
  CheckState: TCheckState;
begin
  if Value then
    CheckState := csCheckedNormal
  else
    CheckState := csUncheckedNormal;

  for Node in FVarnamesList.Nodes() do
    begin
      Node^.CheckState := CheckState;
      FDataModel.SelectVariable[Node^.Index] := value;
    end;

  FVarnamesList.Invalidate;
end;

procedure TFreqVariableView.VarnamesHeaderClick(Sender: TVTHeader;
  HitInfo: TVTHeaderHitInfo);
begin
  if (HitInfo.Column <> 0) then exit;
  if (not (hhiOnCheckbox in HitInfo.HitPosition)) then exit;

  // When this method is called, the internal state of the column has
  // already changed, so change the list of fields accordingly.
  case Sender.Columns[0].CheckState of
    csUncheckedNormal,
    csUncheckedPressed:
      CheckAllFields(false);
    csCheckedNormal,
    csCheckedPressed:
      CheckAllFields(true);
  end;

  DoModified();
end;

procedure TFreqVariableView.VarnameListChecked(Sender: TBaseVirtualTree;
  Node: PVirtualNode);
begin
  case (Node^.CheckState) of
    csUncheckedNormal,
    csUncheckedPressed:
      FDataModel.SelectVariable[Node^.Index] := false;

    csCheckedNormal,
    csCheckedPressed:
      FDataModel.SelectVariable[Node^.Index] := true;
  end;

  UpdateSelectAllCheckbox();
  DoModified();
end;

procedure TFreqVariableView.VarnamesListGetText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: String);
var
  F: TEpiField;
begin
  F := FFields[Node^.Index];

  if (not Assigned(F.DataFile)) then
    Exit;

  case Column of
    1: CellText := F.Name + IfThen(F.IsKeyfield, '*', '');
    2: CellText := EpiTypeNamesShort[F.FieldType];
    3: CellText := F.Question.Text;
  end;
end;

procedure TFreqVariableView.VarnamesListInitNode(Sender: TBaseVirtualTree;
  ParentNode, Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
begin
  Node^.CheckType := ctCheckBox;
  Node^.CheckState := csUncheckedNormal;
  FDataModel.SelectVariable[Node^.Index] := false;
end;

procedure TFreqVariableView.VarnamesListGetImageIndex(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
  var Ghosted: Boolean; var ImageIndex: Integer);
var
  F: TEpiField;
begin
  if (Column <> 2) then exit;

  Ghosted := false;

  F := FFields[Node^.Index];
  ImageIndex := DM.GetImageIndex(F.FieldType);
end;

constructor TFreqVariableView.Create(TheOwner: TComponent);
var
  Column: TVirtualTreeColumn;
const
  CHECKBOX_WIDTH = 40;
begin
  inherited Create(TheOwner);

  FFirstView := false;
  Caption := 'Variables';

  FVarnamesList := TVirtualStringTree.Create(Self);
  with FVarnamesList do
    begin
      Parent := Self;
      Align  := alClient;

      // Checkbox
      Column := Header.Columns.Add;
      with Column do
        begin
          MinWidth := CHECKBOX_WIDTH;
          Width := CHECKBOX_WIDTH;
          MaxWidth := CHECKBOX_WIDTH;
          Options := [coAllowClick, coEnabled, coParentBidiMode, coParentColor, coVisible, coAllowFocus];
          CheckBox := true;
          CheckType := ctTriStateCheckBox;
          Position := 0;
          Text := ''
        end;

      // Name
      Column := Header.Columns.Add;
      with Column do
        begin
          MinWidth := 10;
          Options := [coAllowClick, coEnabled, coParentBidiMode, coParentColor, coResizable, coShowDropMark, coVisible, coAutoSpring, coAllowFocus, coEditable];
          Position := 1;
          Text := 'Name';
        end;

      // Type
      Column := Header.Columns.Add;
      with Column do
        begin
          MinWidth := 10;
          Options := [coAllowClick, coDraggable, coEnabled, coParentBidiMode, coParentColor, coResizable, coShowDropMark, coVisible, coAutoSpring, coAllowFocus, coEditable];
          Position := 2;
          Text := 'Type';
        end;

      // Label
      Column := Header.Columns.Add;
      with Column do
        begin
          Options := [coAllowClick, coDraggable, coEnabled, coParentBidiMode, coParentColor, coResizable, coShowDropMark, coVisible, coAutoSpring, coAllowFocus, coEditable];
          Position := 3;
          Text := 'Label';
        end;

      Header.AutoSizeIndex := 3;
      Header.Options := [hoAutoResize, hoColumnResize, hoDblClickResize, hoDrag, hoShowImages, hoShowSortGlyphs, hoVisible, hoAutoSpring];
      TreeOptions.MiscOptions := [toFullRepaintOnResize, toGridExtensions, toInitOnSave, toToggleOnDblClick, toWheelPanning, toCheckSupport];
      TreeOptions.PaintOptions := [toShowButtons, toShowDropmark, toShowVertGridLines, toThemeAware, toUseBlendedImages, toUseBlendedSelection];
      TreeOptions.SelectionOptions := [toFullRowSelect, toMultiSelect];

      Images := DM.Icons16;

      OnHeaderClick := @VarnamesHeaderClick;
      OnGetText := @VarnamesListGetText;
      OnGetImageIndex := @VarnamesListGetImageIndex;
      OnInitNode := @VarnamesListInitNode;
      OnChecked := @VarnameListChecked;
    end;
end;

procedure TFreqVariableView.EnterView();
begin
  if (not FFirstView) then
    begin
      UpdateTreeView();
      FFirstView := true;
    end;
end;

function TFreqVariableView.ExitView(): boolean;
begin
  result := true;
end;

function TFreqVariableView.GetViewCaption(): UTF8String;
begin
  result := 'Variables';
end;

function TFreqVariableView.IsDefined(): boolean;
begin
  result := FDataModel.IsDefined();
end;

procedure TFreqVariableView.ResetView();
begin
  UpdateTreeView();
  DoModified();
end;

procedure TFreqVariableView.SetModel(DataModel: TFreqVariableModel);
begin
  FDataModel := DataModel;
  UpdateTreeView();
end;

end.

