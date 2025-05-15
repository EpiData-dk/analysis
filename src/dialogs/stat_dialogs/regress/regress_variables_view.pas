unit regress_variables_view;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, stat_dialog_custom_view, stat_dialog_contribution, regress_variables_model,
  StdCtrls, ExtCtrls, Controls, spin, fields_combobox, epidatafiles, laz.VirtualTrees;

type

  { TRegressVariableView }


  TRegressVariableView = class(TCustomStatDialogView)
  private
    FFirstView: boolean;
    FDataModel: TRegressVariableModel;
    FComboBoxes: Array of TEpiFieldsComboBox;
    FTypeRadios: TRadioGroup;
    FDegreeBox: TGroupBox;
    FDegreeLabel: TLabel;
    FDegreeEdit: TSpinEdit;
    FXFields: TEpiFields;
    FVarnamesList: TLazVirtualStringTree;
    FOnModified: IStatDiaglogViewModified;
    procedure VariableSelect(Sender: TObject);
    procedure UpdateCombos();
    procedure CreateTypeRadios(RadioGroup: TRadioGroup);
    procedure TypeSelectionChanged(Sender: TObject);
    procedure CreateDegreeEdit(Field: TSpinEdit);
    procedure DegreeChanged(Sender: TObject);
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
    procedure SetModel(DataModel: TRegressVariableModel);
  end;

implementation

uses
  epiv_datamodule, StrUtils, epifields_helper, epimiscutils, epidatafilestypes;

const
  XVARIABLE_TAG = Ord(rvX);
  YVARIABLE_TAG = Ord(rvY);
  FITVARIABLE_TAG = Ord(rvF);

{ TRegressVariableView }

procedure TRegressVariableView.CreateTypeRadios(RadioGroup: TRadioGroup);
begin
  RadioGroup.Items.Add('Simple linear regression (one independent variable)');
  RadioGroup.Items.Add('Multivariable linear regression');
  RadioGroup.Items.Add('Polynomial regression (one independent variable)');
  RadioGroup.Items.Add('Logistic regression');
  RadioGroup.ItemIndex := 1;
  RadioGroup.OnSelectionChanged := @TypeSelectionChanged;
end;

procedure TRegressVariableView.CreateDegreeEdit(Field: TSpinEdit);
begin
  Field.OnChange := @DegreeChanged;
end;

procedure TRegressVariableView.VariableSelect(Sender: TObject);
var
  Field: TEpiField;
  ComboBox: TCustomComboBox;
begin
  ComboBox := TCustomComboBox(Sender);
  Field := TEpiField(ComboBox.Items.Objects[ComboBox.ItemIndex]);

  case ComboBox.Tag of
    XVARIABLE_TAG:
      FDataModel.XVariable := Field;
    YVARIABLE_TAG:
      FDataModel.YVariable := Field;
    FITVARIABLE_TAG:
      FDataModel.FitVariable := Field;
  end;
  UpdateTreeView();
  UpdateCombos();
  DoModified();
end;

procedure TRegressVariableView.UpdateCombos();
var
  Field: TEpiField;
  ComboBox: TEpiFieldsComboBox;
  i: Integer;
begin
  for i := Low(FComboBoxes) to High(FComboBoxes) do
    begin
      ComboBox := FComboBoxes[i];

      Field := ComboBox.SelectedField;
      ComboBox.Fields.Free;
      ComboBox.Fields := nil;
      ComboBox.Fields := FDataModel.GetComboFields(TRegressVariable(i));
      ComboBox.ItemIndex := ComboBox.Items.IndexOfObject(Field);
    end;
end;

procedure TRegressVariableView.UpdateTreeView();
begin
  FXFields := FDataModel.GetListFields();

  FVarnamesList.RootNodeCount := FXFields.Count;
  FVarnamesList.ReinitNode(FVarnamesList.RootNode, true);
  FVarnamesList.InvalidateChildren(nil, true);
  FVarnamesList.Header.Columns[0].CheckState := csUncheckedNormal;

  // Fails during create because this view has no parent yet and
  // the TreeView requires a handle when autofitting.
  if (HandleAllocated) then
    FVarnamesList.Header.AutoFitColumns(false);
end;

procedure TRegressVariableView.UpdateSelectAllCheckbox();
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

procedure TRegressVariableView.CheckAllFields(Value: boolean);
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

procedure TRegressVariableView.VarnamesHeaderClick(Sender: TVTHeader;
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

procedure TRegressVariableView.VarnameListChecked(Sender: TBaseVirtualTree;
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

procedure TRegressVariableView.VarnamesListGetText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: String);
var
  F: TEpiField;
begin
  F := FXFields[Node^.Index];

  if (not Assigned(F.DataFile)) then
    Exit;

  case Column of
    1: CellText := F.Name + IfThen(F.IsKeyfield, '*', '');
    2: CellText := EpiTypeNamesShort[F.FieldType];
    3: CellText := F.Question.Text;
  end;
end;

procedure TRegressVariableView.VarnamesListInitNode(Sender: TBaseVirtualTree;
  ParentNode, Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
begin
  Node^.CheckType := ctCheckBox;
  Node^.CheckState := csUncheckedNormal;
  FDataModel.SelectVariable[Node^.Index] := false;
end;

procedure TRegressVariableView.VarnamesListGetImageIndex(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
  var Ghosted: Boolean; var ImageIndex: Integer);
var
  F: TEpiField;
begin
  if (Column <> 2) then exit;

  Ghosted := false;

  F := FXFields[Node^.Index];
  ImageIndex := DM.GetImageIndex(F.FieldType);
end;

constructor TRegressVariableView.Create(TheOwner: TComponent);
var
  ComboBox: TEpiFieldsComboBox;
  PrevCombo: TEpiFieldsComboBox;
  Column: TVirtualTreeColumn;
const
  CHECKBOX_WIDTH = 40;
begin
  inherited Create(TheOwner);

  FFirstView := false;
//  Caption := 'Regression Type and Variables';

  {Type and Dependent variable}
  FTypeRadios := TRadioGroup.Create(self);
  CreateTypeRadios(FTypeRadios);

  FDegreeBox := TGroupBox.Create(self);
  FDegreeLabel := TLabel.Create(self);

  FDegreeEdit := TSpinEdit.Create(self);
  CreateDegreeEdit(FDegreeEdit);
  FDegreeEdit.MinValue := 2;
  FDegreeEdit.MaxValue := 10;

  SetLength(FComboBoxes, 3);

  FTypeRadios.Parent := self;
  FTypeRadios.Caption := 'Regression Type';
  FTypeRadios.Anchors := [];
  FTypeRadios.AnchorParallel(akTop, 5, Self);
  FTypeRadios.AnchorParallel(akLeft, 5, self);
  FTypeRadios.AnchorParallel(akRight, 5, self);

  FDegreeBox.Parent := self;
  FDegreeBox.Anchors := [];
  FDegreeBox.AnchorToNeighbour(akTop, 0, FTypeRadios);
  FDegreeBox.AnchorParallel(akLeft, 5, self);
  FDegreeBox.AnchorParallel(akRight, 5, self);

  FDegreeLabel.Parent := FDegreeBox;
  // This is an ugly way to position the label
  // There are 2 problems
  // 1 Setting FDegreeBox caption triggers the change action (yields error)
  // 2 The two controls within FDegreeBox do not follow anchors at all,
  //   whether to each other or their parent. Thare are always placed one on top of the other
  FDegreeLabel.Caption := '                Degree of polynomial';
//  FDegreeLabel.Anchors := [];
//  FDegreeLabel.AnchorParallel(akTop, 5, self);
//  FDegreeLabel.AnchorParallel(akleft, 5, self);

  FDegreeEdit.Parent := FDegreeBox;
//  FDegreeEdit.Anchors := [];
//  FDegreeEdit.AnchorParallel(akTop, 35, self);
//  FDegreeEdit.AnchorParallel(akLeft, 10, self);

  ComboBox := TEpiFieldsComboBox.Create(TheOwner);
  ComboBox.Filter := [ftInteger, ftFloat];
  ComboBox.Parent := self;
  ComboBox.AnchorParallel(akLeft, 10, Self);
  ComboBox.AnchorParallel(akRight, 10, Self);
  ComboBox.AnchorToNeighbour(akTop, 10, FDegreeBox);
  ComboBox.OnSelect := @VariableSelect;
  ComboBox.Tag := YVARIABLE_TAG;
  ComboBox.NoItemText := 'Dependent Variable';
  FComboBoxes[YVARIABLE_TAG] := ComboBox;
  PrevCombo := ComboBox;

  ComboBox := TEpiFieldsComboBox.Create(TheOwner);
  ComboBox.Parent := self;
  ComboBox.AnchorParallel(akLeft, 10, Self);
  ComboBox.AnchorParallel(akRight, 10, Self);
  ComboBox.AnchorToNeighbour(akTop, 10, PrevCombo);
  ComboBox.OnSelect := @VariableSelect;
  ComboBox.Tag := XVARIABLE_TAG;
  ComboBox.NoItemText := 'Single independent Variable';
  FComboBoxes[XVARIABLE_TAG] := ComboBox;
  PrevCombo := ComboBox;

  ComboBox := TEpiFieldsComboBox.Create(TheOwner);
  ComboBox.Parent := self;
  ComboBox.AnchorParallel(akLeft, 10, Self);
  ComboBox.AnchorParallel(akRight, 10, Self);
  ComboBox.AnchorToNeighbour(akTop, 10, PrevCombo);
  ComboBox.OnSelect := @VariableSelect;
  ComboBox.Tag := FITVARIABLE_TAG;
  ComboBox.NoItemText := 'Save fitted values to variable';
  FComboBoxes[FITVARIABLE_TAG] := ComboBox;

  {Independent variable list}

  FVarnamesList := TLazVirtualStringTree.Create(Self);
  with FVarnamesList do
    begin
      Parent := Self;
      //Align  := alClient;  // must align to previous combobox
      AnchorToNeighbour(akTop, 10, ComboBox);
      AnchorParallel(akBottom, 0, self);
      AnchorParallel(akRight, 10, self);

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


procedure TRegressVariableView.TypeSelectionChanged(Sender: TObject);
begin
  case TRadioGroup(Sender).ItemIndex of
    0: begin
      FDataModel.RegressType := rtSimple;
      FDegreeBox.Visible := false;
      FComboBoxes[XVARIABLE_TAG].Visible := true;
      FVarnamesList.Visible := false;
      CheckAllFields(false);
    end;
    1: begin
      FDataModel.RegressType := rtLinear;
      FDataModel.XVariable := nil;
      FDegreeBox.Visible := false;
      FComboBoxes[XVARIABLE_TAG].Visible := false;
      FVarnamesList.Visible := true;
    end;
    2: begin
      FDataModel.RegressType := rtPolynomial;
      FDegreeBox.Visible := true;
      FComboBoxes[XVARIABLE_TAG].Visible := true;
      FVarnamesList.Visible := false;
      CheckAllFields(false);
    end;
    3: begin
      FDataModel.RegressType := rtLogistic;
      FDataModel.XVariable := nil;
      FDegreeBox.Visible := false;
      FComboBoxes[XVARIABLE_TAG].Visible := false;
      FVarnamesList.Visible := true;
    end;
  end;
  FComboBoxes[XVARIABLE_TAG].ItemIndex := 0;
  UpdateCombos();
  DoModified();
end;

procedure TRegressVariableView.DegreeChanged(Sender: TObject);
begin
  FDataModel.Degree := FDegreeEdit.Value;
  DoModified();
end;

procedure TRegressVariableView.EnterView();
begin
  if (not FFirstView) then
    begin
      UpdateTreeView();
      FFirstView := true;
    end;
end;

function TRegressVariableView.ExitView(): boolean;
begin
  result := true;
end;

function TRegressVariableView.GetViewCaption(): UTF8String;
begin
  result := 'Regression type and Variables';
end;

function TRegressVariableView.IsDefined(): boolean;
begin
  result := FDataModel.IsDefined();
end;

procedure TRegressVariableView.ResetView();
var
  Combobox: TCustomComboBox;
begin
  for Combobox in FComboBoxes do
    Combobox.ItemIndex := 0;

  FDataModel.XVariable := nil;
  FDataModel.YVariable := nil;
  FDataModel.FitVariable := nil;
  UpdateCombos();
  FTypeRadios.ItemIndex := 1;
  FComboBoxes[XVARIABLE_TAG].Visible := false;
  FDegreeBox.Visible := false;
  FDataModel.RegressType := rtLinear;
  FDataModel.Degree := 2;

  UpdateTreeView();
  DoModified();
end;

procedure TRegressVariableView.SetModel(DataModel: TRegressVariableModel);
begin
  FDataModel := DataModel;
  UpdateTreeView();
end;

end.

