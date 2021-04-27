unit common_select_view;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, stat_dialog_contribution, Controls, ExtCtrls, LazUTF8, LCLType,
  StdCtrls, common_select_model, fields_combobox, epidatafiles, Buttons,
  stat_dialog_custom_view, epidatafilestypes;

type

  { TCommonSelectView }

  TCommonSelectView = class(TCustomStatDialogView)
  private
    FDataModel: TCommonSelectModel;
    FFieldCombo: TEpiFieldsComboBox;
    FCriteriaList: TListBox;
    FExpressionBox: TEdit;
    FSelectButton: TBitBtn;
    FOnModified: IStatDiaglogViewModified;
    procedure FieldSelect(Sender: TObject);
    procedure CriteriaSelect(Sender: TObject);
    procedure GetExpression(Sender: TObject);
    procedure UpdateField();
    procedure UpdateCriteria();
  public
    constructor Create(TheOwner: TComponent);
    procedure EnterView(); override;
    function ExitView(): boolean; override;
    function GetViewCaption(): UTF8String; override;
    procedure ResetView(); override;
    function IsDefined(): boolean; override;
    procedure SetModel(DataModel: TCommonSelectModel);
  end;

implementation

{ TCommonSelectView }

procedure TCommonSelectView.FieldSelect(Sender: TObject);
var
  Field: TEpiField;
  ComboBox: TCustomComboBox;
begin
  ComboBox := TCustomComboBox(Sender);
  Field := TEpiField(ComboBox.Items.Objects[ComboBox.ItemIndex]);

  FDataModel.SelectVariable := Field;

  UpdateCriteria();
  DoModified();
  FSelectButton.Hide;
end;

procedure TCommonSelectView.CriteriaSelect(Sender: TObject);
var
  Criteria: TMatchCriteria;
  ListBox: TListBox;
begin
  ListBox := TListBox(Sender);
  Criteria := TMatchCriteria(PtrInt(ListBox.Items.Objects[ListBox.ItemIndex]));

  FDataModel.MatchCriteria := Criteria;

  if Criteria in MatchCriteriaNoTextSearch then begin
    FExpressionBox.Text := 'Value';
    FExpressionBox.Hide;
    FDataModel.Expression := '';
  end
  else begin
    FExpressionBox.Show;
  end;
  FSelectButton.Show;
  DoModified();
end;

procedure TCommonSelectView.GetExpression(Sender: TObject);
var
  theText: UTF8String;
begin
  theText := FExpressionBox.Text;
  if FDataModel.SelectVariable.FieldType = ftUpperString then
    begin
      theText := upCase(theText);
    end;
  FDataModel.Expression := theText;
  FExpressionBox.Text := theText;
  DoModified();
end;

procedure TCommonSelectView.UpdateField();
begin
  UpdateCriteria();
  DoModified();
end;

procedure TCommonSelectView.UpdateCriteria();
var
  Field: TEpiField;
begin
  if (FDataModel.SelectVariable = nil) then exit;
  Field := FDataModel.SelectVariable;
  FCriteriaList.Show;
  FDataModel.GetComboCriteria(FCriteriaList, Field.FieldType);
  FExpressionBox.Text := 'Value';
  FExpressionBox.Hide;
  FDataModel.Expression := '';
  DoModified();
end;

constructor TCommonSelectView.Create(TheOwner: TComponent);

begin
  inherited Create(TheOwner);

  FFieldCombo := TEpiFieldsComboBox.Create(TheOwner);
  with FFieldCombo do
  begin
    Parent := self;
    Anchors := [];
    AnchorParallel(akTop, 10, Self);
    AnchorParallel(akLeft, 10, Self);
    AnchorParallel(akBottom, 10, Self);
    OnSelect := @FieldSelect;
    NoItemText := 'Variable';
    Caption := 'Variable';
  end;

  FCriteriaList := TListBox.Create(TheOwner);
  with FCriteriaList do
  begin
    Parent := self;
    Anchors := [];
    AnchorParallel(akTop, 10, Self);
    AnchorToNeighbour(akLeft, 10, FFieldCombo);
    // hack to set reasonable height, since default shows only 5 options
    ClientHeight := (ClientHeight * 8) div 5;
    OnClick := @CriteriaSelect;
    Caption := 'Comparison';
    Visible := false;
  end;

  FExpressionBox := TEdit.Create(TheOwner);
  with FExpressionBox do
  begin
    Parent := self;
    Anchors := [];
    AnchorParallel(akTop, 10, Self);
    AnchorToNeighbour(akLeft, 10, FCriteriaList);
    Caption := 'Value';
    Visible := false;
  end;

  FSelectButton := TBitBtn.Create(TheOwner);
  with FSelectButton do
  begin
    AnchorParallel(akTop, 10, Self);
    AnchorToNeighbour(akLeft,10,FExpressionBox);
    Parent := self;
    OnClick := @GetExpression;
    Caption := 'Select';
    Visible := false;
  end;
end;

procedure TCommonSelectView.EnterView();
begin
end;

function TCommonSelectView.ExitView(): boolean;
begin
  result := true;
end;

function TCommonSelectView.GetViewCaption(): UTF8String;
begin
  result := 'Select criteria';
end;

procedure TCommonSelectView.ResetView();
begin
  FFieldCombo.ItemIndex := -1;
  FCriteriaList.Hide;
  FExpressionBox.Text := 'Value';
  FExpressionBox.Hide;
  FSelectButton.Hide;
  FDataModel.SelectVariable := nil;
  FDataModel.MatchCriteria := mcEQ;
  FDataModel.Expression := '';

  DoModified();
end;

function TCommonSelectView.IsDefined(): boolean;
begin
  result := FDataModel.IsDefined();
end;

procedure TCommonSelectView.SetModel(
  DataModel: TCommonSelectModel);
begin
  FDataModel := DataModel;

// fields do not change, so set them now
  FFieldCombo.Fields := FDataModel.GetComboFields();

end;

end.
