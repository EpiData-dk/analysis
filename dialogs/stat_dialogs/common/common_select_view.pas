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
    procedure GetExpression(Sender: TObject); //Sender: TObject; var UTF8Key: TUTF8Char);
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
end;

procedure TCommonSelectView.CriteriaSelect(Sender: TObject);
var
  Criteria: TMatchCriteria;
  ListBox: TListBox;
begin
  ListBox := TListBox(Sender);
  Criteria := TMatchCriteria(PtrInt(ListBox.Items.Objects[ListBox.ItemIndex]));// ComboBox.Items.Objects[ComboBox.ItemIndex].;

  FDataModel.MatchCriteria := Criteria;

  if Criteria in MatchCriteriaNoTextSearch then begin
    FExpressionBox.Visible := false;
    FDataModel.Expression := '';
  end
  else
    FExpressionBox.Visible := true;

  DoModified();
end;

procedure TCommonSelectView.GetExpression(Sender: TObject); //Sender: TObject; var UTF8Key: TUTF8Char);
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
end;

procedure TCommonSelectView.UpdateField();
var
  Field: TEpiField;
begin
  UpdateCriteria();
  DoModified();
end;

procedure TCommonSelectView.UpdateCriteria();
var
  Field: TEpiField;
  Criteria: TMatchCriteria;
begin
  if (FDataModel.SelectVariable = nil) then exit;
  Field := FDataModel.SelectVariable;
  FDataModel.GetComboCriteria(FCriteriaList, Field.FieldType);
  DoModified();
end;

constructor TCommonSelectView.Create(TheOwner: TComponent);

begin
  inherited Create(TheOwner);

  FFieldCombo := TEpiFieldsComboBox.Create(TheOwner);
  FFieldCombo.Parent := self;
  FFieldCombo.AnchorParallel(akTop, 10, Self);
  FFieldCombo.AnchorParallel(akLeft, 10, Self);
  FFieldCombo.AnchorParallel(akBottom, 10, Self);
  FFieldCombo.OnSelect := @FieldSelect;
  FFieldCombo.NoItemText := 'Variable';
  FFieldCombo.Caption := 'Variable';

  FCriteriaList := TListBox.Create(TheOwner);
  FCriteriaList.Parent := self;
  FCriteriaList.AnchorParallel(akTop, 10, Self);
  FCriteriaList.AnchorToNeighbour(akLeft, 10, FFieldCombo);
  FCriteriaList.AnchorParallel(akBottom, 10, Self);
  FCriteriaList.OnClick := @CriteriaSelect;
  FCriteriaList.Caption := 'Comparison operator';

  FExpressionBox := TEdit.Create(TheOwner);
  FExpressionBox.Parent := self;
  FExpressionBox.Height := 20;
  FExpressionBox.AnchorParallel(akTop, 10, Self);
  FExpressionBox.AnchorToNeighbour(akLeft, 10, FCriteriaList);
  FExpressionBox.AnchorParallel(akBottom, 10, Self);
  FExpressionBox.Caption := 'Compare to number or text';
  FExpressionBox.Visible := false;

  FSelectButton := TBitBtn.Create(TheOwner);
  FSelectButton.AnchorParallel(akTop, 10, Self);
  FSelectButton.AnchorToNeighbour(akLeft,10,FExpressionBox);
  FSelectButton.Parent := self;
  FSelectButton.OnClick := @GetExpression;
  FSelectButton.Caption := 'Select';

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
var
  Combobox: TCustomComboBox;
begin
  FFieldCombo.ItemIndex := 0;
//  FCriteriaList.ItemIndex := 0;
  FDataModel.SelectVariable := nil;
  FDataModel.MatchCriteria := mcEq;
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
