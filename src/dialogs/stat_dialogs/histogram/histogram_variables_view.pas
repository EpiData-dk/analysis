unit histogram_variables_view;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls, ExtCtrls, StdCtrls,
  stat_dialog_contribution, histogram_model, fields_combobox, stat_dialog_custom_view;

type

  { THistogramStatDialogVariablesView }

  THistogramStatDialogVariablesView = class(TCustomStatDialogView)
  private
    FDataModel: THistogramStatDialogVariableModel;
    FComboBoxes: Array of TEpiFieldsComboBox;
    FOnModified: IStatDiaglogViewModified;
    FOptionGroup: TCheckGroup;
    FHorizontalDivider: TBevel;
    procedure VariableSelect(Sender: TObject);
    procedure UpdateCombos();
    procedure OptionGroupCheck(Sender: TObject; Index: integer);
  public
    constructor Create(TheOwner: TComponent);
    procedure EnterView(); override;
    function ExitView(): boolean; override;
    function GetViewCaption(): UTF8String; override;
    procedure ResetView(); override;
    function IsDefined(): boolean; override;
    procedure SetModel(DataModel: THistogramStatDialogVariableModel);
  end;

implementation

uses
  epidatafiles;

const
  XVARIABLE_TAG   = Ord(tvX);
  YVARIABLE_TAG   = Ord(tvY);
  WVARIABLE_TAG   = Ord(tvW);

{ THistogramStatDialogVariablesView }

procedure THistogramStatDialogVariablesView.VariableSelect(Sender: TObject);
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

    WVARIABLE_TAG:
      FDataModel.WVariable := Field;

  end;

  UpdateCombos();
  DoModified();
end;

procedure THistogramStatDialogVariablesView.UpdateCombos();
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
      ComboBox.Fields := FDataModel.GetComboFields(THistogramStatDialogVariable(i));
      ComboBox.ItemIndex := ComboBox.Items.IndexOfObject(Field);
    end;
end;

procedure THistogramStatDialogVariablesView.OptionGroupCheck(Sender: TObject; Index: integer);
var
  Value: Boolean;
begin
  Value := TCheckGroup(Sender).Checked[Index];
  case Index of
    0:  FDataModel.Epicurve := Value;
  end;
end;

constructor THistogramStatDialogVariablesView.Create(TheOwner: TComponent);
var
  ComboBox: TEpiFieldsComboBox;
  PrevCombo: TEpiFieldsComboBox;
begin
  inherited Create(TheOwner);

  SetLength(FComboBoxes, Ord(High(THistogramStatDialogVariable)) + 1);

  FOptionGroup := TCheckGroup.Create(self);

  FHorizontalDivider := TBevel.Create(self);
  FHorizontalDivider.Parent := self;
  FHorizontalDivider.Style := bsLowered;
  FHorizontalDivider.Shape := bsSpacer;
  FHorizontalDivider.Height := 5;
  FHorizontalDivider.Anchors := [];
  FHorizontalDivider.AnchorParallel(akLeft, 0, Self);
  FHorizontalDivider.AnchorParallel(akRight, 0, Self);

  ComboBox := TEpiFieldsComboBox.Create(TheOwner);
  ComboBox.Parent := self;
  ComboBox.AnchorParallel(akTop, 10, Self);
  ComboBox.AnchorParallel(akLeft, 10, Self);
  ComboBox.AnchorParallel(akRight, 10, Self);
  ComboBox.OnSelect := @VariableSelect;
  ComboBox.Tag := XVARIABLE_TAG;
  ComboBox.NoItemText := 'Time Variable';
  FComboBoxes[XVARIABLE_TAG] := ComboBox;
  PrevCombo := ComboBox;

  ComboBox := TEpiFieldsComboBox.Create(TheOwner);
  ComboBox.Parent := self;
  ComboBox.AnchorToNeighbour(akTop, 10, PrevCombo);
  ComboBox.AnchorParallel(akLeft, 10, Self);
  ComboBox.AnchorParallel(akRight, 10, Self);
  ComboBox.OnSelect := @VariableSelect;
  ComboBox.Tag := YVARIABLE_TAG;
  ComboBox.NoItemText := 'Stratifying Variable';
  FComboBoxes[YVARIABLE_TAG] := ComboBox;
  PrevCombo := ComboBox;

  ComboBox := TEpiFieldsComboBox.Create(TheOwner);
  ComboBox.Parent := self;
  ComboBox.AnchorToNeighbour(akTop, 10, PrevCombo);
  ComboBox.AnchorParallel(akLeft, 10, Self);
  ComboBox.AnchorParallel(akRight, 10, Self);
  ComboBox.OnSelect := @VariableSelect;
  ComboBox.Tag := WVARIABLE_TAG;
  ComboBox.NoItemText := 'Weight Variable (optional; ignored for epicurve)';
  FComboBoxes[WVARIABLE_TAG] := ComboBox;

  FOptionGroup.Parent := self;
  FOptionGroup.Caption := 'Histogram or Epicurve';
  FOptionGroup.Anchors := [];
  FOptionGroup.AnchorParallel(akLeft, 5, self);
  FOptionGroup.AnchorParallel(akBottom, 5, Self);
  FOptionGroup.AnchorToNeighbour(akTop, 0, FHorizontalDivider);
  FOptionGroup.Items.Add('Epicurve');
  FOptionGroup.OnItemClick:= @OptionGroupCheck;

  EnterView(); // Must do this to get combo boxes aligned and visible
end;

procedure THistogramStatDialogVariablesView.EnterView();
begin
  FHorizontalDivider.Top := ((Self.Height - FHorizontalDivider.Height) div 2);
end;

function THistogramStatDialogVariablesView.ExitView(): boolean;
begin
  result := true;
end;

function THistogramStatDialogVariablesView.GetViewCaption(): UTF8String;
begin
  result := 'Variables';
end;

procedure THistogramStatDialogVariablesView.ResetView();
var
  Combobox: TCustomComboBox;
begin
  for Combobox in FComboBoxes do
    Combobox.ItemIndex := 0;

  FDataModel.XVariable := nil;
  FDataModel.YVariable := nil;
  FDataModel.WVariable := nil;

  UpdateCombos();
  DoModified();
end;

function THistogramStatDialogVariablesView.IsDefined(): boolean;
begin
  result := FDataModel.IsDefined();
end;

procedure THistogramStatDialogVariablesView.SetModel(
  DataModel: THistogramStatDialogVariableModel);
begin
  FDataModel := DataModel;

  UpdateCombos();
end;

end.
