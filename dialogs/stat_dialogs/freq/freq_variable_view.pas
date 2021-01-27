unit freq_variable_view;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, stat_dialog_contribution, ExtCtrls, freq_variable_model,
  Controls, fields_combobox, epidatafiles;

type

  { TFreqVariableView }

  TFreqVariableView = class(TPanel, IStatDialogView)
  private
    FDataModel: TFreqVariableModel;
    FOnModified: IStatDiaglogViewModified;
    FComboBoxes: Array of TEpiFieldsComboBox;
    procedure VariableSelect(Sender: TObject);
    procedure UpdateCombos();
  protected
    procedure DoModified();
  public
    constructor Create(TheOwner: TComponent); override;
    procedure EnterView();
    function ExitView(): boolean;
    function GetControl(): TControl;
    function GetViewCaption(): UTF8String;
    function IsDefined(): boolean;
    procedure ResetView();
    procedure SetModel(DataModel: TFreqVariableModel);
    procedure SetOnModified(OnModified: IStatDiaglogViewModified);
  end;

implementation

{ TFreqVariableView }

procedure TFreqVariableView.VariableSelect(Sender: TObject);
var
  ComboBox: TEpiFieldsComboBox;
begin
  ComboBox := TEpiFieldsComboBox(Sender);
  FDataModel.Variables[ComboBox.Tag] := ComboBox.SelectedField;
  DoModified();
end;

procedure TFreqVariableView.UpdateCombos();
var
  ComboBox: TEpiFieldsComboBox;
  Field: TEpiField;
  i: Integer;
begin
  for i := 0 to FREQ_VARIABLES_COUNT - 1 do
    begin
      ComboBox := FComboBoxes[i];

      Field := ComboBox.SelectedField;
      ComboBox.Fields.Free;
      ComboBox.Fields := nil;
      ComboBox.Fields := FDataModel.GetComboFields();
      ComboBox.ItemIndex := ComboBox.Items.IndexOfObject(Field);
    end;
end;

procedure TFreqVariableView.DoModified();
begin
  if (Assigned(FOnModified)) then
    FOnModified.OnViewModified(Self);
end;

constructor TFreqVariableView.Create(TheOwner: TComponent);
var
  ComboBox, PrevCombo: TEpiFieldsComboBox;
  i: Integer;
begin
  inherited Create(TheOwner);

  SetLength(FComboBoxes, FREQ_VARIABLES_COUNT);

  PrevCombo := nil;
  for i := 0 to FREQ_VARIABLES_COUNT - 1 do
    begin
      ComboBox := TEpiFieldsComboBox.Create(TheOwner);
      ComboBox.Parent := self;
      ComboBox.AnchorParallel(akLeft, 10, Self);
      ComboBox.AnchorParallel(akRight, 10, Self);
      if (i = 0) then
        begin
          ComboBox.AnchorParallel(akTop, 10, Self);
          ComboBox.NoItemText := 'Variable';
        end
      else
        begin
          ComboBox.AnchorToNeighbour(akTop, 10, PrevCombo);
          ComboBox.NoItemText := 'Variable (Optional)';
        end;
      ComboBox.OnSelect := @VariableSelect;
      ComboBox.Tag := i;
      FComboBoxes[i] := ComboBox;

      PrevCombo := ComboBox;
    end;
end;

procedure TFreqVariableView.EnterView();
begin
  //
end;

function TFreqVariableView.ExitView(): boolean;
begin
  result := true;
end;

function TFreqVariableView.GetControl(): TControl;
begin
  result := self;
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
var
  Combobox: TEpiFieldsComboBox;
  i: Integer;
begin
  for Combobox in FComboBoxes do
    Combobox.ItemIndex := 0;

  for i := 0 to FREQ_VARIABLES_COUNT -1 do
    FDataModel.Variables[i] := nil;

  UpdateCombos();
  DoModified();
end;

procedure TFreqVariableView.SetModel(DataModel: TFreqVariableModel);
begin
  FDataModel := DataModel;

  UpdateCombos();
end;

procedure TFreqVariableView.SetOnModified(OnModified: IStatDiaglogViewModified);
begin
  FOnModified := OnModified;
end;

end.

