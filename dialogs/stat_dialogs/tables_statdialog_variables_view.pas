unit tables_statdialog_variables_view;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, stat_dialog_contribution, Controls, ExtCtrls,
  StdCtrls, tables_statdialog_model;

type

  { TTableStatDialogVariablesView }

  TTableStatDialogVariablesView = class(TPanel, IStatDialogView)
  private
    FDataModel: TTableStatDialogVariableModel;
    FXVariableCombo: TComboBox;
    procedure UpdateView();
    procedure VariableSelect(Sender: TObject);
  public
    constructor Create(TheOwner: TComponent); override;
    procedure EnterView();
    function ExitView(): boolean;
    function GetControl(): TControl;
    function GetViewCaption: UTF8String;
    procedure SetModel(DataModel: TTableStatDialogVariableModel);
  end;

implementation

{ TTableStatDialogVariablesView }

procedure TTableStatDialogVariablesView.UpdateView();
begin
  ;
end;

procedure TTableStatDialogVariablesView.VariableSelect(Sender: TObject);
begin
  FDataModel.XVariable := FXVariableCombo.Items.Objects[FXVariableCombo.ItemIndex];
end;

constructor TTableStatDialogVariablesView.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);

  FXVariableCombo := TComboBox.Create(TheOwner);
  FXVariableCombo.Parent := self;
  FXVariableCombo.AnchorParallel(akLeft, 10, Self);
  FXVariableCombo.AnchorParallel(akRight, 10, Self);
  FXVariableCombo.AnchorParallel(akTop, 10, Self);
  FXVariableCombo.OnSelect := @VariableSelect;
end;

procedure TTableStatDialogVariablesView.EnterView();
begin
  UpdateView();
end;

function TTableStatDialogVariablesView.ExitView(): boolean;
begin
  result := true;
end;

function TTableStatDialogVariablesView.GetControl(): TControl;
begin
  result := self;
end;

function TTableStatDialogVariablesView.GetViewCaption: UTF8String;
begin
  result := 'Variables';
end;

procedure TTableStatDialogVariablesView.SetModel(
  DataModel: TTableStatDialogVariableModel);
begin
  FDataModel := DataModel;
end;

end.


