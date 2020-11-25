unit fields_combobox;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, StdCtrls, epidatafiles, epidatafilestypes;

type

  { TEpiFieldsComboBox }

  TEpiFieldsComboBox = class(TCustomComboBox)
  private
    FFields: TEpiFields;
    FFilter: TEpiFieldTypes;
    FNoItemText: UTF8String;
    procedure SetFields(AValue: TEpiFields);
    procedure SetFilter(AValue: TEpiFieldTypes);
    procedure SetNoItemText(AValue: UTF8String);
    procedure UpdateItems();
  public
    constructor Create(TheOwner: TComponent); override;
    property Fields: TEpiFields read FFields write SetFields;
    property Filter: TEpiFieldTypes read FFilter write SetFilter;
    property NoItemText: UTF8String read FNoItemText write SetNoItemText;
  published
    property OnSelect;
  end;

implementation

{ TEpiFieldsComboBox }

procedure TEpiFieldsComboBox.SetFields(AValue: TEpiFields);
begin
  if FFields = AValue then Exit;
  FFields := AValue;

  UpdateItems();
end;

procedure TEpiFieldsComboBox.SetFilter(AValue: TEpiFieldTypes);
begin
  if FFilter = AValue then Exit;
  FFilter := AValue;

  UpdateItems();
end;

procedure TEpiFieldsComboBox.SetNoItemText(AValue: UTF8String);
begin
  if FNoItemText = AValue then Exit;
  FNoItemText := AValue;

  UpdateItems();
end;

procedure TEpiFieldsComboBox.UpdateItems();
var
  Field: TEpiField;
begin
  Clear;

  if (FNoItemText <> '') then
    AddItem(FNoItemText, nil);

  for Field in FFields do
    if (Field.FieldType in FFilter) then
      AddItem(Field.Name, Field);

  if (FNoItemText <> '') then
    ItemIndex := 0;
end;

constructor TEpiFieldsComboBox.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);

  SetStyle(csDropDownList);
  FFilter := AllFieldTypes;
end;

end.

