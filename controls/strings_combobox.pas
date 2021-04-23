unit strings_combobox;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, StdCtrls, epidatafiles, epidatafilestypes;

type

  { TStringsComboBox }

  TStringsComboBox = class(TCustomComboBox)
  private
    FStrings: TStrings;
    FNoItemText: UTF8String;
    function GetSelectedString: UTF8String;
    procedure SetStrings(AValue: TStrings);
//    procedure SetFilter(AValue: TStringTypes);
    procedure SetNoItemText(AValue: UTF8String);
    procedure UpdateItems();
  public
    constructor Create(TheOwner: TComponent); override;
    property Fields: TStrings read FStrings write SetStrings;
//    property Filter: TEpiFieldTypes read FFilter write SetFilter;
    property NoItemText: UTF8String read FNoItemText write SetNoItemText;
    property SelectedString: UTF8String read GetSelectedString;
  published
    property OnSelect;
  end;

implementation

{ TStringsComboBox }

procedure TStringsComboBox.SetStrings(AValue: TStrings);
begin
  if FStrings = AValue then Exit;
  FStrings := AValue;

  UpdateItems();
end;

function TStringsComboBox.GetSelectedString: UTF8String;
begin
  Result := Items.Objects[ItemIndex].ToString.ToLower;
end;

{procedure TStringsComboBox.SetFilter(AValue: TEpiFieldTypes);
begin
  if FFilter = AValue then Exit;
  FFilter := AValue;

  UpdateItems();
end;
}
procedure TStringsComboBox.SetNoItemText(AValue: UTF8String);
begin
  if FNoItemText = AValue then Exit;
  FNoItemText := AValue;

  UpdateItems();
end;

procedure TStringsComboBox.UpdateItems();
var
  AString: UTF8String;
begin
  Clear;

  if (FNoItemText <> '') then
    AddItem(FNoItemText, nil);

  if (FNoItemText <> '') then
    ItemIndex := 0;
end;

constructor TStringsComboBox.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);

  SetStyle(csDropDownList);
//  FFilter := AllFieldTypes;
  FStrings := nil;
end;

end.

