unit options_table;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, options_hashmap;

type

  { TTablePercentFormatOption }

  TTablePercentFormatOption = class(TSetOption)
  private
    function GetLeftChar: UTF8String;
    function GetRigthChar: UTF8String;
  protected
    procedure SetValue(AValue: UTF8String); override;
  public
    property LeftChar: UTF8String read GetLeftChar;
    property RigthChar: UTF8String read GetRigthChar;
  end;


implementation

{ TTablePercentFormatOption }

function TTablePercentFormatOption.GetLeftChar: UTF8String;
begin
  result := Value[1];
end;

function TTablePercentFormatOption.GetRigthChar: UTF8String;
begin
  result := Value[2];
end;

procedure TTablePercentFormatOption.SetValue(AValue: UTF8String);
begin
  if (Length(AValue) <> 2) then
    DoError('Set Option value must contain exactly 2 characters!');

  inherited SetValue(AValue);
end;

end.

