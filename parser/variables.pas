unit Variables;

{$MODE Delphi}

{
'================================================================================
' Class Name:
'      VariableList
'
' Instancing:
'      Private; Internal  (VB Setting: 1 - Private)
'
' Purpose:
'      This is a very simple class that stores a list of "variables". The GOLDParser
'      class uses a this class to store the parameter fields.
'
' Author(s):
'      Devin Cook
'      GOLDParser@DevinCook.com
'
' Dependacies:
'      (None)
'
'================================================================================
 Conversion to Delphi:
      Beany
      Beany@cloud.demon.nl

 Conversion status: Done, not tested
================================================================================
}

interface

uses Classes, LazUTF8Classes;

type

  { TVariableList }

  TVariableList = class
  private
    MemberList: TStringListUTF8;
    function GetCount: Integer;
    function GetValue(Name: UTF8String): UTF8String;
    procedure SetValue(Name: UTF8String; Value: UTF8String);
    function GetName(Index: Integer): UTF8String;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Add(Name: UTF8String; Value: UTF8String);

    procedure Clear;
    property Count: Integer read GetCount;
    property Value[Name: UTF8String]: UTF8String read GetValue write SetValue;
    property Names[Index : integer] : UTF8String read GetName;
  end;

implementation

constructor TVariableList.Create;
begin
  inherited Create;
  MemberList := TStringListUTF8.Create;
end;

destructor TVariableList.Destroy;
begin
   MemberList.Free;
   inherited Destroy;
end;

procedure TVariableList.Add(Name: UTF8String; Value: UTF8String);
begin
  MemberList.Values[Name] := Value;
end;

procedure TVariableList.Clear;
begin
  MemberList.Clear;
end;

function TVariableList.GetCount: Integer;
begin
   Result := MemberList.Count;
end;

function TVariableList.GetName(Index: Integer): UTF8String;
begin
  Result := MemberList.Names[Index];
end;

function TVariableList.GetValue(Name: UTF8String): UTF8String;
begin
  Result := MemberList.Values[Name];
end;

procedure TVariableList.SetValue(Name: UTF8String; Value: UTF8String);
begin
  MemberList.Values[Name] := Value;
end;

end.

