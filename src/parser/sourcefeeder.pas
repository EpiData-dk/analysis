unit SourceFeeder;

{$MODE Delphi}

interface

uses
   Classes, LazUTF8;

type

  { TSourceFeeder }

  TSourceFeeder = class
  private
    FCaretPos,
    FLinePos: Integer;
    FIndex: Integer;
    FString: UTF8String;
    procedure SetText(const Value: UTF8String);
    function GetText: UTF8String;
  public
    constructor Create;
    destructor Destroy; override;
    function  GetNextCharacter: UTF8String;
    procedure PutCharacter(C: UTF8String);
    procedure NewLine;

    function Done: Boolean;
    function ReadLine: UTF8String;
    property Text: UTF8String read GetText write SetText;
    property LinePos: Integer read FLinePos;
    property CaretPos: Integer read FCaretPos;
    property ByteIndex: Integer read FIndex;
  end;

implementation

constructor TSourceFeeder.Create;
begin
  inherited;
end;

destructor TSourceFeeder.Destroy;
begin
  inherited;
end;

function TSourceFeeder.GetNextCharacter: UTF8String;
var
  P: PChar;
  L, i: Integer;
begin
  P := @FString[FIndex];
  L := UTF8CharacterLength(P);
  SetLength(Result, L);
  for i := 1 to L do
    result[i] := FString[FIndex + i - 1];
  Inc(FIndex, L);
  Inc(FCaretPos);
end;

procedure TSourceFeeder.PutCharacter(C: UTF8String);
begin
  Dec(FIndex, UTF8CharacterLength(@C[1]));
  Dec(FCaretPos);
end;

procedure TSourceFeeder.NewLine;
begin
  Inc(FLinePos);
  FCaretPos := 1;
end;

function TSourceFeeder.Done: Boolean;
begin
  Result := FIndex > Length(FString);
end;

function TSourceFeeder.GetText: UTF8String;
begin
  Result := FString;
end;

function TSourceFeeder.ReadLine: UTF8String;
var EndReached: Boolean;
    ch: UTF8String;
begin
  EndReached := False;

  while not (EndReached) and (not Done) do
    begin
      ch := GetNextCharacter; //ReadFromBuffer(1, True, True);

      case ch[1] of
        #10: EndReached := true;
        #13: begin
               ch := GetNextCharacter;
               if (not (Ch = #10)) then
                 PutCharacter(Ch);
               EndReached := true;
             end;
      else
        Result := Result + ch;
      end;

{      if (ch = #10) or (ch = #13) then
        begin
          ch := GetNextCharacter;// ReadFromBuffer(1, False, True);
          if not (ch = #13) then
            PutCharacter(Ch); // ReadFromBuffer(1, True, True)

          EndReached := True;
        end
      else
        Result := Result + ch;
      }
    end;
  NewLine;
end;

procedure TSourceFeeder.SetText(const Value: UTF8String);
begin
  FString := Value;
  FIndex := 1;
  FLinePos := 1;
  FCaretPos := 1;
end;

end.
