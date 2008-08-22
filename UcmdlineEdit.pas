unit UcmdlineEdit;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls,SMUtils,ansDatatypes;

type

  TCommandLineEditor = class(TEdit)
  private
    fOnCommand: TNotifyEvent;
    fHistory: TStringList;
    fCurrentCmd: integer;
    function GetLineNr : Integer;
    function GetColNr : Integer;
  protected
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyPress(var Key: Char);override;
  public
    constructor Create(AOwner: TComponent); override;
    Destructor Destroy;override;
    procedure insert(s : string);
    procedure activate;
    function doCommand(const cmd:string):boolean;
    property History: TStringList read  fHistory;
    // EXTREMELY UGLY HACK - BUT NEEDED IN ORDER TO INCLUDE FileName IN COMMANDS
    function HackHistory(PrevCommand, NewCommand: string): boolean;
    function GetWords(FromWord, ToWord: integer): string;
  published
    function SaveHistoryToFile(const fn :String):boolean;
    function LoadHistoryFromFile(const fn :String):boolean;
    procedure Cancel;
    property LineNo : Integer read GetLineNr;
    property ColNo  : Integer read GetColNr;
    property CurrentCmd : integer read fCurrentCmd write fCurrentCmd;
    property OnCommand : TNotifyEvent read  fOnCommand write fOnCommand;
  end;



implementation

{
procedure TCommandLineEditor.Undo;
begin
  Perform(WM_Undo,0,0);
end;
}

function TCommandLineEditor.GetLineNr : Integer;
begin
  Result:=Perform(EM_LINEFROMCHAR,-1,0);
end;

function TCommandLineEditor.GetColNr : Integer;
begin
  Result:=Abs(Perform(EM_LINEINDEX,-1,0) - SelStart);
end;


procedure TCommandLineEditor.KeyPress(var Key: Char);
begin
  inherited KeyPress(Key);
  if (Key = Char(VK_RETURN)) then Key := #0;
end;

procedure TCommandLineEditor.KeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited keydown(key, shift);
  case key of
    VK_RETURN:
      begin
      try
       if trim(text)<> '' then
       begin
         fHistory.Add(trim(text));
         fCurrentCmd:=fHistory.Count;
       end;
       if assigned(fOnCommand) then fOnCommand(self);
      finally
        text:='';
        key:=0;
       end;
      end;
    VK_UP:
      begin
        key:=0;
        if (fhistory.Count > 0) and (fCurrentCmd > 0) then
        begin
           dec(fCurrentCmd);
           text := fhistory[fCurrentCmd];
           SelStart:= length(text)+1;
        end;
      end;
    VK_DOWN:
      begin
        key:=0;
        if (fhistory.Count > 0) and (fCurrentCmd < fhistory.count-1) then
        begin
           inc(fCurrentCmd);
           text := fhistory[fCurrentCmd];
           SelStart:= length(text)+1;
        end else begin
           text := '';
           SelStart := 0;
           fCurrentCmd := fHistory.Count;
        end;
      end;
    VK_ESCAPE:
      begin
        key:=0;
        fCurrentCmd:=fhistory.count;
        text :='';
      end;
  else
    if assigned(TForm(Owner).OnKeyDown) then
      TForm(Owner).OnKeyDown(self, key, shift);
  end;//case
end;

constructor TCommandLineEditor.Create(AOwner: TComponent);
begin
 inherited Create(Aowner);
 fHistory := TStringList.create;
end;

destructor TCommandLineEditor.Destroy;
begin
 fHistory.free;
 inherited;
end;

procedure TCommandLineEditor.insert( s: string);
var
  Len: Integer;
  IsFunction: Boolean;
  s1,s2 : string;
begin
  if S = '' then S := ' ';
  if text='' then
  begin
       text:=s;
       exit;
  end;
  s1:=copy(text,1,SelStart);
  s2:=copy(text,Selstart+selLength+1,length(text));
  clearSelection;
  text := s1+ s+ s2 ;
  selstart:= length(text)+1;
end;

procedure TCommandLineEditor.activate;
begin
  setfocus;
  SelStart:= length(text)+1;
  keybd_event(VK_RIGHT,0,0,0);
  keybd_event(VK_RIGHT,0,Keyeventf_keyup,0);
end;

function TCommandLineEditor.doCommand(const cmd:string): boolean;
begin
  activate;
  text :=cmd;
  keybd_event(VK_RETURN,0,0,0);
  keybd_event(VK_RETURN,0,Keyeventf_keyup,0);
  application.ProcessMessages;
end;

function TCommandLineEditor.LoadHistoryFromFile(const fn: String): boolean;
begin
  result:=false;
  if not fileexists(fn) then
     raise exception.createfmt('File not found %s',[fn]);
  fhistory.clear;
  try
    fhistory.LoadFromFile(fn)
  except
     result:=false;
     raise;
  end;
end;

function TCommandLineEditor.SaveHistoryToFile(const fn: String): boolean;
var
 Err : integer;
begin
  result:=false;
  if fhistory.count< 1 then exit;
  if fileexists(fn) then
  begin
  try
    Err:=CanReadWriteFile(fn);
    CheckfileError(Err);
  except
  end;
//    if Err<> '' then
//      raise exception.createfmt('Error opening file %s%s %s',[fn,#13#10,Err]);
  end;
  try
    fhistory.SaveToFile(fn)
  except
     result:=false;
     raise;
  end;
end;

procedure TCommandLineEditor.Cancel;
var
Shift: TShiftState;
key : word;
begin
  key:=VK_ESCAPE;
  KeyDown(key,[]);
end;

function TCommandLineEditor.HackHistory(PrevCommand, NewCommand: string): boolean;
begin
  if fHistory.Count = 0 then
    fHistory.Add(NewCommand)
  else if Copy(AnsiUpperCase(fHistory[fHistory.Count-1]), 1, length(prevcommand)) = AnsiUpperCase(PrevCommand) then
  begin
    fHistory.Delete(fHistory.Count-1);
    fHistory.Add(NewCommand);
  end;
end;

function TCommandLineEditor.GetWords(FromWord, ToWord: integer): string;
// Function that returns a string from the word no. FromWord to the word
// ToWord, both inclusive. Eg. GetWords(1,2) return the first two word of
// the commandline. A word is define to be the characters between spaces.
var
  i, j, k: integer;
begin
  result := '';
  if length(text)<1 then exit;
  i := 1;
  // Ignore any leading blanks.
  while ((text[i] <> ' ') and (i < length(trim(text)) )) do inc(i);

  // Ignore the first FromWord -1 words.
  for j := 2 to FromWord do
  begin
    while text[i] <> ' ' do inc(i);
    while text[i] = ' ' do inc(i);
  end;

  k := i;
  if fromword = 1 then i := 1;   // If looking for first word:

  // Now find the last word we need to include
  for j := FromWord to ToWord do
  begin
    while text[k] <> ' ' do inc(k);
    while text[k] = ' ' do inc(k);
  end;
  result := trim(copy(text, i, k));
end;

end.
