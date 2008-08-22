unit uhtmlutils;

interface

uses
  Windows, SysUtils, Classes;

procedure CopyHTMLToClipBoard(HTML: string);

implementation

var
  cf :word;
{ ************************************************************************* }

function RightS(Txt: string; Size: Integer): string;
{ Returns <Size> chars from right of <Txt> }
begin
  if Size > Length(Txt) then
    Size := Length(Txt);
  Result := Copy(Txt, Length(Txt) - Size + 1, Size);
end;

{ ************************************************************************* }

function LeftS(Txt: string; Size: Integer): string;
{ Returns <Size> chars from left of <Txt> }
begin
  Result := Copy(Txt, 1, Size);
end;

{ ************************************************************************* }

function MidS(Txt: string; Pos, Size: Integer): string;
{ Returns <Size> chars from  <Txt>, starting at <Pos> }
begin
  Result := Copy(Txt, Pos, Size)
end;

{ ************************************************************************* }

function BeginS(Txt: string; Size: Integer): string;
{ Returns chars from beginning up to <Size> chars from end of <Txt> }
begin
  if Length(Txt) >= Size then
    Result := Copy(Txt, 1, (Length(Txt) - Size))
  else
    Result := '';
end;

{ ************************************************************************* }

function EndS(Txt: string; Posn: Integer): string;
{ Returns all chars from <Pos> to end of <Txt> }
begin
  if (Posn <= Length(Txt)) then
    Result := Copy(Txt, Posn, Length(Txt) - Posn + 1)
  else
    Result := '';
end;


// Copy buffer to clipboard in specified format

procedure SetClipBuffer(Format: Integer; var Buffer; Size: Integer);
var
  Data: THandle;
  DataPtr: Pointer;
begin
  Data := GlobalAlloc(GMEM_MOVEABLE + GMEM_DDESHARE, Size);
  try
    DataPtr := GlobalLock(Data);
    try
      Move(Buffer, DataPtr^, Size);
      SetClipboardData(Format, Data);
    finally
      GlobalUnlock(Data);
    end;
  except
    GlobalFree(Data);
    raise;
  end;
end;

{ ************************************************************************* }

procedure CopyHTMLToClipBoard(HTML: string);
var
  StartH, EndH, StartF, EndF: Integer;
  Fragment: string;

  function MakeFragment: string;
  // Helper routine to build a properly-formatted HTML fragment
  begin
    Result := 'Version:0.9'#13#10;
    Result := Result + 'StartHTML:' + Format('%8.8d', [StartH]) + #13#10;
    Result := Result + 'EndHTML:' + Format('%8.8d', [EndH]) + #13#10;
    Result := Result + 'StartFragment:' + Format('%8.8d', [StartF]) + #13#10;
    Result := Result + 'EndFragment:' + Format('%8.8d', [EndF]) + #13#10;
    Result := Result + '<html><body>'#13#10;
    //Result := Result + '<Table>'#13#10;
    Result := Result + '<!--StartFragment-->'#13#10;
    Result := Result + Trim(HTML) + #13#10;
    Result := Result + '<!--EndFragment-->'#13#10;
    //Result := Result + '</Table>'#13#10;
    Result := Result + '</body></html>';
    StartH := Pos('<html>', Result) - 1;
    EndH := Length(Result);
    StartF := Pos('<!--S', Result) - 1;
    EndF := Pos('<!--E', Result) - 1;
  end;

// The actual copier
begin
 // Try to open clipboard
  if OpenClipboard(0) then
  begin
    try
     // Empty clipboard
      EmptyClipboard;
      // Build HTML fragment
      MakeFragment;
      // Return fragment with correct byte offsets embedded
      Fragment := MakeFragment;
      // Send fragment to clipboard
      SetClipBuffer(cf, Fragment[1], Length(Fragment) + 1);
    finally
     // Close clipboard
      CloseClipboard;
    end;
  end;
end;

{
procedure CopyHTMLasTabbedText(HTML: string);
var
  StartH, EndH, StartF, EndF: Integer;
  Fragment: string;
  p,q: pchar;
  inside:integer;
begin
  if html<>'' then
  begin
   p :=pchar(html);
   while p^<>#0 do
    case p^ of
    '<':
    begin
      inside:=1;
      q:=p;
    end;
    '>':
    begin
      if inside=1 then

      q:=p;
    end;
    end;
    inc(p);
   end;
  end;
  if OpenClipboard(0) then
  begin
    try
     // Empty clipboard
      EmptyClipboard;
      // Return fragment with correct byte offsets embedded
      // Send fragment to clipboard
      SetClipBuffer(RegisterClipboardFormat('HTML Format'),
        Fragment[1], Length(Fragment) + 1);
    finally
     // Close clipboard
      CloseClipboard;
    end;
  end;
end;
}

initialization
  cf:= RegisterClipboardFormat('HTML Format');
end.

