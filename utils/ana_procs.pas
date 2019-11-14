unit ana_procs;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, forms, ExtCtrls;

function SaveRecentFilesToIni(Const FileName: string; RecentList: TStrings): boolean;
function LoadRecentFilesFromIni(Const FileName: string; RecentList: TStrings): boolean;
procedure AddToRecent(Const AFilename, RecentFileName: string; RecentList: TStrings);
function GetRecentDataIniFileName: string;
function GetRecentPGMIniFileName: string;

function GetIniFileName: string;
function GetStartupPgm: string;

procedure SaveFormPosition(Const AForm: TCustomForm; Const SectionName: string);
procedure LoadFormPosition(const AForm: TCustomForm; Const SectionName: string);
procedure DeleteFormPosition(Const SectionName: string);
procedure SaveSplitterPosition(Const ASplitter: TSplitter; Const SectionName: string);
procedure LoadSplitterPosition(ASplitter: TSplitter; Const SectionName: string);

procedure ParseCommandLineOpts;

var
  RecentDataFiles: TStringList;
  RecentPGMFiles: TStringList;

const
  MaxRecentFiles = 30;


implementation

uses
  IniFiles, LazFileUtils, LazUTF8, FileUtil, epiversionutils, Dialogs;

var
  IniFileName: string = '';
  RecentDataIniFileName: string = '';
  RecentPGMIniFileName: string = '';

function GetIniFile(Const FileName: String): TIniFile;
begin
  result := TIniFile.Create(UTF8ToSys(FileName));
end;

function SaveRecentFilesToIni(const FileName: string; RecentList: TStrings
  ): boolean;
var
  Ini: TIniFile;
  i: Integer;
begin
  Result := false;

  try
    Ini := GetIniFile(FileName);

    for i := 0 to RecentList.Count - 1 do
      Ini.WriteString('Files', 'file' + inttostr(i), RecentList[i]);
  finally
    Ini.Free;
  end;
end;

function LoadRecentFilesFromIni(const FileName: string; RecentList: TStrings
  ): boolean;
var
  Ini: TIniFile;
  Sec: String;
  i: Integer;
  S: String;
begin
  Result := false;

  try
    Ini := GetIniFile(FileName);
    RecentList.Clear;

    // Read recent files.
    Sec := 'Files';
    I := 0;
    while Ini.ValueExists(Sec, 'file'+inttostr(i)) do
      begin
        RecentList.Add(Ini.ReadString(Sec, 'file'+inttostr(i), ''));
        Inc(i);
      end;
  finally
    Ini.Free;
  end;
end;

function GetRecentDataIniFileName: string;
begin
  // IniFileName has been set during ParCommandLineOpts if
  // it was part of the startup.
  // else set the default path! (only first time required).

  if RecentDataIniFileName = '' then
    RecentDataIniFileName := ExpandFileNameUTF8(GetAppConfigDirUTF8(False) + '..' + PathDelim + 'epidatarecentfiles.ini');

  Result := RecentDataIniFileName;
end;

function GetRecentPGMIniFileName: string;
begin
  // IniFileName has been set during ParCommandLineOpts if
  // it was part of the startup.
  // else set the default path! (only first time required).

  if RecentPGMIniFileName = '' then
    RecentPGMIniFileName := ExpandFileNameUTF8(GetAppConfigDirUTF8(False) + '..' + PathDelim + 'epidatapgmrecentfiles.ini');

  Result := RecentPGMIniFileName;
end;

procedure AddToRecent(const AFilename, RecentFileName: string;
  RecentList: TStrings);
var
  Idx: Integer;
  Fn: String;
begin
  Fn := ExpandFileNameUTF8(AFilename);

  Idx := RecentList.IndexOf(FN);
  if (Idx >= 0) then
    RecentList.Move(Idx, 0)
  else
    RecentList.Insert(0, Fn);

  if RecentList.Count > MaxRecentFiles then
    RecentList.Delete(MaxRecentFiles);

  SaveRecentFilesToIni(RecentFileName, RecentList);
end;

function GetIniFileName: string;
var
  S: string;
begin
  // IniFileName has been set during ParCommandLineOpts if
  // it was part of the startup.
  // else set the default path! (only first time required).
  if IniFileName = '' then
  begin
    IniFileName := GetAppConfigFileUTF8(false,
      {$IFDEF windows}
      false,
      {$ELSE}
      true,
      {$ENDIF}
      true
      );
  end;

  Result := IniFileName;
end;

function GetStartupPgm: string;
begin
  Result := GetAppConfigDirUTF8(false, true) + 'startup.pgm';
end;

procedure SaveFormPosition(const AForm: TCustomForm; const SectionName: string);
var
  Ini: TIniFile;
begin
  try
    Ini := GetIniFile(GetIniFileName);
    with Ini, AForm do
    begin
      WriteInteger(SectionName, 'Top', Top);
      WriteInteger(SectionName, 'Left', Left);
      WriteInteger(SectionName, 'Width', Width);
      WriteInteger(SectionName, 'Height', Height);
    end;
  finally
    Ini.Free;
  end;
end;

procedure LoadFormPosition(const AForm: TCustomForm; const SectionName: string);
var
  Ini: TIniFile;
begin
  try
    Ini := GetIniFile(GetIniFileName);
    with Ini, AForm do
    begin
      LockRealizeBounds;
      Top     := ReadInteger(SectionName, 'Top', Top);
      Left    := ReadInteger(SectionName, 'Left', Left);
      Width   := ReadInteger(SectionName, 'Width', Width);
      Height  := ReadInteger(SectionName, 'Height', Height);
      UnlockRealizeBounds;
    end;
  finally
    Ini.Free;
  end;
end;

procedure DeleteFormPosition(const SectionName: string);
var
  Ini: TIniFile;
begin
  try
    Ini := GetIniFile(GetIniFileName);
    Ini.EraseSection(SectionName);
  finally
    Ini.Free;
  end;
end;

procedure SaveSplitterPosition(const ASplitter: TSplitter;
  const SectionName: string);
var
  Ini: TIniFile;
begin
  try
    Ini := GetIniFile(GetIniFileName);
    Ini.WriteInteger(SectionName, 'SplitterPosition', ASplitter.GetSplitterPosition);
  finally
    Ini.Free;
  end;
end;

procedure LoadSplitterPosition(ASplitter: TSplitter; const SectionName: string);
var
  Ini: TIniFile;
begin
  try
    Ini := GetIniFile(GetIniFileName);
    ASplitter.SetSplitterPosition(
      Ini.ReadInteger(SectionName, 'SplitterPosition', ASplitter.GetSplitterPosition)
    );
  finally
    Ini.Free;
  end;
end;

procedure ParseCommandLineOpts;
const
  IniFile =            '--inifile';
  IniFileShort =       '-i';
  ShowHelp =           '--help';
  ShowHelpShort =      '-h';
  ShowVersion =        '--version';
  ShowVersionShort =   '-v';

  function ParseLine(Const Param, Option: string; var Value: string): boolean;
  begin
    Result := false;
    if LeftStr(Param, Length(Option)) = Option then
    begin
      Result := true;
      Value := Copy(Param, Length(Option) + 2, Length(Param));
    end;
  end;

  procedure DoOutputText(Const AText: string);
  begin
    if TextRec(Output).Mode = fmClosed then
      MessageDlg('Information:', AText, mtInformation, [mbOk], 0)
    else
      WriteLn(UTF8ToConsole(AText));
  end;

  procedure DoShowHelp;
  var
    HText: TStringList;
  begin
    HText := TStringList.Create;

    HText.Add('Usage:');
    HText.Add(ParamStrUTF8(0) + ' [OPTIONS]');
    HText.Add('');
    HText.Add('Options:');
    HText.Add('-h or --help             Show this help and exit.');
    HText.Add('-v or --version          Show version info and exit.');
    HText.Add('');
    HText.Add('-i or --inifile [FILE]   Uses [FILE] as startup program.');
    HText.Add('                         If no location is specified startup.pgm is used.');
    HText.Add('');
    HText.Add('Example:');
    {$IFDEF MSWINDOWS}
    HText.Add(ParamStrUTF8(0) + ' -i C:\path\to\startup.pgm');
    {$ELSE}
    HText.Add(ParamStrUTF8(0) + ' -i /path/to/startup.pgm');
    {$ENDIF}
    DoOutputText(HText.Text);
    HText.Free;
  end;

  procedure DoShowVersion;
  begin
    DoOutputText(GetEpiVersionInfo(HINSTANCE));
  end;

var
  i: Integer;
  S, P: string;
begin
  for i := 1 to Paramcount do
  begin
    P := ParamStrUTF8(i);

    if ParseLine(P, ShowHelp, S) or
       ParseLine(P, ShowHelpShort, S)
    then
    begin
      DoShowHelp;
      halt(0);
    end;

    if ParseLine(P, ShowVersion, S) or
       ParseLine(P, ShowVersionShort, S)
    then
    begin
      DoShowVersion;
      Halt(0);
    end;
  end;
end;

initialization
begin
  RecentDataFiles := TStringList.Create;
  RecentDataFiles.CaseSensitive := true;

  RecentPGMFiles := TStringList.Create;
  RecentPGMFiles.CaseSensitive := true;
end;

finalization
begin
  RecentDataFiles.Free;
  RecentPGMFiles.Free;
end;

end.

