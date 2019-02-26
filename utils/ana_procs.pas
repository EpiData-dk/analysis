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

procedure SaveFormPosition(Const AForm: TForm; Const SectionName: string);
procedure LoadFormPosition(AForm: TForm; Const SectionName: string);
procedure SaveSplitterPosition(Const ASplitter: TSplitter; Const SectionName: string);
procedure LoadSplitterPosition(ASplitter: TSplitter; Const SectionName: string);

var
  RecentDataFiles: TStringList;
  RecentPGMFiles: TStringList;

const
  MaxRecentFiles = 30;


implementation

uses
  IniFiles, LazFileUtils, LazUTF8, FileUtil;

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

procedure SaveFormPosition(const AForm: TForm; const SectionName: string);
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

procedure LoadFormPosition(AForm: TForm; const SectionName: string);
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

