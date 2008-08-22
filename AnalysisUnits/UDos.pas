unit UDos;

interface

uses SysUtils, classes, Windows, UAnaToken, AAPasTok, UOutput, UCommands;

// ============================================================================
// Declaration of:
// Main methodes, functions and procedures.
// ============================================================================

type

  TDos = class
  private
    // For internally available methods!
    fNoOfFoundFiles : integer;
    fFileTable: TStatTable;
    function dir(const fn:string):boolean;
    procedure FileScanFileFound(SearchRec: TSearchRec;var Stop:boolean);
    function GetFullFilename(const path:string):string;
    function Erase(const fn:string):boolean;
    function DOS(const fn:string; commandID: word):boolean;
    function MkDir(const fn: string): boolean;
  protected
    // For internally available methods!
  public
    // For Externaly available methods.
    destructor Destroy(); override;
    function DOSFileAction(fn:string; action: word):boolean;
    function CD(const fn:string):boolean;
    function copyfile(const old,new:string; cmd: TCommand):boolean;
  end;


var
  ODos: TDos;

implementation

uses UCmdProcessor, UFileDir, AnsDatatypes, Forms, cStrings, smUtils, cFileUtils,
     UOSUtils, StrUtils;


// ============================================================================
// Public methodes.
// ============================================================================

destructor TDos.Destroy();
begin
  if Assigned(fFileTable) then FreeAndNil(fFileTable);
end;

function TDos.DOSFileAction(fn:string; action: word):boolean;
begin
  try
   case action of
     opDIR: dir(fn);
     opcd: cd(fn);
     opErase: Erase(fn);
     opDOS, opDoDos : dos(fn, action);
     opMkDir: MkDir(fn);
   end;  //case
  except
    on E: Exception do
      dm.error('Exception occured: %s', [E.message], 103017);
  end;
end;

function TDos.copyfile(const old,new:string; cmd: TCommand):boolean;
var  NewFile: TFileStream;
    OldFile: TFileStream;
    opt: TEpiOption;
begin
  if new = old then
          dm.Error('New file must have new name', [], 112001);
  if not FileExists(old) then
    dm.Error('File: %s does not exist!', [old], 112002);
  if FileExists(new) and (cmd.ParamByName['REPLACE'] = nil) {and
     (dm.GetOptionValue('REPLACE DATAFILE', Opt) and (ansiuppercase(Opt.Value) = 'OFF'))} THEN
    dm.Error('Destination file exists - to replace: use option /replace or erase %s', [new], 112003);
  dm.Info('Copying from %s to %s', [old,new],  212001);
  try
    OldFile := TFileStream.Create(old, fmOpenRead or fmShareDenyWrite);
    NewFile := TFileStream.Create(New, fmCreate or fmShareDenyRead);
    try
      NewFile.CopyFrom(OldFile, OldFile.Size);
    finally
      FreeAndNil(NewFile);
    end;
  finally
    FreeAndNil(OldFile);
  end;
end;

function TDos.CD(const fn:string):boolean;
var
  path : string;
begin
  if fn='' then
    dm.PrintResult(GetCurrentDir)
  else
  begin
    path := trim(StrRemoveSurroundingQuotes(fn));
    if path='' then exit;
    if path='.' then path:='..';
    path := GetFullFilename(Path);
    try
      ChDir(path);
    except
      dm.Error(GetLastErrorMsg, [], 0);
    end;
  end;
  dm.CurrentDir := GetCurrentDir;
  dm.NotifyInterface(EpiRefreshDir,integer(pchar(dm.CurrentDir)),0);
//  dm.Info('To view graphs, please close logfile and open a new after change of folder - logopen');
end;

// ============================================================================
// Private/Protected methodes.
// ============================================================================

function TDos.MkDir(const fn: string): boolean;
var
  path: string;
  d: integer;
begin
  d := 0;
  if fn='' then
  begin
    dm.Error('Invald path', [], 112004);
    exit;
  end
  else
    path := trim(StrRemoveSurroundingQuotes(fn));
  if not DirectoryExists(path) then
  begin
    if not PathHasDriveLetter(path) then
      path := dm.CurrentDir + '\' + path;
    path := PathCanonical(path);
    if not ForceDirectories(Path) then
    begin
      dm.Error('Unable to create directory: %s', [path], 112005);
      d := 2;
    end
    else
      d := 1;
  end;
  dm.AddResult('$MkDir', EpiTyInteger, d, 0, 0);
end;

function TDos.DIR(const fn:string):boolean;
var
  FileScan: TmFileScan;
  path,ext : string;
  i,co : integer;
  haswildcards : boolean;

begin
  try
    fNoOfFoundFiles:=0;
    FileScan:= TmFileScan.Create(nil) ;
    FileScan.SubDirectories := false;
    FileScan.OnFileFound := FileScanFileFound;
    path:=StrRemoveSurroundingQuotes(fn);
    if path='' then
        path:= GetCurrentDir +'\*.*'
    else if dirExists(path) then
        path:= AdjustDirName(path) +'\*.*';
    FileScan.FilePath:=path;

    fFileTable := dm.OutputList.NewTable(3,1);
    fFileTable.TableType := sttSystem;
    fFileTable.Caption := 'Directory of '+ path;
    fFileTable.Cell[1,1] := 'Name';
    fFileTable.Cell[2,1] := 'Date';
    fFileTable.Cell[3,1] := 'Size';
    FileScan.Start;
    dm.CodeMaker.OutputTable(fFileTable,'');
    dm.Sendoutput;
  finally
    FileScan.free;
    if Assigned(fFileTable) then FreeAndNil(fFileTable);
    dm.AddResult('$FilesFound',EpiTyInteger,fNoOfFoundFiles,0,0);
  end;
end;

procedure TDos.FileScanFileFound(SearchRec: TSearchRec; var Stop: boolean);
var
  Attributes, NewAttributes: Word;
  filename,s: string;
  rows: integer;
begin
  if dm.Cancelled then
  begin
   dm.info('Command cancelled by user', [], 212002);
   stop := dm.Cancelled;
   exit;
  end;
  inc(fNoOfFoundFiles);
  dm.AddResult(format('$FN%d',[fNoOfFoundFiles]),EpiTyString,SearchRec.Name,0,0);

  fFileTable.AddRow;
  rows := fFileTable.RowCount;

  fFileTable.Cell[1,rows] := SearchRec.Name;
  fFileTable.Cell[2,rows] := DateTimeToStr(FileDateToDateTime(SearchRec.time));
  fFileTable.Cell[3,rows] := trim(format('%12.0n',[SearchRec.Size*1.0]));
  Application.ProcessMessages;
end;


function TDos.GetFullFilename(const path:string):string;
var
 path1:string;
begin
 path1 := trim(StrRemoveSurroundingQuotes(path));
 if path1 = '' then exit;
 Result := PathCanonical(path1, '\');
end;

function TDos.Erase(const fn:string):boolean;
var
  s : string;
begin
  s := StrRemoveSurroundingQuotes(fn);
  Result := SysUtils.DeleteFile(s);
  if not result then
    dm.info('File not found or could not be deleted: %s', [fn], 212003)
  else
    dm.Sendoutput;
end;

function TDos.DOS(const fn:string; commandID: word):boolean;
var
 output, param, fn2: String;
begin
  param := '/c ';
  fn2 := fn;
  if pos('/OPEN', AnsiUpperCase(fn)) > 0 then
  begin
    param := '/k ';
    delete(fn2, pos('/OPEN', AnsiUpperCase(fn)), 5);
  end;
  output := '';
  case commandID of
    opDos: ExecSySCommand('command.com /c ' + fn2, output);
    opDoDos: ExecSySCommand('cmd.exe ' + param + fn2, output);
  end;
  dm.info('Result <br> %s',  [output], 212004);
end;

end.
