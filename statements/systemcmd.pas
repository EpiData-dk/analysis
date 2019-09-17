unit systemcmd;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ast, executor, outputcreator, LazUTF8Classes, FileUtil,
  Forms;

type

  { TSystemCmd }

  TSystemCmd = class
  private
    FExecutor: TExecutor;
    FOutputCreator: TOutputCreator;
  { LS }
  private
    FLSList: TStringListUTF8;
    procedure LSEntryFound(FileIterator: TFileIterator);
  protected
    procedure DoCD(ST: TCustomStringCommand); virtual;
    procedure DoLS(ST: TCustomStringCommand); virtual;
    procedure DoTerm(ST: TCustomStringCommand); virtual;
    procedure DoErase(ST: TCustomStringCommand); virtual;
  public
    constructor Create(AExecutor: TExecutor; AOutputCreator: TOutputCreator);
    destructor Destroy; override;
    procedure ExecSystemCmd(ST: TCustomStringCommand);
  end;

implementation

uses
  ast_types, LazFileUtils, UTF8Process, process,
  datamodule;

type
  PSearchRec = ^TSearchRec;

{ TSystemCmd }

procedure TSystemCmd.LSEntryFound(FileIterator: TFileIterator);
var
  PRec: PSearchRec;
begin
  PRec := new(PSearchRec);
  PRec^ := FileIterator.FileInfo;

  FLSList.AddObject(PRec^.Name, TObject(PRec));
end;

procedure TSystemCmd.DoCD(ST: TCustomStringCommand);
var
  S: UTF8String;
  Res: TDMFileResult;
begin
  S := '';

  if Assigned(ST.StringExpr) then
    S := ExpandFileName(ST.StringExpr.AsString);  //permits use of ~ for home directory on MacOS
//    S := ST.StringExpr.AsString;

  if (S = '') then
    begin
      if aDM.OpenDirectory(S) = dfrCanceled
      then
        begin
          FOutputCreator.DoInfoAll('CD canceled!');
          ST.ExecResult := csrFailed;
          Exit;
        end;
    end;

  if DirectoryExistsUTF8(S) then
    begin
      SetCurrentDirUTF8(S);
      FOutputCreator.DoInfoAll(GetCurrentDirUTF8);
    end
  else
    begin
      FExecutor.Error('"' + S + '"' + ' does not exist');
      ST.ExecResult := csrFailed;
    end;
end;

procedure TSystemCmd.DoLS(ST: TCustomStringCommand);
var
  FS: TFileSearcher;
  S: UTF8String;
  T: TOutputTable;
  i: Integer;
  PRec: PSearchRec;
begin
  S := '';

  if Assigned(ST.StringExpr) then
    S := ExpandFileName(ST.StringExpr.AsString);  //permits use of ~ for home directory on MacOS
//    S := ST.StringExpr.AsString;

  if (S = '') then
    S := GetCurrentDirUTF8;

  if (not DirectoryExistsUTF8(S)) then
    begin
      FOutputCreator.DoError('"' + S + '"' + ' does not exist');
      ST.ExecResult := csrFailed;
      Exit;
    end;

  FLSList := TStringListUTF8.Create;
  FLSList.Sorted := true;

  FS := TFileSearcher.Create;
  FS.OnDirectoryFound := @LSEntryFound;
  FS.OnFileFound      := @LSEntryFound;
  FS.Search(S, '', false);
  FS.Free;

  T := FOutputCreator.AddTable;
  T.RowCount := FLSList.Count + 1;
  T.ColCount := 4;

  T.Cell[0,0].Text := '{\b Name}';
  T.SetColAlignment(0, taLeftJustify);

  T.Cell[1,0].Text := '{\b Type}';
  T.SetColAlignment(1, taLeftJustify);

  T.Cell[2,0].Text := '{\b Date}';
  T.SetColAlignment(2, taLeftJustify);

  T.Cell[3,0].Text := '{\b Size}';
  T.SetColAlignment(3, taRightJustify);

  T.Cell[3,0].Alignment := taLeftJustify;

  for i := 0 to FLSList.Count - 1 do
    begin
      PRec := PSearchRec(FLSList.Objects[i]);

      T.Cell[0, i + 1].Text := FLSList[i];
      T.Cell[2, i + 1].Text := DateTimeToStr(FileDateToDateTime(PRec^.Time));
      T.Cell[3, i + 1].Text := IntToStr(PRec^.Size);

      if (PRec^.Attr and faDirectory) = faDirectory then
        T.Cell[1, i + 1].Text := 'D'
      else
        T.Cell[1, i + 1].Text := 'F';
    end;
end;

procedure TSystemCmd.DoTerm(ST: TCustomStringCommand);
var
  AProcess: TProcessUTF8;
  OutputStream: TStringStream;
  Buffer: array[1..2048] of byte;
  BytesRead: LongInt;
  I: Integer;
  L: TOutputLine;
begin
  AProcess := TProcessUTF8.Create(nil);
  // The commands for Windows and *nix are different hence the $IFDEFs
  {$IFDEF Windows}
    // In Windows the dir command cannot be used directly because it's a build-in
    // shell command. Therefore cmd.exe and the extra parameters are needed.
    AProcess.Executable := 'c:\windows\system32\cmd.exe';
    AProcess.Parameters.Add('/k');
    AProcess.Parameters.Add('dir c:\windows');
  {$ENDIF Windows}

  {$IFDEF Unix}
    AProcess.Executable := DetectXTerm;
    AProcess.Parameters.Add('-e "ls -l"');
{    AProcess.Parameters.Add('--recursive');
    AProcess.Parameters.Add('--all');
    AProcess.Parameters.Add('-l');
    AProcess.Parameters.Add('/');}
  {$ENDIF Unix}

  // Process option poUsePipes has to be used so the output can be captured.
  // Process option poWaitOnExit can not be used because that would block
  // this program, preventing it from reading the output data of the process.
  AProcess.Options := [];
//  AProcess.StartupOptions := [suoUseShowWindow];
  AProcess.InheritHandles := false;
  AProcess.ShowWindow := swoShowDefault;
  AProcess.CurrentDirectory := GetCurrentDirUTF8;

  // Copy default environment variables including DISPLAY variable for GUI application to work
  for I := 1 to GetEnvironmentVariableCount do
    AProcess.Environment.Add(GetEnvironmentString(I));

  // Start the process (run the dir/ls command)
  AProcess.Execute;

  // Create a stream object to store the generated output in. This could
  // also be a file stream to directly save the output to disk.
 { OutputStream := TStringStream.Create('');

  // All generated output from AProcess is read in a loop until no more data is available
  repeat
    // Get the new data from the process to a maximum of the buffer size that was allocated.
    // Note that all read(...) calls will block except for the last one, which returns 0 (zero).
    BytesRead := AProcess.Output.Read(Buffer, 2048);

    // Add the bytes that were read to the stream for later usage
    OutputStream.Write(Buffer, BytesRead)

  until BytesRead = 0;  // Stop if no more data is available

  L := FOutputCreator.AddLine;
  L.Text := OutputStream.DataString;    }

  // The process has finished so it can be cleaned up
  AProcess.Free;

  // Clean up
//  OutputStream.Free;
end;

procedure TSystemCmd.DoErase(ST: TCustomStringCommand);
var
  S: String;
begin
  S := '';

  if Assigned(ST.StringExpr) then
    S := ExpandFileName(ST.StringExpr.AsString);  //permits use of ~ for home directory on MacOS
//    S := ST.StringExpr.AsString;

  if FileExistsUTF8(S) then
    begin
      if (not DeleteFileUTF8(S)) then
        begin
          FExecutor.Error('Unable to delete "' + S + '"');
          ST.ExecResult := csrFailed;
          Exit;
        end;

      FOutputCreator.DoInfoAll(S + ' deleted');
      ST.ExecResult := csrSuccess;
    end
  else
    begin
      FExecutor.Error('"' + S + '"' + ' does not exist');
      ST.ExecResult := csrFailed;
    end;
end;

constructor TSystemCmd.Create(AExecutor: TExecutor;
  AOutputCreator: TOutputCreator);
begin
  FExecutor := AExecutor;
  FOutputCreator := AOutputCreator;
end;

destructor TSystemCmd.Destroy;
begin
  inherited Destroy;
end;

procedure TSystemCmd.ExecSystemCmd(ST: TCustomStringCommand);
begin
  Case ST.StatementType of
    stCD:
      DoCD(ST);

    stLS:
      DoLS(ST);

    stTerm:
      DoTerm(ST);

    stErase:
      DoErase(ST);
  end;
end;

end.

