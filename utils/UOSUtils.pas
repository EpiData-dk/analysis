unit UOSUtils;

interface

uses
  Windows, forms, Classes, SysUtils,SMutils;

function CreateDOSProcessRedirected(const CommandLine,InputFile,OutputFile:string;
var ExitCode:cardinal):boolean;
function ExecSySCommand(const command:string;var outfilename:string):integer ;
function Dos2Win(CmdLine:String;var Output:string):integer;
function WinFileExec(const aCmdLine: String; aHide, aWait: Boolean): Boolean;

implementation

function CreateDOSProcessRedirected(const CommandLine,InputFile,OutputFile:string;
var ExitCode:DWORD):boolean;
const
  ROUTINE_ID = '[function: CreateDOSProcessRedirected ]';
var
//  OldCursor     : TCursor;
  pCommandLine  : array[0..MAX_PATH] of char;
  pInputFile,
  pOutPutFile   : array[0..MAX_PATH] of char;
  StartupInfo   : TStartupInfo;
  ProcessInfo   : TProcessInformation;
  SecAtrrs      : TSecurityAttributes;
  hAppProcess,
  hAppThread,
  hInputFile,
  hOutputFile   : THandle;
begin
  Result := False;
  if not FileExists(InputFile) then
    raise Exception.CreateFmt(ROUTINE_ID+#10+#10+'Input file * %s *'+#10+
                              'does not exist'+#10+#10,[InputFile]);
  StrPCopy(pCommandLine, CommandLine);
  StrPCopy(pInputFile, InputFile);
  StrPCopy(pOutPutFile, OutputFile);
  try
    FillChar(SecAtrrs, SizeOf(SecAtrrs), #0);
    SecAtrrs.nLength              := SizeOf(SecAtrrs);
    SecAtrrs.lpSecurityDescriptor := nil;
    SecAtrrs.bInheritHandle       := True;
    hInputFile := CreateFile(pInputFile,GENERIC_READ or GENERIC_WRITE,
                             FILE_SHARE_READ or FILE_SHARE_WRITE,@SecAtrrs,
                             OPEN_ALWAYS,FILE_ATTRIBUTE_NORMAL
                             or FILE_FLAG_WRITE_THROUGH,0);
    if hInputFile = INVALID_HANDLE_VALUE then
      raise Exception.CreateFmt(ROUTINE_ID+#10+#10+
                                'WinApi function CreateFile returned an' +
                                'invalid handle value'  + #10 +
                                'for the input file * %s *' + #10 + #10, [InputFile]);
    hOutputFile := CreateFile(pOutPutFile,GENERIC_READ or GENERIC_WRITE,
                              FILE_SHARE_READ or FILE_SHARE_WRITE,@SecAtrrs,
                              CREATE_ALWAYS,FILE_ATTRIBUTE_NORMAL
                              or FILE_FLAG_WRITE_THROUGH,0);
    if hOutputFile = INVALID_HANDLE_VALUE then
      raise Exception.CreateFmt(ROUTINE_ID+#10+#10+
                                'WinApi function CreateFile returned an' +
                                'invalid handle value'  + #10 +
                                'for the output file * %s *' + #10 + #10 , [OutputFile]);
    FillChar(StartupInfo, SizeOf(StartupInfo), #0);
    StartupInfo.cb          := SizeOf(StartupInfo);
    StartupInfo.dwFlags     := STARTF_USESHOWWINDOW {or STARTF_USESTDHANDLES};
    StartupInfo.wShowWindow := SW_SHOWNORMAL	;//SW_HIDE;
//    StartupInfo.hStdOutput  := hOutputFile;
//    StartUpInfo.hStdError   := hOutputFile;
//    StartupInfo.hStdInput   := hInputFile;
    Result := CreateProcess(nil,pCommandLine,nil,nil,false,NORMAL_PRIORITY_CLASS ,
                            nil,nil,StartupInfo,ProcessInfo);
    if Result then
    begin
//      WaitforSingleObject(ProcessInfo.hProcess, INFINITE);
      WaitforInputIdle(ProcessInfo.hProcess, INFINITE);
      hAppProcess  := ProcessInfo.hProcess;
      hAppThread   := ProcessInfo.hThread;
      GetExitCodeProcess(hAppProcess,Exitcode);
    end
    else
      raise Exception.Create(ROUTINE_ID+#10+#10+
                             'Function failure'+#10+#10 +
                             GetLastErrorMsg);
  finally
    if hOutputFile <> 0 then CloseHandle(hOutputFile);
    if hInputFile <> 0 then CloseHandle(hInputFile);
    if hAppThread <> 0 then CloseHandle(hAppThread);
    if hAppProcess <> 0 then CloseHandle(hAppProcess);
//    Screen.Cursor:= OldCursor;
  end;
end;    { CreateDOSProcessRedirected }


function ExecSySCommand(const command:string;var outfilename:string):integer ;
var
s, p,path,d, infile,outfile : string;
exitcode:DWord;
goon :boolean;
begin
 infile :=SMGetTempfileName('.txt');
 outfile :=SMGetTempfileName('.txt');
 exitcode:=9999;
 CreateDOSProcessRedirected(command,infile,outfile,exitcode);
 result:=exitcode;
// if result=0 then
 outfilename:=outfile
{ else
   outfilename:='';}
end;


function Dos2Win(CmdLine:String;var Output:string):integer;
const BUFSIZE = 2000;
var SecAttr    : TSecurityAttributes;
    hReadPipe,
    hWritePipe : THandle;
    StartupInfo: TStartUpInfo;
    ProcessInfo: TProcessInformation;
    Buffer     : Pchar;
    WaitReason,
    BytesRead  : DWord;
    s          :string;
begin

 with SecAttr do
 begin
   nlength              := SizeOf(TSecurityAttributes);
   binherithandle       := true;
   lpsecuritydescriptor := nil;
 end;
 // Creazione della pipe
 if Createpipe (hReadPipe, hWritePipe, @SecAttr, 0) then
 begin
   Buffer  := AllocMem(BUFSIZE + 1);    // Allochiamo un buffer di dimensioni BUFSIZE+1
   FillChar(StartupInfo, Sizeof(StartupInfo), #0);
   StartupInfo.cb          := SizeOf(StartupInfo);
   StartupInfo.hStdOutput  := hWritePipe;
   StartupInfo.hStdInput   := hReadPipe;
   StartupInfo.dwFlags     := STARTF_USESTDHANDLES +
                              STARTF_USESHOWWINDOW;
   StartupInfo.wShowWindow := SW_HIDE;

   if CreateProcess(nil, PChar(CmdLine),@SecAttr, @SecAttr,
      true, NORMAL_PRIORITY_CLASS,  nil,  nil, StartupInfo,
      ProcessInfo) then
     begin
       // Attendiamo la fine dell'esecuzione del processo
       repeat
         WaitReason := WaitForSingleObject( ProcessInfo.hProcess,100);
//TODO Replace Application.ProcessMessages with a form.pas indepdent version
         Application.ProcessMessages;
       until (WaitReason <> WAIT_TIMEOUT);
       // Leggiamo la pipe
       Repeat
         BytesRead := 0;
         // Leggiamo "BUFSIZE" bytes dalla pipe
         ReadFile(hReadPipe, Buffer[0], BUFSIZE, BytesRead, nil);
         // Convertiamo in una stringa "\0 terminated"
         Buffer[BytesRead]:= #0;
         // Convertiamo i caratteri da DOS ad ANSI
         OemToAnsi(Buffer,Buffer);
         // Scriviamo nell' "OutMemo" l'output ricevuto tramite pipe
         SetString(s,Buffer,BytesRead);
         output:=output+s;
       until (BytesRead < BUFSIZE);
     end;
   FreeMem(Buffer);
   CloseHandle(ProcessInfo.hProcess);
   CloseHandle(ProcessInfo.hThread);
   CloseHandle(hReadPipe);
   CloseHandle(hWritePipe);
 end;
end;


function WinFileExec(const aCmdLine: String; aHide, aWait: Boolean): Boolean;
var
  StartupInfo : TStartupInfo;
  ProcessInfo : TProcessInformation;
begin
  {setup the startup information for the application }
  FillChar(StartupInfo, SizeOf(TStartupInfo), 0);
  with StartupInfo do
  begin
    cb:= SizeOf(TStartupInfo);
    dwFlags:= STARTF_USESHOWWINDOW or STARTF_FORCEONFEEDBACK;
    if aHide then wShowWindow:= SW_HIDE
             else wShowWindow:= SW_SHOWNORMAL;
  end;

  Result := CreateProcess(nil,PChar(aCmdLine), nil, nil, False,
               NORMAL_PRIORITY_CLASS, nil, nil, StartupInfo, ProcessInfo);
  if aWait then
     if Result then
     begin
       WaitForInputIdle(ProcessInfo.hProcess, INFINITE);
       WaitForSingleObject(ProcessInfo.hProcess, INFINITE);
     end;
end;

end.
