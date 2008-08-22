unit UFileDir;

interface

uses
  SysUtils,WinTypes, WinProcs,Messages, Classes, Graphics,Controls,  Forms,
  Dialogs,  StdCtrls,  ExtCtrls;

type
  TOnFileFoundEvent = procedure(SearchRec: TSearchRec;var Stop:boolean) of object;
  TOnReadyEvent = procedure(SearchResult: TStringList) of object;

  TmFileScan = class(TComponent)
  private
    fOnFileFound: TOnFileFoundEvent;
    fOnReady: TOnReadyEvent;
    fSubDir: Boolean;
    fAbout: string;
    fStop: boolean;
    fFilePath: string;
    procedure SetAbout(Value: string);
    procedure Ready;
    function FindFile(const FileFilter: String): TStringList;
  { Private declarations }
  public
    SearchResult: TStringList;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Start;
    procedure Stop;
  { Private declarations }
  published
    property FilePath: string read fFilePath write fFilepath;
    property SubDirectories: Boolean read fSubDir write fSubDir;
    property OnFileFound: TOnFileFoundEvent read fOnFileFound write fOnFileFound;
    property OnReady: TOnReadyEvent read fOnReady write fOnReady;
    property About: string read fAbout write SetAbout;
  { Published declarations }
  end;


implementation

constructor TmFileScan.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  SearchResult:= TStringList.Create;
  fSubDir:= true;
end;

destructor TmFileScan.Destroy;
begin
  SearchResult.Free;
  inherited Destroy;
end;


procedure TmFileScan.Ready;
begin
  if Assigned(fOnReady) then fOnReady(SearchResult);
end;

procedure TmFileScan.SetAbout(Value: string);
begin
  Exit;
end;


procedure TmFileScan.Start;
begin
  fStop:= false;
  SearchResult.Clear;
  SearchResult:= FindFile(fFilePath);
  Ready;
end;

function TmFileScan.FindFile(const FileFilter: String): TStringList;
var
  spec: string;
  list: TStringList;
  n: integer;

  procedure RFindFile(const folder: TFileName);
  var
    SearchRec: TSearchRec;
  begin
  // Locate all matching files in the current
  // folder and add their names to the list
    if FindFirst(folder , faAnyFile, SearchRec) = 0 then begin
      try
        repeat
          Application.ProcessMessages;
          if (SearchRec.Attr and faDirectory = 0) or
            (SearchRec.Name <> '.') and (SearchRec.Name <> '..') then
          begin
            list.Add(SearchRec.Name);
            if Assigned(fOnFileFound) then fOnFileFound(SearchRec,fStop);
          end;
        until (FindNext(SearchRec) <> 0) or fStop;
      except
        SysUtils.FindClose(SearchRec);
        raise;
      end;
      SysUtils.FindClose(SearchRec);
    end;
  // Now search the subfolders
    if fSubDir then
    begin
      if FindFirst(folder + '*.*', faAnyFile, SearchRec) = 0 then
      begin
        try
          repeat
            if ((SearchRec.Attr and faDirectory) <> 0) and
              (SearchRec.Name <> '.') and (SearchRec.Name <> '..') then
              RFindFile(folder + SearchRec.Name + '\');
          until (FindNext(SearchRec) <> 0) or fStop;
        except
          SysUtils.FindClose(SearchRec);
          raise;
        end;
        SysUtils.FindClose(SearchRec);
      end;
    end;
  end; // procedure RFindFile inside of FindFile

begin // function FindFile
  list:= TStringList.Create;
  try
    RFindFile(fFilePath);
    Result:= list;
  except
    list.Free;
    raise;
  end;
end;


procedure TmFileScan.Stop;
begin
  fStop:= true;
end;

end.

