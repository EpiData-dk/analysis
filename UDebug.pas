unit UDebug;

interface

uses
  Classes;

type
  TDebug = class(TObject)
  private
    fDebugLevel: word;
    fData: TStringList;
    fIndentLevel: integer;
    procedure SetDebugLevel(Level: word);
    function AddIndent(): string;
  protected
    //
  public
    constructor Create(DebugLevel: word);
    destructor Destroy(); override;
    procedure IncIndent();
    procedure DecIndent();
    procedure Add(S: String; DebugLevel: Word); overload;
    procedure Add(UnitName, ClassName, ProcName, Version: string; DebugLevel: Word); overload;
    procedure SaveToDisk(FileName: string);
    property DebugLevel: word read fDebugLevel write SetDebugLevel;
  end;

var
  ODebug: TDebug;

implementation

uses SysUtils;

const
  MaxLevel: word = 5;

constructor TDebug.Create(DebugLevel: word);
begin
  fDebugLevel := DebugLevel;
  fIndentLevel := 0;
  fData := TStringList.Create();
end;

destructor TDebug.Destroy();
begin
  if Assigned(fData) then FreeAndNil(fData);
end;

function TDebug.AddIndent(): string;
var
  i: integer;
begin
  result := '';
  for i := 1 to fIndentLevel do
    result := result + ' '; 
end;

procedure TDebug.IncIndent();
begin
  inc(fIndentLevel);
end;

procedure TDebug.DecIndent();
begin
  Dec(fIndentLevel);
end;

procedure TDebug.Add(S: String; DebugLevel: Word);
begin
  if DebugLevel <= fDebugLevel then
    fData.Add(AddIndent + S);
end;

procedure TDebug.Add(UnitName, ClassName, ProcName, Version: string; DebugLevel: Word);
var
  s: string;
begin
  s := UnitName;
  if classname <> '' then
    s := s + '(' + classname + ')';
  s := s + ': ' + ProcName + ' - ' + version;
  Add(S, DebugLevel);
end;

procedure TDebug.SetDebugLevel(Level: word);
begin
  if Level > MaxLevel then
    fDebugLevel := MaxLevel
  else
    fDebugLevel := Level;
end;

procedure TDebug.SaveToDisk(FileName: string);
var
  TempList: TStringList;
begin
  if fData.Count = 0 then exit;
  try
    try
      templist := TStringList.Create();
      if FileExists(FileName) then
        templist.LoadFromFile(filename);
      templist.AddStrings(fData);
      templist.SaveToFile(filename);
      fData.Clear;
      fIndentLevel := 0;
    finally
      FreeAndNil(TempList);
    end;
  except
  end;
end;

end.

