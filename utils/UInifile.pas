unit UInifile;

interface

uses SysUtils, UVectors, classes, ansDataTypes, UVectorOp, UOutput, UCommands,
     Windows, UAnaToken, IniFiles, Forms;

// ============================================================================
// Declaration of:
// Main methodes, functions and procedures.
// ============================================================================

type

  TFormDefaults = record
    Section:        String;
    Top, Left,
    Width, Height:  Integer;
    Maximize:       Boolean;
  end;

  TInifile = class
  private
    // For internally available methods!
    Ini: TMemIniFile;
  protected
    //
  public
    // For Externaly available methods.
    constructor Create();
    destructor Destroy(); override;
    function Initialize(fn: string): boolean;
    procedure SaveCurrentForm(Form: TCustomForm; Section: string);
    procedure SaveForm(Section: string;top,left,width,height:Integer;max: Boolean);
    procedure LoadForm(Form: TCustomForm; Defaults: TFormDefaults);
    procedure SaveSplitter(Pos: integer);
    function LoadSplitter(): integer;
//    procedure SaveLanguage(value: string);
//    function LoadLanguage:string;
    function SectionExists(Section: string): boolean;
  end;


var
  OInifile: TInifile;

implementation

uses UCmdProcessor;

// ============================================================================
// Public methodes.
// ============================================================================


constructor TIniFile.Create();
begin
  Ini := nil;
end;

destructor TIniFile.Destroy();
begin
  if Assigned(Ini) then FreeAndNil(Ini);
end;

function TIniFile.Initialize(fn: String): boolean;
begin
  result := true;
  if Assigned(Ini) then FreeAndNil(Ini);
  if FileExists(fn) then
    Ini := TMemIniFile.Create(fn)
  else
    Ini := TMemIniFile.Create(extractfilepath(application.exename)+'epidatastatsetup.ini')
end;

procedure TIniFile.SaveCurrentForm(Form: TCustomForm; Section: string);
begin
  SaveForm(Section,Form.Top,Form.Left,Form.Width,Form.Height,Form.WindowState = wsMaximized);
  Ini.UpdateFile;
end;

procedure TiniFile.SaveForm(Section: string;top,left,width,height: integer;max: boolean);
begin
  Ini.WriteInteger(Section, 'Top', top);
  Ini.WriteInteger(Section, 'Left', Left);
  Ini.WriteInteger(Section, 'Width', Width);
  Ini.WriteInteger(Section, 'Height', Height);
  Ini.WriteBool(Section, 'Max', max);
  Ini.UpdateFile;
end;

procedure TIniFile.LoadForm(Form: TCustomForm; Defaults: TFormDefaults);
begin
  Form.Left := Ini.ReadInteger(Defaults.Section, 'Left', Defaults.Left);
  Form.Top := Ini.ReadInteger(Defaults.Section, 'Top', Defaults.Top);
  Form.Width := Ini.ReadInteger(Defaults.Section, 'Width', Defaults.Width);
  Form.Height := Ini.ReadInteger(Defaults.Section, 'Height', Defaults.Height);
  if Ini.ReadBool(Defaults.Section, 'Max', Defaults.Maximize) then
    Form.WindowState := wsMaximized
  else
    Form.WindowState := wsNormal;
end;

procedure TIniFile.SaveSplitter(Pos: integer);
begin
  Ini.WriteInteger('Main', 'Splitter', pos);
  Ini.UpdateFile;
end;

function TIniFile.LoadSplitter(): integer;
begin
  result := Ini.ReadInteger('Main', 'Splitter', 150)
end;

{procedure TIniFile.SaveLanguage(value:string);
begin
  Ini.WriteString('Language','Language',value);
end;

function TIniFile.LoadLanguage:string;
begin
  result:=Ini.ReadString('Language','Language','');
end;
 }
function TIniFile.SectionExists(Section: string): boolean;
begin
  result := Ini.SectionExists(Section);
end;

// ============================================================================
// Private/Protected methodes.
// ============================================================================

end.
