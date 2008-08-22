unit uepifile;

interface

uses
  Controls,SysUtils,classes,windows, AARecFil,db, ansDataTypes,UEpidataTypes, SMUtils, UCmdTypes;

const
     _OK        = 1;
     _DELETED   = 0;
     _EOF       = -1;
     _BOF       = -2;
     _NEWREC    = -3;
     _RECNOTFOUND =-3;
     FILEWIDTH  = 78;

const
 DELStr:pchar=#63#13#10;   //?
 EOLStr:pchar=#33#13#10;   //!

type
  Float       = single;


TEpiInfoCustomUserData=class
  private
    fbackcolor: integer;
    fPassword: string;
    fDataLabel: string;
    fPreEnteredKey: String;
    fVLAB: boolean;
    fMaxRow: integer;
    FGlobalMissingValues:    TMissingValues;  //used to store 0..2 global values that are interpreted as missing values
    Function  GetGlobalMissingValues(Index: Integer):String;
    Procedure SetGlobalMissingValues(Index: Integer; Value:string);
public
   constructor Create(const pDataLabel,pPassword: string;pbackcolor:integer);
   property DataLabel : string read fDataLabel write fDataLabel;
   property Password : string read fPassword write fPassword;
   property backcolor : integer read  fbackcolor write fbackcolor;
   property EpiInfoFieldNaming : boolean read  fVLAB write fVLAB;
   property MaxRow : integer read fMaxRow write fMaxRow;
   property GlobalMissingValues[Index:integer]:string read GetGlobalMissingValues write SetGlobalMissingValues;
end;

TEpiInfoCustomFieldUserData=class
 private
    fbackcolor: integer;
    fPassword: string;
    fDataLabel: string;
    fVLAB: boolean;
    fFieldChar: Char;
    fFieldType: TEntryType;
    fLabelCol: integer;
    fFieldRow: integer;
    fLabelRow: integer;
    fFieldCol: integer;
    fLabelColor: word;
    fFieldColor: word;
    FLegal:           String;       //Legal values (incl. RANGE values)    MIB 18/5-07
    FRangeDefined:    Boolean;      //True if FLegal includes a Range definition  MIB 18/5-07
    FMax:             String;       //Max value in RANGE
    FMin:             String;       //Min value in RANGE
    FMissingValues: TMissingValues;    //used to store 0..2 values that are interpreted as missing values
    FHasGlobalMissingValues: boolean;  //true if field uses the globaly defined missing values
    Function  GetMissingValues(Index: Integer):String;
    Procedure SetMissingValues(Index: Integer; Value:string);
public
   constructor Create;
   property  LabelCol   : integer read fLabelCol write fLabelCol;
   property  LabelRow   : integer read fLabelRow write fLabelRow;
   property  LabelColor : word  read fLabelColor write fLabelColor;
   property  FieldCol   : integer  read fFieldCol write fFieldCol;
   property  FieldRow   : integer read fFieldRow write fFieldRow;
   property  FieldColor : word read fFieldColor write fFieldColor;
   property  FieldChar  : Char read fFieldChar write fFieldChar;
   property  FieldType: TEntryType read fFieldType write fFieldType;
   property  MissingValues[Index:integer]:string read GetMissingValues write SetMissingValues;
   property  HasGlobalMissingValues:boolean read FHasGlobalMissingValues write FHasGlobalMissingValues;
   property  Legal:string read FLegal write FLegal;
   property  RangeDefined:Boolean read FRangeDefined write FRangeDefined;
   property  Max:string read FMax write FMax;
   property  Min:string read FMin write FMin;
end;


  TEpiField=class;
  TEpiFile=class;


TLabelValueList=class(TStringList)
private
  fLabelName: string;
  function  GetCode(const aLabel: string): string;
  function  GetLabel(const aCode: string): string;
  procedure SetCode(const aLabel, Value: string);
  procedure SetLabel(const aCode, Value: string);
public
  constructor Create;
  Destructor  Destroy;override;
  procedure   clear;override;
  procedure   Assign(Source: TPersistent); override;
  function    AddPair(const Code,clabel:string):integer;
  procedure   Clone(var dest: TLabelValueList);
  procedure   CopyToLabelRec(var aLabelRec: PLabelRec);
  property    Labels[const aCode:string]: string read GetLabel write SetLabel;
  property    Codes[const aLabel:string]: string read GetCode write SetCode;
  property    LabelName : string read fLabelName write fLabelName;
end;

TLabelBlocksList=class(TStringList)
public
  constructor Create;
  Destructor  Destroy;override;
  procedure   clear;override;
  procedure   CopyToValueLabelSet(var aSet: TStringList);
  procedure   CopyFromValueLabelSet(var aSet: TStringList); overload;
  procedure   Clone(var dest: TLabelBlocksList);
end;


  TEpiList = class(TStringList)
  private
    FLocked: Boolean;
    FUpdated: Boolean;
  protected
    procedure ListChanging(Sender: TObject);
    function FindItem(const Name: string; MustExist: Boolean): TObject;
    function GetCount: Integer; override;
    function GetUpdated: Boolean; virtual;
    procedure UpdateList; virtual; abstract;
    property Updated: Boolean read GetUpdated write FUpdated;
    property Locked: Boolean read FLocked write FLocked;
  public
    constructor Create;
    procedure Update;
  end;

  TEpiField=class
  private
    fFieldLabel: String;
    fFieldname: String;
    fFieldLen: word;
    fDecimals: word;
    fFieldNo: word;
    foffset: word;
    fMissing :boolean;
    fEpiFieldType: word;
//    fFieldType: TEntrytype;
    fValueLabel: TLabelValueList;
    fValueLabelName: string;
    fReadOnly: Boolean;
    fRepeated: Boolean;
    fMustEnter: Boolean;
    fFieldFormat: Epistring;
    fFieldUserData: TObject;
    function GetMissing: Boolean;
  protected
    function ReadFromString(const line: string):boolean;
    function WriteToString:string;
  public
    constructor Create; overload;
    constructor Create(aField:TEpiField); overload;
    destructor destroy; override;
    property  FieldLen   : word read fFieldLen write fFieldLen; (* 0 means no entry for this field *)
//    property  FieldEntry : String[80];
    property  FieldLabel : String read fFieldLabel write fFieldLabel;
    property  Fieldname  : String read fFieldname write fFieldname;
    property  Ismissing    : Boolean read fMissing write fMissing;
//    property  FieldType  : TEntryType read fFieldType write fFieldType;   //xx
    property  MustEnter  : Boolean read fMustEnter write fMustEnter;
    property  Repeated   : Boolean read fRepeated write fRepeated;
    property  ReadOnly   : Boolean read fReadOnly write fReadOnly;
    property  Decimals   : word read fDecimals write fDecimals;
    property  offset     : word read foffset write foffset;
//    property  HasMin     : Boolean;
//    property  FieldMin   : ValueRecord;
//    property  HasMax     : Boolean;
//    property  FieldMax   : ValueRecord;
//    property  Legal      : LegalPtr;
//    property  Jumps      : LegalPtr;
//    property  Codes      : LegalPtr;
//    property  CodeField  : FieldPtr;
//    property  AutoJump   : FieldPtr;
//    property  RealEntryT : EntryType;
    property  FieldNo      : word read fFieldNo write fFieldNo;
    property  DataType : word read fEpiFieldType write fEpiFieldType;
    property ValueLabel: TLabelValueList read fValueLabel write fValueLabel;
    property ValueLabelName : string read fValueLabelName write fValueLabelName;
    property FieldFormat: Epistring read fFieldFormat Write fFieldFormat;
    property FieldUserData: TObject read fFieldUserData write fFieldUserData;
  end;

  TEpiFieldList = class(TEpiList)
  private
    function GetField(Index: Integer): TEpiField;
  protected
//    function LoadFromFile(const filename: string):boolean;
//    procedure UpdateList; override;
  public
    Constructor Create;
     procedure UpdateList; override;
    function FieldByName(const Name: string): TEpiField;
    function Find(const Name: string): TEpiField; reintroduce;
    function AddField(aField:TEpiField):TEpiField;
    property Fields[Index: Integer]: TEpiField read GetField; default;
  end;


 TEpiFile = class(TaaRecordStream){TaaRecordStreamCache}
    private
      fIsDeleted: boolean;
      fFileLabel: string;
      fFileName: string;
      fLabelBlocks: TLabelBlocksList;
      fUserData: TObject;
      fPreEnteredKey: String;
      function GetHeaderInfo(const aFileName: string;aCreateNew: boolean): integer;
      function LoadFromFile(const filename: string;var CustomUserData:TObject): boolean;
      function SaveToFile(const filename :string;Structure: TEpiFieldList;CustomUserData:TObject):boolean;
      function GetBOF: boolean;
      function GetEOF: boolean;
      function ReadCheckFile: integer;
    protected
      fTextBuffer : array[0..8191*2] of char;
      fRecBuffer    : Pchar;
      fCleanedBuffer    : Pchar;
      fRecBufferLen : integer;
      Rsz           : integer;
      fEpiHeaderLen : integer;
      fBackColor    : integer;
      fCurrentRecordNo: integer;
      fRecordCount    : integer;
      fNumFields,fNumValidFields :integer;
      fFieldList       :TEpiFieldList;
      fReadCleaned,fModified : boolean;
      fEpiInfoFieldNaming :boolean;
      function GetRecordCount: integer; override;
      function ReadHeader: boolean;  override;
      procedure SetRecordCount(const Value: integer); override;
      procedure UpdateHeader;  override;
      function WriteHeader: boolean;  override;
      function GetHeaderLen: integer;override;
      procedure AllocRecBuffer;
      procedure FreeRecBuffer;
      procedure rsCreateNew; override;
      procedure rsOpenExisting; override;
      procedure Initialize;
      procedure DeInitialize;
      function SetFieldFormat(Fld: TEpiField):boolean;
   public
      constructor CreateExist(const aFileName  : string;var CustomUserData:TObject; aShareMode : word =fmShareDenyWrite; aCacheSize: integer = 4092);
      constructor CreateNew(const aFileName : string; Structure:TEpiFieldList;CustomUserData:TObject; const pw:string='';
         aShareMode : word =fmShareDenyWrite;aCacheSize   : integer = 4092);
      destructor Destroy; override;
      procedure CleanBuffer(src, dest: pointer);
      procedure PrepareBuffer(src, dest: pointer);
      function AppendRec(aRec: pointer): integer;
      function UpdateRec(aRecNo: integer=-1;aRec:pointer=nil): integer;
      function NewRecord: integer;

      function ReadNextRec(aRec:pointer=nil):integer;
      function ReadPrevRec(aRec: pointer=nil): integer;
      function ReadCurrentRec(aRec: pointer=nil): integer;
      function ReadRec(aRecNo:integer=-1;aRec:pointer=nil): integer;

      function GetFieldData(FldNo: Integer; Dst: Pointer;var Blank:boolean): Boolean;overload;

      function GetFieldData(Fld: TEpiField; Src, Dst: Pointer;var Blank:boolean): Boolean;overload;
      function GetFieldData(Fld: TEpiField; Dst: Pointer;var Blank:boolean): Boolean;overload;

      function GetFieldText(FldNo: Integer; Src, Dst: Pointer;var Blank: boolean): Boolean;overload;
      function GetFieldText(Fld: TEpiField; Src, Dst: Pointer;var Blank: boolean): Boolean;overload;

      function GetFieldText(FldNo: Integer; Dst: Pointer;var Blank:boolean): Boolean;overload;
      function GetFieldText(Fld: TEpiField; Dst: Pointer;var Blank: boolean): Boolean;overload;
      procedure SetFieldData(Fld: TEpiField; Src, Dst: Pointer);overload;
      procedure SetFieldData(Fld: TEpiField; Src: Pointer);overload;

      procedure SetFieldData(FldNo: Integer; Src: Pointer);overload;

      function First:integer;
      function Last:integer;

      property BOF:boolean read GetBOF;
      property EOF:boolean read GetEOF;
      property Fields:TEpiFieldList read fFieldList;
      property CurrentRecordNo : integer read fCurrentRecordNo;
      property IsDeleted : boolean read fIsDeleted write fIsDeleted;
      property FileLabel :string read fFileLabel write  fFileLabel;
      property FileName :string read fFileName write fFileName;
      property LabelBlocks: TLabelBlocksList read fLabelBlocks write fLabelBlocks;
      property FileUserData: TObject read fUserData write fUserData;
  end;

//function FieldTypeToEpiFldType(ET: TFieldType):word ;
//function EpiFldTypeToFieldType(ET: word): TFieldType;


implementation

uses uDateUtils, UcheckParser, EpiDataUtils,PasswordUnit;


{function EpiFldTypeToFieldType(ET: word): TFieldType;
begin
case ET of
  EpiTyUppercase,
  EpiTyString                                    : result := ftString;
  EpiTyInteger                                   : result := ftInteger;
  EpiTyDate                                      : result := ftDate;
  EpiTyBoolean                                   : result := ftBoolean;
  EpiTyFloat                                     : result := ftFloat;
end;
end;

function FieldTypeToEpiFldType(ET: TFieldType):word ;
begin
case ET of
  ftString,ftWideString               : result :=   EpiTyString;
  ftInteger,ftSmallint,ftWord         : result :=  EpiTyInteger;
  ftDate,ftDateTime                   : result :=     EpiTyDate ;
  ftBoolean                           : result :=  EpiTyBoolean ;
  ftFloat,ftCurrency                  : result :=    EpiTyFloat
else
  Result:=EpiTyIgnore;
end;
end;}

(*
    ftBoolean, ftFloat, , ftBCD, ftDate, ftTime, ftDateTime,
    ftBytes, ftVarBytes, ftAutoInc, ftBlob, ftMemo, ftGraphic, ftFmtMemo,
    ftParadoxOle, ftDBaseOle, ftTypedBinary, ftCursor, ftFixedChar, ftWideString,
    ftLargeint, ftADT, ftArray, ftReference, ftDataSet, ftOraBlob, ftOraClob,
    ftVariant, ftInterface, ftIDispatch, ftGuid, ftTimeStamp, ftFMTBcd
 *)


constructor TEpiList.Create;
begin
  inherited Create;
//  OnChanging := ListChanging;
//  FLocked := True;
end;

function TEpiList.FindItem(const Name: string; MustExist: Boolean): TObject;
var
  I: Integer;
begin
  if not Updated then Update;
  I := IndexOf(Name);
  if I > -1 then
    Result := GetObject(I)
  else
  begin
    if MustExist then
      DatabaseErrorFmt('SFieldNotFound', [Name], nil);
    Result := nil;
  end;
end;

function TEpiList.GetCount: Integer;
begin
  if not Updated then Update;
  Result := inherited GetCount;
end;

function TEpiList.GetUpdated: Boolean;
begin
  Result := FUpdated;
end;

procedure TEpiList.ListChanging(Sender: TObject);
begin
  if Locked then
    DatabaseError('SReadOnlyProperty', nil);
end;

procedure TEpiList.Update;
begin
{  if not Updated then
  begin
    Locked := False;
    BeginUpdate;
    try
      Clear;
      UpdateList;
      FUpdated := True;
    finally
      EndUpdate;
      Locked := True;
    end;
  end;}
end;


function TEpiFieldList.Find(const Name: string): TEpiField;
begin
  Result := TEpiField(FindItem(Name, False));
end;

function TEpiFieldList.FieldByName(const Name: string): TEpiField;
begin
  Result := TEpiField(FindItem(Name, True));
end;

function TEpiFieldList.GetField(Index: Integer): TEpiField;
begin
//  if not Updated then Update;
  Result := TEpiField(Objects[Index]);
end;
(*
procedure TFieldList.UpdateList;

  procedure AddFields(const AFields: TFields);
  var
    I: Integer;
    Field: TField;
  begin
    { Using Fields.FList.Count here to exclude sparse fields }
    for I := 0 to AFields.FList.Count - 1 do
    begin
      Field := AFields[I];
      AddObject(Field.FullName, Field);
      if Field.DataType in [ftADT, ftArray] then
        AddFields(TObjectField(Field).FOwnedFields);
    end;
  end;

begin
  AddFields(DataSet.FFields);
end;
*)
{ TEpiFile }
{
constructor TEpiFile.Create(aStream: TStream; aCreateNew: boolean;
  aShareMode: word; aCacheSize: integer);
begin

end;
}
constructor TEpiFile.CreateExist(const aFileName: string;var CustomUserData:TObject;aShareMode: word; aCacheSize: integer);
var
 Err : integer;
begin
  fFieldList :=TEpiFieldList.create;
  fLabelBlocks:= TLabelBlocksList.Create;
  Err:=0;
  Err:=CanReadWriteFile(aFileName);
  if Err<> 0 then
      CheckfileError(Err);
  LoadFromFile(aFileName,CustomUserData);
  inherited CreateFile(aFileName, false, fRecBufferLen{,aCacheSize});
  fFileName:=aFileName;
  sequential:=true;
  Open;
  //ReadCheckFile;   MIB 18/5-07 - SKAL CHECKE OM DETTE GIVER PROBLEMER!
  First;
end;

constructor TEpiFile.CreateNew(const aFileName: string;
  Structure: TEpiFieldList; CustomUserData:TObject; const pw:string=''; aShareMode : word =fmShareDenyWrite;aCacheSize   : integer = 4092);
begin
//  fFieldList :=structure;
  fPreEnteredKey:='';
  if pw<>'' then fPreEnteredKey:=pw;
  SaveToFile(aFileName,structure,CustomUserData);
  CreateExist(aFileName,CustomUserData, aShareMode, aCacheSize);
  fFileName:=aFileName;
end;


destructor TEpiFile.Destroy;
begin
  fFieldList.free;
  LabelBlocks.free;
  inherited;
end;

function TEpiFile.ReadCheckFile: integer;
var
 parser: TCheckParser;
begin
  parser:=nil;
//  LabelBlocks:=nil;
try
try
  parser:= TCheckParser.Create(self);
  if parser<>nil then
    parser.ParseFile;
except
//  LabelBlocks.free;
//  LabelBlocks:=nil;
end;
finally
  parser.free;
end;
end;



function TEpiFile.GetHeaderInfo(const aFileName: string; aCreateNew: boolean): integer;
begin

end;


function TEpiFile.GetHeaderLen: integer;
begin
result:=fEpiHeaderLen;
end;

function TEpiFile.GetRecordCount: integer;
begin
  result:= fRecordCount;
end;


function TEpiFile.ReadHeader: boolean;
begin

end;

procedure TEpiFile.SetRecordCount(const Value: integer);
begin
   fRecordCount:=value;
end;

procedure TEpiFile.UpdateHeader;
begin
 //donothing
end;

function TEpiFile.WriteHeader: boolean;
begin
//donoting
end;

procedure TEpiFile.AllocRecBuffer;
begin
   getmem(fRecBuffer,fRecBufferLen);
   getmem(fCleanedBuffer,Rsz);
end;

procedure TEpiFile.FreeRecBuffer;
begin
   freemem(fRecBuffer);
   freemem(fCleanedBuffer);
end;

procedure TEpiFile.rsCreateNew;
begin
 Initialize;
 inherited;
end;

procedure TEpiFile.rsOpenExisting;
begin
 Initialize;
 inherited;
end;


procedure TEpiFile.Initialize;
begin
  AllocRecBuffer;
end;

procedure TEpiFile.DeInitialize;
begin
  FreeRecBuffer;
end;

procedure TEpiFile.CleanBuffer(src,dest:pointer);
var
 remain, i,j, width : integer;
 flag : array[0..2] of char;
begin
 remain :=fRecBufferLen;
 if ((Rsz Mod FILEWIDTH) <> 0) then dec(remain, 3); //remove the terminator
// move(pchar(src)[remain],flag,3);
 IsDeleted :=pchar(src)[remain]='?';
 i:= 0;j:=0;
 while remain > 0 do
 begin
     width := FILEWIDTH;
     if width > remain then width := remain; //take the smaller of remain or FILEWIDTH
     move(pchar(src)[i],pchar(dest)[j],width);
     j := j + width;
     i:= i+width+3;
     remain:=remain-width-3;
 end;
end;

procedure TEpiFile.PrepareBuffer(src, dest: pointer);
var
 remain, i,j, width : integer;
begin
 remain :=Rsz;
 i:= 0;j:=0;
 while remain > 0 do
 begin
     width := FILEWIDTH;
     if width > remain then width := remain; //take the smaller of remain or FILEWIDTH
     move(pchar(src)[j],pchar(dest)[i],width);
     j := j + width;
     i:= i+width;
     if IsDeleted then
       move(DELStr[0],pchar(dest)[i],3)
     else
       move(EOLStr[0],pchar(dest)[i],3);
     i:= i+3;
     remain:=remain-width;
 end;
end;



function TEpiFile.GetBOF: boolean;
begin
  result:= fCurrentRecordNo<1;
end;

function TEpiFile.GetEOF: boolean;
begin
 result:= fCurrentRecordNo >Recordcount;
end;


function TEpiFile.Last: integer;
begin
//  result:= ReadRec(RecordCount);
fCurrentRecordNo:=Recordcount+1;
end;

function TEpiFile.First: integer;
begin
// result:= ReadRec(0);
fCurrentRecordNo:=0;
end;

function TEpiFile.ReadNextRec(aRec:pointer=nil): integer;
begin
  result:=_EOF;
  if succ(fCurrentRecordNo) > RecordCount then
  begin Last; exit; end;
  inc(fCurrentRecordNo);
  Result:= ReadCurrentRec(aRec);
end;

function TEpiFile.ReadPrevRec(aRec:pointer=nil): integer;
begin
  result:=_BOF;
  if Pred(fCurrentRecordNo) < 1 then
  begin First; exit; end;
  dec(fCurrentRecordNo);
  Result:= ReadCurrentRec(aRec);
end;

function TEpiFile.ReadCurrentRec(aRec:pointer=nil): integer;
begin
   ReadRec(-1,aRec);
end;

function TEpiFile.ReadRec(aRecNo:integer=-1;aRec:pointer=nil): integer;
var
 bRes :boolean;
begin
//internally recordnumber starts at zero
  result :=_RECNOTFOUND;
  if aRecNo=-1 then  aRecNo:= fCurrentRecordNo;
  if (aRecNo<1) then begin result:=_BOF;exit;end;
  if (aRecNo>RecordCount) then begin result:=_EOF;exit;end;
  bRes:= read(aRecNo-1, fRecBuffer^);
  if bRes then
  begin
     fCurrentRecordNo:= aRecNo;
     if (aRec <> nil) then
      begin
         move(fRecBuffer^,aRec^,fRecBufferLen);
      end;
     fReadCleaned:=false;
     result:=fCurrentRecordNo;
  end;
end;

function TEpiFile.UpdateRec(aRecNo:integer=-1;aRec:pointer=nil): integer;
var
 bRes :boolean;
begin
//internally recordnumber starts at zero
  result :=_OK;
  if aRecNo=-1 then aRecNo:=fCurrentRecordNo;
  if (aRecNo <> _NEWREC) and (aRecNo<1) then begin result:=_BOF;exit;end;
  if (aRecNo>RecordCount) then begin result:=_EOF;exit;end;
  if aRec<>nil then
     inherited Write(aRecNo-1,aRec^)
  else
  begin
  if fModified then
    Preparebuffer(fCleanedBuffer,fRecBuffer);
  if (aRecNo = _NEWREC) then
     AppendRec(fRecBuffer)
  else
     inherited Write(aRecNo-1,fRecBuffer^);
  fModified :=false;
  end;
end;


function TEpiFile.AppendRec(aRec:pointer): integer;
var
 bRes :boolean;
begin
  result:= Add(aRec^);
  last;//force a read
//  fCurrentRecordNo:= result;
  fModified :=false;
  IsDeleted:=false;
end;

function TEpiFile.NewRecord: integer;
begin
 fillchar(fRecBuffer^,fRecBufferLen,#32);
 fillchar(fCleanedBuffer^,Rsz,#32);
 fReadCleaned:=true;
 fModified :=true;
 IsDeleted:=false;
 fCurrentRecordNo:= _NEWREC;
end;


{ TEpiField }

constructor TEpiField.Create;
begin
  inherited;
end;

constructor TEpiField.Create(aField: TEpiField);
begin
 create;
end;

destructor TEpiField.destroy;
begin
  inherited;
end;

function TEpiField.GetMissing: Boolean;
begin

end;

function TEpiField.ReadFromString(const line: string): boolean;
begin

end;

function TEpiField.WriteToString: string;
begin

end;

constructor TEpiFieldList.Create;
begin
 inherited Create;
end;



Function TextPos(var F:Textfile):Longint;
BEGIN
  With TTextRec(F) DO
    BEGIN
      Result:=SetFilePointer(Handle,0,nil,FILE_CURRENT);
      IF Mode=FMOutput THEN INC(Result, BufPos)
      ELSE IF BufEnd<>0 THEN Dec(Result, BufEnd-BufPos);
    END;
END;


function TextSeek(var F:Textfile; Offset: Longint; Origin: Word): Longint;
begin
  Result := FileSeek(TTextRec(F).Handle, Offset, Origin);
end;

function TextFileSize(var F:Textfile): Longint;
begin
  Result := FileSeek(TTextRec(F).Handle, 0, 2); //2=end of file
end;

function TEpiFile.LoadFromFile(const filename: string;var CustomUserData:TObject): boolean;
const
 FINAGLEFACTOR=3;
var
   Fld       : TEpiField;
   i ,NumVars,p,ErrorCode       : integer;
   ft, fpos,fpos2       : integer;
   f        : TextFile;
   flabel,fPassword   : string;
   lFieldChar: Char;
   lFieldType: TEntryType;
   lLabelCol, lFieldRow, lLabelRow,lFieldCol,fsize: integer;
   lFieldLabel: String;
   lFieldname: String[10];
   lLabelColor, lFieldColor, lFieldLen, lDecimals: word;
   lFieldNo: word;
   s         :string;
   UserData : TEpiInfoCustomUserData;
begin
     result := FALSE;
     fNumFields:=0;
try
     AssignFile( f, filename );
     FileMode := 0;
     Reset( f );
     SetTextBuf(f, fTextBuffer);
     result:=true;
     system.Read( f, fNumFields );
     fBackColor := 1;
     if EOLN( f ) then
     begin
        Readln( f )            (* Old version *)
     end
     else
     begin
        try
           system.read( f, fBackColor );
        except
          //epidata bug ignore
        end;
        if EOLN( f ) then
           Readln( f )
        else
        begin
          readln( f, s);        (*EpiData *)
          s :=AnsiUpperCase(s);
          fpos :=Pos(' VLAB',s);
          if fpos>0 then
          begin
           fEpiInfoFieldNaming:=False;
           System.delete(s,fPos,5);
          end
          else fEpiInfoFieldNaming:=True;
          fpos :=Pos('~KQ:',s);
          if fpos > 0 then
          begin
              fpos2 :=Pos(':KQ~',s);
              if fpos2 > fpos then
                fPassword:= Copy(s,fPos+4,fpos2-fpos-4);
          end;
          fpos:=Pos('FILELABEL: ',s);
          if fpos > 0 then
              Filelabel:= Copy(s,fPos+Length('FILELABEL: '),Length(s));
        end;
     end;

     UserData := TEpiInfoCustomUserData.Create(Filelabel,fPassword,fBackColor) ;
     UserData.EpiInfoFieldNaming:=fEpiInfoFieldNaming;
     CustomUserData:=UserData;

     Rsz := 0;
//     NumVars := 0;
     if ( fNumFields < 1 ) then
        CheckfileError(-13);
     for i := 1 to fNumFields do
     begin
       if (System.EOF(f)) then  break;
       Readln(f, lFieldChar, lFieldName, lLabelCol, lLabelRow, lLabelColor,
                        lFieldCol, lFieldRow, ft, lFieldLen, lFieldColor,s);
       Fld:=TEpiField.Create;
       fld.fielduserData := TEpiInfoCustomFieldUserData.Create;
       TEpiInfoCustomFieldUserData(fld.fielduserData).FieldChar :=lFieldChar;
       TEpiInfoCustomFieldUserData(fld.fielduserData).LabelCol:=   lLabelCol;
       TEpiInfoCustomFieldUserData(fld.fielduserData).LabelRow:=   lLabelRow;
       if lLabelRow > UserData.MaxRow then
           UserData.MaxRow :=lLabelRow;
       TEpiInfoCustomFieldUserData(fld.fielduserData).LabelColor:= lLabelColor;
       TEpiInfoCustomFieldUserData(fld.fielduserData).FieldCol:=   lFieldCol;
       TEpiInfoCustomFieldUserData(fld.fielduserData).FieldRow:=   lFieldRow;
       if lFieldRow > UserData.MaxRow then
           UserData.MaxRow :=lFieldRow;
       TEpiInfoCustomFieldUserData(fld.fielduserData).FieldColor:= lFieldColor;
       fld.FieldName := trim(ansiuppercase(lFieldName));
       if fEpiInfoFieldNaming then
           Fld.FieldLabel :=trim(s)
       else
       begin //Epidata: ignore the first word.
           p:= pos(' ',s);
           if p > 0 then
              s :=trim(copy(s,p+1,length(s)));
           Fld.FieldLabel:=trim(s);   //MIB 17mar04
       end;

       fld.decimals := 0;
       if ( ft >= 100 ) then
       begin
             TEpiInfoCustomFieldUserData(fld.fielduserData).FieldType := eftrealNum;
             fld.Decimals := ft - 100;
       end
       else
       begin
             TEpiInfoCustomFieldUserData(fld.fielduserData).fieldtype := TEntrytype(ft);
       end;
       fld.offset :=Rsz;
       fld.FieldLen:=lFieldLen;
       if fld.FieldLen =0 then TEpiInfoCustomFieldUserData(fld.fielduserData).fieldtype :=eftQuestion;
       fld.DataType:= EntryTypeToFieldType(TEpiInfoCustomFieldUserData(fld.fielduserData).fieldtype);
       SetFieldFormat(fld);
       if TEpiInfoCustomFieldUserData(fld.fielduserData).fieldtype in [eftTodayType,eftEuroToday,eftYMDToday] then
          fld.ReadOnly:=true;
       inc(Rsz, fld.FieldLen);
       Fields.addField(fld);
     end;//for
     fEpiHeaderLen:=TextPos(f);
     fRecBufferLen := Rsz + ((Rsz Div FILEWIDTH) * 3);
     if ( (Rsz Mod FILEWIDTH) <> 0 ) then
        inc(fRecBufferLen, 3);
     FSize := TextFileSize(f);
     If ( FSize <= (fEpiHeaderLen+FINAGLEFACTOR) ) then
      fRecordCount := 0
     else
      fRecordCount := (FSize - fEpiHeaderLen) DIV fRecBufferLen;
     closefile(f);
except
   on E: EInOutError do ErrorCode:= E.ErrorCode;
   else
   if result then
   begin
          closefile(f);
          result:=false;
   end;
   CheckfileError(ErrorCode);
end;
end;

function TEpiFile.SetFieldFormat(Fld: TEpiField):boolean;
begin
   case fld.DataType of
     EpiTyBoolean:
     begin
     end;
     EpiTyInteger:
     begin
          fld.fieldformat:='%d';
     end;
     EpiTyFloat:
     begin
//         fld.fieldformat:=format('%%%d',[fld.FieldLen]);
     end;
     EpiTyDate:
     begin
      case TEpiInfoCustomFieldUserData(fld.fielduserData).fieldtype of
        eftDate,eftTodayType:           fld.fieldformat:='%MDY';
        eftEuroDate,eftEuroToday:       fld.fieldformat:='%DMY';
        eftYMDDate,eftYMDToday:         fld.fieldformat:='%YMD';
      end;
     end;
     EpiTyString, EpiTyUppercase:
     begin
       fld.fieldformat:=format('%%s%d',[fld.FieldLen]);
     end;
end;
end;


function TEpiFile.SaveToFile(const filename: string;Structure: TEpiFieldList;CustomUserData:TObject): boolean;
var
   Fld       : TEpiField;
   i ,fBackColor,NumVars,EncodedFieldLength, DecodedFieldLength,FieldColor       : integer;
   fNumFields                 :integer;
   ft       : integer;
   f        : TextFile;
   dummy    : char;  (* get rid of space before fieldlabel *)
   flabel,fFileLabel,fpassword   : string;
   UserData : TEpiInfoCustomUserData;
   FieldUserData: TEpiInfoCustomFieldUserData;
   HasEncrypt: boolean;
   s : string;
begin
     result := FALSE;
     fNumFields:=Structure.Count;
     if ( fNumFields < 1 ) then
        CheckfileError(-13);
{$I-}
     AssignFile( f, filename );
     Rewrite(f);
     CheckfileError(ioresult);
//TODO backcolor and epidata label and long names, field labels, field markers

    fFileLabel :='';
    FileUserData:=CustomUserData;
    UserData := TEpiInfoCustomUserData(CustomUserData);
    fBackColor:=UserData.fbackcolor;
    if  fBackColor=0 then  fBackColor:=1;
    fFileLabel :=trim(UserData.datalabel);
    if fFileLabel<>'' then
        fFileLabel:='FileLabel: '+fFileLabel;
    if not Userdata.EpiInfoFieldNaming then
        //fFileLabel:=fFileLabel+' VLAB ';
        fFileLabel:=' VLAB '+fFileLabel;   //MIB rettet140304
    //check if contains encryption fields
    HasEncrypt:=false;
    i:=0;
    while (NOT HasEncrypt) and (i<fNumFields) do
      begin
        fld := Structure[i];
        if TEpiInfoCustomFieldUserData(fld.fielduserData).FieldType=eftCrypt then HasEncrypt:=true;
        inc(i);
      end;
    fPassword:=fPreEnteredKey;
    userdata.Password:=fpassword;
    fpassword :=EncryptString(fpassword,fpassword);
    if (HasEncrypt) and (fPassword='') then   //get a password
      begin
        PasswordForm:=TpasswordForm.Create(nil);
        try
          PasswordForm.DoubleEntry:=true;
          PasswordForm.lbDatafile.caption:=extractFilename(filename);
          REPEAT
            i:=PasswordForm.ShowModal;
            if i=mrOK then fpassword:=PasswordForm.edPW1.Text;
          UNTIL fpassword<>'';
          userdata.Password:=fpassword;
          fpassword:=EncryptString(fpassword,fpassword);
        finally
          PasswordForm.Free;
        end;  //try..finally
      end;  //if HasEncrypt
    //fpassword :=trim(UserData.Password);

    if fpassword <> '' then
       fpassword := '~kq:'+ fpassword+':kq~';
                                   {BackColor}
     system.Writeln(f, fNumFields, ' ', fBackColor ,' ',fpassword,' ',fFileLabel);
     Rsz := 0;
     for i:=0 to fNumFields-1 do
     begin
       fld := Structure[i];
       // build 109: problem, when creating Tepi.....fld.fielduserdata is created wrong.
       FieldUserData:=TEpiInfoCustomFieldUserData(fld.fielduserData);

       //TEpiInfoCustomFieldUserData(fld.fielduserData).fieldtype := TEntrytype(fld.fielduserData);

       if (TEpiInfoCustomFieldUserData(fld.fielduserData).FieldType = eftRealNum) and (fld.decimals > 0) then
             ft := 100 + fld.Decimals
         else
             ft := ord(TEpiInfoCustomFieldUserData(fld.fielduserData).FieldType);

        //RETTET MIB - 4 linier  140304
        dummy:=TEpiInfoCustomFieldUserData(fld.fFieldUserData).FieldChar;
        IF (dummy<>'#') AND (dummy<>'_') THEN dummy:='_';
        IF fld.FieldLabel='' THEN fld.FieldLabel:=fld.Fieldname;
        {MIB rettelse slut}

        //dummy:='_';
        //if userData.fVLAB then
        //   fld.fieldlabel :=fld.FieldName+' ' +fld.fieldlabel;
        if FieldUserData.FieldType=eftCrypt then
          begin
            EncodedFieldLength:=((fld.FieldLen+2) div 3)*4;  //fld.fieldlen is the fieldlength when NOT encrypted. N now = the encoded length
            if EncodedFieldLength>60 then EncodedFieldLength:=60;
            IF fld.FieldLen<15 THEN FieldColor:=111+fld.FieldLen ELSE FieldColor:=fld.FieldLen;
          end
        else
          begin
            EncodedFieldLength:=fld.FieldLen;
            FieldColor:=112;
          end;
        system.Writeln(f, dummy+ padRight(fld.FieldName,' ',10,true), '  ',
           TEpiInfoCustomFieldUserData(fld.fielduserData).LabelCol:3, ' ', TEpiInfoCustomFieldUserData(fld.fielduserData).LabelRow:3,
           ' ', TEpiInfoCustomFieldUserData(fld.fielduserData).LabelColor:3, ' ',
           TEpiInfoCustomFieldUserData(fld.fielduserData).FieldCol:3, ' ',
           TEpiInfoCustomFieldUserData(fld.fielduserData).FieldRow:3,' ', ft:3, ' ',
           EncodedFieldLength:3, ' ',
           FieldColor:3, ' ', fld.fieldlabel);
         Inc(Rsz, fld.FieldLen);
//         CheckfileError(-2);
    end;//for
    fRecBufferLen := Rsz + ((Rsz Div FILEWIDTH) * 3);
    if ( (Rsz Mod FILEWIDTH) <> 0 ) then
        inc(fRecBufferLen, 3);
    closefile(f);
{$I+}
end;


function TEpiFile.GetFieldData(FldNo: Integer; Dst: Pointer;var Blank:boolean): Boolean;
var
  Fld: TEpiField;
begin
  Fld:=Fields[FldNo];
  if fld=nil then raise Exception.create('field not found'); //***
  Result:=GetFieldData(Fld,Dst,Blank);
end;

function TEpiFile.GetFieldData(Fld: TEpiField; Src, Dst: Pointer;var Blank:boolean): Boolean;
var
  s:string;
  dt:TDateTime;
  dt1 : EpiDate;
  E, i: Integer;
  eValue : Extended;
begin
{#TODO1 optimize string handling and triming}
  SetString(s, PChar(Src) + Fld.Offset, Fld.FieldLen);
  s:=Trim(s);
  Blank:=length(s)=0;
  result:=True;
  if Not blank and (Dst<>nil) then// data not needed if Result= FALSE or Dst=nil
  case TEpiInfoCustomFieldUserData(fld.fielduserData).FieldType of
    eftAlpha, eftUpperCase,eftPhoneNum:
    begin
      if TEpiInfoCustomFieldUserData(fld.fielduserData).FieldType = eftUppercase then
        s := Sysutils.AnsiUpperCase(s);
      StrPCopy(Dst,s);
//      LeftBlank(Dst, Fld.FieldLen)^ := #0;
    end;
    eftYesNo,eftCheckBox:
      begin
        if (UpCase(s[1])='T') or (UpCase(s[1])='Y') then Word(Dst^) := 1
        else Word(Dst^) := 0;
      end;
    eftNumeric,eftIDNUM:
      begin
        Val(S, i, E);
        Result:=E = 0;
        if Result then
            integer(Dst^) :=i
        else
            integer(Dst^) :=NA_INT;
//        strtointdef(s,0);
      end;
    eftRealNum:
      begin
        Result:=TextToFloat(pchar(s), eValue, fvExtended);
        if Result then
            EpiFloatStorage(Dst^):= eValue
        else
           EpiFloatStorage(Dst^):=NA_FLOAT;
//         SMStrToFloat(s);
      end;
   eftEurodate,eftEuroToday:
    begin
     Result:=EpistrToDate(s,dt1,dfDMY);
     Integer(Dst^) :=dt1;
    end;
   eftDate,eftTodayType:
    begin
     Result:=EpistrToDate(s,dt1,dfMDY);
     Integer(Dst^) :=dt1;
    end;
    eftYMDDate,eftYMDToday:
    begin
     Result:=EpistrToDate(s,dt1,dfYMD);
     Integer(Dst^) :=dt1;
    end;
  end;
end;


function TEpiFile.GetFieldData(Fld: TEpiField; Dst: Pointer;var Blank: boolean): Boolean;
begin
  if not fReadCleaned then
  begin
     CleanBuffer(fRecBuffer,fCleanedBuffer);
     fReadCleaned:=true;
  end;
  Result:=GetFieldData(Fld,fCleanedBuffer,Dst,Blank);
end;


function TEpiFile.GetFieldText(Fld: TEpiField; Dst: Pointer;var Blank:boolean): Boolean;
begin
  if not fReadCleaned then
  begin
     CleanBuffer(fRecBuffer,fCleanedBuffer);
     fReadCleaned:=true;
  end;
  Result:=GetFieldText(Fld,fCleanedBuffer,Dst,Blank);
end;


function TEpiFile.GetFieldText(FldNo: Integer; Dst: Pointer;var Blank:boolean): Boolean;
begin
  if not fReadCleaned then
  begin
     CleanBuffer(fRecBuffer,fCleanedBuffer);
     fReadCleaned:=true;
  end;
  Result:=GetFieldText(FldNo,fCleanedBuffer,Dst,Blank);
end;



function TEpiFile.GetFieldText(Fld: TEpiField; Src, Dst: Pointer;var Blank:boolean): Boolean;
var
  s:string;
begin
//TODO: optimize string handling here
  SetString(s, PChar(Src) + Fld.Offset, Fld.FieldLen);
  s:=Trim(s);
  result:=length(s)>0;
  Blank:=not result;
  if Dst<>nil then// data not needed if Dst=nil
      StrPCopy(Dst,s);
end;


function TEpiFile.GetFieldText(FldNo: Integer; Src, Dst: Pointer;var Blank:boolean): Boolean;
var
  s:string;
  Fld       : TEpiField;
begin
  Fld:=Fields[FldNo];
  if fld=nil then raise Exception.create('field not found');
//TODO: optimize string handling here
  SetString(s, PChar(Src) + Fld.Offset, Fld.FieldLen);
  s:=Trim(s);
  result:=length(s)>0;
  Blank:=not result;
  if Dst<>nil then// data not needed if Dst=nil
      StrPCopy(Dst,s);
end;



procedure TEpiFile.SetFieldData(Fld: TEpiField; Src, Dst: Pointer);
var
  s,key:string;
  dt:TDateTime;
  ld,lm,ly: word;
  P :pchar;
  ts:TTimeStamp;
  ed : Epidate;
  db : extended;
  i : integer;

begin
{eftNumeric, eftAlpha, eftDate, eftUppercase, eftCheckBox, eftYesNo,
                   eftRealNum, eftPhoneNum, eftTime, eftLocalNum,  eftTodayType, eftEuroDate,
                   eftIDNum,   eftRes4, eftRes5,eftQuestion,eftEuroToday,eftSoundex,eftCrypt,
                   eftYMDDate,eftYMDToday}
  p:= pchar(dst) + Fld.Offset;
  FillChar(p^, Fld.FieldLen, Ord(' '));
  if Assigned(src) then
  begin
    case TEpiInfoCustomFieldUserData(fld.fielduserData).FieldType of
    eftAlpha, eftPhoneNum,eftCrypt,eftSoundex:
    begin
      s:=PChar(Src);
      if TEpiInfoCustomFieldUserData(fld.fielduserData).FieldType=eftCrypt then
        begin
          if Assigned(FileUserData) then key:=TEpiInfoCustomUserData(FileUserData).Password else key:='';
          s:=EncryptString(s,key);
        end;
      if Length(s)<Fld.FieldLen then s:=s+ StringOfChar(' ',Fld.FieldLen-Length(s));
      move(pchar(s)^,p^,Fld.FieldLen)
    end;
    eftUpperCase:
    begin
      s:=PChar(Src);
      s:=SysUtils.Ansiuppercase(s);
      if Length(s)<Fld.FieldLen then s:=s+ StringOfChar(' ',Fld.FieldLen-Length(s));
      move(pchar(s)^,p^,Fld.FieldLen)
    end;
    eftYesNo,eftCheckBox:
      begin
        if Word(Src^) = 1 then
           p[0]:='Y'
        else
           p[0]:='N'
      end;
    eftNumeric,eftIDNUM:
    begin
         i := integer(src^);
         s :=inttostr(i);
         if Length(s)<Fld.FieldLen then s:=StringOfChar(' ',Fld.FieldLen-Length(s)) + s;
         move(pchar(s)^,p^,Fld.FieldLen)
    end;
    eftRealNum:
     begin
       Str(EpiFloatStorage(src^): Fld.FieldLen: Fld.Decimals, S);
       if Length(s)<Fld.FieldLen then s:=StringOfChar(' ',Fld.FieldLen-Length(s)) + s;
       move(pchar(s)^,p^,Fld.FieldLen)
      end;
    eftEurodate,eftEuroToday:
    begin
      ed := Integer(Src^);
      s:= EpiDateToStr(ed,dfDMY,Fld.FieldLen);
      move(pchar(s)^,p^,Fld.FieldLen)
    end;
    eftDate,efttodaytype:
    begin
      ed := Integer(Src^);
      s:= EpiDateToStr(ed,dfMDY,Fld.FieldLen);
      move(pchar(s)^,p^,Fld.FieldLen)
    end;
    eftYMDDate,eftYMDToday:
    begin
      ed := Integer(Src^);
      s:= EpiDateToStr(ed,dfYMD,Fld.FieldLen);
      move(pchar(s)^,p^,Fld.FieldLen)
    end;
    end;//case
 end;//if
end;

procedure TEpiFile.SetFieldData(Fld: TEpiField; Src: Pointer);
begin
  if not fReadCleaned then
  begin
     CleanBuffer(fRecBuffer,fCleanedBuffer);
     fReadCleaned:=true;
  end;
  SetFieldData(Fld,src,fCleanedBuffer);
  fModified :=true;
end;


procedure TEpiFile.SetFieldData(FldNo: Integer; Src: Pointer);
var
  Fld       : TEpiField;
begin
  Fld:=Fields[FldNo];
  if fld=nil then raise Exception.create('field not found');
  SetFieldData(Fld,src);
end;

function TEpiFieldList.AddField(aField: TEpiField): TEpiField;
begin
 addObject(aField.FieldName,aField);
end;

procedure TEpiFieldList.UpdateList;
begin

end;

{ TLabelValueList }

function TLabelValueList.AddPair(const Code, clabel: string): integer;
var
 p : pchar;
begin
    if Find(Code, Result) then
      case Duplicates of
        dupIgnore: Exit;
        dupError: Error('Duplicate code', 0);
      end;
   p:= stralloc(length(clabel)+1);
   strlcopy(p,pchar(clabel),length(clabel));
   AddObject(Code,Tobject(p));
end;

procedure TLabelValueList.Clone(var dest: TLabelValueList);
var
  i: integer;
begin
  if dest = nil then
    dest := TLabelValueList.Create();
  for i := 0 to count-1 do
    dest.AddPair(strings[i], pchar(objects[i]));
  dest.fLabelName := fLabelName;
end;

procedure TLabelValueList.CopyToLabelRec(var aLabelRec: PLabelRec);
var
  p: pchar;
  n: integer;
  tmpLabelRec,nextLabelRec,firstLabelRec: PLabelRec;
  aCode,aLabel: string;
  FirstLabel: boolean;
begin
  tmpLabelRec:=NIL;
  firstLabelRec:=NIL;
  FirstLabel:=true;
  for n:=0 to count-1 do
    begin
      p:=pchar(objects[n]);
      setString(aLabel,p,strlen(p));
      aCode:=strings[n];
      NextLabelRec:=tmpLabelRec;
      New(tmpLabelRec);
      tmpLabelRec^.Next:=NIL;
      IF FirstLabel THEN
        BEGIN
          FirstLabelRec:=tmpLabelRec;
          FirstLabel:=False;
        END
      ELSE NextLabelRec^.Next:=tmpLabelRec;
      tmpLabelRec^.Value:=aCode;
      tmpLabelRec^.Text:=aLabel;
    end;
  aLabelRec:=FirstLabelRec;
end;

procedure TLabelValueList.Assign(Source: TPersistent);
var
 i, co : integer;
begin
  if Source is TLabelValueList then
  begin
    BeginUpdate;
    try
      Clear;
      co := TLabelValueList(source).count;
      for i:= 0 to co-1 do
          addPair(TLabelValueList(source).strings[i],pchar(TLabelValueList(source).objects[i]));
      Labelname :=TLabelValueList(source).labelname;
    finally
      EndUpdate;
    end;
    Exit;
  end;
  inherited Assign(Source);
end;

procedure TLabelValueList.clear;
var
   s : pchar;
   i: integer;
begin
  for i:= 0 to count-1 do
  begin
       s:=pchar(objects[i]);
       if s<> nil then
           strdispose(s);
  end;
  inherited;
end;

constructor TLabelValueList.Create;
begin
  inherited;
  sorted:=true;
  Duplicates:=dupIgnore;
end;


destructor TLabelValueList.Destroy;
begin
 clear;
 inherited;
end;

function TLabelValueList.GetCode(const aLabel: string): string;
begin
end;

function TLabelValueList.GetLabel(const aCode: string): string;
var
 idx : integer;
 p : pchar;
begin
  Result:= aCode; // Edit by Torsten 19-may 2006 from Result := '';
  if not Find(aCode, idx) then exit;
  p:=pchar(objects[idx]);
  setString(result,p,strlen(p));
end;

procedure TLabelValueList.SetCode(const aLabel, Value: string);
begin

end;

procedure TLabelValueList.SetLabel(const aCode, Value: string);
var
  idx: integer;
  s: PChar;
begin
  if not Find(aCode, idx) then Error('Valuecode does not exist', 0);
  s :=pchar(objects[idx]);
  if s <> nil then
    StrDispose(s);
  s := stralloc(length(Value)+1);
  strlcopy(s, pchar(Value), length(Value));
  Objects[idx] := TObject(s);
end;


{ TLabelBlocksList }

procedure TLabelBlocksList.clear;
var
 i, co : integer;
 val : TLabelValueList;
begin
 co := count;
 for i:= 0 to co-1 do
 begin
   val:= TLabelValueList(objects[i]);
   FreeAndNil(val);
 end;
end;

constructor TLabelBlocksList.Create;
begin
 inherited;
 Sorted:=true;
 Duplicates:=dupIgnore;
end;

destructor TLabelBlocksList.Destroy;
begin
  clear;
  inherited;
end;

procedure TLabelBlocksList.CopyToValueLabelSet(var aSet: TStringList);
var
  curLabelSet:integer;
  aLabelSet: TLabelValueList;
  aLabelRec: PLabelRec;
begin
  if aSet=NIL then aSet:=TStringList.Create;
  for curLabelSet:=0 to count-1 do
    begin
      aLabelSet:=TLabelValueList(objects[curLabelSet]);
      aLabelSet.CopyToLabelRec(aLabelRec);
      aSet.AddObject(strings[curLabelSet],TObject(aLabelRec));
    end;
end;

procedure TLabelBlocksList.CopyFromValueLabelSet(var aSet: TStringList);
var
  n:Integer;
  ALabelRec:PLabelRec;
  labelname,acode,alabel: string;
  LabelSet: TLabelValueList;
begin
  if (NOT assigned(aSet)) then exit;
  if (aSet.Count=0) then exit;
  for n:=0 to aSet.Count-1 do
    begin
      labelname:=aSet.Strings[n];
      ALabelRec:=PLabelRec(aSet.Objects[n]);
      LabelSet:=TLabelValueList.create;
      WHILE ALabelRec<>NIL do
        begin
          acode:=ALabelRec^.Value;
          alabel:=ALabelRec^.Text;
          if acode[1]<>'*' then LabelSet.AddPair(acode,alabel);
          ALabelRec:=ALabelRec^.Next;
        end;  //while
      labelset.fLabelName := labelname;
      self.AddObject(labelname,LabelSet);
    end;  //for
end;

procedure TLabelBlocksList.Clone(var dest: TLabelBlocksList);
var
  n:integer;
  labelset: TLabelValueList;
begin
  if (not assigned(dest)) then dest:=TLabelBlocksList.Create;
  for n:=0 to count-1 do
  begin
    labelset := TLabelValueList.Create;
    labelset.Assign(TLabelValueList(objects[n]));
    dest.AddObject(strings[n],labelset);
  end;
end;

{ TEpiInfoCustomUserData }

constructor TEpiInfoCustomUserData.Create(const pDataLabel,
  pPassword: string; pbackcolor: integer);
begin
  inherited Create;
  fDataLabel:=pDataLabel;
  fPassword:= pPassword;
  fbackcolor:= pbackcolor;
end;

function TEpiInfoCustomUserData.GetGlobalMissingValues(index:integer):string;
begin
  result:=FGlobalMissingValues[index];
end;

procedure TEpiInfoCustomUserData.SetGlobalMissingValues(Index: Integer; Value:string);
begin
  FGlobalMissingValues[index]:=Value;
end;


{ TEpiInfoCustomFieldUserData }

constructor TEpiInfoCustomFieldUserData.Create;
begin
  inherited;
  fLabelColor:=14;
  fFieldColor:=15;
end;

function TEpiInfoCustomFieldUserData.GetMissingValues(index:integer):string;
begin
  result:=FMissingValues[index];
end;

procedure TEpiInfoCustomFieldUserData.SetMissingValues(Index: Integer; Value:string);
begin
  FMissingValues[index]:=Value;
end;

end.
