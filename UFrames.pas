unit UFrames;

interface
uses
  controls,sysutils, dialogs, classes,db,ansDataTypes,UEpiDataTypes,Uepifile, windows, EpiDataFile,
  SMUtils, UCmdTypes, UCheckProps, EpiDataUtils, DateUtils, Math,
  {$IFDEF NativeXML}nativexml{$ELSE}XMLdoc, xmlintf, xmldom{$ENDIF};

const
  SNotEditing = 'DataSet not in update state';
  SFieldReadOnly = 'Field "%s" is read only';
  SIncorrectFileHeader = 'DBF file header incorrect';
  SInvalidFileFormat = 'Invalid file format';
  SFileCorrupted = 'File corrupted';
  SReadingAfterEOF = 'Internal Error: read after eof';
  STableNameBlank = 'Table name is blank';
  STableNotExists = 'Table "%s" does not exist';
  SFieldTypeNotSupported = 'Field type not supported in field "%s"';
  SFieldNameTooBig = 'Field name too long in field "%s"';


Type
TEpiDataSetClass=class of TEpiDataSet;

TEpiDataSet=class(TPersistent)
protected
  FOpenMode: Epiint;
  fFileName: TEpiFileName;
  FReadDeletedRecords: Boolean;
  function  GetRecordSize: integer;virtual;
  function  GetSupportsLabels: boolean;virtual;
  procedure SetDeleted(const Value: boolean);virtual;
  function  GetDeleted: boolean;virtual;
  procedure SetVerified(const Value: boolean); virtual;
  function  GetVerified: boolean; virtual;
  function  GetField(Index: integer): TeField; virtual;abstract;
  function  GetFieldCount: integer; virtual;abstract;
  function  GetRecordCount: integer; virtual;abstract;
  function  GetDataSet:TDataSet;virtual;
  function  GetBOF: boolean;virtual;abstract;
  function  GetEOF: boolean;virtual;abstract;
  function  GetFileLabel: string;virtual;
  procedure SetFileLabel(const Value: string);virtual;
  function  getDate(obs: integer; s,dateformat:string; VAR y,m,d:word):boolean;
public
 Constructor Create(const filename: TEpiFileName;Mode:Epiint;var fCheckProperties: TdfChkProp; const pw:string='');virtual;
 Constructor CreateNew(const filename: TEpiFileName; EpiFile:TEpiDataFile; var fCheckProperties: TdfChkProp; const pw:string='');virtual;
 Destructor Destroy;override;
 function GetFieldData(obs:integer; Fld: TeField; Dst: Pointer;var Blank:boolean): Boolean;virtual;abstract;
 procedure SetFieldData(Fld: TeField; Src: Pointer);virtual;abstract;
 Procedure Error(s:string);
 class Function GetNewDataSet(const filename: TEpiFileName; EpiFile: TEpiDataFile; var fCheckProperties: TdfChkProp; const pw:string=''):TEpiDataSet;virtual;
 class function GetErrorText(): string;
 class procedure SetErrorText(const ErrorText: string);
 function First:integer;virtual;abstract;
 function Next:integer; virtual;abstract;
 function Prev:integer; virtual;abstract;
 function Last:integer;virtual;abstract;
 function Append:integer;virtual;abstract;
 function Post:integer;virtual;abstract;
 function GetValueLabel(idx:integer):TLabelValueList;virtual;abstract;
 function GetLabelsCount: integer;virtual;
 property BOF:boolean read GetBOF;
 property EOF:boolean read GetEOF;
 property DataSet : TDataSet read GetDataSet;
 property FileName :TEpiFileName read fFileName;
 property OpenMode : Epiint read FOpenMode;
 property Fields[Index: integer]: TeField read GetField;
 property FieldCount: integer read GetFieldCount;
 property RecordCount: integer read GetRecordCount;
 property RecordSize : integer read GetRecordSize;
 property SupportsLabels : boolean read GetSupportsLabels;
 property FileLabel :string read GetFileLabel write SetFileLabel;
 property Deleted: boolean read GetDeleted write SetDeleted;
 property Verified: boolean read GetVerified write SetVerified;
 Property ErrorText: string read GetErrorText;
 Property ReadDeletedRecords: Boolean read FReadDeletedRecords write FReadDeletedRecords;
end;


TEpiInfoDataset=class(TEpiDataSet)
private
  //FSHEpiFile: TEpiFile;
  FEpiFile: TEpiDataFile;
  //epiFldList: TEpiFieldList;
  FCurrentRec: LongInt;
protected
  function GetBOF: boolean;override;
  function GetEOF: boolean;override;
  function GetField(Index: integer): TeField; override;
  function GetFieldCount: integer; override;
  function GetRecordCount: integer; override;
  function GetRecordSize: integer;override;

public
  Constructor Create(const filename: TEpiFileName;Mode:Epiint;var fCheckProperties: TdfChkProp; const pw:string='');override;
  Constructor CreateNew(const filename: TEpiFileName; EpiFile:TEpiDataFile;var fCheckProperties: TdfChkProp; const pw:string='');override;
  Destructor Destroy;override;
  function GetFieldData(obs:integer; Fld: TeField; Dst: Pointer;var Blank:boolean): Boolean;override;
  procedure SetFieldData(Fld: TeField; Src: Pointer);override;
  class Function GetNewDataSet(const filename: TEpiFileName; EpiFile: TEpiDataFile; var fCheckProperties: TdfChkProp; const pw:string=''):TEpiDataSet;override;
  function GetSupportsLabels: boolean;override;
  function GetLabelsCount: integer;override;
  function GetValueLabel(idx:integer):TLabelValueList;override;
  function First:integer;override;
  function Next:integer; override;
  function Prev:integer; override;
  function Last:integer;override;
  function Append:integer;override;
  function Post:integer;override;
  function GetFileLabel: string;override;
  procedure SetFileLabel(const Value: string);override;
  procedure SetDeleted(const Value: boolean);override;
  function  GetDeleted: boolean;override;
  procedure SetVerified(const Value: boolean); override;
  function  GetVerified: boolean; override;
  procedure RequestPasswordEvent(Sender: TObject; requesttype:TRequestPasswordTypes; var password:String);
end;


  PTableData = ^TTableData;
  TTableData = packed record
    Version: Byte;
    Year, Month, Day: Byte;
    RecordCount: DWord;
    Offset: Word;
    RecordSize: Word;
    Reserved: array[1..20] of Byte;
  end;

  PFieldData = ^TFieldData;
  TFieldData = packed record
    FieldName: array [0..10] of Char;
    FieldType: Char;
    Offset:    Longint;
    FieldLen:  Byte;
    FieldDec:  Byte;
    Reserved:  array[1..14] of Char;
  end;

  PFieldsData = ^TFieldsData;
  TFieldsData = array [0..128] of TFieldData;


TEpiDBFdataset=class(TEpiDataSet)
protected
  fEpiFile: TEpiDataFile;
  FTableName: string;
  FFileHandle: Cardinal;
  FTableData: TTableData;
  FFieldData: PFieldsData;
  FCurrentRec: LongInt;
  FRecordCount: DWord;
  FFieldCount: Integer;
  FRecordSize: Integer;
  FRecBufSize: Integer;
  FOffset:     Integer;
  FRecBuffer:  PChar;
  function GetBOF: boolean;override;
  function GetEOF: boolean;override;
  function GetField(Index: integer): TeField; override;
  function GetFieldCount: integer; override;
  function GetRecordCount: integer; override;
  function GetRecordSize: integer;override;
  function isFieldname(s:string):boolean;
public
  Constructor Create(const filename: TEpiFileName;Mode:Epiint;var fCheckProperties: TdfChkProp; const pw:string='');override;
  Constructor CreateNew(const filename: TEpiFileName; EpiFile:TEpiDataFile;var fCheckProperties: TdfChkProp; const pw:string='');override;
  Destructor Destroy;override;
  function GetFieldData(obs:integer; Fld: TeField; Dst: Pointer;var Blank:boolean): Boolean;override;
  procedure SetFieldData(Fld: TeField; Src: Pointer);override;
  class Function GetNewDataSet(const filename: TEpiFileName; EpiFile:TEpiDataFile;var fCheckProperties: TdfChkProp; const pw:string=''):TEpiDataSet;override;
  function First:integer;override;
  function Next:integer; override;
  function Prev:integer; override;
  function Last:integer;override;
  function Append:integer;override;
  function Post:integer;override;
end;


{TEpiTXTdataset for import of txt-files/cvs-files
 added 5. sep. 2004 by Michael Bruus  }

TkorteFelttyper=      (fktInt,fktFloat,fktFieldname,fktMDY,fktDMY,fktYMD,fktString);
PFelttype=^TkorteFelttyper;


TEpiTXTdataset=class(TEpiDataSet)
protected
  //epiFldList:   TEpiFieldList;
  FEpiFile: TEpiDataFile;
  FTableName:   string;
  FFile:        TStringList;
  FCurrentRec:  LongInt;
  FRecordCount: DWord;
  FFieldCount:  Integer;
  FFieldnamesInRow1: Boolean;
  FSepChar:     Char;
  FRecordSize:  Integer;
  FRecordBuf:   String;
  function GetBOF: boolean;override;
  function GetEOF: boolean;override;
  function GetField(Index: integer): TeField; override;
  function GetFieldCount: integer; override;
  function GetRecordCount: integer; override;
  function GetRecordSize: integer;override;
  function NumberOfChars(s:string; c:char):integer;
  function FieldStartPos(s:string; fieldNo:Integer):integer;
  function FieldEndPos(s:string; startpos:integer):integer;
  function isInt(s:string):boolean;
  function isFloat(s:string):boolean;
  function isFieldname(s:string):boolean;
  function RemoveSpaces(s:string):string;
  function isDate(s:string; datetype:TkorteFelttyper):boolean;
  function testText(s:string;curfelttype:TkorteFelttyper):TkorteFelttyper;
  procedure ReadFromClipboard();
public
  Constructor Create(const filename: TEpiFileName;Mode:Epiint;var fCheckProperties: TdfChkProp; const pw:string='');override;
  Constructor CreateNew(const filename: TEpiFileName; EpiFile:TEpiDataFile;var fCheckProperties: TdfChkProp; const pw:string='');override;
  Destructor  Destroy;override;
  function    GetFieldData(obs:integer; Fld: TeField; Dst: Pointer;var Blank:boolean): Boolean;override;
  procedure   SetFieldData(Fld: TeField; Src: Pointer);override;
  class Function GetNewDataSet(const filename: TEpiFileName; EpiFile: TEpiDataFile; var fCheckProperties: TdfChkProp; const pw:string=''):TEpiDataSet;override;
  function First:integer;override;
  function Next:integer; override;
  function Prev:integer; override;
  function Last:integer;override;
  function Append:integer;override;
  function Post:integer;override;
end;

TEpiEPXDataset=class(TEpiDataSet)
private
  FFirstRec: Boolean;   // Needed because read records procedure in UVectors does both a ADataSet.first AND Adataset.Next to read the first record!!!!
  FRecCount: integer;
  FFieldList: TStringList;
{$IFDEF NativeXML}
  FRecNode: TXmlNode;
  FEpiDocument: TNativeXml;
  FEpiDataFile: TXmlNode;
{$ELSE}
  FRecNode: IXMLNode;
  FEpiDocument: TXMLDocument;
  FEpiDataFile: IXMLNode;
{$ENDIF}
  FEpiDocFormatSettings: TFormatSettings;
  FLocalLabelBlockList: TLabelBlocksList;
  FMissingValuesList: TStringList;
  FDataList: TStringList;
  FVersion: Integer;
private
{$IFDEF NativeXML}
  function GetNode(Const Name: string; Parent: TXmlNode): TXmlNode;
  function GetNodeText(Node: TXmlNode): string;
  function GetAttribute(Const Name: string; Parent: TXmlNode): string;
  function GetFirstChild(Parent: TXmlNode): TXmlNode;
  function GetNextSibling(Node: TXmlNode): TXmlNode;
{$ELSE}
  function GetNode(Const Name: string; Parent: IXMLNode): IXMLNode;
  function GetNodeText(Node: IXMLNode): string;
  function GetAttribute(Const Name: string; Parent: IXMLNode): string;
  function GetFirstChild(Parent: IXMLNode): IXMLNode;
  function GetNextSibling(Node: IXMLNode): IXMLNode;
{$ENDIF}
private
  function FtNumberFromFtText(FTText: string): integer;
  procedure LoadValueLabelSetsV2(VLSetsNode: {$IFDEF NativeXML}TXmlNode{$ELSE}IXMLNode{$ENDIF});
  procedure LoadValueLabelSetsV3(VLSetsNode: {$IFDEF NativeXML}TXmlNode{$ELSE}IXMLNode{$ENDIF});
protected
  function  GetDeleted: boolean; override;
  function  GetField(Index: integer): TeField; override;
  function  GetFieldCount: integer; override;
  function  GetFileLabel: string; override;
  function  GetRecordCount: integer; override;
  function  GetVerified: Boolean; override;
public
  Constructor Create(const filename: TEpiFileName;Mode:Epiint;var fCheckProperties: TdfChkProp; const pw:string='');override;
  Destructor  Destroy;override;
  function    First: integer; override;
  function    GetFieldData(obs:integer; Fld: TeField; Dst: Pointer; var Blank: boolean): Boolean; override;
  function    Next: integer; override;
end;


implementation

uses uDateUtils, PasswordUnit, Forms, UCmdProcessor, ClipBrd, VCLUnZip,
  TypInfo;

CONST
  antallinier=100000;
  QuoteChars: Set of CHAR=['"',''''];
  DecimalpointChars: Set of CHAR=['.',','];
  FieldnameChars:  Set of CHAR=['0'..'9','A'..'Z','a'..'z',' '];
  IntegerChars:    Set of CHAR=['0'..'9','-','+'];
  FloatChars:      Set of CHAR=['0'..'9', '.', ',', '-', '+'];
  DateChars:       Set of CHAR=['0'..'9','/','-'];
  DateSepChars:    Set of CHAR=['/','-'];
  NumChars:        Set of CHAR=['0'..'9'];
  DummyStringComma = #243#176#80#80;  // Unicode private area: http://www.utf8-chartable.de/unicode-utf8-table.pl?start=983040
  DummyStringSpace = #243#176#80#81;
  DummyStringQuote = #243#176#80#82;
  DummyStringDQuote = #243#176#80#83;
var
  FErrorText: string;


{ TEpiDataSet }

function max(a, b: Epiint): longint;
begin
     if (a > b) then
        max := a
     else
       max := b;
end;

constructor TEpiDataSet.Create(const filename: TEpiFileName; Mode: Epiint;var fCheckProperties: TdfChkProp; const pw:string='');
begin
  inherited Create;
  fFilename:=filename;
  fOpenMode := Mode;
end;

constructor TEpiDataSet.CreateNew(const filename: TEpiFileName; EpiFile: TEpiDataFile; var fCheckProperties: TdfChkProp; const pw:string='');
begin
  inherited Create;
end;

Procedure TEpiDataSet.Error(s:string);
BEGIN
  FErrortext:=s;
END;

destructor TEpiDataSet.Destroy;
begin
 inherited;
end;

function TEpiDataSet.GetDataSet: TDataSet;
begin
 result:=nil;
end;

procedure TEpiDataSet.SetDeleted(const Value: boolean);
begin
end;

procedure TEpiDataSet.SetVerified(const Value: boolean);
begin
end;

function TEpiDataSet.GetFileLabel: string;
begin
 Result:='';
end;

function TEpiDataSet.GetLabelsCount: integer;
begin
   Result:=0;
end;

class function TEpiDataSet.GetNewDataSet(const filename: TEpiFileName; EpiFile: TEpiDataFile; var fCheckProperties: TdfChkProp; const pw:string=''): TEpiDataSet;
begin
 Result:=nil;
end;

class function TEpiDataSet.GetErrorText(): string;
begin
  result := FErrorText;
end;

class procedure TEpiDataSet.SetErrorText(const ErrorText: string);
begin
  FErrorText := ErrorText;
end;

function TEpiDataSet.GetRecordSize: integer;
begin
 Result:=0;
end;

function TEpiDataSet.GetSupportsLabels: boolean;
begin
 Result:=false;
end;

function TEpiDataSet.GetDeleted: boolean;
begin
   Result:=false;
end;

function TEpiDataSet.GetVerified: boolean;
begin
  result:=false;
end;

procedure TEpiDataSet.SetFileLabel(const Value: string);
begin
end;

function TEpiDataset.getDate(obs:integer;s,dateformat:string; VAR y,m,d:word):boolean;
var
  n,pos1,pos2,antaldateseptegn:Integer;
  //  s2:string;
begin
//  DecodeDate(mibStrToDate(s, ftDate), y, m, d);
  try
    y:=0;
    m:=0;
    d:=0;
    result:=false;
    if trim(s)='' then exit;
    result:=true;
    antaldateseptegn:=0;
    pos1:=0;
    pos2:=0;
    n:=1;
    WHILE (n<=length(s)) DO
      BEGIN
        if (s[n] in DateSepChars) THEN
          begin
            inc(antaldateseptegn);
            if pos1=0 then pos1:=n else pos2:=n;
          end;
        inc(n);
      END;  //while
    if antaldateseptegn<>2 then begin result:=false;  exit; end;
    if (pos1+1=pos2) or (pos1=1) or (pos2=1) or (pos1=length(s)) or (pos2=length(s)) then begin result:=false;  exit;  end;
    if dateformat='%MDY' then
      begin
        m:=strtoint(copy(s,1,pos1-1));
        d:=strtoint(copy(s,pos1+1,pos2-pos1-1));
        y:=strtoint(copy(s,pos2+1,length(s)));
      end
    else if dateformat='%DMY' then
      begin
        d:=strtoint(copy(s,1,pos1-1));
        m:=strtoint(copy(s,pos1+1,pos2-pos1-1));
        y:=strtoint(copy(s,pos2+1,length(s)));
      end
    else if dateformat='%YMD' then
      begin
        y:=strtoint(copy(s,1,pos1-1));
        m:=strtoint(copy(s,pos1+1,pos2-pos1-1));
        d:=strtoint(copy(s,pos2+1,length(s)));
      end;
    encodedate(y,m,d);
  except
    if (obs <> -1) then  dm.info('Date error: %s in %d', [s,obs], 44003);
    result:=false;
    exit;
  end;
end;


{ TEpiDBFdataset }

function TEpiDBFdataset.Append: integer;
begin

end;

function TEpiDBFdataset.isFieldname(s:string):boolean;
var
  n:Integer;
begin
  result:=false;
  result:=true;
  IF trim(s)='' then exit;
  n:=1;
  WHILE (n<=length(s)) AND (result) DO
    BEGIN
      IF (NOT (s[n] in FieldnameChars)) THEN result:=False;
      INC(n);
    END;
  IF (s[1] in NumChars) THEN result:=False;
end;  //function isFieldname


constructor TEpiDBFdataset.Create(const filename: TEpiFileName; Mode: Epiint;var fCheckProperties: TdfChkProp; const pw:string='');
var
  Index, FldSz, DataSize: Integer;
  FieldName: array [0..10] of Char;
  i, co, vCounter : integer;
  //fld :TEpiField;
  cb: Cardinal;
  FieldDataSize: Integer;
  OffsetCounter: Integer;
  s:String;
  aField: TeField;

begin
  inherited Create(filename, Mode, fCheckProperties);
  FTableName:= filename;
  FEpiFile:=TEpiDataFile.Create;

  FFileHandle := CreateFile(PChar(FTableName), GENERIC_READ + GENERIC_WRITE,
    0, nil, OPEN_EXISTING, FILE_FLAG_RANDOM_ACCESS, 0);
  if FFileHandle = INVALID_HANDLE_VALUE then
    RaiseLastWin32Error;
  Win32Check(ReadFile(FFileHandle, FTableData, sizeof(TTableData), cb, nil));
  if cb <> sizeof(TTableData) then
    DatabaseError(SIncorrectFileHeader);
  FRecordCount:=FTableData.RecordCount;
  FRecordSize:=FTableData.RecordSize;
  GetMem(FRecBuffer,FRecordSize);

  FOffset:=FTableData.Offset;
  FieldDataSize := FTableData.Offset - sizeof(TTableData);
  if FieldDataSize <= 0 then
    DatabaseError(SFileCorrupted);
  FFieldData := AllocMem(FieldDataSize);
  Win32Check(ReadFile(FFileHandle, FFieldData^, FieldDataSize, cb, nil));
  if FieldDataSize <> Integer(cb) then
    DatabaseError(SFileCorrupted);

  FldSz := 0;
  Index := 0;
  FRecBufSize := 1; // delete flag
  OffsetCounter:=1;  //tag højde for delte flag

  vCounter:=1;
  while (FldSz < FTableData.Offset) and (PChar(@FFieldData[Index])^ <> #$0D) do
    begin
      aField:=TeField.Create;
      StrLCopy(FieldName, FFieldData[Index].FieldName, 11);
      s:=Fieldname;
      IF (NOT isFieldname(s)) then
        begin
          aField.FVariableLabel:=s;
          s:='v'+IntToStr(vCounter);
          INC(vCounter);
        end;
      aField.FieldName:=s;
      case FFieldData[Index].FieldType of
        'C':
          begin
            aField.Felttype:=ftAlfa;
            aField.FLength:=FFieldData[Index].FieldLen;
            aField.FFieldFormat:=format('%%s%d',[aField.FLength])
          end;
        'L' :    //MIB rettet 19-08-04
          begin
            aField.Felttype:=ftBoolean;
            aField.FLength:=1;
          end;
        'D':
          begin
            aField.Felttype:=ftEuroDate;
            aField.FLength:=10;
            aField.FFieldFormat:='%DMY';
          end;
        'N':
          begin
            aField.Felttype:=ftInteger;
            aField.FLength:=FFieldData[Index].FieldLen;
            if (aField.FLength>4) or (FFieldData[Index].FieldDec>0) then aField.Felttype:=ftFloat;
            aField.FNumDecimals:=FFieldData[Index].FieldDec;
            if aField.Felttype=ftInteger
            then aField.FFieldFormat:='%d'
            else aField.FFieldFormat:='%'+IntToStr(aField.FLength)+'.'+IntToStr(aField.FNumDecimals)+'f';
          end;
        'F':
          begin
            aField.Felttype:=ftFloat;
            aField.FLength:=FFieldData[Index].FieldLen;
            aField.FNumDecimals:=FFieldData[Index].FieldDec;
            aField.FFieldFormat:='%'+IntToStr(aField.FLength)+'.'+IntToStr(aField.FNumDecimals)+'f';
          end;
      else
        aField.Felttype:=ftRes4;
      end;
      aField.FFieldNo:=Index;
      aField.FStartPos:=OffsetCounter;
      FFieldData[Index].Offset:=OffsetCounter;
      if aField.Felttype<>ftRes4 then
        begin
          FepiFile.AddField(aField);
          INC(FFieldCount);
        end
      else
        begin
          aField.Free;
          DatabaseErrorFmt(SFieldTypeNotSupported, [FieldName]);
        end;
      Inc(FldSz, sizeof(TFieldData));
      FFieldData[Index].Offset := FRecBufSize;
      Inc(FRecBufSize, FFieldData[Index].FieldLen);
      Inc(OffsetCounter,FFieldData[Index].FieldLen);
      Inc(Index);
    end;  //while

  if FRecBufSize <> FTableData.RecordSize then
    DatabaseError(SFileCorrupted);

  SetFilePointer(FFileHandle, 0, nil, FILE_BEGIN);

end;

constructor TEpiDBFdataset.CreateNew(const filename: TEpiFileName; EpiFile: TEpiDataFile; var fCheckProperties: TdfChkProp; const pw:string='');
begin
  inherited CreateNew(filename, EPiFile, fCheckProperties, pw);
end;

destructor TEpiDBFdataset.Destroy;
begin
  CloseHandle(FFileHandle);
  FreeMem(FFieldData);
  FreeMem(FRecBuffer);
  FEpiFile.Free;
  inherited;
end;


function TEpiDBFdataset.First: integer;
begin
  SetFilePointer(FFileHandle, 0, nil, FILE_BEGIN);
end;

function TEpiDBFdataset.GetBOF: boolean;
begin
  result:=FCurrentRec<1;
end;


function TEpiDBFdataset.GetEOF: boolean;
begin
    result:=FCurrentRec>FRecordCount;
end;

function TEpiDBFdataset.GetField(Index: integer): TeField;
begin
  result:=FEpiFile.Fields[index];
end;

function TEpiDBFdataset.GetFieldCount: integer;
begin
   result:=FEpiFile.NumFields;
end;

function TEpiDBFdataset.GetFieldData(obs:integer; Fld: TeField; Dst: Pointer;  var Blank: boolean): Boolean;
var
 //aFld :TEpiField;
 by : char;
 b :boolean;
 int : integer;
 f   : EpiFloatStorage;
 d   : extended;
 buf : array[0..100] of char;
 code: Integer;
 SeekPos,cb: Cardinal;
 FieldData: PFieldData;
 day,year,month:word;
 s:String;
 tmpdecimalseparator:char;
 aField: TeField;
begin
try
  Result:=true;
  Blank:=False;
  aField:=Fld;

  FieldData := @FFieldData[Fld.FFieldNo];  // Fld.FieldNo];

  SeekPos:=SetFilePointer(FFileHandle, 0, nil, FILE_CURRENT);
  IF SeekPos=0 THEN Exit;

  case FieldData.FieldType of
  'C':
    begin
      StrLCopy(buf, FRecBuffer+FieldData.Offset, FieldData.FieldLen);
      s:=buf;
      if trim(s)='' THEN blank:=True else StrPCopy(dst,s);
      //blank:=??
    end;
  'N':
    begin
      StrLCopy(buf, FRecBuffer+FieldData.Offset, FieldData.FieldLen);
      s:=buf;
      if trim(s)='' THEN blank:=true
      else
        begin
          IF (FieldData.FieldDec=0) and (aField.Felttype=ftInteger) THEN
            begin
              Val(Trim(Buf), int, Code);
              int:=trunc(int);
              integer(dst^):=int;
            end
          else
            begin
              Val(trim(buf),d,Code);
              if Code<>0 THEN
                begin
                  tmpDecimalseparator:=DecimalSeparator;
                  IF tmpDecimalseparator='.' THEN DecimalSeparator:=',' ELSE DecimalSeparator:='.';
                  Val(trim(buf),d,Code);
                  Decimalseparator:=tmpDecimalseparator;
                end;
              f:=d;
              EpiFloatStorage(dst^):=d;
            end;
        end
    end;
  'F':
    begin
      StrLCopy(buf,FRecBuffer+FieldData.Offset,FieldData.FieldLen);
      s:=buf;
      if trim(s)='' THEN blank:=true
      else
        begin
          Val(trim(buf),d, Code);
          f:=d;
          EpiFloatStorage(dst^):=d;
        end
    end;
  'L':
    begin
      StrLCopy(buf, FRecBuffer+FieldData.Offset, 1);
      if buf[0]=' ' then blank:=true;
      if (buf[0] in ['T','t','Y','y']) then Byte(dst^) := 1
      else Byte(dst^) := 0;
    end;
  'D':
    begin
      StrLCopy(buf, FRecBuffer+FieldData.Offset, 4);
      s:=trim(buf);
      if s<>'' then year := StrToInt(buf);
      StrLCopy(buf, FRecBuffer+FieldData.Offset + 4, 2);
      s:=s+trim(buf);
      if s<>'' then month := StrToInt(buf);
      StrLCopy(buf, FRecBuffer+FieldData.Offset + 6, 2);
      s:=s+trim(buf);
      if s<>'' then day := StrToInt(buf);
      if trim(s)='' then blank:=true
      else EpiDate(dst^):=EpiYMDToDate(FCurrentRec,year,month,day);
    end;
  end;  //case
except
  Result:=false;
end;
end;

class function TEpiDBFdataset.GetNewDataSet(const filename: TEpiFileName; EpiFile: TEpiDataFile; var fCheckProperties: TdfChkProp; const pw:string=''): TEpiDataSet;
var
  i, co, n, len   : integer;
  //fld        : TEpiField;
  //lDataset  :TSMXBase;
begin

{  co := Structure.count;
  lDataSet := TSMXBase.Create(nil);
  lDataset.TableName:= filename;
  lDataset.FieldDefs.clear;
  for i:= 0 to co-1 do
  begin
     fld := Structure[i] ;
     len := 0;
     if fld.DataType in [EpiTyString] then
        len := fld.FieldLen;
     lDataset.FieldDefs.Add(fld.Fieldname, EpiFldTypeToFieldType(fld.DataType),len, True);
  end;
  lDataset.CreateTable;
  lDataset.close;
  Result:= self.Create(filename,0,CustomUserData);}
end;

function TEpiDBFdataset.GetRecordCount: integer;
begin
  result:=FRecordCount;
end;

function TEpiDBFdataset.GetRecordSize: integer;
begin
  result:= FRecordSize;
end;

function TEpiDBFdataset.Next: integer;
var
  cb,SeekPos: Cardinal;
begin
  SeekPos := SetFilePointer(FFileHandle, 0, nil, FILE_CURRENT);
  IF SeekPos > 0
  THEN SetFilePointer(FFileHandle, FRecordSize, nil, FILE_CURRENT)
  ELSE SetFilePointer(FFileHandle, FTableData.Offset, nil, FILE_BEGIN);
  Win32Check(ReadFile(FFileHandle, FRecBuffer^, FRecordSize, cb, nil));
  SeekPos := SetFilePointer(FFileHandle, -FRecordSize, nil, FILE_CURRENT);
end;

function TEpiDBFdataset.Last: integer;
begin
  SetFilePointer(FFileHandle, -FRecordSize, nil, FILE_END);
end;

function TEpiDBFdataset.Post: integer;
begin

end;

function TEpiDBFdataset.Prev: integer;
var
  SeekPos:Cardinal;
begin
  SeekPos := SetFilePointer(FFileHandle, 0, nil, FILE_CURRENT);
  IF SeekPos=0 THEN Last
  ELSE SetFilePointer(FFileHandle, -FRecordSize, nil, FILE_CURRENT);
end;

procedure TEpiDBFdataset.SetFieldData(Fld: TeField; Src: Pointer);
begin
  raise exception.create('Capability not supported');
end;

{ TEpiInfoDataset }

function TEpiInfoDataset.Append: integer;
begin
  FEpiFile.Append;
end;

constructor TEpiInfoDataset.Create(const filename: TEpiFileName; Mode:Epiint; var fCheckProperties: TdfChkProp; const pw:string='');
//Opens an existing rec-file + checkfile and fills out fCheckProperties for the dataframe
var
 Err,n,p,t : integer;
 ok:Boolean;
 AField: TeField;
 s,s2:String;
begin
  inherited Create(filename, Mode,fCheckProperties);
  Err:=0;
  if Err<> 0 then CheckfileError(Err);
  TRY
    FEpiFile:=TEpiDataFile.Create;
    FEpiFile.password:=pw;
    FEpiFile.OnRequestPassword:=RequestPasswordEvent;
    ok:=FEpiFile.Open(filename,[StoreInMemory,IgnoreCheckFile]);
    if not ok then
      BEGIN
        FErrorText:=FEpiFile.ErrorText;
        checkfileerror(-2);
        exit;
      END;
    IF FEpiFile.HasCheckFile THEN
      BEGIN
        IF not FEpiFile.LoadChecks then
          BEGIN
            s:=FEpiFile.ErrorText;
            s2:='';
            FOR n:=1 TO Length(s) DO
              begin
                if s[n]=#13 then s2:=s2+'<br>'
                else if s[n]<>#10 then s2:=s2+s[n];
              end;
            FErrorText:=s2;
            dm.Error(FErrorText,[],0);
            checkfileerror(-2);
            Exit;
          END;
      END;

    //Set fieldformat
    for n:=0 to FEpiFile.NumFields-1 do
      begin
        case FEpiFile[n].Felttype of
          ftQuestion:             FEpiFile[n].FFieldFormat:='';
          ftInteger,ftIDNUM:      FEpiFile[n].FFieldFormat:='%d';
          ftFloat:                FEpiFile[n].FFieldFormat:='%'+IntToStr(FEpiFile[n].FLength)+'.'+IntToStr(FEpiFile[n].FNumDecimals)+'f';
          ftDate,ftToday:         FEpiFile[n].FFieldFormat:='%MDY';
          ftEuroDate,ftEuroToday: FEpiFile[n].FFieldFormat:='%DMY';
          ftYMDDate,ftYMDToday:   FEpiFile[n].FFieldFormat:='%YMD';
        else
          FEpiFile[n].FFieldFormat:=format('%%s%d',[FEpiFile[n].FLength]);
        end;  //case

        if (FEpiFile[n].Felttype in [ftInteger,ftFloat]) and (FEpiFile[n].MissingValues[0]='') then
          begin
            for t:=0 to MAXDEFINEDMISSINGVALUES do
              FEpiFile[n].MissingValues[t]:=FEpiFile.GlobalMissingValues[t];
          end;
      end;

    //Fill out CheckProperties
    if (not assigned(fCheckProperties)) then fCheckProperties:=TdfChkProp.Create;
    try
      fCheckProperties.CopyFromEpiFile(FEpiFile);
    Except
      raise exception.Create('Error reading EpiData datafile');
    END;
  EXCEPT
    FreeAndNil(FEpiFile);
    raise;
  END;
end;

constructor TEpiInfoDataset.CreateNew(const filename: TEpiFileName; EpiFile: TEpiDataFile; var fCheckProperties: TdfChkProp; const pw:string='');
//Creates a new rec-file from a already define TEpiDataFile structure
//After saving the rec-file, it is reopened on File, not in memory
var
  i,n: integer;
  aField: TeField;
  maxx, maxy, maxvarlabelwidth,x,y: integer;
  yerror: boolean;
begin
    if (NOT assigned(EpiFile)) or (EpiFile.NumDataFields<1) then CheckfileError(-13);
    EpiFile.password:=pw;
    //Set x-pos og y-pos of all fields
    //First: find max width of FQuestion or variabel label
    //       and find the max y-pos
    //If a field's y-pos is less than the current maxy then yerror is set and all y-pos' are recoded
    maxvarlabelwidth:=0;
    maxy:=0;
    yerror:=false;
    for n:=0 to EpiFile.NumFields-1 do
      begin
        maxvarlabelwidth:=max(length(EpiFile[n].FQuestion),maxvarlabelwidth);
        maxy:=max(EpiFile[n].FFieldY,maxy);
        if EpiFile[n].FFieldY<maxy then yerror:=true;
      end;
    inc(maxvarlabelwidth,2);
    inc(maxy);
    if yerror=false then
      begin
        for n:=0 to EpiFile.NumFields-1 do
          begin
            aField:=EpiFile[n];
            if (aField.FQuestX=0) then aField.FQuestX:=1;
            if (aField.FFieldX=0) then aField.FFieldX:=maxvarlabelwidth;
            if (aField.FQuestY=0) or (aField.FFieldY=0) then
              begin
                if (aField.FQuestY=0) and (aField.FFieldY<>0) then aField.FQuestY:=aField.FFieldY
                else if (aField.FQuestY<>0) and (aField.FFieldY=0) then aField.FFieldY:=aField.FQuestY
                else
                  begin
                    aField.FQuestY:=maxy;
                    aField.FFieldY:=maxy;
                    inc(maxy);
                  end;
              end;
          end;
      end
    else
      begin
        y:=1;
        for n:=0 to EpiFile.NumFields-1 do
          begin
            aField:=EpiFile[n];
            aField.FQuestY:=y;
            aField.FQuestX:=1;
            if aField.Felttype<>ftQuestion then
              begin
                aField.FFieldY:=y;
                aField.FFieldX:=maxvarlabelwidth;
              end;
            inc(y);
          end;
      end;

    EpiFile.SaveStructureToFile(filename,true);
    EpiFile.SaveCheckFile;
    fCheckProperties.Password:=EpiFile.password;
    FEpiFile.Free;
    FEpiFile:=NIL;
    FEpiFile:=TEpiDataFile.create;
    FEpiFile.password:=pw;
    FEpiFile.Open(filename,[IgnoreCheckFile]);
    FEpiFile.LoadChecks;
    fCheckProperties.CopyFromEpiFile(FEpiFile);
    FEpiFile.First;
end;

destructor TEpiInfoDataset.Destroy;
VAR
  n:Integer;
begin
  IF Assigned(FEpiFile) THEN FEpiFile.free;
end;

function TEpiInfoDataset.First: integer;
begin
  IF Assigned(FEpiFile) THEN
    BEGIN
      FEpiFile.First;
      FCurrentRec:=0;
    END;
  Result:=0;
end;

function TEpiInfoDataset.GetBOF: boolean;
begin
  IF Assigned(FEpiFile) THEN result:=(FEpiFile.CurRecord=1);
end;

procedure TEpiInfoDataset.SetDeleted(const Value: boolean);
begin
  IF Assigned(FEpiFile) THEN FEpiFile.CurRecDeleted:=value;
end;

procedure TEpiInfoDataset.SetVerified(const Value:boolean);
begin
  IF Assigned(FEpiFile) THEN FEpiFile.CurRecVerified:=value;
end;

function TEpiInfoDataset.GetEOF: boolean;
begin
  IF Assigned(FEpiFile) THEN result:= FEpiFile.CurRecord>=FEpiFile.NumRecords;
end;

function TEpiInfoDataset.GetField(Index: integer): TeField;
begin
  IF Assigned(FEpiFile) THEN result:=FEpiFile[index];
end;

function TEpiInfoDataset.GetFieldCount: integer;
begin
  IF Assigned(FEpiFile) THEN result:=FEpiFile.NumFields;
end;

function TEpiInfoDataset.GetFieldData(obs:integer; Fld: TeField; Dst: Pointer; var Blank: boolean): Boolean;
VAR
  AField: TeField;
  Fieldtext: string;
  b :   boolean;
  c: char;
  int,code,n : integer;
  f   : EpiFloatStorage;
  d   : extended;
  tmpDecimalseparator:char;
  year,month,day:word;
  aDate: TDateTime;
begin
  IF Assigned(FEpiFile) THEN
    BEGIN
      Result:=True;
      Fieldtext:=trim(fld.AsString);
      IF Fieldtext='' then
        begin
          Blank:=True;
          exit;
        end
      ELSE Blank:=False;
      CASE fld.Felttype of
        ftAlfa,ftUpperAlfa,ftSoundex,ftCrypt:
          begin
            if (fld.Felttype = ftUpperAlfa) then
              fieldtext := Sysutils.AnsiUpperCase(fieldtext);
            StrPCopy(dst,fieldtext);
          end;
        ftInteger,ftIDNUM:
          begin
            Val(trim(fieldtext),int,Code);
            Result:=(Code=0);
            int:=trunc(int);
            integer(dst^):=int;
          end;
        ftFloat:
          begin
            FOR n:=1 TO Length(fieldtext) DO
              BEGIN
                IF fieldtext[n]=',' THEN fieldtext[n]:=DecimalSeparator;
                IF fieldtext[n]='.' THEN fieldtext[n]:=DecimalSeparator;
              END;
            d:=StrToFloat(trim(fieldtext));
            EpiFloatStorage(dst^):=d;
          end;
        ftDate,ftToday,ftEuroDate,ftEuroToday,ftYMDDate,ftYMDToday:
          begin
            aDate:=mibStrToDate(fieldtext,fld.Felttype);
            if aDate=0 then
            //if not getDate(obs, fieldtext,fld.FieldFormat,year,month,day) then
              BEGIN
                blank:=true;
                if (sysutils.Trim(fieldtext)<>'') then result:=false;
              END;
            IF (fieldtext='') or (blank=true) then
              EpiDate(dst^):=NA_DATE
            ELSE EpiDate(dst^):=EpiYMDToDate(obs,YearOf(aDate),MonthOf(aDate),DayOf(aDate));
          end;
        ftBoolean:
            begin
            if (Fieldtext='T') or (Fieldtext='Y') then Word(Dst^) := 1
            else Word(Dst^) := 0;
          end;

      END;  //case
    END;  //if
end;

function TEpiInfoDataset.GetFileLabel: string;
begin
  IF Assigned(FEpiFile) THEN result:=FEpiFile.Filelabel;
end;

function TEpiInfoDataset.GetLabelsCount: integer;
begin
  IF Assigned(FEpiFile) THEN result:=FEpiFile.ValueLabels.Count;
end;

Class function TEpiInfoDataset.GetNewDataSet(const filename: TEpiFileName; EpiFile: TEpiDataFile;
  var fCheckProperties:TdfChkProp; const pw:string=''): TEpiDataSet;
var
  i, co, n, fmaxrow, oldrowno   : integer;
  IsNew         :boolean;
begin
  Result:=TEpiInfoDataset.CreateNew(filename,EpiFile,fCheckProperties,pw);
end;

function TEpiInfoDataset.GetRecordCount: integer;
begin
  IF Assigned(FEpiFile) THEN Result:=FEpiFile.NumRecords;
end;

function TEpiInfoDataset.GetRecordSize: integer;
begin
  IF Assigned(FEpiFile) THEN result:=FEpiFile.RecordLength;
end;

function TEpiInfoDataset.GetSupportsLabels: boolean;
begin
 Result:=true;
end;

function TEpiInfoDataset.GetValueLabel(idx: integer): TLabelValueList;
VAR
  ALabelRec: PLabelRec;
  aCode,aLabel:string;
begin
   Result:= TLabelValueList.create;
   IF Assigned(FEpiFile) THEN
     BEGIN
       Result.LabelName:=AnsiUpperCase(FEpiFile.ValueLabels[idx]);
       ALabelRec:=PLabelRec(FEpiFile.ValueLabels.Objects[idx]);
       WHILE (ALabelRec<>NIL) DO
         BEGIN
           aCode:=trim(ALabelRec^.Value);
           aLabel:=trim(ALabelRec^.Text);
           Result.AddPair(aCode,aLabel);
           ALabelRec:=ALabelRec^.Next;
         END;  //while
     END;  //if
end;

function TEpiInfoDataset.Last: integer;
begin
  IF Assigned(FEpiFile) THEN
    BEGIN
      result:=FEpiFile.NumRecords;
      FEpiFile.Last;
    END;
end;

function TEpiInfoDataset.Next: integer;
begin
  IF Assigned(FEpiFile) THEN
    BEGIN
      IF FCurrentRec=0 THEN
        BEGIN
          FEpiFile.Read(1);
          FCurrentRec:=1;
        END
      ELSE
        BEGIN
          IF Succ(FEpiFile.CurRecord)>FEpiFile.NumRecords THEN Result:=Last
          ELSE FEpiFile.Read(Succ(FEpiFile.CurRecord));
        END;
      Result:=FEpiFile.CurRecord-1;
    END;
end;

function TEpiInfoDataset.Post: integer;
begin
  IF Assigned(FEpiFile) THEN FEpiFile.Post;
end;

function TEpiInfoDataset.Prev: integer;
begin
  IF Assigned(FEpiFile) THEN
    BEGIN
      IF pred(FEpiFile.CurRecord)=0 THEN first
      ELSE FEpiFile.Read(pred(FEpiFile.CurRecord));
      Result:=FEpiFile.CurRecord-1;
    END;
end;

function TEpiInfoDataset.GetDeleted: boolean;
begin
  IF Assigned(FEpiFile) THEN Result:=FEpiFile.CurRecDeleted;
end;

function TEpiInfoDataset.GetVerified: boolean;
begin
  result:=false;
  IF Assigned(FEpiFile) then result:=FEpiFile.CurRecVerified;
end;

procedure TEpiInfoDataset.SetFieldData(Fld: TeField; Src: Pointer);
var
  s:string;
  aField: TeField;
  ed : Epidate;
begin
  if (assigned(FEpiFile)) and (assigned(src)) then
    begin
      aField:=fld;
      case aField.Felttype of
        ftAlfa,ftCrypt,ftSoundex:        s:=PChar(Src);
        ftUpperAlfa:                     s:=sysutils.AnsiUpperCase(PChar(Src));
        ftBoolean:                       if Word(Src^) = 1 then s:='Y' else s:='N';
        ftInteger,ftIDNUM:               s:=IntToStr(integer(src^));
        ftFloat:                         Str(EpiFloatStorage(src^): aField.FLength: aField.FNumDecimals, s);
        ftEuroDate,ftEuroToday:
          begin
            ed := Integer(Src^);
            s:= EpiDateToStr(ed,dfDMY,aField.FLength);
          end;
        ftDate,ftToday:
          begin
            ed := Integer(Src^);
            s:= EpiDateToStr(ed,dfMDY,aField.FLength);
          end;
        ftYMDDate,ftYMDToday:
          begin
            ed := Integer(Src^);
            s:= EpiDateToStr(ed,dfYMD,aField.FLength);
          end;
      end;  //case
      aField.AsString:=s;
    end;  //if
end;

procedure TEpiInfoDataset.SetFileLabel(const Value: string);
begin
  IF Assigned(FEpiFile) THEN FEpiFile.Filelabel:=Copy(Value,1,50);
end;

procedure TEpiInfoDataset.RequestPasswordEvent(Sender: TObject; requesttype:TRequestPasswordTypes; var password:String);
var
  s:string;
begin
  PasswordForm:=TPasswordForm.Create(application);
  try
    if requesttype=rpOpen then PasswordForm.DoubleEntry:=False else PasswordForm.DoubleEntry:=true;
    PasswordForm.lbDatafile.Caption:=extractFileName(self.FEpiFile.RecFilename);
    if PasswordForm.ShowModal=mrOK then password:=PasswordForm.edPW1.Text else password:='';
  finally
    PasswordForm.Free;
  end;
end;


{TEpiTXTDataset}

Function TEpiTXTDataset.NumberOfChars(s:string; c:char):integer;
var
  n:integer;
  quoteChar: char;
  inQuote: boolean;
begin
  inQuote:=false;
  quoteChar:=#0;
  result:=0;
  for n:=1 to length(s) DO
    begin
      if (s[n] in QuoteChars) AND (quoteChar=#0) THEN quoteChar:=s[n];
      if s[n]=quoteChar then inQuote:= (NOT inQuote);
      if (NOT inQuote) AND (s[n]=c) THEN inc(result);
    end;  //for
end;  //function NumChars

Function TEpiTXTDataset.FieldStartPos(s:string; fieldNo:Integer):integer;
VAR
  n,counter:integer;
  quoteChar: char;
  inQuote: boolean;
begin
  //fieldNo counts first field in the line as field #0
  IF fieldNo=0 THEN
    BEGIN
      result:=1;
      Exit;
    END;
  inQuote:=false;
  quoteChar:=#0;
  result:=0;
  counter:=0;
  n:=1;
  while result=0 DO
    BEGIN
      if (s[n] in QuoteChars) and (quoteChar=#0) then quoteChar:=s[n];
      if s[n]=quoteChar then inQuote:=(NOT inQuote);
      if (NOT inQuote) AND (s[n]=FSepChar) THEN INC(counter);
      INC(n);
      if n>length(s) then result:=1;
      IF counter=FieldNo THEN
        BEGIN
          result:=n;
          Exit;
        END;
    END;  //while
end;  //funciton FieldStartPos

Function TEpiTXTDataset.FieldEndPos(s:string; startpos:integer):integer;
var
  n:integer;
  quoteChar: char;
  inQuote: boolean;
begin
  if trim(s)='' then begin result:=startpos;  exit;  end;
  inQuote:=false;
  quoteChar:=#0;
  result:=0;
  n:=startpos;
  while result=0 DO
    BEGIN
      if (s[n] in QuoteChars) and (quoteChar=#0) then quoteChar:=s[n];
      if s[n]=quoteChar then inQuote:=(NOT inQuote);
      if (NOT inQuote) AND (s[n]=FSepChar) then result:=n;
      inc(n);
      if n>length(s) then result:=length(s)+1;
    end;  //while
end;  //function FieldEndPos

function TEpiTXTDataset.isInt(s:string):boolean;
var
  n:integer;
begin
  result:=true;
  IF trim(s)='' then exit;
  if length(s)>EpiMaxIntegerLength then
    begin
      result:=false;
      exit;
    end;
  n:=1;
  WHILE (n<=length(s)) and (result) DO
    BEGIN
      IF (NOT (s[n] in IntegerChars)) THEN result:=false;
      inc(n);
    END;
  IF result then
    begin
      IF (pos('+',s)>1) or (pos('-',s)>1) then result:=false;
    end;
end;

function TEpiTXTDataset.isFloat(s:string):boolean;
var
  n,numdecimalchars:integer;
begin
  result:=true;
  IF trim(s)='' then exit;
  if length(s)>EpiMaxFloatLength then
    begin
      result:=false;
      exit;
    end;
  n:=1;
  numdecimalchars:=0;
  WHILE (n<=length(s)) and (result) DO
    BEGIN
      IF (NOT (s[n] in FloatChars)) THEN result:=false;
      IF (s[n] in decimalpointChars) THEN INC(numdecimalchars);
      INC(n);
    END;  //while
  IF result then
    begin
      IF (pos('+',s)>1) or (pos('-',s)>1) then result:=false;
      IF numdecimalchars>1 then result:=false;
    end;  //if
END;  //function isFloat

function TEpiTXTDataset.isFieldname(s:string):boolean;
var
  n:Integer;
begin
  result:=false;
  result:=true;
  IF trim(s)='' then exit;
  n:=1;
  WHILE (n<=length(s)) AND (result) DO
    BEGIN
      IF (NOT (s[n] in FieldnameChars)) THEN result:=False;
      INC(n);
    END;
  IF (s[1] in NumChars) THEN result:=False;
end;  //function isFieldname

function TEpiTXTDataset.RemoveSpaces(s:string):string;
var
  n:Integer;
begin
  result:='';
  IF pos(' ',s)=0 THEN result:=s
  ELSE
    BEGIN
      for n:=1 to length(s) DO
        if s[n]<>' ' then result:=result+s[n];
    END;
end;



function TEpiTXTDataset.isDate(s:string; datetype:TkorteFelttyper):boolean;
var
  n,y,m,d,pos1,pos2,antaldateseptegn:Integer;
  s2:string;
begin
  result:=true;
  IF trim(s)='' then exit;
  antaldateseptegn:=0;
  pos1:=0;
  pos2:=0;
  n:=1;
  WHILE (n<=length(s)) DO
    begin
      if (s[n] in DateSepChars) THEN
        begin
          inc(antaldateseptegn);
          if pos1=0 then pos1:=n else pos2:=n;
        end;
      inc(n);
    end;
  IF antaldateseptegn<>2 then begin result:=false;  exit;  end;
  if (pos1+1=pos2) or (pos1=1) or (pos2=1) or (pos1=length(s)) or (pos2=length(s)) then begin result:=false;  exit;  end;
  try
    case datetype of
      fktMDY:  begin
                m:=strtoint(copy(s,1,pos1-1));
                d:=strtoint(copy(s,pos1+1,pos2-pos1-1));
                y:=strtoint(copy(s,pos2+1,length(s)));
              end;
      fktDMY:  begin
                d:=strtoint(copy(s,1,pos1-1));
                m:=strtoint(copy(s,pos1+1,pos2-pos1-1));
                y:=strtoint(copy(s,pos2+1,length(s)));
              end;
      fktYMD:  begin
                y:=strtoint(copy(s,1,pos1-1));
                m:=strtoint(copy(s,pos1+1,pos2-pos1-1));
                d:=strtoint(copy(s,pos2+1,length(s)));
              end;
    end;  //case
    //if (y<1700) or (y>2200) or (d<1) or (d>31) or (m<1) or (m>12) then
    //  begin
    //    result:=false;
    //    exit;
    //  end;
    encodedate(y,m,d);
  except
    result:=false;
    exit;
  end;
end;  //function isDate

procedure TEpiTXTdataset.ReadFromClipboard();
var
  Data: THandle;
  s: string;
begin
  if Clipboard.HasFormat(CF_TEXT) then
  begin
    try
      Data := Clipboard.GetAsHandle(CF_TEXT);
      FFile.SetText(GlobalLock(Data));
    finally
      GlobalUnlock(Data);
    end;
  end
  else if Clipboard.HasFormat(CF_UNICODETEXT) then
  begin
    try
      Data := Clipboard.GetAsHandle(CF_UNICODETEXT);
      s := WideCharToString(PWideChar(GlobalLock(Data)));
      FFile.SetText(PAnsiChar(s));
    finally
      GlobalUnlock(Data);
    end;
  end;
  if FFile.Count = 0 then
  begin
    FErrorText := 'Clipboard empty';
    raise Exception.Create('Clipboard empty');
  end else
    dm.Info('First line (clipboard): %s', [FFile[0]], 44004);
end;


function TEpiTXTDataset.testText(s:string;curfelttype:TkorteFelttyper):TkorteFelttyper;
begin
  result:=curfelttype;
  IF (result=fktInt)       AND (NOT isInt(s))        THEN result:=succ(result);
  IF (result=fktFloat)     AND (NOT isFloat(s))      THEN result:=succ(result);
  IF (result=fktFieldname) AND (NOT isFieldname(s))  THEN result:=succ(result);
  IF (result=fktMDY) then
    BEGIN
      //text could be a date - test according to curfelttype
      case curfelttype of
      fktMDY:
        begin
          IF (result=fktMDY)       AND (NOT isDate(s,fktMDY)) THEN result:=succ(result);
          IF (result=fktDMY)       AND (NOT isDate(s,fktDMY)) THEN result:=succ(result);
          IF (result=fktYMD)       AND (NOT isDate(s,fktYMD)) THEN result:=succ(result);
        end;
      fktDMY:
        begin
          IF                          (NOT isDate(s,fktDMY)) THEN result:=fktMDY;
          IF (result=fktMDY)       AND (NOT isDate(s,fktMDY)) THEN result:=fktYMD;
          IF (result=fktYMD)       AND (NOT isDate(s,fktYMD)) THEN result:=succ(result);
        end;
      fktYMD:
        begin
          IF                          (NOT isDate(s,fktYMD)) THEN result:=fktMDY;
          IF (result=fktMDY)       AND (NOT isDate(s,fktMDY)) THEN result:=fktDMY;
          IF (result=fktDMY)       AND (NOT isDate(s,fktDMY)) THEN result:=succ(fktYMD);
        end;
      else
        begin
          IF (result=fktMDY)       AND (NOT isDate(s,fktMDY)) THEN result:=succ(result);
          IF (result=fktDMY)       AND (NOT isDate(s,fktDMY)) THEN result:=succ(result);
          IF (result=fktYMD)       AND (NOT isDate(s,fktYMD)) THEN result:=succ(result);
        end;

      end;  //case
    END
end;  //function testText

Constructor TEpiTXTDataset.Create(const filename: TEpiFileName;Mode:Epiint;var fCheckProperties: TdfChkProp; const pw:string='');
var
  lin,feltnavne,kunstrings,fieldlabels:tstringlist;
  felttyper,felttyper1:TList;
  n,w,curlin,curfelt,fieldstart,fieldend:integer;
  s,s2,fieldtext,felttext:string;
  tabcount,semicoloncount,commacount,spacecount:integer;
  ntab, nsemicolon, ncomma, nspace : integer;
  ok,istab,issemicolon,iscomma,isspace,AllFirstAreString,AllOtherAreString:boolean;
  isptab, ispsemi, ispcomma, ispspace: boolean;
  antalfelter,nestefeltstart,feltstart:integer;
  fld : TeField;

  procedure AdjustLines();
  var
    i, j: integer;
    s: string;
  begin
    for i:=0 to Lin.Count-1 do
    begin
      s := Lin[i];
      for j := 1 to ((FFieldCount-1) - NumberOfChars(s, FSepChar)) do
        s := s + FSepChar + '.';
      Lin[i] := s;
      FFile[i] := s;
    end;
  end;

BEGIN
  inherited Create(filename, Mode,fCheckProperties,pw);
  FTableName:= filename;
  lin:=nil;
  felttyper:=nil;
  felttyper1:=nil;
  feltnavne:=nil;
  kunstrings:=nil;
  fieldlabels:=nil;
  try
    //epiFldList:= TEpiFieldList.create;
    FEpiFile:=TEpiDataFile.Create;
    FFile:=TStringList.Create;
    if filename = 'from clipboard' then
      ReadFromClipboard
    else
      FFile.LoadFromFile(filename);
    ok:=true;
    n:=0;
    repeat
      IF trim(FFile[n])='' then
        begin
          FFile.Delete(n);
          n:=0;
        end
      ELSE inc(n);
    until (n=FFile.Count);

    lin:=TStringlist.Create;
    felttyper:=TList.Create;
    felttyper1:=TList.Create;
    feltnavne:=TStringlist.Create;
    kunstrings:=TStringlist.Create;
    fieldlabels:=TStringlist.create;
    tabcount:=9999;   semicoloncount:=9999;   commacount:=9999;   spacecount:=9999;
    ntab:=0; nsemicolon:=0;   ncomma:=0;   nspace:=0;
    istab:=true;      issemicolon:=true;   iscomma:=true;     isspace:=true;
    isptab:=false;    ispsemi:=false;      ispcomma:=false;   ispspace:=false;

    for n := 0 to Math.Min(9, FFile.Count-1) do
    begin
      s := TrimRight(FFile[n]);
      tabcount := NumberOfChars(s,#9);
      ntab := ntab + tabcount;
      semicoloncount := NumberOfChars(s,';');
      nsemicolon := nsemicolon + semicoloncount;
      commacount := NumberOfChars(s,',');
      ncomma := ncomma + commacount;
      spacecount := NumberOfChars(s,' ');
      nspace := nspace + spacecount;
    end;
    tabcount := Round(ntab / Math.Min(10, FFile.Count));
    semicoloncount := Round(nsemicolon / Math.Min(10, FFile.Count));
    commacount := Round(ncomma / Math.Min(10, FFile.Count));
    spacecount := Round(nspace / Math.Min(10, FFile.Count));

    ntab:=0; nsemicolon:=0;   ncomma:=0;   nspace:=0;
    for n:=0 to antallinier-1 do
      begin
        if n>FFile.Count-1 then break;
//        s:=FFile[n];
        s := TrimRight(FFile[n]);
        if trim(s)='' then continue;
        lin.Append(s);
        kunstrings.append('Y');

        {  Look for field separator char
           Priority: tab, semicolon, comma, space    }
        ntab := ntab + NumberOfChars(s,#9);
        nsemicolon := nsemicolon + NumberOfChars(s,';');
        ncomma := ncomma + NumberOfChars(s,',');
        nspace := nspace + NumberOfChars(s,' ');

        if (istab) then
          begin
            w:=NumberOfChars(s,#9);
            if w = 0 then istab := false;
            if w > tabcount then istab := false;
            if w < tabcount then isptab := (istab and true);
//            if (tabcount=9999) then tabcount:=w;
//            if (tabcount<>w) or (w=0) then istab:=false;
          end;

        if (issemicolon) then
          begin
            w:=NumberOfChars(s,';');
//            if (semicoloncount=9999) then semicoloncount:=w;
//            if (semicoloncount<>w) or (w=0) then issemicolon:=false;
            if w = 0 then issemicolon := false;
            if w > semicoloncount then issemicolon := false;
            if w < semicoloncount then ispsemi := (issemicolon and true);
          end;

        if (iscomma) then
          begin
            w:=NumberOfChars(s,',');
//            if (commacount=9999) then commacount:=w;
//            if (commacount<>w) or (w=0) then iscomma:=false;
            if w = 0 then iscomma := false;
            if w > commacount then iscomma := false;
            if w < commacount then ispcomma := (iscomma and true);
          end;

        if (isspace) then
          begin
            w:=NumberOfChars(s,' ');
//            if (spacecount=9999) then spacecount:=w;
//            if (spacecount<>w) or (w=0) then isspace:=false;
            if w = 0 then isspace := false;
            if w > spacecount then isspace := false;
            if w < spacecount then ispspace := (isspace and true);
          end;

      end; //for
    IF lin.Count=0 then DatabaseError('Datafile empty.',nil);
    inc(tabcount);
    inc(semicoloncount);
    inc(commacount);
    inc(spacecount);
    s2:= format('(tab: %d)(semicolon: %d)(comma: %d)(space: %d) Lines: %d',[ntab,nsemicolon,ncomma,nspace,lin.Count]);
    if istab            then begin FSepChar:=#9;   FFieldCount:=tabcount;  s2:= ' Tab ' + s2;     end
    else if issemicolon then begin FSepChar:=';';  FFieldCount:=semicoloncount;  s2:= ' ; ' + s2; end
    else if iscomma     then begin FSepChar:=',';  FFieldCount:=commacount;  s2:= ' comma ' + s2;    end
    else if isspace     then begin FSepChar:=' ';  FFieldCount:=spacecount;  s2:= ' space ' + s2;    end
    //else databaserror('Illegal formal of textfile. Fieldseparator not found.',nil);
    else begin
          s2:= 'Separators: ' + s2+ '<br>Cannot define number of variables';
          dm.error(s2,[],0);
          exit;
          end;
    AdjustLines();
    for n:=0 to FFieldCount-1 DO
      begin
        felttyper.Add(new(PFelttype));
        PFelttype(felttyper.Items[felttyper.Count-1])^:=fktInt;
        felttyper1.Add(new(PFelttype));
        PFelttype(felttyper1.Items[felttyper1.Count-1])^:=fktInt;
        feltnavne.append('N'+inttostr(n+1));
        fieldlabels.append('');
      end;

    {Test for field types and test if fieldnames in first row:
                 integer [0..9,-,+], if present, then minus/plus must be first char
                 float [0..9,-,+,.,','], Only one comma/dot present. If present, then minus/plus must be first char
                 fieldnametype [A..Z,a..z,0..9], first char not numeric
                 dates [0..9,/,-], Year must be 4-digit, recognizes MDY, DMY, YMD
                 string [all chars]
    }

    FFieldnamesInRow1:=true;
    for curlin:=0 to lin.count-1 DO
      BEGIN
        s:=lin[curlin];
        if trim(s)<>'' THEN
          begin
            feltstart:=1;
            for curfelt:=1 to FFieldCount do
              begin
                n:=FieldEndPos(s,feltstart);
                if n>0 then s[n]:=#250;
                nestefeltstart:=n+1;
                felttext:=copy(s,feltstart,nestefeltstart-feltstart-1);
                feltstart:=nestefeltstart;
                if length(felttext)>0 then
                  if (felttext[1] in QuoteChars) and (felttext[length(felttext)] in Quotechars) and (felttext[1]=felttext[length(felttext)])
                  then felttext:=copy(felttext,2,length(felttext)-2);   //remove quotes
                IF curlin=0 THEN
                  BEGIN
                    PFelttype(Felttyper1.items[curfelt-1])^:=TestText(felttext,PFelttype(Felttyper1.items[curfelt-1])^);
                    IF PFelttype(Felttyper1.items[curfelt-1])^<>fktFieldname THEN FFieldnamesInRow1:=false;
                    Feltnavne[curfelt-1]:=RemoveSpaces(felttext);
                    IF Length(Feltnavne[curfelt-1])>10 THEN
                      begin
                        FFieldnamesInRow1:=false;
                        IF PFelttype(Felttyper1.items[curfelt-1])^=fktFieldname then PFelttype(Felttyper1.items[curfelt-1])^:=fktString;
                      end;
                  END
                ELSE
                  BEGIN
                    PFelttype(Felttyper.items[curfelt-1])^:=TestText(felttext,PFelttype(Felttyper.items[curfelt-1])^);
                    IF PFelttype(Felttyper.items[curfelt-1])^=fktFieldname THEN PFelttype(Felttyper.items[curfelt-1])^:=fktString;
                  END;
              end;  //for curfelt
          end;  //if
      END;  //for curlin

    IF (NOT FFieldnamesInRow1) THEN
      BEGIN
        AllFirstAreString:=true;
        AllOtherAreString:=true;
        for n:=0 TO FFieldCount-1 DO
          BEGIN
            IF (NOT (PFelttype(Felttyper1.Items[n])^ in [fktFieldname,fktString])) THEN AllFirstAreString:=false;
            IF (NOT (PFelttype(Felttyper.Items[n])^=fktString)) THEN AllOtherAreString:=false;
          END;
        IF (AllFirstAreString) AND (NOT AllOtherAreString) THEN
          BEGIN
            //First row are all ftFieldname or ftString AND other rows contains ftInt or ftFloat: make legal fieldnames
            FFieldnamesInRow1:=true;
            w:=0;
            FOR n:=0 TO FFieldCount-1 DO
              BEGIN
                IF PFelttype(Felttyper1.Items[n])^<>fktFieldname THEN
                  BEGIN
                    repeat
                      INC(w);
                      s:='v'+inttostr(w);
                    until feltnavne.IndexOf(s)=-1;
                    fieldlabels[n]:=feltnavne[n];
                    feltnavne[n]:=s;
                  END;  //if
              END;  //for
          END
        ELSE
          for n:=0 to FFieldCount-1 DO
            feltnavne[n]:=('v'+inttostr(n+1));
      END;  //if

    FRecordSize:=0;
    FOR n:=0 TO FFieldCount-1 DO
      BEGIN
        fld:=TeField.Create;
        IF (NOT FFieldnamesInRow1) THEN s:='v'+IntToStr(n+1) ELSE s:=feltnavne[n];
        fld.Fieldname:=s;
        fld.FVariableLabel:=fieldlabels[n];
        CASE PFelttype(Felttyper.Items[n])^ OF
          fktInt:
            begin
              fld.Felttype:= ftInteger;
              fld.FLength:=1;
              INC(FRecordSize,14);
            end;
          fktFloat:
            begin
              fld.Felttype:= ftFloat;
              fld.FNumDecimals:=1;
              fld.FLength:=3;
              INC(FRecordSize,14)
            end;
          fktMDY:
            begin
              fld.Felttype := ftDate;
              fld.FFieldFormat:='%MDY';
              fld.FLength:=10;
              INC(FRecordSize,10);
            end;
          fktDMY:
            begin
              fld.Felttype := ftEuroDate;
              fld.FFieldformat:='%DMY';
              fld.FLength:=10;
              INC(FRecordSize,10);
            end;
          fktYMD:
            begin
              fld.Felttype := ftYMDDate;
              fld.FFieldformat:='%YMD';
              fld.FLength:=10;
              INC(FRecordSize,10);
            end;
          fktString,fktFieldname:
            begin
              fld.Felttype:=ftAlfa;
              fld.FLength:=2;
              INC(FRecordSize,80);
            end;
        ELSE
          fld.Felttype := ftRes4;
        END;  //case
        fld.FFieldNo:=n;
        IF fld.Felttype <> ftRes4 then
          begin
            FEpiFile.AddField(fld);
          end
        else
          begin
            fld.Free;
            DatabaseErrorFmt(SFieldTypeNotSupported, [s]);
          end;

      END;  //for

    FCurrentRec:=0;
    IF FFieldnamesInRow1 THEN
      begin
        FFile.Delete(0);
        s2 := s2 + ' (incl. field names) ';
      end;

    dm.info('Separator: ' + s2,[],0);

    FRecordCount:=FFile.Count;


    //check all data for fieldlengths and -formats
    FOR curlin:=0 to FFile.Count-1 DO
      BEGIN
        FRecordBuf:=FFile[curlin];
        IF trim(FRecordBuf)<>'' THEN
          BEGIN
            FOR curfelt:=0 to FFieldCount-1 DO
              BEGIN
                fld:=FEpiFile[curfelt];
                FieldStart:=FieldStartPos(FRecordBuf,fld.FFieldNo);
                fieldEnd:=FieldEndPos(FRecordBuf,FieldStart);
                fieldtext:=copy(FRecordBuf,FieldStart,FieldEnd-FieldStart);
                if trim(fieldtext)<>'' then
                  IF (fieldtext[1] in QuoteChars) AND (fieldtext[length(fieldtext)] in QuoteChars) AND (fieldtext[1]=fieldtext[length(fieldtext)]) THEN fieldtext:=copy(fieldtext,2,length(fieldtext)-2);
                IF trim(fieldtext)<>'' THEN
                CASE fld.Felttype of
                  ftAlfa,ftInteger,ftUpperAlfa: IF length(fieldtext)>fld.FLength THEN fld.FLength:=length(fieldtext);
                  ftFloat:
                    begin
                      IF length(fieldtext)>fld.FLength THEN fld.FLength:=length(fieldtext);
                      n:=pos('.',fieldtext);
                      IF n=0 then n:=pos(',',fieldtext);
                      IF n>0 THEN
                        begin
                          n:=length(fieldtext)-n;  //n equals number of decimals
                          IF n>fld.FNumDecimals THEN fld.FNumDecimals:=n;
                        end
                      else
                        begin
                          if length(fieldtext)+1+fld.FNumDecimals>fld.FLength THEN fld.FLength:=Length(fieldtext)+1+fld.FNumDecimals;
                        end;
                    end;
                END;  //case
                ok:=true;
                CASE fld.Felttype of
                  ftAlfa,ftUpperAlfa:          ok:=(fld.FLength<=EpiMaxStringLength);
                  ftInteger:                   ok:=(fld.FLength<=EpiMaxIntegerLength);
                  ftFloat:                     ok:=(fld.FLength<=EpiMaxFloatLength);
                  ftDate,ftEuroDate,ftYMDDate: ok:=(fld.FLength<=10);
                end;  //case
                if (not ok) then
                  begin
                    dm.Error('Data in field %s, record %d exceed maximum length',[fld.Fieldname,Curlin+1], 44001);
                    exit;
                  end;
              END; //for curfelt
          END;  //if
      END;  //for curlin

    FRecordSize:=0;
    FOR curfelt:=0 TO FFieldCount-1 DO
      BEGIN
        fld:=FEpiFile[curfelt];
        INC(FRecordSize,fld.FLength);
        CASE fld.Felttype of
          ftAlfa,ftUpperAlfa:          fld.FFieldFormat:='%'+IntToStr(fld.FLength)+'s';
          ftInteger:                   fld.FFieldFormat:='%'+IntToStr(fld.FLength)+'d';
          ftFloat:                     fld.FFieldFormat:='%'+IntToStr(fld.FLength)+'.'+IntToStr(fld.FNumDecimals)+'f';
          ftDate:                      fld.FFieldFormat:='%MDY';
          ftEuroDate:                  fld.FFieldFormat:='%DMY';
          ftYMDDate:                   fld.FFieldFormat:='%YMD';
        END;  //case
        fld.FVariableLabel := fld.Fieldname;
      END;  //for

  finally
    if Assigned(lin) then FreeAndNil(lin);
    if Assigned(feltnavne) then FreeAndNil(feltnavne);
    if Assigned(kunstrings) then FreeAndNil(kunstrings);
    if Assigned(fieldlabels) then FreeAndNil(fieldlabels);
    if assigned(felttyper) then
    begin
      for n:=0 to felttyper.Count-1 DO
        BEGIN
          dispose(felttyper.Items[n]);
          dispose(felttyper1.Items[n]);
        END;
      FreeAndNil(felttyper);
    end;
    if Assigned(felttyper1) then FreeAndNil(felttyper1);
  end;
END;  //create

constructor TEpiTXTdataset.CreateNew(const filename: TEpiFileName; EpiFile: TEpiDataFile; var fCheckProperties: TdfChkProp; const pw:string='');
begin
  //not implemented - can't yet save as txt-file
end;


function TEpiTXTdataset.GetBOF: boolean;
begin
  result:=FCurrentRec<1;
end;


function TEpiTXTdataset.GetEOF: boolean;
begin
    result:=FCurrentRec>FRecordCount;
end;

function TEpiTXTdataset.GetField(Index: integer): TeField;
begin
  result:=FEpiFile[index];
end;

function TEpiTXTdataset.GetFieldCount: integer;
begin
   result:=FEpiFile.NumFields;
end;

function TEpiTXTdataset.GetRecordCount: integer;
begin
  result:=FRecordCount;
end;

function TEpiTXTdataset.GetRecordSize: integer;
begin
  result:= FRecordSize;
end;

destructor TEpiTXTdataset.Destroy;
VAR
  n:Integer;
begin
  FFile.Free;
  FEpiFile.Free;
  inherited;
end;

procedure TEpiTXTdataset.SetFieldData(Fld: TeField; Src: Pointer);
begin
  raise exception.create('Capability not supported');
end;

class function TEpiTXTdataset.GetNewDataSet(const filename: TEpiFileName; EpiFile: TEpiDataFile; var fCheckProperties: TdfChkProp; const pw:string=''): TEpiDataSet;
begin
  //not implemented - can't create new datasets as txt
end;

function TEpiTXTdataset.First: integer;
begin
  FCurrentRec:=-1;
  FRecordBuf:='';
end;

function TEpiTXTdataset.Next: integer;
begin
  INC(FCurrentRec);
  result:=FCurrentRec;
  IF (FCurrentRec<FFile.Count) AND (FCurrentRec>=0) THEN FRecordBuf:=FFile[FCurrentRec] ELSE FRecordBuf:='';
end;

function TEpiTXTdataset.Last: integer;
begin
  FCurrentRec:=FRecordCount-1;
  Result:=FCurrentRec;
  IF (FCurrentRec<FFile.Count) AND (FCurrentRec>=0) THEN FRecordBuf:=FFile[FCurrentRec] ELSE FRecordBuf:='';
end;

function TEpiTXTdataset.Post: integer;
begin

end;

function TEpiTXTdataset.Prev: integer;
begin
  IF FCurrentRec=0 THEN Last
  ELSE DEC(FCurrentRec);
  Result:=FCurrentRec;
  IF (FCurrentRec<FFile.Count) AND (FCurrentRec>=0) THEN FRecordBuf:=FFile[FCurrentRec] ELSE FRecordBuf:='';
end;

function TEpiTXTdataset.Append: integer;
begin

end;


function TEpiTXTdataset.GetFieldData(obs:integer; Fld: TeField; Dst: Pointer; var Blank: boolean): Boolean;
var
 b :   boolean;
 int : integer;
 f   : EpiFloatStorage;
 d   : extended;
 code: Integer;
 day,year,month:word;
 {s,} fieldtext:String;
 tmpdecimalseparator:char;
 n:Integer;
 fieldstart,fieldend:Integer;
BEGIN
try
  if trim(FRecordBuf)='' then
    begin
      blank:=true;
      result:=false;
    end;
  Result:=true;
  Blank:=False;
  FieldStart:=FieldStartPos(FRecordBuf,fld.FFieldNo);
  fieldEnd:=FieldEndPos(FRecordBuf,FieldStart);
  fieldtext:=copy(FRecordBuf,FieldStart,FieldEnd-FieldStart);
  if trim(fieldtext)<>'' then
    IF (fieldtext[1] in QuoteChars) AND (fieldtext[length(fieldtext)] in QuoteChars) AND (fieldtext[1]=fieldtext[length(fieldtext)]) THEN fieldtext:=copy(fieldtext,2,length(fieldtext)-2);
  IF ((trim(fieldtext)='')  or (trim(fieldtext)='.'))  THEN
    begin
      Blank:=True;
      exit;
    end;
  CASE fld.Felttype of
    ftAlfa,ftCrypt,ftSoundex: StrPCopy(dst,fieldtext);
    ftUpperAlfa:              StrPCopy(dst,Sysutils.AnsiUpperCase(fieldtext));
    ftBoolean:
      begin
        if (Fieldtext='T') or (Fieldtext='Y') then Word(Dst^) := 1
        else Word(Dst^) := 0;
      end;
    ftInteger:
      begin
        Val(trim(fieldtext),int,Code);
        int:=trunc(int);
        integer(dst^):=int;
      end;
    ftFloat:
      begin
        Val(trim(fieldtext),d,Code);
        if Code<>0 THEN
          begin
            tmpDecimalseparator:=DecimalSeparator;
            IF tmpDecimalseparator='.' THEN DecimalSeparator:=',' ELSE DecimalSeparator:='.';
            Val(trim(fieldtext),d,Code);
            Decimalseparator:=tmpDecimalseparator;
          end;
        EpiFloatStorage(dst^):=d;
      end;
    ftDate,ftToday,ftEuroDate,ftEuroToday,ftYMDDate,ftYMDToday:
      begin
        if not getDate(obs,fieldtext,fld.FFieldFormat,year,month,day) then blank:=true;
        if blank then dm.info('# %d date error: %s',[obs,fieldtext], 44002);
        EpiDate(dst^):=EpiYMDToDate(FCurrentRec,year,month,day);
      end;
  END;  //case
except
  Result:=false;
end;  //try..except
end;  //function getFieldData


{ TEpiEPXDataset }

constructor TEpiEPXDataset.Create(const filename: TEpiFileName;
  Mode: Epiint; var fCheckProperties: TdfChkProp; const pw: string);
var
  Doc,
  Node, FNode, ANode,
  VLNode: {$IFDEF NativeXML}TXmlNode{$ELSE}IXMLNode{$ENDIF};
  AField: TeField;
  FVLSet: TLabelValueList;
  Idx, I: Integer;
  S: String;
  MS: TMemoryStream;
  Unzip: TVCLUnZip;
  EVal: Extended;

const
  XMLFieldTypesToEpiFieldTypes: array[0..13] of TFelttyper = (
    ftBoolean,                            // Boolean                       (0)
    ftInteger, ftIDNUM, ftFloat,          // Integer, AutoInc, Float       (3)
    ftEuroDate, ftDate, ftYMDDate,        // DMYDate, MDYDate, YMDDate     (6)
    ftEuroToday, ftToday, ftYMDToday,     // DMYToday, MDYToday, YMDToday  (9)
    ftFloat, ftFloat,                     // Time, TimeNow                (11)
    ftAlfa, ftUpperAlfa                   // String, UpperString          (13)
  );
begin
  inherited Create(filename, Mode, fCheckProperties);
  FRecCount := -1;

  try
    MS := TMemoryStream.Create;
    if ExtractFileExt(fFileName) = '.epz' then
    begin
      Unzip := TVCLUnZip.Create(nil);
      Unzip.ZipName := fFileName;
      Unzip.UnZipToStreamByIndex(MS, 0);
      UnZip.Free;
    end else
      MS.LoadFromFile(fFileName);

    {$IFDEF NativeXML}
    FEpiDocument := TNativeXml.Create(nil);
    FEpiDocument.LoadFromStream(MS);
    Doc := FEpiDocument.Root;
    {$ELSE}
    FEpiDocument := TXMLDocument.Create(DM);
    FEpiDocument.LoadFromStream(MS, xetUTF_8);
    Doc := FEpiDocument.DocumentElement;
    {$ENDIF}
    MS.Free;

    FVersion := StrToInt(GetAttribute('version', Doc));

    if (FVersion >= 2) and
       (
        (Doc.HasAttribute('password')) or
        (Assigned(GetNode('Crypto', Doc)))
       )
    then
    begin
      dm.Error('Error: Reading encrypted EPX/EPZ files are not supported!', [], 0);
      checkfileerror(-2);
    end;


    Node := GetNode('Settings', Doc);// {$IFDEF NativeXML}Doc.NodeByName('Settings'){$ELSE}Doc.ChildNodes['Settings']{$ENDIF};
    
    FEpiDocFormatSettings.DateSeparator := GetAttribute('dateSeparator', node)[1];
    FEpiDocFormatSettings.TimeSeparator := GetAttribute('timeSeparator', node)[1];
    FEpiDocFormatSettings.DecimalSeparator := GetAttribute('decimalSeparator', node)[1];
    FEpiDocFormatSettings.ShortTimeFormat  := 'HH:NN:SS';

    // Read valuelabels.
    if Not Assigned(fCheckProperties) then
      fCheckProperties := TdfChkProp.Create;
    FLocalLabelBlockList := fCheckProperties.ValueLabels;


    Node := GetNode('ValueLabelSets', Doc);
    if Assigned(Node) { and Node.HasChildNodes} then
    begin
      case FVersion of
        2: LoadValueLabelSetsV2(Node);
      else
      // version 3 + 4 (so far)
        LoadValueLabelSetsV3(Node);
      end;
    end;

    // Only support for reading the first datafile.
    FEpiDataFile := GetFirstChild(GetNode('DataFiles', Doc)); // Doc.ChildNodes['DataFiles'].ChildNodes[0];
    FFieldList := TStringList.Create;

    Node := GetFirstChild(GetNode('Sections', FEpiDataFile));

    // Create the TeFields - store in list for later retrieval.
    while Assigned(Node) do
    begin
      FNode := GetFirstChild(GetNode('Fields', Node));

      while Assigned(FNode) do
      begin
        AField                := TeField.Create;

        AField.FFieldColor    := FtNumberFromFtText(GetAttribute('type', FNode));
        AField.Felttype       := XMLFieldTypesToEpiFieldTypes[AField.FFieldColor];
        AField.FieldName      := GetAttribute('id', FNode);
        AField.FLength        := StrToInt(GetAttribute('length', FNode));
        AField.FNumDecimals   := StrToInt(GetAttribute('decimals', FNode));

        AField.FValueLabel    := GetAttribute('valueLabelRef', FNode);
        AField.FVariableLabel := GetNodeText(GetFirstChild(GetNode('Question', FNode))); // <Question><Text xml:lang="..."> TEXT </Text></Question>

        with AField do
        case FFieldColor of   // The XML Field type.
          1,2:    FFieldFormat := '%d';
          3:      FFieldFormat := '%' + Format('%d.%d', [FLength, FNumDecimals]) + 'f';
          4,7:    FFieldFormat := '%DMY';
          5,8:    FFieldFormat := '%MDY';
          6,9:    FFieldFormat := '%YMD';
          10,11:  begin
                    FFieldFormat := '%1.6f';
                    FNumDecimals := 6;
                  end;  
        end;

        if (AField.FValueLabel <> '') and
           (FMissingValuesList.IndexOf(AField.FValueLabel) <> -1) then
        begin
          Idx := FMissingValuesList.IndexOf(AField.FValueLabel);
          ANode := {$IFDEF NativeXML}{$ELSE}IXMLNode{$ENDIf}(Pointer(FMissingValuesList.Objects[Idx]));
          VLNode := GetFirstChild(ANode);

          I := 0;
          while Assigned(VLNode) do
          begin
            if (GetAttribute('missing', VLNode) = 'true') then
            begin
              S := GetAttribute('value', VLNode);

              if (AField.Felttype = ftFloat) then
                begin
                  if (not TryStrToFloat(S, EVal)) then
                  begin
                    if Pos(',', S) > 0 then
                      S := StringReplace(S, ',', '.', [])
                    else
                      S := StringReplace(S, '.', ',', []);
                  end;
                end;
              AField.MissingValues[I] := S;

              Inc(I);
              if I = MAXDEFINEDMISSINGVALUES then break;
            end;
            VLNode := GetNextSibling(VLNode);
          end;
        end;

        FFieldList.AddObject(AField.FieldName, AField);
        FNode := GetNextSibling(FNode);
      end;
      Node := GetNextSibling(Node);
    end;
  except
    FreeAndNil(FEpiDocument);
    FreeAndNil(FMissingValuesList);
    raise;
  end;
end;

destructor TEpiEPXDataset.Destroy;
begin
  FreeAndNil(FEpiDocument);
  FreeAndNil(FMissingValuesList);
  inherited;
end;

function TEpiEPXDataset.First: integer;
var
  i: integer;
begin
  FFirstRec := true;
  if GetRecordCount = 0 then exit;

  // Hack: Update valuelabels here...
  FLocalLabelBlockList.Sorted := false;
  for i := 0 to FLocalLabelBlockList.Count - 1 do
    FLocalLabelBlockList[i] := TLabelValueList(FLocalLabelBlockList.Objects[i]).LabelName;
  FLocalLabelBlockList.Sorted := true;

  FRecNode := GetFirstChild(GetNode('Records', FEpiDataFile)); // FEpiDataFile.ChildNodes['Records'].ChildNodes[0];
  FDataList := TStringList.Create;

  case FVersion of
    2: FDataList.Delimiter := ',';
//    3: FDataList.Delimiter := ';';
  end;

  FDataList.QuoteChar := '"';
end;

function TEpiEPXDataset.GetDeleted: boolean;
begin
  result := GetAttribute('status', FRecNode) = 'rsDeleted';
end;

function TEpiEPXDataset.GetField(Index: integer): TeField;
begin
  result := TeField(FFieldList.Objects[Index]);
end;

function TEpiEPXDataset.GetFieldCount: integer;
begin
  result := FFieldList.Count;
end;

function TEpiEPXDataset.GetFieldData(obs: integer; Fld: TeField;
  Dst: Pointer; var Blank: boolean): Boolean;
var
  S, Id: String;
  I: Integer;
  F: EpiFloat;
  D: TDateTime;
  Code: integer;

  function UnDummyfy(Const T: string): string;
  begin
    if (FVersion = 2) then
    begin
      result := StringReplace(T, DummyStringSpace, ' ', [rfReplaceAll]);
      result := StringReplace(result, DummyStringComma, ',', [rfReplaceAll])
    end else
      Result := T;
  end;

begin
  result := true;
  With FFieldList do
    Id := Strings[IndexOfObject(Fld)];

  Blank := FDataList.IndexOfName(Id) = -1;
  if Blank then exit;

  S := UnDummyfy(FDataList.Values[Id]);
  case Fld.Felttype of
    ftAlfa,ftUpperAlfa:
      begin
        if (fld.Felttype = ftUpperAlfa) then
          S := Sysutils.AnsiUpperCase(S);
        StrPLCopy(dst, S, 1024);
      end;
    ftInteger, ftIDNUM:
      begin
        Result := TryStrToInt(S, I);
        integer(dst^) := I;
      end;
    ftFloat:
      begin
        if Fld.FFieldColor in [10, 11] then
        begin
          // This was a time field.
          result := TryStrToTime(S, D, FEpiDocFormatSettings);
          EpiFloat(dst^) := D;
        end else begin
          // This was a floating field.
          result := TryStrToFloat(S, F, FEpiDocFormatSettings);
          EpiFloat(dst^) := F;
        end;
      end;
    ftDate,ftToday,ftEuroDate,ftEuroToday,ftYMDDate,ftYMDToday:
      begin
        if Fld.Felttype in [ftEuroDate,ftEuroToday] then FEpiDocFormatSettings.ShortDateFormat := 'dd/mm/yyyy';
        if Fld.Felttype in [ftDate,ftToday]         then FEpiDocFormatSettings.ShortDateFormat := 'mm/dd/yyyy';
        if Fld.Felttype in [ftYMDDate,ftYMDToday]   then FEpiDocFormatSettings.ShortDateFormat := 'yyyy/mm/dd';
        Result := TryStrToDate(S, D, FEpiDocFormatSettings);
        EpiDate(dst^) := Trunc(D) + 36522; // Magic analysis constant!?!?!?
      end;
    ftBoolean:
      begin
        if (S = 'Y') then
          Word(Dst^) := 1
        else
          Word(Dst^) := 0;
      end;
  end;
end;

function TEpiEPXDataset.GetFileLabel: string;
begin
  result := GetNodeText(GetFirstChild(GetNode('Title', GetNode('StudyInfo', {$IFDEF NativeXml}FEpiDocument.Root{$ELSE}FEpiDocument.DocumentElement{$ENDIF}))));
// result := FEpiDocument.DocumentElement.ChildNodes['StudyInfo'].ChildNodes['Title'].ChildNodes[0].Text;
end;

function TEpiEPXDataset.GetRecordCount: integer;
begin
  if FRecCount < 0 then
  {$IFDEF NativeXml}
    FRecCount := GetNode('Records', FEpiDataFile).ContainerCount;
  {$ELSE}
    FrecCount := FEpiDataFile.ChildNodes['Records'].ChildNodes.Count;
  {$ENDIF}
  Result := FRecCount;
end;

function TEpiEPXDataset.GetVerified: Boolean;
begin
  result := GetAttribute('status', FRecNode) = 'rsVerified';
end;

function TEpiEPXDataset.FtNumberFromFtText(FTText: string): integer;
begin
  if FTText = 'ftBoolean'         then Result := 0
  else if FTText = 'ftInteger'    then Result := 1
  else if FTText = 'ftAutoInc'    then Result := 2
  else if FTText = 'ftFloat'      then Result := 3
  else if FTText = 'ftDMYDate'    then Result := 4
  else if FTText = 'ftMDYDate'    then Result := 5
  else if FTText = 'ftYMDDate'    then Result := 6
  else if FTText = 'ftDMYAuto'    then Result := 7
  else if FTText = 'ftMDYAuto'    then Result := 8
  else if FTText = 'ftYMDAuto'    then Result := 9
  else if FTText = 'ftTime'       then Result := 10
  else if FTText = 'ftTimeAuto'   then Result := 11
  else if FTText = 'ftString'     then Result := 12
  else if FTText = 'ftUpperString' then Result := 13;
end;

procedure TEpiEPXDataset.LoadValueLabelSetsV2(VLSetsNode: {$IFDEF NativeXML}TXmlNode{$ELSE}IXMLNode{$ENDIF});
var
  Node, FNode, VLNode: {$IFDEF NativeXml}TXmlNode{$ELSE}IXMLNode{$ENDIF};
  FVLSet: TLabelValueList;
  S: string;
begin
  FMissingValuesList := TStringList.Create;

  Node := GetFirstChild(VLSetsNode);
  while Assigned(Node) do
  begin
    // Compatability testing.
    FNode := GetNode('External', Node);
    if Assigned(FNode) then
    begin
      // No support for external value labels import within an EPX file.
      Node := GetNextSibling(Node);
      Continue;
    end;

    if not (FtNumberFromFtText(GetAttribute('type', Node)) in [1, 12]) then
    begin
      // Only Interger and String valuelabels supported.
      Node := GetNextSibling(Node);
      Continue;
    end;

    // Create valuelabel set
    FVLSet := TLabelValueList.Create;
    FNode := GetNode('Internal', Node);
    FVLSet.LabelName := GetAttribute('id', Node);

    // Read the values and labels.
    VLNode := GetFirstChild(FNode);
    while Assigned(VLNode) do
    begin
      S := GetNodeText(GetNode('Label', VLNode));

      FVLSet.AddPair(GetAttribute('value', VLNode), S);

      S := GetAttribute('missing', VLNode);

      if (S = 'true') and
         (FMissingValuesList.IndexOf(FVLSet.LabelName) = -1) then
      begin
        FMissingValuesList.AddObject(FVLSet.LabelName, TObject(Pointer(FNode)));
      end;
      VLNode := GetNextSibling(VLNode); 
    end;

    // Add the set.
    // - using a little dirty trick here: ID is stored in list of valuelabel sets,
    //   but the actual name of the valuelabelset is store with the ValueLabel set itself.
    //   This way the field lookup in TEpiDataFrame.LoadFromDataSet find the right ValueLabelSet,
    //   as field in .EPX files use ID references.
    // - To restore correctness this is fixed later in TEpiDataSet.First.
    FLocalLabelBlockList.AddObject(FVLSet.LabelName, FVLSet);
    Node := GetNextSibling(Node);
  end;
end;

procedure TEpiEPXDataset.LoadValueLabelSetsV3(VLSetsNode: {$IFDEF NativeXML}TXmlNode{$ELSE}IXMLNode{$ENDIF});
var
  VLSetNode, VLNode: {$IFDEF NativeXml}TXmlNode{$ELSE}IXMLNode{$ENDIF};
  FVLSet: TLabelValueList;
  S, T: string;
  TypeNo: Integer;
  EVal: Extended;
begin
  FMissingValuesList := TStringList.Create;
  
  VLSetNode := GetFirstChild(VLSetsNode);
  while Assigned(VLSetNode) do
  begin
    // Compatability testing.
    if GetAttribute('scope', VLSetNode) = 'vlsExternal' then
    begin
      // No support for external value labels import within an EPX file for Analysis.
      VLSetNode := GetNextSibling(VLSetNode);
      Continue;
    end;

    TypeNo := FtNumberFromFtText(GetAttribute('type', VLSetNode));
    if not (TypeNo in [1, 3, 12]) then
    begin
      // Only Interger and String valuelabels supported.
      VLSetNode := GetNextSibling(VLSetNode);
      Continue;
    end;

    // Create valuelabel set
    FVLSet := TLabelValueList.Create;
    FVLSet.LabelName := GetAttribute('id', VLSetNode);

    // Read the values and labels.
    VLNode := GetFirstChild(VLSetNode);
    while Assigned(VLNode) do
    begin
      S := GetNodeText(GetNode('Label', VLNode));
      T := GetAttribute('value', VLNode);
      if (TypeNo = 3) then
        begin
          // EPX Float valuelabel
          if (not TryStrToFloat(T, EVal)) then
          begin
            if Pos(',', T) > 0 then
              T := StringReplace(T, ',', '.', [])
            else
              T := StringReplace(T, '.', ',', []);
          end;
        end;

      FVLSet.AddPair(T, S);

      if (GetAttribute('missing', VLNode) = 'true') and
         (FMissingValuesList.IndexOf(FVLSet.LabelName) = -1) then
      begin
        FMissingValuesList.AddObject(FVLSet.LabelName, TObject(Pointer(VLSetNode)));
      end;
      VLNode := GetNextSibling(VLNode);
    end;

    // Add the set.
    // - using a little dirty trick here: ID is stored in list of valuelabel sets,
    //   but the actual name of the valuelabelset is store with the ValueLabel set itself.
    //   This way the field lookup in TEpiDataFrame.LoadFromDataSet find the right ValueLabelSet,
    //   as field in .EPX files use ID references.
    // - To restore correctness this is fixed later in TEpiDataSet.First.
    FLocalLabelBlockList.AddObject(FVLSet.LabelName, FVLSet);
    VLSetNode := GetNextSibling(VLSetNode); 
  end;
end;

function TEpiEPXDataset.Next: integer;
var
  S: string;

  function DummyFy(Const T: string): string;
  begin
    if (FVersion = 2) then
    begin
      result := StringReplace(T,      '\,', DummyStringComma, [rfReplaceAll]);
      result := StringReplace(Result, ' ',  DummyStringSpace, [rfReplaceAll]);
    end;

    if (FVersion = 3) then
    begin
      Result := StringReplace(T,      '""', DummyStringDQuote, [rfReplaceAll]);
      Result := StringReplace(Result, '"', DummyStringQuote, [rfReplaceAll]);
    end;
  end;

  procedure ReadV3Record(Const T: string);
  var
    L: Integer;
    PEnd: PChar;
    PCur: PChar;
    PStart: PChar;
    FieldName, Value: String;
    EscapedQuotes: Boolean;
  begin
    FDataList.Clear;
    
    L := Length(T);

    PStart := @T[1];
    PCur := PStart;

    while (PCur - PStart) < L do
    begin
      // Find field name (FN)
      PEnd := PCur;
      while PEnd^ <> '=' do Inc(PEnd);
      FieldName := Copy(T, (PCur - PStart) + 1, (PEnd - PCur));

      PCur := PEnd;
      Inc(PCur);

      if PCur^ = '"' then
        begin
          Inc(PCur);
          PEnd := PCur;

          // Detect 2xdouble quotes, which translates to a single double qoute: "" -> "
          EscapedQuotes := false;
          while true
          do
            begin
              if (PEnd^ = '"') then
                if ((Pend + 1)^ <> '"')
                 then
                   // A regular "Stop"
                   Break
                 else
                   begin
                     // 2x double quotes found! Step over next "
                     Inc(PEnd);
                     EscapedQuotes := true;
                   end;
              Inc(PEnd);
            end;

          Value := Copy(T, (PCur - PStart) + 1, (PEnd - PCur));

          if EscapedQuotes then
            Value := StringReplace(Value, '""', '"', [rfReplaceAll]);

          Inc(PEnd);
        end
      else
        begin
          PEnd := PCur;
          while (PEnd^ <> ';') and
                (PEnd^ <> #0)
          do
            Inc(PEnd);

          Value := Copy(T, (PCur - PStart) + 1, (PEnd - PCur));
        end;

      Inc(PEnd);

      FDataList.Values[FieldName] := Value;

      PCur := PEnd;
    end;
  end;

begin
  if FFirstRec then
    FFirstRec := false
  else
    FRecNode := GetNextSibling(FRecNode); 

  if Assigned(FRecNode) then
  begin
    S := GetNodeText(FRecNode);

    if (FVersion = 2) then
      FDataList.DelimitedText := DummyFy(S);

    if (FVersion >= 3) then
      ReadV3Record(S);
  end;
end;

{$IFDEF NativeXML}
function TEpiEPXDataset.GetNode(Const Name: string; Parent: TXmlNode): TXmlNode;
var
s: string;
begin
  result := nil;
  if Assigned(Parent) then
  begin
    S := Parent.ClassName;
    result := Parent.NodeByName(Name);
  end;
end;

function TEpiEPXDataset.GetNodeText(Node: TXmlNode): string;
begin
  result := '';
  if Assigned(Node) then
    result := Utf8ToAnsi(Node.Value);
end;

function TEpiEPXDataset.GetAttribute(Const Name: string; Parent: TXmlNode): string;
begin
  result := '';
  if Assigned(Parent) then
    result := Parent.AttributeValueByName[Name];
end;

function TEpiEPXDataset.GetFirstChild(Parent: TXmlNode): TXmlNode;
begin
  result := nil;
  if Assigned(Parent) and
     (Parent.NodeCount > 0)
  then
    result := Parent.FirstNodeByType(xeElement); // Nodes[0];
end;

function TEpiEPXDataset.GetNextSibling(Node: TXmlNode): TXmlNode;
begin
  result := nil;
  if Assigned(Node) and
     Assigned(Node.Parent)
  then
    result := Node.Parent.NextSibling(Node);
end;


{$ELSE}
function TEpiEPXDataset.GetAttribute(const Name: string; Parent: IXMLNode): string;
begin
  result := '';
  if Assigned(Parent) and
     Parent.HasAttribute(Name)
  then
    result := Parent.Attributes[Name];
end;

function TEpiEPXDataset.GetNode(const Name: string; Parent: IXMLNode): IXMLNode;
begin
  result := nil;
  if (Parent.ChildNodes.IndexOf(Name) >= 0) then
    result := Parent.ChildNodes.Nodes[Name];
end;

function TEpiEPXDataset.GetNodeText(Node: IXMLNode): string;
begin
  result := '';
  if (Assigned(Node)) then
    result := Node.Text;
end;

function TEpiEPXDataset.GetFirstChild(Parent: IXMLNode): IXMLNode;
begin
  result := nil;
  if (Assigned(Parent)) and
     (Parent.HasChildNodes)
  then
    result := Parent.ChildNodes[0];
end;

function TEpiEPXDataset.GetNextSibling(Node: IXMLNode): IXMLNode;
begin
  result := nil;
  if Assigned(Node) then
    result := Node.NextSibling;
end;
{$ENDIF}

end.
