unit EpiDataFile;


{

  To do:

  Tilføj DontGetPassword til brug f.eks. ved GetDatafileStructure
  I codebook m.v.: tilføj en mulighed for at afbryde - f.eks. i progressevent
  I codebook: tilføj mulighed for options

}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs, stdctrls,UCmdTypes;


CONST
  MaxRecLineLength=78;
  EOLchars: array[0..2] of char = '!'#13#10;
  NewRecord:LongInt=-1;


  {Errorconstants}
  epi_OK=0;
  EPI_NO_MEMORY=1;
  epi_OPEN_FILE_ERROR=2;
  epi_FILE_NOT_EXISTS=3;
  epi_WRITE_ERROR=4;
  epi_DATAFILE_FORMAT_ERROR=5;
  epi_CREATE_FILE_ERROR=6;
  epi_READ_FILE_ERROR=7;
  EPI_TABLEFULL_ERROR=8;
  epi_DATAFILE_NOT_OPEN=9;
  epi_RECORD_NOT_FOUND=10;
  EPI_INVALID_FIELDHANDLE=11;
  epi_VALUE_NOT_NUMBER_ERROR=12;
  epi_TOO_MANY_FIELDS_ERROR=13;
  EPI_CHECKFILE_ERROR=14;
  EPI_FILE_OERROR=15;
  EPI_INVALID_HANDLE=16;
  EPI_OPEN_WRMODE=17;
  EPI_HEADER_NOTCOMMIT=18;
  EPI_INVALID_FIELDNAME=19;
  EPI_BUFFER_TOO_SMALL=20;

TYPE
  str10=String[10];
  str15=String[15];
  str16=String[16];
  str30=String[30];

  TOpenFileOptions = Set of (StoreInMemory, IgnoreCheckFile, OpenIndexFile);

  RecBuf=Array[0..20000] OF Char;
  PRecBuf=^RecBuf;

  TIndexFields=Array[1..MaxIndices] OF Integer;
  TIndexIsUnique=Array[1..MaxIndices] OF Boolean;
  TIndexFile=File of str30;

  TLeaveStyles=(lsEnter,lsBrowse,lsJumpFirst,lsJumpLast,lsChangeRec,lsNone);
  //TScopes=(scLocal,scGlobal,scCumulative);
  TBeepTypes=(btWarning,btConfirmation,btStandard);

  //Event types
  TRequestPasswordTypes=(rpOpen,rpCreate);
  TRequestPasswordEvent = procedure(Sender: TObject; requesttype:TRequestPasswordTypes; var password:String) of object;
  TTranslateEvent = procedure(Sender: TObject; code:integer; var translation:string) of object;

//***************** TeField *****************************'

  TeField=Class(TObject)
  private
    FName:            String;
    FFieldText:       String;       //Entry made in the field (= text property)
    FScope:           TScopes;      //Scope (Local, global, comulative), used only in DEFINEd variables
    FMissingValues:    TMissingValues;    //legal missing values
    Function  GetFieldName:String;
    Function  GetAsString:String;
    Procedure SetAsString(Value:String);
    Function  GetAsLabel:String;
    Function  GetHasValueLabels:Boolean;
    Function  GetMissingValues(Index: Integer):Str15;
    Procedure SetMissingValues(Index: Integer; Value:str15);
  public
    FVariableLabel:   String[80];   //Variable label
    Felttype:         TFelttyper;   //Field type
    FLength:          Byte;         //Length of data in field
    FCryptEntryLength:Byte;         //Entrylength of encrypt fields (Flength is coded length)   //&&
    FLengthInFile:    Byte;         //Length of field in file
    FNumDecimals:     Byte;         //Number of decimals in numeric fields
    FQuestion:        String[80];   //The field's question (to the left of field)
    FOriginalQuest:   String[80];   //Question as it is saved in REC-file
    LastField:        Boolean;      //True if field is last field in dataform
    FStartPos:        Integer;       //Start position in datafile record of field
    FFieldChar:       Char;
    FFieldColor:      Integer;
    FQuestColor:      Integer;
    FFieldNo:         Integer;
    {Entryfield coordinates}
    FQuestTop:        Integer;      //Question pixel-based top-position   *
    FQuestLeft:       Integer;      //Question pixel-based left-position   *
    FFieldTop:        Integer;      //Entry field's pixel-based top         *
    FFieldLeft:       Integer;      //Entry field's pixel-based left         *
    FFieldWidth:      Integer;      //Entry field's width in pixels          *
    FFieldX,FFieldY:  Integer;      //Field's coordinates in characters
    FQuestX,FQuestY:  Integer;      //Question's coordinates in characters
    {Check related properties}
    FMustEnter:       Boolean;      //True if MUSTENTER is set
    FRepeat:          Boolean;      //True if REPEAT is set
    FMin:             String;       //Mininum value (set by RANGE)
    FMax:             String;       //Maximum value (set by RANGE)
    FLegal:           String;       //Legal values (incl. RANGE values)
    FRangeDefined:    Boolean;      //True if FLegal includes a Range definition
    FCommentLegalRec: PLabelRec;    //Pointer to comment legal record (value label)
    FShowLegalPickList: Boolean;    //True if Comment Legal Show (forces picklist to be shown)
    FPickListNoSelect: Boolean;     //If True then no item is automatically selected in LegalPickList
    FValueLabel:      String[40];   //Name of value label = Comment legal label
    FJumps:           String;       //Jumps definitions
    FJumpResetChar:   Char;         //Fill char when JUMPS RESET "-" is used
    {autosearch properties}
    FAutosearch:      Boolean;      //True if field has autosearch
    FAutoFields:      String;       //CSV-string of fields to autosearch
    FAutoList:        Boolean;      //True if Autosearch has LIST parameter set
    {other properties}
    FNoEnter:         Boolean;
    EntryField:       Pointer;      //Pointer to TEntryField on Dataform
    FFieldComments:   String;       //Comments in checkfile in fieldblock - used only in checkfilemode
    FieldN:           Integer;      //'Free' property for different uses
    FIndex:           Byte;         //Key number = index number
    FIsTypeStatusBar: Boolean;      //Indicates if field has TYPE STATUSBAR
    FTypeComments:    Boolean;      //Fields has TYPE COMMENT
    FTypeString:      Boolean;      //Indicates that field has a TYPE "lkjlkj" command
    FTypeCommentField: Integer;     //Used with TYPE COMMENT fieldname - holds number of field to receive the comment
    FTypeCommentFieldStr: string;   //Used with TYPE COMMENT fieldname - holds name of field to receive the comment
    FTypeField:       TLabel;       //Label on dataform with TYPE-text
    FTypeColor:       TColor;       //Color of TYPE-label
    FConfirm:         Boolean;      //If true then confirm with ENTER before field is left (overrides df^.Confirm)
    AfterCmds:        TList;        //Commands run After Entry
    BeforeCmds:       TList;        //Commands run Before Entry
    OldReadOnly:      Boolean;      //Used to save the ReadOnly status before setting ReadOnly=True during Relate-operations
    FTopOfScreen:     Boolean;      //True=Move field to top of screen when entered ("New Page")
    FTopOfScreenLines: Byte;        //Number of lines to move topofscreen field down
    FHasGlobalMissing: Boolean;   //Set if MISSINGVALUE var1-var2 9 8 7 type command is run previously
    FDefaultValue:     string;      //A default value of the field defined by DEFAULTVALUE x
    FHasGlobalDefaultValue: Boolean;   //A default value define by DEFAULTVALUE ALL X or DEFAULTVALUE field-field, field X
    FFieldFormat:     string;       //Used by analysis: defines the formatting of the data, e.g. %d for integers
    constructor Create;
    destructor  Destroy; override;
    Procedure   ResetCheckProperties;
    Procedure   ResetField;
    Function    HasCheckProperties:Boolean;
    Function    HasSpecialChecks:Boolean;
    Function    Value2Label(Value: string):string;
    Procedure   Clone(dest: TeField; clonevalue:boolean=false);
    Property    AsString:String read GetAsString write SetAsString;
    Property    AsLabel:String read GetAsLabel;
    Property    FieldName:String read GetFieldName write FName;
    Property    Scope:TScopes read FScope write FScope;
    Property    HasValueLabels:Boolean read GetHasValueLabels;
    Property    MissingValues[Index: integer]:str15 read GetMissingValues write SetMissingValues;
  END;



//****************** TEpiDataFile ********************************

  TEpiDataFile = class(TObject)
  private
    { Private declarations }
    FRecFilename,FQESFilename,FCHKFilename,FIndexFilename: TFilename;
    //Datafile properties
    FDatFile:           TFilestream;
    FMemFile:           TMemorystream;
    FOffset:            Longint;        //pointer in datafile where data begins
    FRecLength:         Word;           //Length of one record in datafile incl. NewLine and terminator
    FShortRecLength:    Word;           //Length of one record excl. newline and terminator
    FHasEOFMarker:      Boolean;        //True if datafile has an EOF marker
    FNumRecords:        Integer;        //Total number of records in datafile
    FHasCheckFile:      Boolean;        //does the datafile have a checkfile?
    FRecBuf:            PRecBuf;        //Buffer for reading and writing to/from datafile
    FStoredInMemory:     Boolean;       //True if file is loaded into memory, false if file is on disc
    FFileLabel:         string;          //Label for datafile
    FFieldCheckSum:     Integer;
    FFileModified:      Boolean;        //True if data in file has been modified
    FFilesize:          LongInt;        //Size of datafile in bytes;

    {Field properties}
    FFieldList:         TList;          //List of eFields Records
    FFieldNames:        TStringList;    //List of fieldnames (excl. questions) - created only when needed
    FFieldNamesList:    TStringList;    //List of fieldnames+fieldlabel - created only when needed by dataformunit.FindField1Click
    FNumFields:         Integer;        //Number of fields in datafile incl. question-fields
    FNumDataFields:     Integer;        //Number of fields in datafile excl. question-fields
    FCurField:          Integer;        //Used in CheckFileMode to indicate current field
    FLeaveStyle:        TLeaveStyles;   //Indicates how a field is exited
    FCanExit:           Boolean;        //Flag to indicate if field can be exited
    FFocusedField:      Integer;        //Used in PeekApplyCheckFile to indicate current field

    {Record properties}
    FCurRecord:         Integer;        //Record number of current record
    FCurRecDeleted:     Boolean;        //Is current record marked as deleted?
    FCurRecModified:    Boolean;        //Has current record been modified?
    FDatafileModified:  Boolean;        //Has as record been saved in this session?
    FCurRecVerified:    Boolean;        //Is current record marked as verified (^)

    {Create fields properties}
    FEpiInfoFieldNaming:Boolean;        //Flag to indicate how fieldnames are created
    FUpdateFieldnameInQuestion: Boolean;
    FValueLabels:       TStringList;       //List of valueLabels (pLabelRecs) used
    FHasLongFieldNames: Boolean;        //Flag to indicate if 10-chars fieldnames occur

    {Index related vars}
    FIndexCount:        Byte;           //Number of used indices
    FIndex:             TMemoryStream;  //Index values sorted by rec-no.
    FSortIndex:         TMemoryStream;  //List of rec-numbers (integers) that points to Index (used to sort index)
    FIndexFields:       TIndexFields;   //Fieldnumber of fields with index
    FIndexIsUnique:     TIndexIsUnique; //TRUE means that index[n] is unique
    FIndexFile:         TIndexFile;     //IndexFile = File of Str30

    {Define vars}
    FHasDefined:        Boolean;        //True if checkfile contains define
    FDefList:           TStringList;    //List of defined variables - local for this instance of TEpiDataFile
    FGlobalDefList:     TStringList;    //List of global defined fields - common to mother + related files

    {Encrypt properties}
    FHasCrypt:          Boolean;        //Indicates that a encrypt fields exists in datafile    //&&
    FKey:               String;         //encrypt key

    {IDNumber properties}
    FIDNUMField:        Integer;        //does the datafile contain a IDNUM field?
    FFirstIDNumber:     LongInt;        //First IDNumber used in new datafiles
    FCurIDNumber:       Longint;        //Current IDNumber

    {Checkfile related properties}
    FChkTopComments:    TStringList;    //Commentlines in the top of the checkfile - used only in checkfilemode
    FHasRepeatField:    Boolean;        //If one or more fields have a Repeat check
    FBeforeFileCmds:    TList;          //Commands to be run when file is opened
    FAfterFileCmds:     TList;          //Commands to be run when file is closed
    FBeforeRecordCmds:  TList;          //Commands to be run before current record changes
    FAfterRecordCmds:   TList;          //Commands to be run when changing current record
    FRecodeCmds:        TList;          //Commands to be run during Recode Datafile
    FAssertList:        TStringList;    //used only to store Asserts for checkfilemode
    FConfirm:           Boolean;        //If true then a field is not let automatically when filled out
    FAutoSave:          Boolean;        //IF true then user is not asked "Save record to disk?"
    FGlobalMissingValues: TMissingValues;
    FGlobalDefaultValue:  string;    //Global default value defined by DEFAULTVALUE ALL X or DEFAULTVALUE field-field, field X
    FGlobalTypeCom:     Boolean;   //Show that all fields has a Type Comment Fieldname
    FGlobalTypeComColor: Integer;
    FHasIncludeCmd:     Boolean;
    FHasKeyUnique:      Boolean;        //True = one or more KEYs are KEY UNIQUE
    FErrorInCheckFile:  Boolean;        //True if checkfile exists, but it has errors
    FCheckWriter:       TObject;   //Object that handles writing check-files from internal data structure

    {Relatefile system properties}
    FIsRelateTop:       Boolean;          //True=this instance is the top of the relate hierarchi
    FTopEpiDataFile:    TObject;          //TObject(top TEpiDataFile)
    FIsRelateFile:      Boolean;          //True if this instance is a relate file, i.e. not top of hierarchi
    FRelateFiles:       TStringList;      //List of relate files; is only stored in FTopEpiDataFile;
    FRelateMothers:     TList;            //List of the relate files' mothers; stored only in FTopEpiDatafile;
    FRelateInfo:        PRelateInfo;      //Information on relates in the datafile
    FHasRelate:          Boolean;          //Indicates that the datafile contains at least one relate command

    {Type Statusbar properties}
    FTypeStatusBarField:Integer;        //Fieldno. of TYPE STATUSBAR field
    FTypeStatusBarText: ShortString;    //Prefix text in TYPE STATUSBAR
    FTypeStatusBarColor: TColor;        //Color to write Type StatusBar in

    {Error properties}
    FErrorCode:         Integer;
    FErrorText:         String;

    {Events}
    FOnRequestPassword: TRequestPasswordEvent;
    FOnTranslate:       TTranslateEvent;

    {Colors}
    FQuestionText:      TColor;
    FQuestionBg:        TColor;
    FFieldText:         TColor;
    FFieldBg:           TColor;
    FBackGround:        TColor;

    {misc}
    FMissingAction: TMissingActions;    //What to do with missing values
    FFieldHighlightAct: Boolean;        //highlight active field
    FFieldHighlightCol: TColor;         //color af highlight of active field
    FBackupList:        TStringList;    //List of files to backup
    FOpenOptions: TOpenFileOptions;     //Used to store options given in Open-method
    FComLegalCounter: Integer;          //Counter used to name value labels
    FShowLastRecord:    Boolean;        //if set, then last record is shown when datafile is opened; if false (default) then

    Function  TextPos(var F:Textfile):Longint;
    Procedure ResetEpiDataFile;
    Procedure DisposeFieldList(VAR AList:TList);
    //Procedure DisposeCommandList(VAR AList:TList);
    Function  GetFieldTypeNames(OrdOfFieldtype:Integer):String;
    Function  CountRecords:LongInt;
    Function  GetField(Index: Integer): TeField;
    Function  GetFieldByName(Fieldname: String): TeField;
    Function  GetFieldNumber(Fieldname: String): Integer;
    Function  GetDefFieldByName(Fieldname: string): TeField;
    Function  GetDefFieldNumber(Fieldname: string): Integer;
    Function  GetIndexFields(Index: Integer):Integer;
    Procedure SetIndexFields(Index: Integer; Value:Integer);
    Function  GetIndexIsUnique(Index: Integer):Boolean;
    Procedure SetIndexIsUnique(Index: Integer; Value:Boolean);
    Function  GetGlobalMissingValues(Index: Integer):Str15;
    Procedure SetGlobalMissingValues(Index: Integer; Value:str15);
    Function  GetFileSize:LongInt;
    function  GetCheckLines:TStringList;
    function  GetQesLines: string;
  public
    CheckFileMode:Boolean;
    constructor Create;
    destructor  Destroy;  override;
    Function    Lang(langkode:Integer; CONST langtext:string):String;
    Function    Open(Const filename:String; OpenOptions:TOpenFileOptions):Boolean;
    //Methods related to creating new datafile and adding fields, record
    Function    SaveStructureToFile(filename: string; OverwriteExisting:boolean=false):boolean;
    procedure   SaveCheckFile;
    procedure   AddField(aField: TeField);
    //Read and write methods
    Procedure   Read(RecNum:LongInt);
    Procedure   Write(RecNum:LongInt);
    Function    ReadFromMem(AField:TeField; RecNo:LongInt; VAR RecIsDeleted:Boolean):String;
    procedure   Next;
    procedure   Prev;
    procedure   First;
    procedure   Last;
    procedure   Append;
    procedure   Post;
    //Checkfile related methods
    Function    LoadChecks:Boolean;
    Function    MakeIndexFile:Boolean;
    Function    ApplyIndex:Boolean;
    Function    ReadFromIndex(IndexNo,RecNo: Integer):str30;
    Function    ReadCommonIndex(RecNo: Integer):String;
    Procedure   InitSortIndex;
    Procedure   DoSort(L,R:Integer);
    Function    ReadIndexNoFromSortIndex(SortPos: Integer):Integer;
    Procedure   WriteIndexNoToSortIndex(SortPos,num:Integer);
    Function    ReadCommonViaSortIndex(SortPos: Integer):String;
    Procedure   WriteToIndex(IndexNo,RecNo: Integer; s:Str30);
    Function    SearchIndex(IndexNo: Integer; SearchStr: Str30):LongInt;
    Function    SearchIndexFrom(IndexNo: Integer; SearchStr: str30; RecNo:Integer; direction:TDirections):LongInt;
    Function    IndexHasDuplicates(IndexNo:Integer):Boolean;
    Procedure   DecryptIndex;
    Function    DoRebuildIndex: Boolean;
    PROCEDURE DestroyValueLabels(VAR ALabelList:TStringList);

    //Properties
    Property Fields[Index: Integer]: TeField read GetField; default;
    Property FieldsByName[Fieldname: string]: TeField read GetFieldByName;
    Property FieldNumbers[Fieldname: string]: Integer read GetFieldNumber;
    Property DefFieldsByName[Fieldname: string]: TeField read GetDefFieldByName;
    Property DefFieldNumbers[Fieldname: string]: Integer read GetDefFieldNumber;
    Property StoredInMemory:Boolean read FStoredInMemory;
    Property NumRecords: LongInt read FNumRecords;
    Property HasCheckFile:Boolean read FHasCheckFile write FHasCheckFile;
    Property NumFields:Integer read FNumfields;
    Property NumDataFields:Integer Read FNumDataFields;
    Property Curfield:Integer read FCurField;
    Property LeaveStyle:TLeaveStyles read FLeaveStyle write FLeaveStyle;
    Property CanExit:Boolean read FCanExit write FCanExit;
    Property FocusedField:Integer read FFocusedField write FFocusedField;
    Property CurRecord:Integer read FCurRecord;   //write SetCurRecord
    Property CurRecDeleted:Boolean read FCurRecDeleted write FCurRecDeleted;
    Property CurRecVerified:boolean read FCurRecVerified write FCurRecVerified;
    Property CurRecModified:Boolean read FCurRecModified;
    Property DatafileModified:Boolean read FDatafileModified;
    Property RecFilename:TFilename read FRecFilename write FRecFilename;
    Property ChkFilename:TFilename read FChkFilename write FChkFilename;
    Property Indexfilename:TFilename read FIndexfilename write FIndexfilename;
    Property Filelabel:string read FFilelabel write FFilelabel;
    Property EpiInfoFieldNaming:Boolean read FEpiInfoFieldNaming write FEpiInfoFieldNaming;
    Property ErrorCode:Integer read FErrorCode write FErrorCode;
    Property ErrorText:String read FErrorText write FErrorText;
    Property OnRequestPassword: TRequestPasswordEvent read FOnRequestPassword write FOnRequestPassword;
    Property OnTranslate: TTranslateEvent read FOnTranslate write FOnTranslate;
    Property HasIncludeCmd:Boolean read FHasIncludeCmd write FHasIncludeCmd;
    Property ChkTopComments:TStringList read FChkTopComments write FChkTopComments;
    Property BeforeFileCmds:TList read FBeforeFileCmds write FBeforeFileCmds;
    Property AfterFileCmds:TList read FAfterFileCmds write FAfterFileCmds;
    Property BeforeRecordCmds:TList read FBeforeRecordCmds write FBeforeRecordCmds;
    Property AfterRecordCmds:TList read FAfterRecordCmds write FAfterRecordCmds;
    Property RecodeCmds:TList read FRecodeCmds write FRecodeCmds;
    Property IndexCount:Byte read FIndexCount write FIndexCount;
    Property EIndex:TMemoryStream read FIndex write FIndex;
    Property SortIndex:TMemoryStream read FSortIndex write FSortIndex;
    Property IndexFields[Index:Integer]:Integer read GetIndexFields write SetIndexFields;
    Property IndexIsUnique[Index:Integer]:Boolean read GetIndexIsUnique write SetIndexIsUnique;
    Property IndexFile:TIndexFile read FIndexFile write FIndexFile;
    Property HasRepeatField:Boolean read FHasRepeatField write FHasRepeatField;
    Property ValueLabels:TStringList read FValueLabels write FValueLabels;
    Property AssertList:TStringList read FAssertList write FAssertList;
    Property TopEpiDataFile:TObject read FTopEpiDataFile write FTopEpiDataFile;
    Property DefList:TStringList read FDefList write FDefList;
    Property GlobalDefList:TStringList read FGlobalDefList write FGlobalDefList;
    Property Confirm:Boolean read FConfirm write FConfirm;
    Property Autosave:Boolean read FAutosave write FAutosave;
    Property GlobalMissingValues[Index: integer]:str15 read GetGlobalMissingValues write SetGlobalMissingValues;
    Property GlobalDefaultValue:string read FGlobalDefaultValue write FGlobalDefaultValue;
    Property MissingAction:TMissingActions read FMissingAction write FMissingAction;
    Property GlobalTypeCom:Boolean read FGlobalTypeCom write FGlobalTypeCom;
    Property GlobalTypeComColor:Integer read FGlobalTypeComColor write FGlobalTypeComColor;
    Property FieldHighlightAct: Boolean read FFieldHighlightAct write FFieldHighlightAct;
    Property FieldHighlightCol: TColor read FFieldHighlightCol write FFieldHighlightCol;
    Property BackupList: TStringList read FBackupList write FBackupList;
    Property IsRelateTop:Boolean read FIsRelateTop write FIsRelateTop;
    Property IsRelateFile:Boolean read FIsrelateFile write FIsrelateFile;
    Property RelateFiles: TStringList read FRelateFiles write FRelateFiles;
    Property RelateMothers: TList read FRelateMothers write FRelateMothers;
    Property RelateInfo: PRelateInfo read FRelateInfo write FRelateInfo;
    Property HasRelate:Boolean read FHasRelate write FHasRelate;
    Property QuestionText:TColor read FQuestionText write FQuestionText;
    Property QuestionBg:TColor read FQuestionBg write FQuestionBg;
    Property FieldText:TColor read FFieldText write FFieldText;
    Property FieldBg:TColor read FFieldBg write FFieldBg;
    Property BackGround:TColor read FBackGround write FBackGround;
    Property ComLegalCounter:Integer read FComLegalCounter write FComLegalCounter;
    Property TypeStatusBarField:Integer read FTypeStatusBarField write FTypeStatusBarField;
    Property TypeStatusBarText: ShortString read FTypeStatusBarText write FTypeStatusBarText;
    Property TypeStatusBarColor: TColor read FTypeStatusBarColor write FTypeStatusBarColor;
    Property DatafileSize:LongInt read GetFileSize;
    Property ErrorInCheckFile:Boolean read FErrorInCheckFile write FErrorInCheckFile;
    Property FieldtypeNames[OrdOfFieldtype:Integer]:String read GetFieldtypeNames;
    Property ShowLastRecord:boolean read FShowLastRecord write FShowLastRecord;
    Property password:string read FKey write FKey;
    Property RecordLength:word read FRecLength;
    Property CheckLines:TStringList read GetCheckLines;
    property QesLines: string read GetQesLines;
  published
    { Published declarations }
  end;






implementation

USES
  EpiDataUtils,CheckObjUnit;

//TEpiDataFile************************************************

Procedure peWrite(VAR f:ByteFile; Const s:String);
VAR
  t,n:Byte;
BEGIN
  FOR n:=1 TO Length(s) DO
    BEGIN
      t:=ORD(s[n]);
      Write(f,t);
    END;  //for
END;  //procedure peWrite


constructor TEpiDataFile.Create;
VAR
  n:Integer;
BEGIN
  inherited Create;
  FRecFilename:='';
  FQesFilename:='';
  FChkFilename:='';
  FIndexFilename:='';
  FDatFile:=NIL;
  FMemFile:=NIL;
  FStoredInMemory:=False;
  FFieldList:=NIL;
  FHasEOFMarker:=False;
  FNumRecords:=0;
  FHasCheckFile:=False;
  FHasIncludeCmd:=False;
  FFileModified:=False;
  FFieldNames:=NIL;
  FFieldNamesList:=NIL;
  FNumFields:=0;
  FCurField:=0;
  FCurRecord:=0;
  FCurRecModified:=False;
  FDatafileModified:=False;
  FEpiInfoFieldNaming:=False;
  FUpdateFieldnameInQuestion:=False;
  FValueLabels:=TStringList.Create;
  FHasLongFieldNames:=False;
  FGlobalDefList:=TStringList.Create;
  FGlobalDefaultValue:='';
  FIndex:=NIL;
  FSortIndex:=NIL;
  FOR n:=1 TO MaxIndices DO
    BEGIN
      FIndexFields[n]:=-1;
      FIndexIsUnique[n]:=False;
    END;
  FRecBuf:=NIL;
  FDefList:=NIL;
  FHasDefined:=False;
  FHasCrypt:=False;
  FKey:='';
  FFilelabel:='';
  FChkTopComments:=NIL;
  FHasRepeatField:=False;
  FBeforeFileCmds:=NIL;
  FAfterFileCmds:=NIL;
  FBeforeRecordCmds:=NIL;
  FAfterRecordCmds:=NIL;
  FRecodeCmds:=NIL;
  FAssertList:=NIL;
  FConfirm:=False;
  FAutoSave:=False;
  FGlobalMissingValues[0]:='';
  FGlobalMissingValues[1]:='';
  FGlobalMissingValues[2]:='';
  FGlobalTypeCom:=False;
  FGlobalTypeComColor:=clBlue;
  FIsRelateTop:=True;
  FIsRelateFile:=False;
  FOnRequestPassword:=NIL;
  FChkTopComments:=NIL;
  FTopEpiDataFile:=TObject(self);
  FGlobalTypeCom:=False;
  CheckFileMode:=False;
  FHasKeyUnique:=False;
  FCheckWriter:=NIL;
END;

Function TEpiDataFile.TextPos(var F:Textfile):Longint;
BEGIN
  With TTextRec(F) DO
    BEGIN
      Result:=SetFilePointer(Handle,0,nil,FILE_CURRENT);
      IF Mode=FMOutput THEN INC(Result, BufPos)
      ELSE IF BufEnd<>0 THEN Dec(Result, BufEnd-BufPos);
    END;
END;


{Procedure TEpiDataFile.DisposeCommandList(VAR AList:TList);
VAR
  n:Integer;
BEGIN
  FOR n:=0 TO AList.Count-1 DO
    BEGIN
      tmpCmdRec:=PCmds(AList.Items[n])^;
      CASE tmpCmdRec.Command OF
        cmdIF:
          BEGIN
            IF tmpCmdRec.IfCmds<>NIL THEN DisposeCommandList(tmpCmdRec.IfCmds);
            IF tmpCmdRec.ElseCmds<>NIL THEN DisposeCommandList(tmpCmdRec.ElseCmds);
          END;
      END;  //case
      Dispose(Alist.Items[n]);
    END;  //for
  AList.Free;
  AList:=NIL;
END;  //procedure DisposeCommandList}


Function TepiDataFile.Lang(langkode:Integer; CONST langtext:string):String;
var
  s:string;
BEGIN
  s:='';
  IF Assigned(FOnTranslate) THEN
    BEGIN
      FOnTranslate(self,langkode, s);
      Result:=s;
    END
  ELSE Result:=langtext;
  if result='' then result:=langtext;
END;

Function  TEpiDataFile.GetFieldTypeNames(OrdOfFieldtype:Integer):String;
BEGIN
  CASE OrdOfFieldtype OF
    0:  Result:=Lang(50100,'Numeric');
    1:  Result:=Lang(50101,'Text');
    2:  Result:=Lang(50102,'Date (mdy)');
    3:  Result:=Lang(50103,'Uppercase text');
    4:  Result:='Checkbox';
    5:  Result:=Lang(50105,'Boolean');
    6:  Result:=Lang(50100,'Numeric');
    7:  Result:='Phonenumber';
    8:  Result:='Time';
    9:  Result:='Local phonenumber';
    10: Result:=Lang(50110,'Today (mdy)');
    11: Result:=Lang(50111,'Date (dmy)');
    12: Result:=Lang(50112,'ID-number');
    15: Result:=Lang(50115,'Question');
    16: Result:=Lang(50116,'Today (dmy)');
    17: Result:=Lang(50117,'Soundex');
    18: Result:=Lang(50118,'Encryptfield');
    19: Result:=Lang(50119,'Date (ymd)');
    20: Result:=Lang(50120,'Today (ymd)');
  ELSE
    Result:='Unknown type';
  END;  //case
END;


Procedure TEpiDataFile.DisposeFieldList(VAR AList:TList);
VAR
  n:Integer;
BEGIN
  FOR n:=0 TO AList.Count-1 DO
    BEGIN
      TeField(AList.items[n]).Free;
    END;
  AList.Free;
  AList:=NIL;
END;  //procedure DisposeFieldList

PROCEDURE TEpiDataFile.DestroyValueLabels(VAR ALabelList:TStringList);
VAR
  n:Integer;
  tmpLabelRec,NextLabelRec:PLabelRec;
BEGIN
  IF ALabelList.Count>0 THEN
    BEGIN
      FOR n:=0 TO ALabelList.Count-1 DO
        BEGIN
          tmpLabelRec:=PLabelRec(ALabelList.Objects[n]);
          NextLabelRec:=NIL;
          REPEAT
            IF tmpLabelRec<>NIL THEN NextLabelRec:=tmpLabelRec^.Next;
            IF tmpLabelRec<>NIL THEN Dispose(tmpLabelRec);
            tmpLabelRec:=NextLabelRec;
          UNTIL NextLabelRec=NIL;
        END;  //for n
    END;  //if count>0
  ALabelList.Free;
  ALabelList:=NIL;
END;



Procedure TEpiDataFile.ResetEpiDatafile;
VAR
  n:Integer;
BEGIN
  IF Assigned(FFieldList) THEN DisposeFieldList(FFieldList);
  IF Assigned(FValueLabels) THEN DestroyValueLabels(FValueLabels);
  IF FRecBuf<>NIL THEN
    BEGIN
      FreeMem(FRecBuf);
      FRecBuf:=NIL;
    END;
  FMemFile.Free;
  FMemFile:=NIL;
  FDatfile.Free;
  FDatfile:=NIL;
  {$I-}
  CloseFile(FIndexFile);
  n:=IOResult;
  {$I+}
  IF Assigned(FIndex) THEN FreeAndNil(FIndex);
  IF Assigned(FSortIndex) THEN FreeAndNil(FSortIndex);
  IF Assigned(FFieldNames) THEN FreeAndNil(FFieldNames);
  IF Assigned(FFieldNamesList) THEN FreeAndNil(FFieldNamesList);
  IF Assigned(FDefList) THEN
    BEGIN
      FOR n:=0 TO FDefList.Count-1 DO
        TeField(FDefList.Objects[n]).Free;
      FreeAndNil(FDefList);
    END;
  IF (FIsRelateTop) AND (FGlobalDefList<>NIL) THEN
    BEGIN
      FOR n:=0 TO FGlobalDefList.Count-1 DO
        TeField(FGlobalDefList.Objects[n]).Free;
      FreeAndNil(FGlobalDefList);
    END;
  IF Assigned(FBeforeFileCmds)   THEN DisposeCommandList(FBeforeFileCmds);
  IF Assigned(FAfterFileCmds)    THEN DisposeCommandList(FAfterFileCmds);
  IF Assigned(FBeforeRecordCmds) THEN DisposeCommandList(FBeforeRecordCmds);
  IF Assigned(FAfterRecordCmds)  THEN DisposeCommandList(FAfterRecordCmds);
  IF Assigned(FRecodeCmds)       THEN DisposeCommandList(FRecodeCmds);
//  IF Assigned(FLastCommands)     THEN DisposeCommandList(FLastCommands);
  IF Assigned(FAssertList)       THEN FAssertList.Free;
  IF Assigned(FBackupList)       THEN FBackupList.Free;
  IF Assigned(FChkTopComments) THEN FChkTopComments.Free;
  FChkTopComments:=NIL;
  FRecFilename:='';
  FQESFilename:='';
  FCHKFilename:='';
  FIndexFilename:='';
  FErrorCode:=0;
  FErrorText:='';
  FHasCrypt:=False;
  FHasIncludeCmd:=False;
  FFileModified:=False;
  FGlobalTypeCom:=False;
  FIsRelateFile:=False;
  FIsRelateTop:=True;
  FHasKeyUnique:=False;
END;  //TEpiDataFile.ResetEpiDatafile;


Destructor TEpiDataFile.Destroy;
VAR
  n:Integer;
BEGIN
  ResetEpiDataFile;
  inherited destroy;
END;  //destructor


Function TEpiDataFile.CountRecords:LongInt;
TYPE
  smallBuf=Array[0..20] OF Char;
  PsmallBuf=^smallBuf;

VAR
  b: PsmallBuf;
BEGIN
  GetMem(b,3);
  IF FStoredInMemory THEN
    BEGIN
      IF FMemFile.Size=FOffset THEN Result:=0
      ELSE
        BEGIN
          Result:=FMemFile.Size-FOffset;
          FMemFile.Position:=FMemFile.Size-3;
          FMemFile.ReadBuffer(b^,3);
        END;
    END
  ELSE
    BEGIN
      IF FDatfile.size=FOffset THEN Result:=0
      ELSE
        BEGIN
          Result:=FDatfile.Size-FOffset;
          FDatfile.Position:=FDatfile.size-3;
          FDatfile.ReadBuffer(b^,3);
        END;
    END;
  IF Result<>0 THEN
    BEGIN
      IF b^[2]<>#26 THEN   //is b3=EOF mark?
        BEGIN
          IF (b^[1]<>#13) or (b^[2]<>#10) THEN INC(Result,2);
        END
      ELSE
        BEGIN
          Dec(Result);
          FHasEOFMarker:=True;
          IF (b^[0]<>#13) or (b^[1]<>#10) THEN INC(Result,2);
        END;
      IF Result MOD FRecLength <> 0 THEN Result:=-1
      ELSE Result:=Result DIV FRecLength;
    END;
  FreeMem(b);
END;  //function PeekCountRecords



Function TEpiDataFile.Open(Const filename:String; OpenOptions:TOpenFileOptions):Boolean;
VAR
  TempResult,ok: Boolean;
  F: TextFile;
  eLine,OrigFieldname,TempStr,TempKey,PreEnteredKey: String;
  tmpName:str10;
  TempInt,TempInt2,Curfield: Integer;
  eField: TeField;
  FieldChar,Dummy: Char;
  QuestColor,FieldColor,ft,n,t,FieldNumberCounter: Integer;
BEGIN
  FieldNumberCounter:=1;
  PreEnteredKey:=FKey;
  ResetEpiDatafile;
  FValueLabels:=TStringList.Create;
  FGlobalDefList:=TStringList.Create;
  FOpenOptions:=OpenOptions;
  FRecfilename:=filename;
  FNumFields:=0;
  TempResult:=True;
  AssignFile(F,FRecFilename);
  {$I-}
  Reset(F);
  TempInt:=IOResult;
  {$I+}
  IF TempInt=0 THEN   //datafile could be opened
    BEGIN
      FOffset:=0;
      FFieldList:=TList.Create;
      {Read first line i datafile - number of fields}
      ReadLn(F,eLine);
      eLine:=eLine+' ';
      TempStr:=COPY(eLine,1,POS(' ',eLine)-1);
      IF IsInteger(TempStr) THEN FNumFields:=StrToInt(TempStr)
      ELSE
        BEGIN
          CloseFile(F);
          FErrorText:=Format(Lang(20112,'Incorrect format of datafile %s'),[FRECfilename]);  //Incorrect format of datafile %s.
          FErrorCode:=epi_DATAFILE_FORMAT_ERROR;
          Result:=False;
          Exit;
        END;
      IF TempResult THEN   //Begin reading the field-info
        BEGIN
          TempInt:=pos('~kq:',eLine);    //&&
          IF TempInt>0 THEN
            BEGIN
              //Datafile contains a crypt-key
              TempInt2:=pos(':kq~',eLine);
              IF (TempInt2>0) AND (TempInt2>TempInt) THEN FKey:=copy(eLine,TempInt+4,TempInt2-TempInt-4);
            END;
          TempInt:=Pos('FILELABEL: ',AnsiUpperCase(eLine));
          IF TempInt<>0 THEN FFilelabel:=Copy(eLine,TempInt+Length('FILELABEL: '),Length(eLine));
          IF Pos(' VLAB',eLine)>0 THEN FEpiInfoFieldNaming:=False ELSE FEpiInfoFieldNaming:=True;
          FFieldList.Capacity:=FNumFields;
          FRecLength:=0;
          FIDNumField:=-1;

          FOR CurField:=1 to NumFields DO
            BEGIN
              //Eventuelt lægge et progress-event ind her: ProgressBar.Position:=CurField  /  pct = CurField/NumFields
              eField:=TeField.Create;
              TRY
                WITH eField DO
                  BEGIN
                    ReadLn(F,FFieldChar,tmpName,FQuestX,FQuestY,FQuestColor,FFieldX,FFieldY,
                           ft,FLength,FFieldColor,dummy,FQuestion);
                    Fieldname:=tmpName;
                    OrigFieldname:=fieldname;
                    //['0'..'9','A'..'Z','a'..'z',' ']
                    ok:=true;
                    FOR n:=1 TO Length(Fieldname) do
                      if (NOT (Fieldname[n] in ['0'..'9','A'..'Z','a'..'z',' '])) THEN ok:=false;
                    IF (not ok) THEN
                      BEGIN
                        //Fieldname includes illegal chars
                        ok:=false;
                        repeat
                          Fieldname:='V'+IntToStr(FieldNumberCounter);
                          IF FFieldlist.Count>0 THEN
                            begin
                              ok:=true;
                              for t:=0 to FFieldList.Count-1 DO
                                if trim(ansilowercase(TeField(FFieldList.Items[t]).FName))=trim(ansilowercase(Fieldname)) then ok:=false;
                            end  //if
                          else ok:=true;
                          INC(FieldNumberCounter);
                        until ok;
                      END;
                    IF Length(FieldName)>8 THEN FHasLongFieldNames:=True;
                    WHILE Pos('_',FQuestion)>0 DO FQuestion[Pos('_',FQuestion)]:='-';
                    FNumDecimals:=0;
                    IF ft>=100 THEN BEGIN
                      FNumDecimals:=ft-100;
                      Felttype:=ftFloat;
                    END  //if ftFloat
                    ELSE BEGIN
                      FeltType:=ftInteger;
                      FNumDecimals:=0;
                      WHILE ft>ORD(FeltType) DO FeltType:=Succ(FeltType);
                    END;
                    IF FLength=0 THEN FeltType:=ftQuestion;
{                    IF (FeltType=ftPhoneNum) or (FeltType=ftLocalNum) THEN FeltType:=ftAlfa;

                    IF (FeltType in [ftCheckBox,ftPhoneNum,ftTime,ftLocalNum,ftRes4,ftRes5]) THEN
                      BEGIN
                        FErrorText:=Format(Lang(20144,'Datafile %s contains a field of the type %s~This fieldtype is not supported by EpiData.'),[FRECFilename,FieldTypeNames[ORD(FeltType)]]);   //20144=Datafile %s contains a field of the type %s~This fieldtype is not supported by EpiData.
                        FErrorCode:=epi_DATAFILE_FORMAT_ERROR;
                        CloseFile(F);
                        Result:=False;
                        Exit;
                      END;}
                    IF (NOT (FeltType in SupportedFieldTypes)) or ((Felttype in DateFieldTypes) and (FLength<10))
                    then FeltType:=ftAlfa;
                    FCryptEntryLength:=0;   //&&
                    FLengthInFile:=FLength;
                    IF (FeltType=ftCrypt) AND (FHasCrypt=false) THEN
                      BEGIN
                        FHasCrypt:=True;
                        //IF FieldColor>111 THEN FCryptEntryLength:=FieldColor-111 ELSE FCryptEntryLength:=FieldColor;
                        TempKey:='';
                        TRY
                          IF SysUtils.Trim(PreEnteredKey)<>'' then TempKey:=PreEnteredKey
                          ELSE IF Assigned(FOnRequestPassword) THEN FOnRequestPassword(self,rpOpen,TempKey);
                          //Tilføj eventuelt en rutine der checker password f.eks. 3 gange før afbrydelse
                          IF TempKey=DecryptString(FKey,TempKey) THEN FKey:=TempKey
                          ELSE
                            BEGIN
                              FErrorText:=Lang(9020,'Incorrect password entered');
                              FErrorcode:=epi_DATAFILE_FORMAT_ERROR;
                              Result:=False;
                              CloseFile(F);
                              Exit;
                            END;
                        EXCEPT
                          FErrorText:=Lang(9022,'Error encouted during decryption of password');
                          FErrorcode:=epi_DATAFILE_FORMAT_ERROR;
                          Result:=False;
                          CloseFile(F);
                          Exit;
                        END;  //try..except
                      END;   //if Crypt and HasCrypt=False
                    IF FeltType=ftCrypt THEN         //MIB 19jan07
                      BEGIN
                        IF FFieldColor>111 THEN FCryptEntryLength:=FFieldColor-111
                        ELSE FCryptEntryLength:=FFieldColor;
                      END;
                    FStartPos:=FRecLength+(FRecLength DIV MaxRecLineLength)*3;
                    FRecLength:=FRecLength+FLength;
                    FVariableLabel:=trim(FQuestion);
                    FFieldNo:=Curfield-1;
                    IF NOT (FEpiInfoFieldNaming) AND (trim(FVariableLabel)<>'') THEN
                      BEGIN
                        TempStr:=FirstWord(FVariableLabel);
                        Delete(FVariableLabel,Pos(TempStr,FVariableLabel),Length(TempStr));
                        FVariableLabel:=trim(FVariableLabel);
                      END;
                    IF FieldName<>OrigFieldname THEN FVariableLabel:=OrigFieldname+' '+FVariableLabel;
                    eField.ResetCheckProperties;
                    FFieldCheckSum:=FFieldCheckSum+ORD(eField.Felttype)+Length(eField.FQuestion)+eField.FLength;
                  END;  //with eField
                FFieldList.Add(eField);
              EXCEPT
                FErrorText:=Format(Lang(20116,'Error in the datafile %s.~~The field definition in line %d could not be read or interpreted.'),[FRECfilename,CurField+1]);
                FErrorCode:=epi_DATAFILE_FORMAT_ERROR;
                CloseFile(F);
                Result:=False;
                Exit;
              END;  //try..except
            END;  //for CurField
          FShortRecLength:=FRecLength;
          FRecLength:=FRecLength+(((FRecLength-1) DIV MaxRecLineLength)+1)*3;  //Add NewLine+LineFeed+Terminatorchar.
          FOffSet:=TextPos(F);

          GetMem(FRecBuf,FRecLength);
          CloseFile(F);

          IF (StoreInMemory in OpenOptions) THEN
            BEGIN
              //store file in memory
              FStoredInMemory:=True;
              FMemFile:=TMemoryStream.Create;
              FMemFile.LoadFromFile(FRECFilename);
            END
          ELSE
            BEGIN
              FStoredInMemory:=False;
              FDatfile:=TFileStream.Create(FRECFilename,fmOpenReadWrite OR fmShareExclusive);
            END;
          FNumDataFields:=0;
          FOR CurField:=0 TO FFieldList.Count-1 DO
            BEGIN
              IF TeField(FFieldList.Items[CurField]).FeltType=ftIDNUM THEN FIDNUMField:=CurField;
              IF TeField(FFieldList.Items[CurField]).FeltType<>ftQuestion THEN INC(FNumDataFields);
            END;
          FNumRecords:=CountRecords;
          IF FNumRecords=-1 THEN
            BEGIN
              FErrorText:=Format(Lang(20118,'Error in datafile %s.~~One or more records are corrupted.'),[FRECFilename]);
              FErrorCode:=epi_DATAFILE_FORMAT_ERROR;
              Result:=False;
              Exit;
            END;
        END;  //if fieldinfo can be read
      FHasCheckFile:=FileExists(ChangeFileExt(FRECFilename,'.chk'));
      if FHasCheckFile then FCHKFilename:=ChangeFileExt(FRECFilename,'.chk');
    END //if datafile could be opened
  ELSE
    BEGIN
      FErrorText:=Format(Lang(20108,'Data file %s could not be opened.'),[FRECFilename])+#13+Lang(20208,'Please check if the file is in use and that the file name is legal.');
      FErrorCode:=epi_DATAFILE_FORMAT_ERROR;
      TempResult:=False;
    END;
  FCurRecord:=-1;
  Result:=TempResult;
  IF (Result) AND (NOT (IgnoreCheckFile in OpenOptions)) THEN Result:=LoadChecks;
END;


Procedure TEpiDataFile.Read(RecNum:LongInt);
VAR
  ABuf: PRecBuf;
  n, rdN, LineCharCount, BufCount: Integer;
  ss: String;
  ok: Boolean;
BEGIN
  IF (RecNum<=FNumRecords) AND (RecNum>0) THEN
    BEGIN
      ABuf:=FRecBuf;
      IF FStoredInMemory THEN
        BEGIN
          FMemFile.Position:=FOffset+((RecNum-1)*FRecLength);
          n:=0;
          REPEAT
            INC(n);
            ok:=True;
            TRY
              FMemFile.ReadBuffer(ABuf^,FRecLength);
            EXCEPT
              ok:=False;
              IF n>=3 THEN raise Exception.Create(Lang(20464,'Error reading record'));  //20464=Error reading record
            END;  //try..except
          UNTIL ok;
        END
      ELSE
        BEGIN
          FDatFile.Position:=FOffset+((RecNum-1)*FRecLength);
          n:=0;
          REPEAT
            INC(n);
            ok:=True;
            TRY
              FDatFile.ReadBuffer(ABuf^,FRecLength);
            EXCEPT
              ok:=False;
              IF n>=3 THEN raise Exception.Create(Lang(20464,'Error reading record'));  //20464=Error reading record
            END;  //try..except
          UNTIL ok;
        END;
      IF n>=3 THEN raise Exception.Create(Lang(20464,'Error reading record'));    //20464=Error reading record
      LineCharCount:=MaxRecLineLength;
      BufCount:=0;
      FOR rdN:=0 TO FFieldList.Count-1 DO
        BEGIN
          WITH TeField(FFieldList.Items[rdN]) DO
            BEGIN
              IF Felttype<>ftQuestion THEN
                BEGIN
                  FFieldText:=cFill(' ',FLength);
                  FOR n:=1 TO FLength DO
                    BEGIN
                      FFieldText[n]:=ABuf^[BufCount];
                      INC(BufCount);
                      DEC(LineCharCount);
                      IF LineCharCount<=0 THEN
                        BEGIN
                          LineCharCount:=MaxRecLineLength;
                          INC(BufCount,3);
                        END;
                    END;  //for
                  FFieldText:=trim(FFieldText);
                  IF (Felttype=ftCrypt) AND (FKey<>'') THEN
                    BEGIN
                      ss:=FFieldText;
                      ss:=DecryptString(ss,FKey);  //&&
                      FFieldtext:=ss;
                    END;

                  //**************** Eventuel opdatering af Dataform ****************
                  {
                  IF Assigned(df^.DatForm) THEN
                    BEGIN
                      ChangeGoingOn:=True;
                      TEntryField(EntryField).Text:=FFieldText;
                      TEntryField(EntryField).Modified:=False;
                      ChangeGoingOn:=False;
                      IF (FTypeComments) OR (FTypeString) THEN FTypeField.Caption:='';
                    END;  //if assigned
                  }
                  //****************************************

                END;  //if not ftQuestion
            END;  //with
        END;  //for
      FCurRecModified:=False;
      FCurRecord:=RecNum;
      FCurRecDeleted:=(ABuf^[BufCount]='?');
      FCurRecVerified:=(ABuf^[BufCount]='^');
      {
      IF NOT NoUpDateCurRecEdit
      THEN TDataForm(df^.DatForm).UpdateCurRecEdit(RecNum, df^.NumRecords);
      }
    END;  //if RecNum<=NumRecords
END;   //TepiDatafile.Read

procedure TEpiDataFile.Next;
begin
  if FCurRecord<>NewRecord then Read(FCurRecord+1);
end;

procedure TEpiDataFile.Prev;
begin
  if FCurRecord<>NewRecord then Read(FCurRecord-1);
end;

procedure TEpiDataFile.First;
begin
  Read(1);
end;

procedure TEpiDataFile.Last;
begin
  Read(FNumRecords);
end;

procedure TEpiDataFile.Append;
//Empties values of all fields, sets FCurRecord to NewRecord
//i.e. creates an empty record which can be saved later
var
  n:integer;
begin
  FCurRecord:=NewRecord;
  FCurRecModified:=false;
  FCurRecDeleted:=false;
  FCurRecVerified:=false;
  for n:=0 to FNumFields-1 do
    GetField(n).FFieldText:='';
end;

procedure TEpiDataFile.Post;
begin
  Write(FCurRecord);
end;

Procedure TEpiDataFile.Write(RecNum:LongInt);
VAR
  wrN,n,repcounter,ecode:Integer;
  TempS:String[80];
  s:Str30;
  eRecString,s2:String;
  ABuf: PRecBuf;
  BufCount,LineCharCount: Integer;
  ok:Boolean;
BEGIN
  ABuf:=FRecBuf;
  IF RecNum=NewRecord THEN
    BEGIN
      IF FHasEOFMarker THEN
        BEGIN
          FHasEOFMarker:=False;
          IF FStoredInMemory THEN
            BEGIN
              FMemFile.SetSize(FMemFile.Size-1+FRecLength);
              FMemFile.Position:=FMemFile.Size-FRecLength;
            END
          ELSE FDatfile.Position:=FDatfile.Size-1;
        END    //if HasEOFMarker
      ELSE
        BEGIN
          IF FStoredInMemory THEN
            BEGIN
              FMemFile.SetSize(FMemFile.Size+FRecLength);
              FMemFile.Position:=FMemFile.Size-FRecLength;
            END
          ELSE FDatfile.Position:=FDatFile.Size;
        END;  //if not HasEOFMarker
      INC(FNumRecords);
      //Add empty record to indexfile and resize index

      //TODO: Add handling of IndexFiles
      IF (false) and (FIndexCount>0) THEN
        BEGIN
          repcounter:=0;
          REPEAT
            ok:=True;
            INC(repcounter);
            TRY
              s:='';
              Seek(FIndexFile,Filesize(FIndexFile));
              FOR n:=1 TO FIndexCount DO System.Write(FIndexfile,s);
            EXCEPT
              ok:=False;
              IF repcounter>=3 THEN
                //IF eDlg(Format(Lang(20460,'%d attempts of writing current record failed~~Retry?'),[repcounter]),     //20460=%d attempts of writing current record failed~~Retry?
                //mtWarning,[mbYes,mbNo],0)=mrNo THEN
                //  BEGIN
                //    ok:=True;
                //    repcounter:=-1;
                //  END;
                ok:=True;
                Raise EWriteError.Create(Lang(20462,'Current record not saved!'));
            END;  //try..except
          UNTIL ok;
          //IF repcounter=-1 THEN raise EWriteError.Create(Lang(20462));  //20462=Current record not saved!

          FIndex.SetSize(FIndex.Size+(FIndexCount*31));
          //If assigned(df^.SortIndex) THEN....
        END;
    END  //if NewRecord
  ELSE
    BEGIN
      IF StoredInMemory THEN FMemFile.position:=FOffSet+((RecNum-1)*FRecLength)
      ELSE FDatfile.position:=FOffset+((RecNum-1)*FRecLength);
    END;
  eRecString:='';
  BufCount:=0;
  LineCharCount:=MaxRecLineLength;
  FOR wrN:=0 TO FFieldList.Count-1 DO    //Iterate through all fields
    BEGIN
      WITH TeField(FFieldList.Items[wrN]) DO
        BEGIN
          IF (FeltType in [ftToday,ftEuroToday,ftYMDToday]) THEN FFieldText:=mibDateToStr(now,FeltType);
          //Add indices
          //TODO: Add handling of Index files
          IF (false) and (TeField(FFieldList.Items[wrN]).FIndex>0) THEN
            BEGIN
              IF RecNum=NewRecord THEN n:=FNumRecords ELSE n:=RecNum;
              IF Felttype=ftCrypt THEN s:=Copy(FFieldText,1,21)
              ELSE s:=Copy(FFieldText,1,30);
              CASE Felttype OF
                ftInteger,ftFloat: s:=FormatNumberToIndex(s);
                ftDate,ftEuroDate,ftToday,ftEuroToday,ftYMDDate,ftYMDToday:  //&&
                  s:=Format('%30s',[FloatToStr(mibStrToDate(s,Felttype))]);
               ELSE
                 s:=Format('%-30s',[s]);
              END;  //case
              WriteToIndex(FIndex,n,s);
              //WriteToSortIndex ???
              //Write to indexfile
              repcounter:=0;
              ok:=True;
              REPEAT
                INC(repcounter);
                TRY
                  Seek(FIndexFile,((n-1)*FIndexCount)+TeField(FFieldList.Items[wrN]).FIndex);
                  IF Felttype=ftCrypt THEN    //&&
                    BEGIN
                      s2:=s;
                      s:=EncryptString(trim(s2),FKey);
                    END;
                  System.Write(FIndexFile,s);   //&&
                EXCEPT
                  ok:=False;
                  IF repcounter>=3 THEN
                    //IF eDlg(Format(Lang(20460),[repcounter]),     //20460=%d attempts of writing current record failed~~Retry?
                    //mtWarning,[mbYes,mbNo],0)=mrNo THEN
                    //  BEGIN
                    //    ok:=True;
                    //    repcounter:=-1;
                    //  END;
                    ok:=True;
                    raise EWriteError.Create(Lang(20462,'Current record not saved!'));  //20462=Current record not saved!
                END;  //try..except
              UNTIL ok;
              //IF repcounter=-1 THEN raise EWriteError.Create(Lang(20462));  //20462=Current record not saved!
            END;
          //Make RecString
          TempS:=FFieldText;
          IF (Felttype=ftCrypt) AND (FKey<>'') THEN TempS:=EncryptString(trim(TempS),FKey);
          IF ((Felttype=ftInteger) or (Felttype=ftFloat))
            AND (Trim(TempS)<>'') THEN
            BEGIN
              IF Felttype=ftFloat THEN
                BEGIN
                  WHILE pos(',',TempS)<>0 DO TempS[Pos(',',TempS)]:='.';
                  TempS:=FormatFloating(TempS,FLength);
                END  //if ftFloat
              ELSE
                TempS:=FormatInt(strToInt(TempS),FLength);
            END   //if ftInteger or ftFloat
          ELSE IF Felttype<>ftQuestion THEN TempS:=FormatStr(TempS,Flength);
          FOR n:=1 TO FLength DO
            BEGIN
              ABuf^[BufCount]:=TempS[n];
              DEC(LineCharCount);
              INC(BufCount);
              IF LinecharCount=0 THEN
                BEGIN
                  Move(EOLchars, ABuf^[BufCount], length(EOLChars));
                  INC(BufCount, sizeof(EOLchars));
                  LinecharCount:=MaxRecLineLength;
                END;
            END;
        END;  //with
    END;  //for wrN - iterate through fields
  IF (LineCharCount<>MaxRecLineLength)
  THEN Move(EOLchars, ABuf^[BufCount], sizeof(EOLchars));
  IF FCurRecDeleted THEN
    BEGIN
      WHILE ABuf^[BufCount]<>'!' DO Dec(BufCount);
      ABuf^[BufCount]:='?';
    END
  else if FCurRecVerified then
    begin
      WHILE ABuf^[BufCount]<>'!' DO Dec(BufCount);
      ABuf^[BufCount]:='^';
    END;
  repcounter:=0;
  REPEAT
    ok:=True;
    INC(repcounter);
    TRY
      IF StoredInMemory THEN FMemFile.WriteBuffer(ABuf^,FRecLength)
      ELSE FDatFile.WriteBuffer(ABuf^,FRecLength);
    EXCEPT
      ok:=False;
      IF repcounter>=3 THEN
        //IF eDlg(Format(Lang(20460),[repcounter]),     //20460=%d attempts of writing current record failed~~Retry?
        //mtWarning,[mbYes,mbNo],0)=mrNo THEN
        // BEGIN
        //    ok:=True;
        //    repcounter:=-1;
        //  END;
        ok:=True;
        raise EWriteError.Create(Lang(20462,'Current record not saved!'));  //20462=Current record not saved!
    END;
  UNTIL ok;
  //IF repcounter=-1 THEN raise EWriteError.Create(Lang(20462));  //20462=Current record not saved!
  FCurRecModified:=False;
  FFileModified:=True;
  IF (FIDNUMField<>-1) AND (RecNum=NewRecord) THEN INC(FCurIDNumber);
END;   //TEpiDataFile.Write


Function TEpiDataFile.GetField(Index: Integer): TeField;
BEGIN
  Result:=NIL;
  IF NOT Assigned(FFieldList) THEN Exit;
  IF (Index<0) or (Index>FFieldList.Count-1) THEN Exit;
  //IF FCurRecord<0 THEN Exit;
  Result:=TeField(FFieldList.Items[Index]);
END;

Function TEpiDataFile.GetFieldByName(Fieldname: String): TeField;
VAR
  n:Integer;
  tmpS:String;
BEGIN
  Result:=NIL;
  IF FFieldList=NIL THEN Exit;
  IF FFieldList.Count=0 THEN Exit;
  tmpS:=trim(Fieldname);
  n:=-1;
  REPEAT
    INC(n);
    IF Comparetext(tmpS,TeField(FFieldList.items[n]).FieldName)=0
    THEN Result:=TeField(FFieldList.Items[n]);
  UNTIL (n=FFieldList.Count-1) OR (Result<>NIL);
END;

Function TEpiDataFile.GetFieldNumber(Fieldname: String): Integer;
VAR
  n:Integer;
  tmpS:String;
BEGIN
  Result:=-1;
  IF FFieldList=NIL THEN Exit;
  IF FFieldList.Count=0 THEN Exit;
  tmpS:=trim(Fieldname);
  n:=-1;
  REPEAT
    INC(n);
    //IF tmpS=AnsiUpperCase(trim(PeField(df^.FieldList.Items[n])^.FName))
    IF CompareText(tmpS,TeField(FFieldList.items[n]).FieldName)=0
    THEN Result:=n;
  UNTIL (n=FFieldList.Count-1) or (Result<>-1);
END;

Function TEpiDataFile.GetDefFieldByName(Fieldname: string): TeField;
VAR
  n:Integer;
  Found: Boolean;
  s: String;
  TopDF:TEpiDataFile;
BEGIN
  Result:=NIL;
  n:=-1;
  Found:=False;
  AnsiUpperCase(trim(Fieldname));
  IF (FDefList<>NIL) AND (FDefList.Count<>0) THEN
    BEGIN
      REPEAT
        INC(n);
        IF s=AnsiUpperCase(trim(FdefList[n])) THEN Found:=True;
      UNTIL (n=FDefList.Count-1) or (Found);
      IF Found THEN Result:=TeField(FDefList.Objects[n]);
    END;
  {Search list of global vars}
  TopDF:=TEpiDataFile(FTopEpiDataFile);
  IF (result=NIL) AND (TopDF.GlobalDefList<>NIL) AND (TopDF.GlobalDefList.Count>0) THEN
    BEGIN
      n:=-1;
      Found:=False;
      REPEAT
        INC(n);
        IF s=AnsiUpperCase(trim(TopDF.GlobalDefList[n])) THEN Found:=True;
      UNTIL (n=TopDF.GlobalDefList.Count-1) OR (Found);
      IF Found THEN Result:=TeField(TopDF.GlobalDefList.Objects[n]);
    END;
END;

Function TEpiDataFile.GetDefFieldNumber(Fieldname: string): Integer;
VAR
  n:Integer;
  Found: Boolean;
  s: String;
  TopDF:TEpiDataFile;
BEGIN
  Result:=-1;
  n:=-1;
  Found:=False;
  AnsiUpperCase(trim(Fieldname));
  IF (FDefList<>NIL) AND (FDefList.Count<>0) THEN
    BEGIN
      REPEAT
        INC(n);
        IF s=AnsiUpperCase(trim(FdefList[n])) THEN Found:=True;
      UNTIL (n=FDefList.Count-1) or (Found);
      IF Found THEN Result:=n;
    END;
  {Search list of global vars}
  TopDF:=TEpiDataFile(FTopEpiDataFile);
  IF (result=-1) AND (TopDF.FGlobalDefList<>NIL) AND (TopDF.FGlobalDefList.Count>0) THEN
    BEGIN
      n:=-1;
      Found:=False;
      REPEAT
        INC(n);
        IF s=AnsiUpperCase(trim(TopDF.FGlobalDefList[n])) THEN Found:=True;
      UNTIL (n=TopDF.FGlobalDefList.Count-1) OR (Found);
      IF Found THEN Result:=n;
    END;
END;


Function TEpiDataFile.ReadFromMem(AField:TeField; RecNo:LongInt; VAR RecIsDeleted:Boolean):String;
VAR
  RecordPos:LongInt;
  CharPointer: ^CHAR;
  FieldT:PChar;
  FieldText:String;
BEGIN
  New(CharPointer);
  TRY
    Result:='';
    IF (RecNo<1) OR (RecNo>FNumRecords) THEN Exit;
    IF AField=NIL THEN Exit;
    IF (FStoredInMemory) AND (NOT Assigned(FMemFile)) THEN Exit;
    IF (NOT FStoredInMemory) AND (NOT Assigned(FDatFile)) THEN Exit;

    RecordPos:=FOffset+((RecNo-1)*FRecLength);
    IF FStoredInMemory THEN
      BEGIN
        FMemFile.Position:=RecordPos+FRecLength-3;
        FMemFile.Read(CharPointer^,1);
      END
    ELSE
      BEGIN
        FDatFile.Position:=RecordPos+FRecLength-3;
        FDatFile.Read(CharPointer^,1);
      END;
    IF CharPointer^='?' THEN RecIsDeleted:=True ELSE RecIsDeleted:=False;
    IF AField.FeltType<>ftQuestion THEN
      BEGIN
        {Read value of field}
        FieldT:=PChar(cFill(#0,AField.FLength+3));
        IF FStoredInMemory THEN
          BEGIN
            FMemFile.Position:=RecordPos+AField.FStartPos;
            FMemFile.ReadBuffer(FieldT^,AField.FLength);
          END
        ELSE
          BEGIN
            FDatFile.Position:=RecordPos+AField.FStartPos;
            FDatFile.ReadBuffer(FieldT^,AField.FLength);
          END;
        FieldText:=FieldT;
        IF Pos('!',FieldText)>0 THEN
          BEGIN
            IF FStoredInMemory THEN
              BEGIN
                FMemFile.Position:=RecordPos+AField.FStartPos;
                FMemFile.ReadBuffer(FieldT^, AField.FLength+3);
              END
            ELSE
              BEGIN
                FDatFile.Position:=RecordPos+AField.FStartPos;
                FDatFile.ReadBuffer(FieldT^, AField.FLength+3);
              END;
            FieldText:=FieldT;
            Delete(FieldText,Pos('!',FieldText),3);
          END;
        Result:=trim(FieldText);
      END;
  FINALLY
    Dispose(CharPointer);
  END;
END;


Function TEpiDataFile.LoadChecks:Boolean;
VAR
  CheckObj: TCheckObj;
  tmpChecks: TStringList;
  s:String;
BEGIN
  Result:=True;
  IF FCHKFilename='' THEN FCHKFilename:=ChangeFileExt(FRECFilename,'.chk');
  IF NOT FileExists(FCHKFilename) THEN Exit;
  Result:=False;
  TRY
    tmpChecks:=TStringList.Create;
    TRY
      tmpChecks.LoadFromFile(FCHKFilename);
    EXCEPT
      tmpChecks.Free;
      FErrorText:=Format(Lang(20130,'Error reading the checkfile %s'),[FCHKFilename]);
      FErrorCode:=EPI_CHECKFILE_ERROR;
      Exit;
    END;  //try..Except
    s:=tmpChecks.Text;
    TRY
      CheckObj:=TCheckObj.Create;
      CheckObj.ChkFileMode:=CheckFileMode;
      if assigned(FOnTranslate) then CheckObj.OnTranslate:=FOnTranslate;
      try
        Result:=CheckObj.ApplyChecks(self,s);
      except
        result:=false;
      end;
      IF (NOT Result) THEN
        BEGIN
          FErrorText:=CheckObj.ErrorList;
          FErrorCode:=EPI_CHECKFILE_ERROR;
        END
      ELSE
        BEGIN
          FErrorCode:=0;
          FErrorText:='';
        END;
    Finally
      CheckObj.Free;
    END;
  FINALLY
    tmpChecks.Free;
  END;
END;  //TEpiDataFile.LoadChecks

Function  TEpiDataFile.GetIndexFields(Index: Integer):Integer;
BEGIN
  Result:=FIndexFields[Index];
END;

Procedure TEpiDataFile.SetIndexFields(Index: Integer; Value:Integer);
BEGIN
  FIndexfields[Index]:=Value;
END;


Function TEpiDataFile.GetIndexIsUnique(Index: Integer):Boolean;
BEGIN
  Result:=FIndexIsUnique[Index];
END;

Procedure TEpiDataFile.SetIndexIsUnique(Index: Integer; Value:Boolean);
BEGIN
  FIndexIsUnique[Index]:=Value;
END;

Function TEpiDataFile.GetGlobalMissingValues(Index: Integer):Str15;
BEGIN
  Result:=FGlobalMissingValues[Index];
END;

Procedure TEpiDataFile.SetGlobalMissingValues(Index: Integer; Value:str15);
BEGIN
  FGlobalMissingValues[Index]:=Value;
END;


Function TEpiDataFile.ReadFromIndex(IndexNo,RecNo: Integer):str30;
VAR
  tmpS:str30;
  ptmpS:Array[0..30] of byte absolute tmpS;
BEGIN
  FIndex.Position:=31+( (RecNo-1)*(31*FIndexCount) ) + ( 31*(IndexNo-1) );
  FIndex.Read(ptmpS,31);
  Result:=tmpS;
END;

Function TEpiDataFile.ReadCommonIndex(RecNo: Integer):String;
VAR
  pS:Array[0..310] of Char;
  n:Integer;
BEGIN
  FillChar(pS,310,0);
  FIndex.Position:=31+((RecNo-1)*(31*FIndexCount));
  FIndex.Read(pS,31*FIndexCount);
  Result:=StrPas(pS);
  FOR n:=1 TO FIndexCount Do
    Delete(Result,((n-1)*31)+1,1);
END;  //function ReadCommonIndex


Procedure TEpiDataFile.InitSortIndex;
VAR
  n:Integer;
  pn:ARRAY[0..3] OF Byte Absolute n;
BEGIN
  {Initialize}
  FSortIndex:=TMemoryStream.Create;
  FSortIndex.SetSize(FNumRecords*4);
  FSortIndex.Position:=0;
  FOR n:=1 TO FNumRecords DO
    FSortIndex.Write(pn,4);
  DoSort(1,FNumRecords);
END;  //procedure InitSortIndex


Procedure TEpiDataFile.DoSort(L,R:Integer);
VAR
  P:String;
  I,J,n2,n3: Integer;
BEGIN
  repeat
    I := L;
    J := R;
    P := ReadCommonViaSortIndex((L+R) shr 1);
    repeat
      while ReadCommonViaSortIndex(I) < P do
        INC(I);
      while ReadCommonViaSortIndex(J) > P do
        DEC(J);
      if I <= J then
      begin
        n2:=ReadIndexNoFromSortIndex(I);
        n3:=ReadIndexNoFromSortIndex(J);
        WriteIndexNoToSortIndex(I,n3);
        WriteIndexNoToSortIndex(J,n2);
        Inc(I);
        Dec(J);
      end;
    until I > J;
    if L < J then DoSort(L, J);
    L := I;
  until I >= R;
END;  //procedure DoSort


Function TEpiDataFile.ReadIndexNoFromSortIndex(SortPos: Integer):Integer;
VAR
  n: Integer;
  pN: Array[0..3] of Byte Absolute n;
BEGIN
  FSortIndex.Position:=(SortPos-1)*4;
  FSortIndex.Read(pN,4);
  Result:=n;
END;  //function ReadIndexNoFromSortIndex


Procedure TEpiDataFile.WriteIndexNoToSortIndex(SortPos,num:Integer);
VAR
  pNum:ARRAY[0..3] of byte absolute num;
BEGIN
  FSortIndex.Position:=(SortPos-1)*4;
  FSortIndex.Write(pNum,4);
END;


Function TEpiDataFile.ReadCommonViaSortIndex(SortPos: Integer):String;
VAR
  n:Integer;
  tmpS:String;
BEGIN
  {Returns the common indexvalue pointer to by SortIndex[Posi]}
  n:=ReadIndexNoFromSortIndex(SortPos);
  tmpS:=ReadCommonIndex(n);
  Result:=tmpS;
END;  //function ReadCommonViaSortIndex


Procedure TEpiDataFile.WriteToIndex(IndexNo,RecNo: Integer; s:Str30);
VAR
  tmpS:str30;
  ptmpS:Array[0..30] of byte absolute tmpS;
BEGIN
  FIndex.Position:=31+( (RecNo-1)*(31*FIndexCount) ) + ( 31*(IndexNo-1) );
  tmpS:=s;
  FIndex.Write(ptmpS,31);
END;

Function TEpiDataFile.SearchIndex(IndexNo: Integer; SearchStr: Str30):LongInt;
VAR
  Found:Boolean;
  tmpCurRec: LongInt;
BEGIN
  Found:=False;
  tmpCurRec:=0;
  WHILE (tmpCurRec<FNumRecords) AND (NOT Found) DO
    BEGIN
      INC(tmpCurRec);
      Found:=(AnsiCompareText(SearchStr,ReadFromIndex(IndexNo,tmpCurRec))=0);
    END;
  IF Found THEN Result:=tmpCurRec ELSE Result:=-1;
END;

Function TEpiDataFile.SearchIndexFrom(IndexNo: Integer; SearchStr: str30; RecNo:Integer; direction:TDirections):LongInt;
VAR
  Found:Boolean;
  tmpCurRec,EndRec: LongInt;
BEGIN
  Found:=False;
  tmpCurRec:=RecNo;
  CASE direction OF
    dirForward,dirFirst:  BEGIN  EndRec:=FNumRecords;  DEC(tmpCurRec);   END;
    dirBackward,dirLast:  BEGIN  EndRec:=1;               INC(tmpCurRec);   END;
    dirAbsolute:          BEGIN  EndRec:=RecNo;           INC(tmpCurRec);   END;
  END;
  WHILE (tmpCurRec<>EndRec) AND (NOT Found) DO
    BEGIN
      IF (direction=dirForward) OR (direction=dirFirst) THEN INC(tmpCurRec) ELSE DEC(tmpCurRec);
      Found:=(AnsiCompareText(SearchStr,trim(ReadFromIndex(IndexNo,tmpCurRec)))=0);
    END;  //while
  IF Found THEN Result:=tmpCurRec ELSE Result:=-1;
END;


Function TEpiDataFile.IndexHasDuplicates(IndexNo:Integer):Boolean;
VAR
  FirstRec,SecondRec:Integer;
  pSecondrec,pFirstRec:Pointer;
BEGIN
  Result:=True;
  FirstRec:=0;
  WHILE (FirstRec<=FNumRecords-1) AND (Result) DO
    BEGIN
      INC(FirstRec);
      pFirstRec:=Pointer(LongInt(FIndex.Memory)+31+( (FirstRec-1)*(31*FIndexcount) ) + (31*(IndexNo-1))+1);
      SecondRec:=FirstRec;
      WHILE (SecondRec<=FNumRecords) AND (Result) DO
        BEGIN
          INC(SecondRec);
          pSecondrec:=Pointer(LongInt(FIndex.Memory)+31+( (SecondRec-1)*(31*FIndexcount) ) + (31*(IndexNo-1))+1);
          Result:=NOT CompareMem(pFirstRec,pSecondRec,30);
        END;  //while Secondrec
    END;  //while FirstRec
  IF NOT Result
  THEN FErrorText:=Format('%s'+#13+Lang(20126,'The field %s is KEY UNIQUE, but duplicate keys are found in records %d and %d'),
    [ExtractFilename(FRECFilename),TeField(FFieldList[IndexNo]).Fieldname,FirstRec,SecondRec]);
END;  //IndexHasDuplicates


Procedure TEpiDataFile.DecryptIndex;
VAR
  AField: TeField;
  n,CurRec: Integer;
  s: str30;
BEGIN
  IF (NOT FHasCrypt) OR (FKey='') THEN Exit;
  FOR n:=1 TO FIndexCount DO
    BEGIN
      AField:=TeField(FFieldList[FIndexFields[n]]);  //  PeField(df^.FieldList.Items[df^.IndexFields[n]]);
      IF AField.Felttype=ftCrypt THEN
        BEGIN
          FOR CurRec:=1 TO FNumRecords DO
            BEGIN
              s:=ReadFromIndex(n,CurRec);
              s:=DecryptString(trim(s),FKey);
              s:=Format('%-30s',[s]);
              WriteToIndex(n,CurRec,s);
            END;  //for
        END;  //if ftCrypt
    END;  //for
END;   //procedure DecryptIndex




Function TEpiDataFile.MakeIndexFile:Boolean;
VAR
  tmpS: Str30;
  s: String;
  n: Integer;
  CurRec: LongInt;
  //tmpRecFile: TextFile;
  AField: TeField;
  HasKeyUnique,ok,OldFStoredInMemory: Boolean;
BEGIN
  IF (FStoredInMemory) AND (FMemFile=NIL) THEN Exit;
  IF (NOT FStoredInMemory) AND (FDatFile=NIL) THEN Exit;
  IF FIndexFilename='' THEN FIndexFilename:=ChangeFileExt(FRECFilename,'.eix');
  TRY
    AssignFile(FIndexFile,FIndexFilename);
    Rewrite(FIndexFile);
  EXCEPT
    FErrorText:=Lang(21112,'Index file could not be created');
    FErrorCode:=epi_CREATE_FILE_ERROR;
    Result:=False;
    Exit;
  END;  //try..Except

  OldFStoredInMemory:=FStoredInMemory;
  TRY
    IF (NOT FStoredInMemory) THEN
      BEGIN
        FreeAndNil(FDatFile);
        FMemFile:=TMemoryStream.Create;
        FMemFile.LoadFromFile(FRECFilename);
        FStoredInMemory:=True;
      END;
  EXCEPT
    FErrorText:=Format(Lang(20120,'Error reading the datafile %s.'),[FRECFilename]);
    FErrorCode:=epi_READ_FILE_ERROR;
    Result:=False;
    Exit;
  END;
  tmpS:=IntToStr(FIndexCount);
  FOR n:=1 TO FIndexCount DO
    tmpS:=tmpS+'@'+IntToStr(FIndexFields[n]);
  system.Write(FIndexFile,tmpS);
  IF FNumRecords>0 THEN
    BEGIN
      FOR CurRec:=1 TO FNumRecords DO
        BEGIN
          Self.Read(CurRec);           //eReadOnlyNextRecord(df,tmpRecFile);
           FOR n:=1 TO FIndexCount DO
             BEGIN
               AField:=Fields[FIndexFields[n]];   //  PeField(df^.FieldList.Items[df^.IndexFields[n]]);
               IF AField.Felttype=ftCrypt THEN s:=EncryptString(trim(Copy(AField.FFieldText,1,21)),FKey)
               ELSE s:=trim(Copy(AField.FFieldText,1,30));
               tmpS:=s;
               CASE AField.Felttype OF
                 ftInteger,ftFloat: tmpS:=FormatNumberToIndex(tmpS); //Format('%30s',[tmpS]);
                 ftDate,ftEuroDate,ftToday,ftEuroToday,ftYMDDate,ftYMDToday:   //&&
                   tmpS:=Format('%30s',[FloatToStr(mibStrToDate(tmpS,AField.Felttype))]);
               ELSE
                 tmpS:=Format('%-30s',[tmpS]);
               END;  //case
               system.Write(FIndexFile,tmpS);
             END;  //for n
        END;  //for CurRec
    END;  //if NumRecords>0
  CloseFile(FIndexFile);
  IF OldFStoredInMemory=False THEN
    BEGIN
      FStoredInMemory:=False;
      FreeAndNil(FMemFile);
      FDatfile:=TFileStream.Create(FRECFilename,fmOpenReadWrite OR fmShareExclusive);
    END;
  Result:=True;

  //Check KEY UNIQUE
  FOR n:=1 TO FIndexCount DO
    IF FIndexIsUnique[n] THEN FHasKeyUnique:=True;
  IF FHasKeyUnique THEN
    BEGIN
      TRY
        FIndex:=TMemoryStream.Create;
        FIndex.LoadFromFile(FIndexfilename);
        n:=1;
        ok:=True;
        WHILE (n<=FIndexCount) AND (ok) DO
          BEGIN
            IF FIndexIsUnique[n] THEN ok:=IndexHasDuplicates(n);
            INC(n);
          END;
        Result:=ok;
        IF NOT Result THEN ok:=Sysutils.DeleteFile(FIndexfilename);
      FINALLY
        FIndex.Free;
        FIndex:=NIL;
      END;  //try..Except
    END;  //if HasKeyUnique
END;  //procedure MakeIndexFile



Function TEpiDataFile.ApplyIndex:Boolean;
VAR
  n: Integer;
  tmpS: Str30;
  s:String;
  ok: Boolean;
  iStr: str30;
  piStr: Array[0..30] of byte absolute iStr;
BEGIN
  Result:=True;
  IF FIndexCount=0 THEN Exit;
  FIndexFilename:=ChangeFileExt(FRECFilename,'.eix');
  IF NOT FileExists(FIndexFilename) THEN Result:=MakeIndexFile;
  IF NOT Result THEN Exit;

  TRY
    FIndex:=TMemoryStream.Create;
    FIndex.LoadFromFile(FIndexfilename);
  EXCEPT
    FErrorText:=Format(Lang(20122,'Index file %s is in use or does not exist.'),[FIndexFileName]);
    FErrorCode:=epi_OPEN_FILE_ERROR;
    Result:=False;
    Exit;
  END;  //try..Except

  IF FIndex.Size<>(FNumRecords*FIndexCount*31)+31 THEN
    BEGIN
      FIndex.Clear;
      Result:=MakeIndexFile;
      IF Result THEN
        BEGIN
          TRY
            FIndex:=TMemoryStream.Create;
            FIndex.LoadFromFile(FIndexfilename);
          EXCEPT
            Result:=False;
          END;  //try..except
        END;
      IF NOT Result THEN
        BEGIN
          FIndex.Clear;
          Exit;
        END;
    END;

  {Read header record from indexfile}
  TRY
    AssignFile(FIndexFile,FIndexFilename);
    Reset(FIndexFile);
    Seek(FIndexFile,FileSize(FIndexFile));
  EXCEPT
    FErrorText:=Format(Lang(20122,'Index file %s is in use or does not exist.'),[FIndexFileName]);
    FErrorCode:=epi_OPEN_FILE_ERROR;
    Result:=False;
    Exit;
  END;  //try..Except
  ok:=True;
  FIndex.Position:=0;
  FIndex.Read(piStr,31);
  tmpS:=iStr;

//  Read(df^.IndexFile,tmpS);
  tmpS:=tmpS+'@';
  s:=Copy(tmpS,1,Pos('@',tmpS)-1);
  IF (Length(s)=0) or (NOT IsInteger(s)) THEN ok:=False;
  IF ok THEN
    BEGIN
      IF FIndexCount<>StrToInt(s) THEN ok:=False;
      IF ok THEN
        BEGIN
          n:=0;
          REPEAT
            INC(n);
            Delete(tmpS,1,Pos('@',tmpS));
            s:=Copy(tmpS,1,Pos('@',tmpS)-1);
            IF (Length(s)=0) OR (NOT IsInteger(s)) THEN ok:=False;
            IF ok THEN IF StrToInt(s)<>FIndexFields[n] THEN ok:=False;
          UNTIL (n=FIndexCount) or (NOT ok);
        END;  //if ok
    END;  //if ok
  IF NOT ok THEN
    BEGIN
      FIndex.Clear;
      CloseFile(FIndexFile);
      Result:=MakeIndexFile;
      IF NOT Result THEN Exit
      ELSE
        BEGIN
          TRY
            FIndex:=TMemoryStream.Create;
            FIndex.LoadFromFile(FIndexFilename);
            AssignFile(FIndexFile,FIndexFilename);
            Reset(FIndexFile);
          EXCEPT
            Result:=False;
            FIndex.Clear;
            Exit;
          END;
        END;
    END;
  IF Result THEN DecryptIndex;
END;  //procedure ApplyIndex


Function TEpiDataFile.DoRebuildIndex: Boolean;
VAR
  tmpStr:String;
  tmpBool:Boolean;
BEGIN
  //Datafile must be open (with checkfile)
  FIndexFilename:=ChangeFileExt(FRECFilename,'.eix');

  IF FIndexCount=0 THEN
    BEGIN
      Result:=False;
      FErrorText:=Lang(21104,'No key fields found.')+#13#13+Lang(21102,'In order to build an index one or more fields need to have the command KEY in a checkfile.');
      FErrorCode:=epi_CREATE_FILE_ERROR;
      Exit;
    END;
  tmpBool:=MakeIndexfile;
  IF tmpBool THEN
    BEGIN
      Result:=True;
      Exit;
    END
  ELSE
    BEGIN
      FErrorText:=Format(Lang(21108,'Could not create index for %s'),[FRECFilename]);
      FErrorCode:=epi_CREATE_FILE_ERROR;
      Exit;
    END;
{
  ELSE
    BEGIN
      Screen.Cursor:=crDefault;
      ErrorMsg(Lang(21110)    //'Checkfile not found.~~In order to build an index one or more fields need to have the command KEY in a checkfile.'
        +#13#13+Lang(21102));   //'Rebuild Index terminates.')
      Exit;
    END;
}
END;  //function DoRebuildIndex


Function TEpiDataFile.GetFileSize:LongInt;
BEGIN
  Result:=-1;
  IF ((FStoredInMemory) AND (Assigned(FMemFile))) THEN Result:=FMemFile.Size;
  IF ((NOT FStoredInMemory) AND (Assigned(FDatFile))) THEN Result:=FDatFile.Size;
END;

procedure TEpiDataFile.AddField(aField: TeField);
begin
  if (NOT Assigned(FFieldList)) then FFieldList:=TList.Create;
  if (aField.Felttype=ftInteger) and (aField.FLength>9) then aField.Felttype:=ftFloat;
  if (aField.Felttype=ftQuestion) then aField.FLength:=0;
  if (aField.Felttype=ftBoolean) then aField.FLength:=1;
  if (aField.Felttype=ftCrypt) then
    begin
      aField.FCryptEntryLength:=aField.FLength;
      aField.FLength:=GetEncodedLength(AField.FCryptEntryLength);
    end;
  if (aField.Felttype=ftIDNUM) then FIDNUMField:=FFieldList.Count;
  FFieldList.Add(aField);
  IF aField.Felttype<>ftQuestion then INC(FNumDataFields);
  INC(FNumFields);
end;

function TEpiDataFile.GetCheckLines:TStringList;
begin
  FCheckWriter:=TCheckWriter.create(self);
  try
    result:=TCheckWriter(FCheckWriter).CheckLines;
  finally
    FCheckWriter.Free;
  end;
end;

function TEpiDataFile.GetQesLines: string;
VAR
  CurField,rN,Lx,Indent,PrevIndent,nc,qc:Integer;
  TempStr,tmpFieldStr,s,q,tmpFName:String;
  QES: TStringList;
  InBracket: Boolean;
  aField: TeField;
BEGIN
  TRY
    QES:=TStringList.Create;
    result:='';
    try
      if (not assigned(FFieldList)) or (FFieldList.Count=0) then exit;
      Indent:=0;
      PrevIndent:=0;
      FOR CurField:=0 TO FFieldList.Count-1 do
        BEGIN
          WITH TeField(FFieldList.Items[CurField]) DO
            BEGIN
              Indent:=0;
              IF FQuestY<>QES.Count THEN PrevIndent:=0;
              q:=FQuestion;
              tmpFName:=trim(FName);
              IF (FUpdateFieldnameInQuestion) AND (Felttype<>ftQuestion) THEN
                BEGIN
                  IF trim(FQuestion)<>'' THEN
                    BEGIN
                      IF FEpiInfoFieldNaming THEN
                        BEGIN
                          IF NOT (AnsiUpperCase(tmpFName)=AnsiUpperCase(trim(q))) THEN
                            BEGIN
                              //test if fieldname can be made from question
                              nc:=1;
                              qc:=1;
                              WHILE (nc<=Length(tmpFName)) AND (qc<Length(q)) DO
                                BEGIN
                                  IF UpCase(q[qc])=UpCase(tmpFName[nc]) THEN INC(nc);
                                  INC(qc);
                                END;  //while
                              IF nc=Length(tmpFName)+1 THEN
                                BEGIN
                                  //Fieldname can be made from question
                                  nc:=1;
                                  qc:=1;
                                  InBracket:=False;
                                  s:='';
                                  WHILE qc<=Length(q) DO
                                    BEGIN
                                      IF nc<=Length(tmpFName)+1 THEN
                                        BEGIN
                                          IF UpCase(q[qc])=UpCase(tmpFName[nc]) THEN
                                            BEGIN
                                              INC(nc);
                                              IF NOT InBracket THEN
                                                BEGIN
                                                  InBracket:=True;
                                                  s:=s+'{';
                                                  INC(Indent);
                                                END;
                                            END  //if fieldname letter found
                                          ELSE IF InBracket THEN
                                            BEGIN
                                              s:=s+'}';
                                              INC(Indent);
                                              InBracket:=False;
                                            END;
                                        END;  //if parts of fieldname is still missing
                                      s:=s+q[qc];
                                      INC(qc)
                                    END;  //while qc<=Length(q)
                                  IF InBracket THEN
                                    BEGIN
                                      s:=s+'}';
                                      INC(Indent);
                                    END;
                                  q:=s;
                                END   //if question contains fieldname
                              ELSE
                                BEGIN
                                  //question does not contain fieldname
                                  nc:=1;
                                  WHILE q[nc]=' ' DO INC(nc);
                                  Insert('{'+tmpFName+'} ',q,nc);
                                  Indent:=Length(tmpFName)+3;
                                END;
                            END;  //if question<>fname
                        END  //if EpiInfoFieldNaming
                      ELSE
                        BEGIN
                          //First word is used as fieldname
                          IF AnsiUpperCase(FirstWord(q))<>AnsiUpperCase(tmpFName) THEN
                            BEGIN
                              nc:=1;
                              WHILE q[nc]=' ' DO INC(nc);
                              Insert(tmpFName+' ',q,nc);
                              Indent:=Length(tmpFName)+1;
                            END;
                        END;
                    END  //if there is a question
                  ELSE
                    BEGIN
                      IF q='' THEN
                        BEGIN
                          q:=tmpFName+' ';
                          FQuestY:=FFieldY;
                          FQuestX:=FFieldX;
                        END
                      ELSE
                        BEGIN
                          nc:=1;
                          WHILE q[nc]=' ' DO INC(nc);
                          Insert(tmpFName+' ',q,nc);
                        END;
                      Indent:=Length(tmpFName)+1;
                    END;
                END;  //if InsertFieldname
              IF trim(q)<>'' THEN   //is there a question?
                BEGIN
                  {Get the nessary number of lines}
                  WHILE FQuestY>QES.Count DO QES.Append('');
                  Lx:=FQuestY-1;
                  tempStr:=QES[Lx];
                  {Get the nessaray number of chars in the line}
                  WHILE Length(tempStr) < FQuestX-1+Length(q)+PrevIndent DO
                    tempStr:=tempStr+' ';
                  {put FQuestion in tempStr}
                  FOR rN:=1 TO Length(q) DO
                    tempStr[FQuestX-1+rN+PrevIndent]:=q[rN];
                  QES[Lx]:=tempStr;
                END;  //if trim(FQuestion)<>''
              PrevIndent:=PrevIndent+Indent;
              IF FLength>0 THEN  //is there a field?
                BEGIN
                  WHILE FFieldY>QES.Count DO QES.Append('');
                  Lx:=FFieldY-1;
                  tmpFieldStr:='';
                  CASE FeltType of
                    ftInteger: tmpFieldStr:=cFill('#',FLength);
                    ftAlfa: tmpFieldStr:=cFill('_',FLength);
                    ftDate:BEGIN
                      CASE FLength of
                        5: tmpFieldStr:='<mm/dd>';
                        8: tmpFieldStr:='<mm/dd/yy>';
                        10: tmpFieldStr:='<mm/dd/yyyy>';
                      ELSE tmpFieldStr:='<ERROR>';
                      END;  //case FLength
                      END;  //Case FeltType of ftDate
                    ftUpperAlfa: tmpFieldStr:='<A'+cFill(' ',FLength-1)+'>';
                    ftCrypt:     tmpFieldStr:='<E'+cFill(' ',FCryptEntryLength-1)+'>';   //&&
                    ftIDNUM: tmpFieldStr:='<IDNUM'+cFill(' ',FLength-5)+'>';
                    ftBoolean: tmpFieldStr:='<Y>';
                    ftFloat: BEGIN
                      tmpFieldStr:=cFill('#',FLength-1-FNumDecimals);
                      IF FNumDecimals=0 THEN tmpFieldStr:=tmpFieldStr+'#'
                      ELSE tmpFieldStr:=tmpFieldStr+'.'+cFill('#',FNumDecimals);
                      END;   //Case FeltType of ftFloat
                    ftYMDToday: tmpFieldStr:='<TODAY-YMD>';     //&&
                    ftYMDDate:  tmpFieldStr:='<yyyy/mm/dd>';    //&&
                    ftToday: BEGIN
                      CASE FLength of
                        5: tmpFieldStr:='<TODAY>';
                        8: tmpFieldStr:='<TODAY/YY>';
                        10: tmpFieldStr:='<TODAY-MDY>';
                      END;  //Case FLength
                      END;  //Case FeltType of ftToday;
                    ftEuroDate: BEGIN
                      CASE FLength of
                        5: tmpFieldStr:='<dd/mm>';
                        8: tmpFieldStr:='<dd/mm/yy>';
                        10: tmpFieldStr:='<dd/mm/yyyy>';
                      ELSE tmpFieldStr:='<ERROR>';
                      END;  //case FLength
                      END;  //Case FeltType of ftEuroDate
                    ftEuroToday: IF FLength=10 THEN tmpFieldStr:='<today-dmy>'
                                 ELSE tmpFieldStr:='<ERROR>';
                    ftSoundex: tmpFieldStr:='<S'+cFill(' ',FLength-1)+'>';
                    ELSE  tmpFieldStr:='<ERROR>';
                  END;  //Case FeltType

                  IF true THEN
                    BEGIN
                      tempStr:=QES[Lx];
                      WHILE Length(tempStr) < FFieldX-1+Length(tmpFieldStr)+PrevIndent DO
                        tempStr:=tempStr+' ';

                      FOR rN:=1 TO Length(tmpFieldStr) DO
                        tempStr[FFieldX-1+rN+PrevIndent]:=tmpFieldStr[rN];

                      QES[Lx]:=Tempstr;

                    END;  //if legal field found
                END;  //is there a field?
            END;   //with TempField
        END;   //for CurField
      result:=QES.Text;
    finally
      qes.Free;
    end;
  EXCEPT
    raise exception.Create('Error during creation of Qes-lines');
    Result:='';
  END;  //try..except
end;

function TEpiDataFile.SaveStructureToFile(filename:string; OverwriteExisting:boolean=false):boolean;
VAR
  TempResult:Boolean;
  N,TempInt,colorN:Integer;
  ff:ByteFile;
  s:string;
  aField: TeField;
begin
  IF (NOT Assigned(FFieldList)) OR (FFieldList.Count=0) THEN
    BEGIN
      raise Exception.Create('No fields defined');
      Result:=False;
      Exit;
    END;
  if (FRECFilename='') and (filename='') then
    begin
      raise Exception.Create('No data file name defined');
      result:=false;
      exit;
    end;
  FRECFilename:=filename;
  FRECFilename:=changeFileExt(FRECFilename,'.rec');
  if (fileexists(FRECFilename)) and (OverwriteExisting=false) then
    begin
      raise Exception.Create('Data file '+FRECFilename+' already exists');
      result:=false;
      exit;
    end;
  TempResult:=True;
  AssignFile(ff,FRECFilename);
  {$I-}
  Rewrite(ff);
  TempInt:=IOResult;
  {$I+}
  IF TempInt=0 THEN
    BEGIN
      {Check if datafile contains encrypt-field}    //&&
      n:=0;
      FHasCrypt:=false;
      REPEAT
        IF  GetField(n).Felttype=ftCrypt then FHasCrypt:=true;
        INC(n);
      UNTIL (n=FFieldList.Count) or (FHasCrypt);
      IF FHasCrypt THEN
        BEGIN
          IF FKey='' THEN
            BEGIN
              s:='';
              if Assigned(FOnRequestPassword) then FOnRequestPassword(self,rpCreate,s);
              FKey:=s;
            END;  //if key already assigned
          if (FKey='') then raise Exception.Create('A password is needed for data files with encrypted fields');
        END  //if HasCrypt
      ELSE FKey:='';
      {Write No of fields + background colour + FileLabel}
      peWrite(ff,IntToStr(FFieldList.Count)+' 1');
      IF NOT FEpiInfoFieldNaming THEN peWrite(ff,' VLAB');
      IF FKey<>'' THEN peWrite(ff,' ~kq:'+EncryptString(FKey,FKey)+':kq~');
      IF trim(FFileLabel)<>'' THEN peWrite(ff,' Filelabel: '+FFilelabel);
      peWrite(ff,chr(NewLine));
      peWrite(ff,chr(LineFeed));
      FRecLength:=0;
      FOR n:=0 TO FFieldList.Count-1 DO
        BEGIN
          aField:=GetField(n);
          WITH aField DO
            BEGIN
              {write fieldchar}
              IF (FeltType=ftInteger) OR (FeltType=ftFloat) OR (FeltType=ftIDNUM)
              THEN peWrite(ff,'#') ELSE peWrite(ff,'_');
              peWrite(ff,FormatStr(FName,10));   //Name of field
              peWrite(ff,' ');                   //Space required for some unknown reason
              peWrite(ff,FormatInt(FQuestX,4));  //Question X-position
              peWrite(ff,FormatInt(FQuestY,4));  //Question Y-position
              peWrite(ff,FormatInt(30,4));       //Question colorcode
              peWrite(ff,FormatInt(FFieldX,4));  //Entry X-position
              peWrite(ff,FormatInt(FFieldY,4));  //Entry Y-position
              {Write FieldType
               0=Question without entryfield, i.e. text only
               100+Number of decimals = Floating point number
               For all other: use the fieldtype-code (FeltType)}
              IF FeltType=ftQuestion THEN peWrite(ff,FormatInt(0,4))
                ELSE IF (FeltType=ftFloat) AND (FNumDecimals>0) THEN peWrite(ff,FormatInt(100+fNumDecimals,4))
                  ELSE peWrite(ff,FormatInt(ORD(FeltType),4));
              {Write length of field - use 0 for text only}
              IF FeltType=ftQuestion THEN peWrite(ff,FormatInt(0,4))
              ELSE
                BEGIN
                  peWrite(ff,FormatInt(FLength,4));
                  FRecLength:=FRecLength+FLength;
                END;
              {write entry colorcode - special use in encrypted fields (holds entrylength of field)}
              IF FeltType<>ftCrypt THEN colorN:=112   //&&
              ELSE
                BEGIN
                  IF FCryptEntryLength<15 THEN colorN:=111+FCryptEntryLength ELSE colorN:=FCryptEntryLength;
                END;  //else
              peWrite(ff,FormatInt(colorN,4));         //Entry colorcode
              peWrite(ff,' ');                      //Another unnescessary blank
              if FOriginalQuest='' then FOriginalQuest:=FQuestion;
              peWrite(ff,FOriginalQuest);
              peWrite(ff,chr(NewLine));
              peWrite(ff,chr(LineFeed));
            END;  //with
        END;  //for n
      FOffset:=Filesize(ff);
      FCurRecModified:=False;
      FShortRecLength:=FRecLength;
      FRecLength:=FRecLength+((FRecLength DIV MaxRecLineLength)+1)*3;  //Add NewLine+LineFeed+Terminatorchar.
      FNumRecords:=0;
      FCurRecord:=NewRecord;
      FHasEOFMarker:=False;
      CloseFile(ff);
    END  //if TempInt=0
  ELSE TempResult:=False;
  Result:=TempResult;
end;  //SaveStructureToFile

procedure TEpiDataFile.SaveCheckFile;
var
  CheckLines: TStringList;
begin
  if (FRECFilename='') then raise Exception.Create('Cannot save check file: No data file name found');
  if (FCHKFilename='') then FCHKFilename:=ChangeFileExt(FRECFilename,'.chk');
  TRY
    CheckLines:=GetCheckLines;
    if CheckLines.Count>0 then
      CheckLines.SaveToFile(FCHKFilename)
    else if FileExists(FCHKFilename) and ((FileGetAttr(FCHKFilename) and SysUtils.faReadOnly) = 0) then
      DeleteFile(FCHKFilename);
  EXCEPT
    raise Exception.Create('Error saving check file');
  end;
end;

// TeField **************************************************************

Procedure TeField.ResetCheckProperties;
BEGIN
  FMustEnter:=False;
  FRepeat:=False;
  FMin:='';
  FMax:='';
  FLegal:='';
  FRangeDefined:=False;
  FCommentLegalRec:=NIL;
  FShowLegalPickList:=False;
  FPickListNoSelect:=False;
  FFieldComments:='';
  FValueLabel:='';
  FJumps:='';
  FJumpResetChar:=#0;
  FNoEnter:=False;
  FIndex:=0;
  FIsTypeStatusBar:=False;
  FTypeColor:=0;
  FTypeComments:=False;
  FTypeString:=False;
  FTypeCommentField:=-1;
  FTypeCommentFieldStr:='';
  FConfirm:=False;
  FTopOfScreen:=False;
  FTopOfScreenLines:=0;
  FTypeField:=NIL;
  //HUSK HER: Kald aftercmds.free - commandlist skal kunne frigive sine egne kommandoer
  AfterCmds:=NIL;
  BeforeCmds:=NIL;
  //*************************************************'
  FMissingValues[0]:='';
  FMissingValues[1]:='';
  FMissingValues[2]:='';
  FAutosearch:=False;
  FAutoFields:='';
  FAutoList:=False;
END;

Procedure TeField.ResetField;
BEGIN
  ResetCheckProperties;
  FName:='';
  FVariableLabel:='';
  Felttype:=ftInteger;
  FLength:=0;
  FCryptEntryLength:=0;
  FNumDecimals:=0;
  FQuestion:='';
  FOriginalQuest:='';
  LastField:=False;
  FFieldText:='';
  FStartPos:=0;
  EntryField:=NIL;
  FFieldComments:='';
  FieldN:=0;
  FTypeField:=NIL;
  OldReadOnly:=False;
  FHasGlobalMissing:=False;
  FFieldFormat:='';
END;

Constructor TeField.Create;
BEGIN
  inherited Create;
  ResetField;
END;

Destructor TeField.Destroy;
BEGIN
  Resetfield;
  inherited Destroy;
END;

Function TeField.HasCheckProperties:Boolean;
BEGIN
  IF (FMin<>'') OR (FMax<>'') OR (FLegal<>'') OR (FJumps<>'')
  OR (trim(FValueLabel)<>'') OR (FMustEnter=True) OR (FRepeat=True)
  OR (FFieldComments<>'') OR (AfterCmds<>NIL) OR (BeforeCmds<>NIL)
  OR (FNoEnter=True) OR (FIsTypeStatusBar=True) OR (FTypeComments)
  OR (FIndex>0) OR (FConfirm) OR (FTopOfScreen) OR (FAutosearch)
  OR (FMissingValues[0]<>'') OR (FMissingValues[1]<>'') OR (FMissingValues[2]<>'')
  THEN Result:=True ELSE Result:=False;
END;

Function TeField.HasSpecialChecks:Boolean;
BEGIN
  IF (FFieldComments<>'') OR (AfterCmds<>NIL) OR (BeforeCmds<>NIL)
  OR (FNoEnter=True) OR (FIsTypeStatusBar=True) OR (FTypeComments)
  OR (FIndex>0) OR (FConfirm)
  THEN Result:=True ELSE Result:=False;
END;

Function TeField.GetAsString:String;
BEGIN
  Result:=trim(FFieldText);
END;

Procedure TeField.SetAsString(Value:String);
BEGIN
  if Felttype=ftCrypt then FFieldText:=copy(Value,1,FCryptEntryLength) else FFieldText:=Copy(Value,1,FLength);
END;

Function TeField.GetAsLabel:String;
BEGIN
  IF FCommentLegalRec<>NIL
  THEN result:=trim(GetCommentLegalText(FFieldText,FCommentLegalRec))
  ELSE result:=trim(FFieldText);
END;

Function TeField.Value2Label(Value: string):string;
BEGIN
  IF FCommentLegalRec<>NIL
  THEN result:=trim(GetCommentLegalText(Value,FCommentLegalRec))
  ELSE result:='';
END;

Function TeField.GetHasValueLabels:Boolean;
BEGIN
  Result:=(FCommentLegalRec<>NIL);
END;


Function TeField.GetFieldname:String;
BEGIN
  Result:=trim(FName);
END;

Function TeField.GetMissingValues(Index: Integer):Str15;
BEGIN
  Result:=FMissingValues[Index];
END;

Procedure TeField.SetMissingValues(Index: Integer; Value:str15);
BEGIN
  FMissingValues[Index]:=Value;
END;

Procedure TeField.Clone(dest: TeField; clonevalue:boolean=false);
begin
  if (not assigned(dest)) then raise Exception.Create('Destination field is not assigned');
  dest.FName:=FName;
  dest.FVariableLabel:=FVariableLabel;
  dest.Felttype:=Felttype;
  dest.FLength:=FLength;
  dest.FCryptEntryLength:=FCryptEntryLength;
  dest.FLengthInFile:=FLengthInFile;
  dest.FNumDecimals:=FNumDecimals;
  dest.FQuestion:=FQuestion;
  dest.FOriginalQuest:=FOriginalQuest;
  dest.LastField:=LastField;
  if (clonevalue) then dest.FFieldText:=FFieldText;
  dest.FStartPos:=FStartPos;
  dest.EntryField:=EntryField;
  dest.FFieldComments:=FFieldComments;
  dest.FieldN:=FieldN;
  dest.FTypeField:=FTypeField;
  dest.OldReadOnly:=OldReadOnly;
  dest.FHasGlobalMissing:=FHasGlobalMissing;
  dest.FMustEnter:=FMustEnter;
  dest.FRepeat:=FRepeat;
  dest.FMin:=FMin;
  dest.FMax:=FMax;
  dest.FLegal:=FLegal;
  dest.FRangeDefined:=FRangeDefined;

  dest.FCommentLegalRec:=NIL;      //Mangler implementering

  dest.FShowLegalPickList:=FShowLegalPickList;
  dest.FPickListNoSelect:=FPickListNoSelect;
  dest.FFieldComments:=FFieldComments;
  dest.FValueLabel:='';   //Mangler implementering;
  dest.FJumps:=FJumps;
  dest.FJumpResetChar:=FJumpResetChar;
  dest.FNoEnter:=FNoEnter;
  dest.FIndex:=FIndex;
  dest.FIsTypeStatusBar:=FIsTypeStatusBar;
  dest.FTypeColor:=FTypeColor;
  dest.FTypeComments:=FTypeComments;
  dest.FTypeString:=FTypeString;
  dest.FTypeCommentField:=FTypeCommentField;
  dest.FTypeCommentFieldStr:=FTypeCommentFieldStr;
  dest.FConfirm:=FConfirm;
  dest.FTopOfScreen:=FTopOfScreen;
  dest.FTopOfScreenLines:=FTopOfScreenLines;
  dest.FTypeField:=FTypeField;

  dest.AfterCmds:=NIL;  //Mangler implementering
  dest.BeforeCmds:=NIL; //Mangler implementering

  dest.FMissingValues[0]:=FMissingValues[0];
  dest.FMissingValues[1]:=FMissingValues[1];
  dest.FMissingValues[2]:=FMissingValues[2];
  dest.FAutosearch:=FAutosearch;
  dest.FAutoFields:=FAutoFields;
  dest.FAutoList:=FAutoList;


  dest.FStartPos:=FStartPos;
  dest.FFieldChar:=FFieldChar;
  dest.FFieldColor:=FFieldColor;
  dest.FQuestColor:=FQuestColor;
  dest.FFieldNo:=FFieldNo;
  dest.FQuestTop:=FQuestTop;
  dest.FQuestLeft:=FQuestLeft;
  dest.FFieldTop:= FFieldTop;
  dest.FFieldLeft:=FFieldLeft;
  dest.FFieldWidth:=FFieldWidth;
  dest.FFieldX:= FFieldX;
  dest.FFieldY:= FFieldY;
  dest.FQuestX:=FQuestX;
  dest.FQuestY:=FQuestY;
  dest.FDefaultValue:=FDefaultValue;
  dest.FHasGlobalDefaultValue:=FHasGlobalDefaultValue;
end;

// ********************** TeField END***************************




end.
