unit UCheckProps;

interface

uses
  SysUtils,Classes,Graphics,EpiDataFile,Uepifile,CheckObjUnit,UCmdTypes, ansDataTypes;

TYPE
  {TdfChkProp tilknyttes en dataframe og anvendes til at gemme
   en række properties på dataframe/datafile-niveau, som hentes fra rec-filer og check-filer
   i forbindelse med indlæsning af en datafil.}
  TdfChkProp=class(TObject)
  private
    //Properties related to rec-file
    FFileLabel:         string;          //Label for datafile
    FPassword:          string;         //password stored in rec-file
    FEpiInfoFieldNaming:Boolean;        //Flag to indicate how fieldnames are created (true=automatic, false=first word)
    //Properties related to checkfile
    FValueLabels:         TLabelBlocksList;  //Object holding all valuelabel-sets - defined in UEpiFile
    FBeforeFileCmds:      TList;          //Commands to be run when file is opened - Listitems are of the type UCmds (in unit CheckObjUnit)
    FAfterFileCmds:       TList;          //Commands to be run when file is closed
    FBeforeRecordCmds:    TList;          //Commands to be run before current record changes
    FAfterRecordCmds:     TList;          //Commands to be run when changing current record
    FRecodeCmds:          TList;          //Commands to be run during Recode Datafile
    FAssertList:          TStringList;    //Stores commands in the Consistencyblock in the checkfile
    FConfirm:             Boolean;        //If true then fields are not left automatically when filled out
    FAutoSave:            Boolean;        //If true then user is not asked "Save record to disk?" during data entry
    FGlobalMissingValues: TMissingValues; //Holds list of MISSINGVALUE ALL - TMissingValues is defined in ansDatatypes
    FGlobalDefaultValue:  string;         //Global default value defined by DEFAULTVALUE ALL X or DEFAULTVALUE field-field, field X
    FGlobalTypeCom:       Boolean;        //Show that all fields has a Type Comment Fieldname, i.e. TYPE COMMENT ALLFIELDS [color]
    FGlobalTypeComColor:  Integer;        //Holds the color defined in a TYPE COMMENT ALLFIELDS command
    FTypeStatusBarField:  string;         //name of TYPE STATUSBAR field
    FTypeStatusBarText:   ShortString;    //Prefix text in TYPE STATUSBAR
    FTypeStatusBarColor:  TColor;         //Color to write Type StatusBar in
    FMissingAction:       TMissingActions;      //What to do with missing values - TMissingActions defined in UCmdTypes
    FMaxRow:              Integer;        //Controls the Y-position of fields during save datafile
    Procedure             InitProperties; //Initializes all of the above properties
    procedure             DeInitProperties;  //Frees and resets all properties
    Function  GetGlobalMissingValues(Index: Integer):Str15;
    Procedure SetGlobalMissingValues(Index: Integer; Value:str15);
  public
    constructor Create;
    destructor  Destroy;  override;
    procedure   Clone(destination: TdfChkProp);
    procedure   CopyToEpiFile(EpiFile: TEpiDataFile);
    procedure   CopyFromEpiFile(EpiFile: TEpiDataFile);

    Property Filelabel:string read FFilelabel write FFilelabel;
    Property Password:string read FPassword write FPassword;
    Property EpiInfoFieldNaming:Boolean read FEpiInfoFieldNaming write FEpiInfoFieldNaming;
    Property ValueLabels:TLabelBlocksList read FValueLabels write FValueLabels;
    Property BeforeFileCmds:TList read FBeforeFileCmds write FBeforeFileCmds;
    Property AfterFileCmds:TList read FAfterFileCmds write FAfterFileCmds;
    Property BeforeRecordCmds:TList read FBeforeRecordCmds write FBeforeRecordCmds;
    Property AfterRecordCmds:TList read FAfterRecordCmds write FAfterRecordCmds;
    Property RecodeCmds:TList read FRecodeCmds write FRecodeCmds;
    Property AssertList:TStringList read FAssertList write FAssertList;
    Property Confirm:Boolean read FConfirm write FConfirm;
    Property Autosave:Boolean read FAutosave write FAutosave;
    Property GlobalMissingValues[Index: integer]:str15 read GetGlobalMissingValues write SetGlobalMissingValues;
    Property GlobalDefaultValue:string read FGlobalDefaultValue write FGlobalDefaultValue;
    Property GlobalTypeCom:Boolean read FGlobalTypeCom write FGlobalTypeCom;
    Property GlobalTypeComColor:Integer read FGlobalTypeComColor write FGlobalTypeComColor;
    Property TypeStatusBarField:string read FTypeStatusBarField write FTypeStatusBarField;
    Property TypeStatusBarText: ShortString read FTypeStatusBarText write FTypeStatusBarText;
    Property TypeStatusBarColor: TColor read FTypeStatusBarColor write FTypeStatusBarColor;
    Property MissingAction: TMissingActions read FMissingAction write FMissingAction;
    Property MaxRow:Integer read FMaxRow write FMaxRow;
  end;


  {TVectorChkProp tilknyttes en vector og anvendes til at gemme
   en række properties på vector/felt-niveau, som hentes fra rec-filer og check-filer
   i forbindelse med indlæsning af en datafil.}
  TVectorChkProp=class(TObject)
  private
    // Private handle to Parenting TDfChkProp. NOTE: Could be nil if vector is
    // not part of any dataframe.
    FDfChkProp:         TdfChkProp;
    //Properties from rec-file
    FOrigFelttype:      TFelttyper;   //Original field type
    // TODO : Til Michael - Lænden på en vector skal hentes fra Vector.fFldDataSize
    FLength:            Byte;         //Length of data in field
    // TODO : Til Michael - Krypteret længde skal beregnes On-The-Fly i stedet for statisk værdi.
    FCryptEntryLength:  Byte;         //Entrylength of encrypt fields (Flength is coded length)
    FFieldChar:         Char;         //First character in the rec-files fieldrecord (# or _)
    FFieldColor:        Integer;      //Color of entry field as coded in rec-file
    FQuestColor:        Integer;      //Color of question as coded in rec-file
    FFieldX:            Integer;      //X-coordinate of entry field (in characters)
    FFieldY:            Integer;      //Y-coordinate of entry field (in characters)
    FQuestX:            Integer;      //X-coordinate of question (in characters)
    FQuestY:            Integer;      //Y-coordinate of question (in characters)
    FFieldNo:           Integer;      //The fields position in the list of fields
    {Check related properties}
    FMissingValues:     TMissingValues;    //legal missing values
    FMustEnter:         Boolean;      //True if MUSTENTER is set
    FRepeat:            Boolean;      //True if REPEAT is set
    FMin:               String;       //Mininum value (set by RANGE)
    FMax:               String;       //Maximum value (set by RANGE)
    FLegal:             String;       //Legal values (incl. RANGE values)
    FRangeDefined:      Boolean;      //True if FLegal includes a Range definition
    FValueLabelSet:     TLabelValueList;  //Valuelabelset - TLabelValueList is defined in uepifile
    FShowLegalPickList: Boolean;      //True if Comment Legal Show (forces picklist to be shown)
    FJumps:             String;       //Jumps definitions  (format: value>fieldname, value>fieldname) TODO: tilføj GetFieldFromValue-function
    FJumpResetChar:     Char;         //Fill char when JUMPS RESET "-" is used
    FAutosearch:        Boolean;      //True if field has autosearch
    FAutoFields:        String;       //CSV-string of fields to autosearch
    FAutoList:          Boolean;      //True if Autosearch has LIST parameter set
    FNoEnter:           Boolean;
    FFieldComments:     String;       //Comments in checkfile in fieldblock
    FIndex:             Byte;         //Key number = index number
    FIsTypeStatusBar:   Boolean;      //Indicates if field has TYPE STATUSBAR
    FTypeComments:      Boolean;      //Fields has TYPE COMMENT
    FTypeString:        Boolean;      //Indicates that field has a TYPE "lkjlkj" command
    FTypeCommentField:  string;       //Used with TYPE COMMENT fieldname - holds name of field to receive the comment
    FTypeColor:         TColor;       //Color of TYPE-label
    FConfirm:           Boolean;      //If true then confirm with ENTER before field is left (overrides df^.Confirm)
    FAfterCmds:          TList;        //Commands run After Entry - items are PCmds (defined in UCmdTypes)
    FBeforeCmds:         TList;        //Commands run Before Entry - items are PCmds (defined in UCmdTypes)
    FTopOfScreen:       Boolean;      //True=Move field to top of screen when entered ("New Page")
    FTopOfScreenLines:  Byte;         //Number of lines to move topofscreen field down
    FHasGlobalMissing:  Boolean;      //Set if MISSINGVALUE var1-var2 9 8 7 type command is run previously
    FDefaultValue:      string;       //A default value of the field defined by DEFAULTVALUE x
    FHasGlobalDefaultValue: Boolean;  //A default value define by DEFAULTVALUE ALL X or DEFAULTVALUE field-field, field X
    Function  GetMissingValues(Index: Integer):Str15;
    Procedure SetMissingValues(Index: Integer; Value:str15);
    procedure InitProperties;
    procedure DeInitProperties;
  public
    constructor Create;
    destructor  Destroy; override;
    procedure   Clone(destination: TVectorChkProp; SameDataframe: boolean = true);
    procedure   CopyToField(aField: TeField);
    procedure   CopyFromField(aField: TeField);
    Property    DfChkProp: TdfChkProp read FDfChkProp write FDfChkProp;
    Property    MissingValues[Index: integer]:str15 read GetMissingValues write SetMissingValues;
    Property    OrigFelttype:TFelttyper read FOrigFelttype write FOrigFelttype;
    Property    Length:byte read FLength write FLength;
    Property    CryptEntryLength:byte read FCryptEntryLength write FCryptEntryLength;
    Property    FieldChar:char read FFieldChar write FFieldChar;
    Property    FieldColor:integer read FFieldColor write FFieldColor;
    Property    QuestColor:integer read FQuestColor write FQuestColor;
    Property    FieldX:integer read FFieldX write FFieldX;
    Property    FieldY:integer read FFieldY write FFieldY;
    Property    QuestX:integer read FQuestX write FQuestX;
    Property    QuestY:integer read FQuestY write FQuestY;
    Property    MustEnter:boolean read FMustEnter write FMustEnter;
    Property    HasRepeat:boolean read FRepeat write FRepeat;
    Property    RangeMin:string read FMin write FMin;
    Property    RangeMax:string read FMax write FMax;
    Property    Legal:string read FLegal write FLegal;
    Property    RangeDefined:boolean read FRangeDefined write FRangeDefined;
    Property    ValueLabelSet:TLabelValueList read FValueLabelSet write FValueLabelSet;
    Property    ShowLegalPickList: Boolean read FShowLegalPickList write FShowLegalPickList;
    Property    Jumps:String read FJumps write FJumps;
    Property    JumpResetChar:Char read FJumpResetChar write FJumpResetChar;
    Property    Autosearch:boolean read FAutosearch write FAutosearch;
    Property    AutoFields:String read FAutoFields write FAutoFields;
    Property    AutoList:boolean read FAutoList write FAutoList;
    Property    NoEnter:boolean read FNoEnter write FNoEnter;
    Property    FieldComments:String read FFieldComments write FFieldComments;
    Property    Index:Byte read FIndex write FIndex;
    Property    IsTypeStatusBar:Boolean read FIsTypeStatusBar write FIsTypeStatusBar;
    Property    TypeComments:Boolean read FTypeComments write FTypeComments;
    Property    TypeString:Boolean read FTypeString write FTypeString;
    Property    TypeCommentField:string read FTypeCommentField write FTypeCommentField;
    Property    TypeColor:TColor read FTypeColor write FTypeColor;
    Property    Confirm:boolean read FConfirm write FConfirm;
    Property    AfterCmds:TList read FAfterCmds write FAfterCmds;
    Property    BeforeCmds:TList read FBeforeCmds write FBeforeCmds;
    Property    TopOfScreen:Boolean read FTopOfScreen write FTopOfScreen;
    Property    TopOfScreenLines: Byte read FTopOfScreenLines write FTopOfScreenLines;
    Property    HasGlobalMissing:Boolean read FHasGlobalMissing write FHasGlobalMissing;
    Property    DefaultValue:string read FDefaultValue write FDefaultValue;
    Property    HasGlobalDefaultValue: Boolean read FHasGlobalDefaultValue write FHasGlobalDefaultValue;
  end;

implementation

uses StrUtils;

procedure CloneCommandList(source,dest: TList);
var
  sourceCmd,destCmd: PCmds;
  t: integer;
begin
  dest:=NIL;
  if assigned(source) then
    begin
      for t:=0 to source.Count-1 do
        begin
          sourceCmd:=PCmds(source.Items[t]);
          new(destCmd);
          destCmd^:=sourceCmd^;
          if sourceCmd.IfCmds<>NIL then
            begin
              destCmd^.IfCmds:=TList.Create;
              CloneCommandList(sourceCmd^.IfCmds,destCmd^.IfCmds);
            end;
          if sourceCmd.ElseCmds<>NIL then
            begin
              destCmd^.ElseCmds:=TList.Create;
              CloneCommandList(sourceCmd^.ElseCmds,destCmd^.ElseCmds);
            end;
        end;  //for
    end; //if
end;



{TdfChkProp}

constructor TdfChkProp.Create;
begin
  inherited Create;
  initproperties;
end;

destructor TdfChkProp.Destroy;
begin
  DeInitProperties;
  inherited destroy;
end;

procedure TdfChkProp.initproperties;
var
  n: integer;
begin
  FFileLabel:='';
  FPassword:='';
  FEpiInfoFieldNaming:=false;
  FValueLabels:=TLabelBlocksList.Create;
  FBeforeFileCmds:=nil;
  FAfterFileCmds:=nil;
  FBeforeRecordCmds:=nil;
  FAfterRecordCmds:=nil;
  FRecodeCmds:=nil;
  FAssertList:=nil;
  FConfirm:=false;
  FAutoSave:=false;
  for n:=0 to MAXDEFINEDMISSINGVALUES do FGlobalMissingValues[n]:='';
  FGlobalDefaultValue:='';
  FGlobalTypeCom:=false;
  FGlobalTypeComColor:=0;
  FTypeStatusBarField:='';
  FTypeStatusBarText:='';
  FTypeStatusBarColor:=clBlue;
  FMissingAction:=maIgnoreMissing;
  FMaxRow:=0;
end;

procedure TdfChkProp.DeInitProperties;
var
  n:integer;
begin
  FFileLabel:='';
  FPassword:='';
  FEpiInfoFieldNaming:=false;
  FValueLabels.Free;
  FValueLabels:=nil;
  IF Assigned(FBeforeFileCmds)   THEN DisposeCommandList(FBeforeFileCmds);
  IF Assigned(FAfterFileCmds)    THEN DisposeCommandList(FAfterFileCmds);
  IF Assigned(FBeforeRecordCmds) THEN DisposeCommandList(FBeforeRecordCmds);
  IF Assigned(FAfterRecordCmds)  THEN DisposeCommandList(FAfterRecordCmds);
  IF Assigned(FRecodeCmds)       THEN DisposeCommandList(FRecodeCmds);
  IF Assigned(FAssertList)       THEN FAssertList.Free;
  FConfirm:=false;
  FAutoSave:=false;
  for n:=0 to MAXDEFINEDMISSINGVALUES do FGlobalMissingValues[n]:='';
  FGlobalDefaultValue:='';
  FGlobalTypeCom:=false;
  FGlobalTypeComColor:=0;
  FTypeStatusBarField:='';
  FTypeStatusBarText:='';
  FTypeStatusBarColor:=clBlue;
  FMissingAction:=maIgnoreMissing;
  FMaxRow:=0;
end;

Function TdfChkProp.GetGlobalMissingValues(Index: Integer):Str15;
BEGIN
  Result:=FGlobalMissingValues[Index];
END;

Procedure TdfChkProp.SetGlobalMissingValues(Index: Integer; Value:str15);
BEGIN
  FGlobalMissingValues[Index]:=Value;
END;

procedure TdfChkProp.Clone(destination: TdfChkProp);
var
  n: integer;
begin
  destination.FileLabel := FFileLabel;
  destination.Password := FPassword;
  destination.EpiInfoFieldNaming := FEpiInfoFieldNaming;
  if assigned(destination.fValueLabels) then FreeAndNil(destination.fValueLabels);
  FValueLabels.clone(destination.fValueLabels);  //  --  DET SKAL LAVES EN clone PROCEDURE I TLabelBlocksList
  if assigned(destination.BeforeFileCmds) then DisposeCommandList(destination.BeforeFileCmds);
  CloneCommandList(FBeforeFileCmds,destination.BeforeFileCmds);
  if assigned(destination.AfterFileCmds) then DisposeCommandList(destination.AfterFileCmds);
  CloneCommandList(FAfterFileCmds,destination.AfterFileCmds);
  if assigned(destination.BeforeRecordCmds) then DisposeCommandList(destination.BeforeRecordCmds);
  CloneCommandList(FBeforeRecordCmds,destination.BeforeRecordCmds);
  if assigned(destination.AfterRecordCmds) then DisposeCommandList(destination.AfterRecordCmds);
  CloneCommandList(FAfterRecordCmds,destination.AfterRecordCmds);
  if assigned(destination.RecodeCmds) then DisposeCommandList(destination.RecodeCmds);
  CloneCommandList(FRecodeCmds,destination.RecodeCmds);
  if assigned(destination.AssertList) then destination.AssertList.Free;
  destination.AssertList := nil;
  if assigned(FAssertList) then destination.AssertList.Assign(FAssertList);
  destination.Confirm := FConfirm;
  destination.AutoSave := FAutoSave;
  for n := 0 to MAXDEFINEDMISSINGVALUES do destination.GlobalMissingValues[n]:=FGlobalMissingValues[n];
  destination.GlobalDefaultValue := FGlobalDefaultValue;
  destination.GlobalTypeCom := FGlobalTypeCom;
  destination.GlobalTypeComColor := FGlobalTypeComColor;
  destination.TypeStatusBarField := FTypeStatusBarField;
  destination.TypeStatusBarText := FTypeStatusBarText;
  destination.TypeStatusBarColor := FTypeStatusBarColor;
  destination.MissingAction := FMissingAction;
  destination.MaxRow := FMaxRow;
end;

procedure TdfChkProp.CopyToEpiFile(EpiFile: TEpiDataFile);
var
  n:Integer;
  aStringList: TStringList;
begin
  aStringList:=NIL;
  EpiFile.Filelabel:=FFileLabel;
  EpiFile.password:=FPassword;
  EpiFile.EpiInfoFieldNaming:=FEpiInfoFieldNaming;
  if assigned(EpiFile.ValueLabels) then
    begin
      aStringList:=EpiFile.ValueLabels;
      EpiFile.DestroyValueLabels(aStringList);
    end;
  EpiFile.ValueLabels:=NIL;
  if Assigned(FValueLabels) then
    begin
      if assigned(aStringList) then aStringList.Clear;
      FValueLabels.CopyToValueLabelSet(aStringList);
      EpiFile.ValueLabels:=aStringList;
    end;

  if assigned(EpiFile.BeforeFileCmds) then DisposeCommandList(EpiFile.BeforeFileCmds);
  CloneCommandList(FBeforeFileCmds,EpiFile.BeforeFileCmds);
  if assigned(EpiFile.AfterFileCmds) then DisposeCommandList(EpiFile.AfterFileCmds);
  CloneCommandList(FAfterFileCmds,EpiFile.AfterFileCmds);
  if assigned(EpiFile.BeforeRecordCmds) then DisposeCommandList(EpiFile.BeforeRecordCmds);
  CloneCommandList(FBeforeRecordCmds,EpiFile.BeforeRecordCmds);
  if assigned(EpiFile.AfterRecordCmds) then DisposeCommandList(EpiFile.AfterRecordCmds);
  CloneCommandList(FAfterRecordCmds,EpiFile.AfterRecordCmds);
  if assigned(EpiFile.RecodeCmds) then DisposeCommandList(EpiFile.RecodeCmds);
  CloneCommandList(FRecodeCmds,EpiFile.RecodeCmds);
  if assigned(EpiFile.AssertList) then EpiFile.AssertList.Free;
  EpiFile.AssertList:=nil;
  if assigned(FAssertList) then EpiFile.AssertList.Assign(FAssertList);
  EpiFile.Confirm:=FConfirm;
  EpiFile.AutoSave:=FAutoSave;
  for n:=0 to MAXDEFINEDMISSINGVALUES do EpiFile.GlobalMissingValues[n]:=FGlobalMissingValues[n];
  EpiFile.GlobalDefaultValue:=FGlobalDefaultValue;
  EpiFile.GlobalTypeCom:=FGlobalTypeCom;
  EpiFile.GlobalTypeComColor:=FGlobalTypeComColor;
  //TODO : EpiFile.TypeStatusBarField:=FTypeStatusBarField;
  EpiFile.TypeStatusBarText:=FTypeStatusBarText;
  EpiFile.TypeStatusBarColor:=FTypeStatusBarColor;
  EpiFile.MissingAction:=FMissingAction;
end;

procedure TdfChkProp.CopyFromEpiFile(EpiFile: TEpiDataFile);
var
  n:Integer;
  aStringList: TStringList;
begin
  aStringList:=NIL;
  FFileLabel:=EpiFile.Filelabel;
  FPassword:=EpiFile.password;
  FEpiInfoFieldNaming:=EpiFile.EpiInfoFieldNaming;
  if assigned(FValueLabels) then FreeAndNil(FValueLabels);
  if assigned(EpiFile.ValueLabels) then
  begin
    FValueLabels:=TLabelBlocksList.Create;
    aStringList:=EpiFile.ValueLabels;
    FValueLabels.CopyFromValueLabelSet(aStringList);
  end;
  if assigned(FBeforeFileCmds) then DisposeCommandList(FBeforeFileCmds);
  CloneCommandList(EpiFile.BeforeFileCmds,FBeforeFileCmds);
  if assigned(FAfterFileCmds) then DisposeCommandList(FAfterFileCmds);
  CloneCommandList(EpiFile.AfterFileCmds,FAfterFileCmds);
  if assigned(FBeforeRecordCmds) then DisposeCommandList(FBeforeRecordCmds);
  CloneCommandList(EpiFile.BeforeRecordCmds,FBeforeRecordCmds);
  if assigned(FAfterRecordCmds) then DisposeCommandList(FAfterRecordCmds);
  CloneCommandList(EpiFile.AfterRecordCmds,FAfterRecordCmds);
  if assigned(FRecodeCmds) then DisposeCommandList(FRecodeCmds);
  CloneCommandList(EpiFile.RecodeCmds,FRecodeCmds);
  if assigned(FAssertList) then FAssertList.Free;
  FAssertList:=nil;
  if assigned(EpiFile.AssertList) then FAssertList.Assign(EpiFile.AssertList);
  FConfirm:=EpiFile.Confirm;
  FAutoSave:=EpiFile.AutoSave;
  for n:=0 to MAXDEFINEDMISSINGVALUES do FGlobalMissingValues[n]:=EpiFile.GlobalMissingValues[n];
  FGlobalDefaultValue:=EpiFile.GlobalDefaultValue;
  FGlobalTypeCom:=EpiFile.GlobalTypeCom;
  FGlobalTypeComColor:=EpiFile.GlobalTypeComColor;
  //TODO: FTypeStatusBarField:=EpiFile.TypeStatusBarField;
  FTypeStatusBarText:=EpiFile.TypeStatusBarText;
  FTypeStatusBarColor:=EpiFile.TypeStatusBarColor;
  FMissingAction:=EpiFile.MissingAction;
end;


{TVectorChkProp}

constructor TVectorChkProp.create;
begin
  inherited create;
  InitProperties;
end;

destructor TVectorChkProp.Destroy;
begin
  DeInitProperties;
  inherited destroy;
end;

procedure TVectorChkProp.InitProperties;
var
  n:integer;
begin
  FDfChkProp := nil;
  FOrigFelttype:=ftRes4;
  FLength:=0;
  FCryptEntryLength:=0;
  FFieldChar:='#';
  FFieldColor:=30;  //yellow on blue background
  FQuestColor:=31;  //white on blue background
  FFieldX:=0;
  FFieldY:=0;
  FQuestX:=0;
  FQuestY:=0;
  FFieldNo:=0;
  for n:=0 to MAXDEFINEDMISSINGVALUES do FMissingValues[n]:='';
  FMustEnter:=false;
  FRepeat:=false;
  FMin:='';
  FMax:='';
  FLegal:='';
  FRangeDefined:=false;
  FValueLabelSet:=nil;
  FShowLegalPickList:=false;
  FJumps:='';
  FJumpResetChar:=#0;
  FAutosearch:=false;
  FAutoFields:='';
  FAutoList:=false;
  FNoEnter:=false;
  FFieldComments:='';
  FIndex:=0;
  FIsTypeStatusBar:=false;
  FTypeComments:=false;
  FTypeString:=false;
  FTypeCommentField:='';
  FTypeColor:=clBlue;
  FConfirm:=false;
  FAfterCmds:=NIL;
  FBeforeCmds:=NIL;
  FTopOfScreen:=false;
  FTopOfScreenLines:=0;
  FHasGlobalMissing:=false;
  FDefaultValue:='';
  FHasGlobalDefaultValue:=false;
end;

procedure TVectorChkProp.DeInitProperties;
begin
  if Assigned(FAfterCmds) then DisposeCommandList(FAfterCmds);
  if Assigned(FBeforeCmds) then DisposeCommandList(FBeforeCmds);
  InitProperties;
end;

Function TVectorChkProp.GetMissingValues(Index: Integer):Str15;
BEGIN
  Result:=FMissingValues[Index];
END;

Procedure TVectorChkProp.SetMissingValues(Index: Integer; Value:str15);
BEGIN
  FMissingValues[Index]:=Value;
END;

Procedure TVectorChkProp.Clone(destination: TVectorChkProp; SameDataframe: boolean = true);
var
  n:integer;
begin
  destination.OrigFelttype:=FOrigFelttype;
  destination.Length:=FLength;
  destination.CryptEntryLength:=FCryptEntryLength;
  destination.FieldChar:=FFieldChar;
  destination.FieldColor:=FFieldColor;
  destination.QuestColor:=FQuestColor;
  destination.FieldX:=FFieldX;
  destination.FieldY:=FFieldY;
  destination.QuestX:=FQuestX;
  destination.QuestY:=FQuestY;
  destination.FFieldNo:=FFieldNo;
  for n:=0 to MAXDEFINEDMISSINGVALUES do destination.MissingValues[n]:=FMissingValues[n];
  destination.MustEnter:=FMustEnter;
  destination.HasRepeat:=FRepeat;
  destination.RangeMin:=FMin;
  destination.RangeMax:=FMax;
  destination.Legal:=FLegal;
  destination.RangeDefined:=FRangeDefined;
  if assigned(destination.DfChkProp) and assigned(FValueLabelSet) then
  begin
    // A Valuelabel set exists for the original vector. Three cases:
    //  1: I'm being copied to a completely new dataframe (i.e. mergeing)
    //  2:  - do -, but a Valuelab exists with same name.
    //  3: I'm being copied to a clone of the original dataframe (i.e. prepareDataframe)
    n := destination.DfChkProp.FValueLabels.IndexOf(FValueLabelSet.LabelName);
    if not SameDataframe then
    begin
      // Case 1: Just create the valuelabelset in the destination DfChkProp.
      ValueLabelSet.Clone(destination.FValueLabelSet);
      if Pos('FROM', AnsiUppercase(ValueLabelSet.LabelName)) > -1 then
      begin
        // The original value labels was read from an external file - this cannot be replicated, so
        // instead drop reference to external file and use cached value labels instead.
        // Invent new label name.
        destination.ValueLabelSet.LabelName := AnsiReplaceStr(ValueLabelSet.LabelName, 'from', '');
        destination.ValueLabelSet.LabelName := AnsiReplaceStr(destination.ValueLabelSet.LabelName, ' ', '_');
      end;
      if n <> -1 then
      repeat
        // Case 2: Labelname exists, invent a new one ;)
        destination.FValueLabelSet.LabelName := '_' + destination.FValueLabelSet.LabelName;
      until (destination.DfChkProp.FValueLabels.IndexOf(destination.FValueLabelSet.LabelName) = -1);
      destination.DfChkProp.FValueLabels.AddObject(destination.ValueLabelSet.LabelName, destination.ValueLabelSet);
    end else begin
      // Case 3: Destination DfChkProp has the ValuelabelSet. Assign pointer to it!
      destination.ValueLabelSet := TLabelValueList(destination.DfChkProp.FValueLabels.Objects[n]);
    end;
  end;
  destination.ShowLegalPickList:=FShowLegalPickList;
  destination.Jumps:=FJumps;
  destination.JumpResetChar:=FJumpResetChar;
  destination.Autosearch:=FAutosearch;
  destination.AutoFields:=FAutoFields;
  destination.AutoList:=FAutoList;
  destination.NoEnter:=FNoEnter;
  destination.FieldComments:=FFieldComments;
  destination.Index:=FIndex;
  destination.IsTypeStatusBar:=FIsTypeStatusBar;
  destination.TypeComments:=FTypeComments;
  destination.TypeString:=FTypeString;
  destination.TypeCommentField:=FTypeCommentField;
  destination.TypeColor:=FTypeColor;
  destination.Confirm:=FConfirm;
  if assigned(destination.AfterCmds) then DisposeCommandList(destination.AfterCmds);
  CloneCommandList(FAfterCmds,destination.AfterCmds);
  if assigned(destination.BeforeCmds) then DisposeCommandList(destination.BeforeCmds);
  CloneCommandList(FBeforeCmds,destination.BeforeCmds);
  destination.TopOfScreen:=FTopOfScreen;
  destination.TopOfScreenLines:=FTopOfScreenLines;
  destination.HasGlobalMissing:=FHasGlobalMissing;
  destination.DefaultValue:=FDefaultValue;
  destination.HasGlobalDefaultValue:=FHasGlobalDefaultValue;
end;

procedure TVectorChkProp.CopyToField(aField: TeField);
var
  n:integer;
begin
//  aField.FLength:=FLength;
  aField.FCryptEntryLength:=FCryptEntryLength;
  aField.FFieldChar:=FFieldChar;
  aField.FFieldColor:=FFieldColor;
  aField.FQuestColor:=FQuestColor;
  aField.FFieldX:=FFieldX;
  aField.FFieldY:=FFieldY;
  aField.FQuestX:=FQuestX;
  aField.FQuestY:=FQuestY;
  aField.FFieldNo:=FFieldNo;
  for n:=0 to MAXDEFINEDMISSINGVALUES do aField.MissingValues[n]:=FMissingValues[n];
  aField.FMustEnter:=FMustEnter;
  aField.FRepeat:=FRepeat;
  aField.FMin:=FMin;
  aField.FMax:=FMax;
  aField.FLegal:=FLegal;
  aField.FRangeDefined:=FRangeDefined;
  if Assigned(FValueLabelSet) then
    aField.FValueLabel := FValueLabelSet.LabelName;
  aField.FCommentLegalRec := NIL;
  aField.FShowLegalPickList:=FShowLegalPickList;
  aField.FJumps:=FJumps;
  aField.FJumpResetChar:=FJumpResetChar;
  aField.FAutosearch:=FAutosearch;
  aField.FAutoFields:=FAutoFields;
  aField.FAutoList:=FAutoList;
  aField.FNoEnter:=FNoEnter;
  aField.FFieldComments:=FFieldComments;
  aField.FIndex:=FIndex;
  aField.FIsTypeStatusBar:=FIsTypeStatusBar;
  aField.FTypeComments:=FTypeComments;
  aField.FTypeString:=FTypeString;
  aField.FTypeCommentFieldStr:=FTypeCommentField;
  aField.FTypeColor:=FTypeColor;
  aField.FConfirm:=FConfirm;
  if assigned(aField.AfterCmds) then DisposeCommandList(aField.AfterCmds);
  CloneCommandList(FAfterCmds,aField.AfterCmds);
  if assigned(aField.BeforeCmds) then DisposeCommandList(aField.BeforeCmds);
  CloneCommandList(FBeforeCmds,aField.BeforeCmds);
  aField.FTopOfScreen:=FTopOfScreen;
  aField.FTopOfScreenLines:=FTopOfScreenLines;
  aField.FHasGlobalMissing:=FHasGlobalMissing;
  aField.FDefaultValue:=FDefaultValue;
  aField.FHasGlobalDefaultValue:=FHasGlobalDefaultValue;
end;

procedure TVectorChkProp.CopyFromField(aField: TeField);
var
  n:integer;
begin
  FOrigFelttype:=aField.Felttype;
  FLength:=aField.FLength;
  FCryptEntryLength:=aField.FCryptEntryLength;
  FFieldChar:=aField.FFieldChar;
  FFieldColor:=aField.FFieldColor;
  FQuestColor:=aField.FQuestColor;
  FFieldX:=aField.FFieldX;
  FFieldY:=aField.FFieldY;
  FQuestX:=aField.FQuestX;
  FQuestY:=aField.FQuestY;
  FFieldNo:=aField.FFieldNo;
  for n:=0 to MAXDEFINEDMISSINGVALUES do FMissingValues[n]:=aField.MissingValues[n];
  FMustEnter:=aField.FMustEnter;
  FRepeat:=aField.FRepeat;
  FMin:=aField.FMin;
  FMax:=aField.FMax;
  FLegal:=aField.FLegal;
  FRangeDefined:=aField.FRangeDefined;
  //TODO: handling valuelabelsets if assigned(aField.FCommentLegalRec.ValueLabelSet) then destination.ValueLabelSet.Free;
  //FCommentLegalRec:=NIL;   //aField.FCommentLegalRec:=NIL;  //.ValueLabelSet.Assign(FValueLabelSet);
  FValueLabelSet:=NIL;
  FShowLegalPickList:=aField.FShowLegalPickList;
  FJumps:=aField.FJumps;
  FJumpResetChar:=aField.FJumpResetChar;
  FAutosearch:=aField.FAutosearch;
  FAutoFields:=aField.FAutoFields;
  FAutoList:=aField.FAutoList;
  FNoEnter:=aField.FNoEnter;
  FFieldComments:=aField.FFieldComments;
  FIndex:=aField.FIndex;
  FIsTypeStatusBar:=aField.FIsTypeStatusBar;
  FTypeComments:=aField.FTypeComments;
  FTypeString:=aField.FTypeString;
  FTypeCommentField:=aField.FTypeCommentFieldStr;
  FTypeColor:=aField.FTypeColor;
  FConfirm:=aField.FConfirm;
  if assigned(FAfterCmds) then DisposeCommandList(FAfterCmds);
  CloneCommandList(aField.AfterCmds,FAfterCmds);
  if assigned(FBeforeCmds) then DisposeCommandList(FBeforeCmds);
  CloneCommandList(aField.BeforeCmds,FBeforeCmds);
  FTopOfScreen:=aField.FTopOfScreen;
  FTopOfScreenLines:=aField.FTopOfScreenLines;
  FHasGlobalMissing:=aField.FHasGlobalMissing;
  FDefaultValue:=aField.FDefaultValue;
  FHasGlobalDefaultValue:=aField.FHasGlobalDefaultValue;
end;

end.
