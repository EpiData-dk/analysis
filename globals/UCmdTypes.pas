unit UCmdTypes;

interface

USES
  SysUtils,Classes,Dialogs,Graphics;

TYPE
  str10=String[10];
  str15=String[15];
  str30=String[30];


CONST
MaxIndices=20;
NumChars:        Set of CHAR=['0'..'9'];
AlfaNumChars:    Set of CHAR=['0'..'9','A'..'Z','a'..'z'];
AlfaChars:       Set of CHAR=['A'..'Z','a'..'z'];
IntegerChars:    Set of CHAR=['0'..'9','-','+'];
FloatChars:      Set of CHAR=['0'..'9', '.', ',', '-', '+'];
DateChars:       Set of CHAR=['0'..'9','/'];
//BooleanChars:    Set of CHAR=['y','Y','n','N','1','0'];
BooleanChars:    Set of CHAR=['n','N','y','Y','0','1'];
BooleanYesChars: Set of CHAR=['y','Y','1'];
BooleanNoChars:  Set of CHAR=['n','N','0'];
DaysInMonth:     ARRAY[1..12] OF BYTE = (31,29,31,30,31,30,31,31,30,31,30,31);
TerminatorChars: Set of CHAR=['!','?','^'];
NewLine:Byte=13;
LineFeed:Byte=10;
MinNumber:Double=5E-325;
MaxNumber:Double=1.7E308;
MAXDEFINEDMISSINGVALUES = 2;



ColorNames:ARRAY[0..17] OF str10 = ('AQUA','BLACK','BLUE','DKGRAY','FUCHSIA','GRAY',
  'GREEN','LIME','LTGRAY','MAROON','NAVY','OLIVE','PURPLE','RED','SILVER','TEAL','WHITE','YELLOW');
ColorValues:ARRAY[0..17] OF TColor = (clAqua, clBlack, clBlue, clDkGray, clFuchsia,
  clGray, clGreen, clLime, clLtGray, clMaroon, clNavy, clOlive,
  clPurple, clRed, clSilver, clTeal, clWhite, clYellow);
TextColors:array[0..15] of TColor = (clBlack,clNavy,clGreen,clTeal,
                        clMaroon,clPurple,clOlive,clSilver,clGray,
                        clBlue,clLime,clAqua,clRed,clFuchsia,clYellow,clWhite);
BgColors:array[0..7] of TColor = (clBlack,clNavy,clGreen,clTeal,clMaroon,clPurple,clOlive,clSilver);


TYPE
  str45=string[45];
    ByteFile=File of Byte;
  TFelttyper = (ftInteger,ftAlfa,ftDate,ftUpperAlfa,ftCheckBox,  
                ftBoolean,ftFloat,ftPhoneNum,ftTime,ftLocalNum,
                ftToday,ftEuroDate,ftIDNUM,ftRes4,ftRes5,
                ftQuestion,ftEuroToday,ftSoundex,ftCrypt,ftYMDDate,ftYMDToday);    //&&
  TMissingValues = array[0..MAXDEFINEDMISSINGVALUES] of string;


CONST
  SupportedFieldTypes: SET OF TFelttyper = [ftInteger,ftAlfa,ftDate,ftUpperAlfa,
                          ftBoolean,ftFloat,ftToday,ftEuroDate,ftIDNUM,ftQuestion,
                          ftEuroToday,ftSoundex,ftCrypt,ftYMDDate,ftYMDToday];
  DateFieldTypes: SET OF TFelttyper = [ftDate,ftToday,ftEuroDate,ftEuroToday,ftYMDDate,ftYMDToday];

TYPE
  PLabelRec=^TLabelRec;
  TLabelRec=Record
      Value:String[30];
      Text:String[80];
      Next:Pointer;
    END;
  PRelateInfo=^TRelateInfo;
  TRelateInfo=Record
      RelFileNo:    Integer;
      RelFieldNo:   Integer;
      CmdInFieldNo: Integer;
      One2One:      Boolean;
      Next:         Pointer;
    END;
  TRelActions=(raNothing,raOpenFileInEditor,raOpenTwoFilesInEditor);
  TOpenRelAction=Record
    Action: TRelActions;
    File1:  TFilename;
    File2:  TFilename;
    end;
  TExportTypes=(etTxt,etDBase,etXLS,etStata,etRecToQes,etListData,etSPSS,etSAS,etEpiData,etCodebook,etASP);
  TBeepTypes=(btWarning,btConfirmation,btStandard);
  TMissingActions=(maIgnoreMissing,maRejectMissing);
  TIndexFields=Array[1..MaxIndices] OF Integer;
  TIndexIsUnique=Array[1..MaxIndices] OF Boolean;
  TIndexFile=File of str30;
  TScopes=(scLocal,scGlobal,scCumulative);
  TDirections=(dirForward,dirBackward,dirFirst,dirLast,dirAbsolute);
  TLastSelectFilestype=(sfNone,sfMakeDatafile,sfRevise,sfAssert,sfRecode,sfRec2Qes,sfValDup,
                        sfImportStata,sfMerge);

  //Types related to searching datafiles
  TSearchStyle=(ssEquals,ssBeginsWith,ssContains);
  TScopeStyle=(ssForward,ssBackWard,ssAll);

  TFindOperators = (opNone,opEq,opNEq,opGT,opLT,opBW,opEW,opCON);

  TCrites=Record
    Fieldno: Integer;
    Opr: TFindOperators;
    SearchText: String;
    SearchValue: Double;
  END;



  Commands=(cmdIF,cmdHelp,cmdHide,cmdUnhide,cmdClear,cmdGoTo,cmdComLegal,
            cmdExit,cmdDefine,cmdAutosave,cmdConfirm,cmdTypeString,
            cmdRelate,cmdIgnoreMissing,cmdWriteNote,cmdBackup,cmdBeep,cmdLoad,cmdExecute,
            cmdColor,cmdMissingAll,cmdQuit,cmdCopyToClipboard,cmdShowLastRecord,cmdDefaultAll,cmdLet,cmdComment,cmdLeaveField);       //¤¤
            //NB! Insert new codes BEFORE cmdLet
                                                                                                             //cmdLeaveField is only used internally (in conn. with relate)

CONST
  CommandNames:Array[Commands] of String[16]=
    ('IF','HELP','HIDE','UNHIDE','CLEAR','GOTO','COMMENT','EXIT','DEFINE',
    'AUTOSAVE','CONFIRM','TYPE','RELATE','IGNOREMISSING','WRITENOTE','BACKUP',
    'BEEP','LOAD','EXECUTE','COLOR','MISSINGVALUE','QUIT','COPYTOCLIPBOARD',
    'SHOWLASTRECORD','DEFAULTVALUE',
    'LET','dummy','leavefield');                                            //¤¤


TYPE
  TChangeFieldActions=(cfNext,cfPrev,cfFirst,cfLast,cfValidate);
  TLeaveStyles=(lsEnter,lsBrowse,lsJumpFirst,lsJumpLast,lsChangeRec,lsNone);
  PCmds=^TCmds;
  TCmds=RECORD
          Next: PCmds;
          CASE Command: Commands OF
            cmdIF:
               (IfExpr:          String[200];
                IfShowExpr:      String[200];
                IfCmds:          TList;
                ElseCmds:        TList);
            cmdHelp:
               (HelpString:      String[250];
                HelpType:        TMsgDlgType;
                HelpKeys:        String[10]);
            cmdHide,cmdUnHide,cmdClear,cmdGoto:
               (HideVarNumber:   Integer;
                HideVarName:     String[10]);
            cmdComLegal:
               (clVarNumber:     Integer;
                ValueLabel:      String[40];
                CommentLegalRec: PLabelRec;
                ShowList:        Boolean);
            cmdTypeString:
               (tsVarNumber:     Integer;
                TypeText:        String[40];
                Typecolor:       TColor);
            cmdRelate:
               (RelField:        String[10];
                RelFileNo:       Integer;
                RelFileStr:      String[200];
                One2One:         Boolean);
            cmdLet:
               (VarName:         String[20];
                VarNumber:       Integer;
                VarIsField:      Boolean;
                CodedWithLET:    Boolean;
                LetExpr:         String[200]);
            cmdComment:
               (Comment:         String[200]);
            cmdDefine:
               (FName:           String[20];
                FeltType:        TFelttyper;
                FLength:         Integer;
                FNumDecimals:    Byte;
                FScope:          TScopes);
            cmdWriteNote:
               (FNote:           String[200];
                ShowNotes:       Boolean);
            cmdCopyToClipboard:
               (CopyStr:         String[200]);
            cmdBackup:
               (DestLib:         String[200];
                zipit:           Boolean;
                encryptit:       Boolean;
                filename:        String[200];
                pw:              string[30];
                dateit:          Boolean);
            cmdBeep:
               (BeepType:        TBeepTypes);
            cmdLoad:
               (DLLName:         String[200]);
            cmdExecute:
               (ExecCmdLine:     String[255];
                ExecParams:      String[255];
                ExecHide:        Boolean;
                ExecWait:        Boolean);
            cmdLeaveField:
               (cLeaveStyle:     TLeaveStyles;
                IsLastField:     Boolean);
            cmdColor:
               (ColorCmd:        Byte;      //1=color question, 2=color data,  3=color background, 4=color fieldname
                TxtColor:        Byte;
                BgColor:         Byte;
                IsEpiInfoNo:     Boolean;
                CFieldno:        Byte);
          END;

  PAssert=^TAssert;
  TAssert=RECORD
      AssName: String[40];
      AssExpr: String[200];
      OrigExpr: String[200];
      ViolCount: Integer;
      Violaters: String;
    END;

  PDefVar=^TDefVar;
  TDefVar=RECORD
    FName:             String[16];
    Felttype:          TFelttyper;
    FLength:           Integer;
    FNumDecimals:      Byte;
    FFieldText:        String;
    FScope:            TScopes;
    END;


Procedure DisposeCommandList(AList:TList);
Procedure DisposeLabelRec(VAR ALabel:PLabelRec);
PROCEDURE DestroyFieldList(VAR AList:TList);

Implementation

Procedure DisposeCommandList(AList:TList);
VAR
  n:Integer;
  tmpCmdRec:TCmds;  
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
  FreeAndNil(Alist);
END;  //procedure DisposeCommandList

Procedure DisposeLabelRec(VAR ALabel:PLabelRec);
VAR
  NextLabelRec:PLabelRec;
BEGIN
  WHILE ALabel<>NIL DO
    BEGIN
      NextLabelRec:=ALabel^.Next;
      Dispose(ALabel);
      ALabel:=NextLabelRec;
    END;
END;

PROCEDURE DestroyFieldList(VAR AList:TList);
VAR
  n:integer;
BEGIN
  FOR n:=0 TO AList.Count-1 DO
    Dispose(AList.Items[n]);
  AList.Free;
  AList:=NIL;
END;  //DestroyFieldList


end.
