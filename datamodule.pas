unit datamodule;

{$codepage UTF-8}
{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Dialogs, epiopenfile, epidatafiles, epiexport,
  epicustombase, epidocument, outputcreator, epieximtypes, ast, ana_documentfile;

type

  TDocFileCreatedEvent = procedure(Sender: TObject; DocFile: TEpiDocumentFile) of object;

  TDMFileResult = (
    dfrSuccess,
    dfrCanceled,
    dfrError
  );

  { TaDM }

  TaDM = class(TDataModule)
    OpenDialog1: TOpenDialog;
    SaveDialog1: TSaveDialog;
    SelectDirectoryDialog1: TSelectDirectoryDialog;
  private
    FOnProgress: TEpiProgressEvent;
    FOutputCreator: TOutputCreator;
    FLastEpiCtrl: TEpiCustomControlItem;

    { private declarations }
    procedure DoError(Const Msg: UTF8String);
    procedure ImportFeedback(const Sender: TObject;
      FeedbackType: TEpiFeedBackType; const Msg: UTF8String);
    procedure AfterDocumentCreated(const Sender: TObject;
      const ADocument: TEpiDocument);
    procedure DocumentChangeHook(const Sender: TEpiCustomBase;
      const Initiator: TEpiCustomBase; EventGroup: TEpiEventGroup;
      EventType: Word; Data: Pointer);
    procedure ImportProgress(const Sender: TEpiCustomBase;
      ProgressType: TEpiProgressType; CurrentPos, MaxPos: Cardinal;
      var Canceled: Boolean);
    procedure ImportPositionItems(const Sender: TObject;
      const ControlItem: TEpiCustomControlItem; var Top, Left: Integer);
  private
    FReadCmd: TCustomStringCommand;
    procedure GetReadCommand(Sender: TAnaDocumentFile; out Cmd: TCustomStringCommand);
  public
    { public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
    function    OpenFile(ST: TCustomStringCommand; Out DocFile: TEpiDocumentFile): TDMFileResult;
    function    OpenPgmFile(var FN: UTF8String): TDMFileResult;
    function    OpenDirectory(Var Dir: UTF8String): TDMFileResult;
    property    OnProgress: TEpiProgressEvent read FOnProgress write FOnProgress;
    property    OutputCreator: TOutputCreator read FOutputCreator write FOutputCreator;

  // Events
  private
    FOnDialogFilename: TGetStrProc;
    FOnOpenFileError: TOpenEpiErrorEvent;
    FOnDocFileCreated: TDocFileCreatedEvent;
  protected
    procedure   DoDialogFilename(Const S: String);
  public
    property    OnDialogFilename: TGetStrProc read FOnDialogFilename write FOnDialogFilename;
    property    OnOpenFileError:  TOpenEpiErrorEvent read FOnOpenFileError write FOnOpenFileError;
    property    OnDocFileCreated: TDocFileCreatedEvent read FOnDocFileCreated write FOnDocFileCreated;
  end;

var
  aDM: TaDM;

implementation

uses
  LazFileUtils, LazUTF8, epiimport, epimiscutils, episervice_asynchandler,
  epiimportsettings, epiadmin, main,
  Clipbrd, LCLType;

{$R *.lfm}

{ TaDM }

procedure TaDM.DoError(const Msg: UTF8String);
begin
  raise Exception.Create(Msg);
end;

procedure TaDM.ImportFeedback(const Sender: TObject;
  FeedbackType: TEpiFeedBackType; const Msg: UTF8String);
begin
  if (not Assigned(FOutputCreator)) then exit;

  case FeedbackType of
    fbInfo:
      FOutputCreator.DoInfoAll(Msg);

    fbWarning:
      FOutputCreator.DoWarning(Msg);

    fgError:
      DoError(Msg);
  end;
end;

procedure TaDM.AfterDocumentCreated(const Sender: TObject;
  const ADocument: TEpiDocument);
begin
  EpiAsyncHandlerGlobal.AddDocument(ADocument);
  ADocument.RegisterOnChangeHook(@DocumentChangeHook, true);
end;

procedure TaDM.DocumentChangeHook(const Sender: TEpiCustomBase;
  const Initiator: TEpiCustomBase; EventGroup: TEpiEventGroup; EventType: Word;
  Data: Pointer);
begin
  if (EventGroup = eegAdmin) and
     (TEpiAdminChangeEventType(EventType) = eaceAdminInitializing)
  then
    if Assigned(TEpiDocument(Sender).Logger) then
      TEpiDocument(Sender).Logger.LogEvents := false;
end;

procedure TaDM.ImportProgress(const Sender: TEpiCustomBase;
  ProgressType: TEpiProgressType; CurrentPos, MaxPos: Cardinal;
  var Canceled: Boolean);
begin
  if Assigned(OnProgress) then
    OnProgress(Sender, ProgressType, CurrentPos, MaxPos, Canceled);
end;

procedure TaDM.ImportPositionItems(const Sender: TObject;
  const ControlItem: TEpiCustomControlItem; var Top, Left: Integer);
const
  FieldHeigth = 25;
  InterSpaceDist = 10;
var
  Pt: TPoint;
begin
  if FLastEpiCtrl = nil then
  begin
    Pt := Point(200, 5);
  end else begin
    Pt.Y := FLastEpiCtrl.Top + FieldHeigth + InterSpaceDist;
    Pt.X := FLastEpiCtrl.Left;
  end;

  FLastEpiCtrl := ControlItem;
  Top         := Pt.Y;
  Left        := Pt.X;
end;

procedure TaDM.GetReadCommand(Sender: TAnaDocumentFile; out Cmd: TCustomStringCommand
  );
begin
  Cmd := nil;
  if Assigned(FReadCmd) then
    Cmd := FReadCmd;
end;

constructor TaDM.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FOnProgress := nil;

  OpenDialog1.Filter := GetEpiDialogFilter(dfImport);
  SaveDialog1.Filter := GetEpiDialogFilter([dfDTA, dfEPX, dfEPZ, dfText])
end;

destructor TaDM.Destroy;
begin
  inherited Destroy;
end;

function TaDM.OpenFile(ST: TCustomStringCommand; out DocFile: TEpiDocumentFile
  ): TDMFileResult;
var
  I: TEpiImport;
  DF: TEpiDataFile;
  Ft: TEpiDialogFilter;
  FN, S: UTF8String;
  Setting: TEpiTextImportSetting;
  Opt: TOption;
  Str: TStream;
  ImpClipBrd: Boolean;
  OldWarning: TOpenEpiWarningEvent;
begin
  ImpClipBrd := ST.HasOption('cb');
  FReadCmd := ST;
  FN := '';

  if Assigned(ST.StringExpr) then
    FN := ExpandFileNameUTF8(ST.StringExpr.AsString);

  if (FN = '') and (not ImpClipBrd)
  then
    begin
      OpenDialog1.InitialDir := GetCurrentDirUTF8;
      OpenDialog1.Filter     := GetEpiDialogFilter(dfImport);
      OpenDialog1.FileName   := OpenDialog1.InitialDir;
      OpenDialog1.Options    := [ofNoChangeDir];
//      OpenDialog1.LastSelectionChangeFilename := '';
      if OpenDialog1.Execute then
      begin
        FN := OpenDialog1.FileName;
        DoDialogFilename(FN);
        SetCurrentDirUTF8(ExtractFilePath(FN));    // just to be sure
      end
      else
        Exit(dfrCanceled);
    end;

  DocFile := TAnaDocumentFile.Create;
  Docfile.OnAfterDocumentCreated := @AfterDocumentCreated;
  DocFile.OnError := FOnOpenFileError;
  TAnaDocumentFile(DocFile).OnGetReadCommand := @GetReadCommand;

  if (Assigned(OnDocFileCreated)) then
    OnDocFileCreated(Self, Docfile);

  if (ImpClipBrd) then
    begin
      if (not Clipboard.HasFormatName(PredefinedClipboardMimeTypes[pcfText])) then
        begin
          result := dfrError;
          DoError('The clipboard does not contain any text data!');
        end;

      Ft := dfText;
    end
  else
    if (not FilenameToFileType(FN, Ft, dfImport)) then
      begin
        Result := dfrError;
        DoError('Unknown filetype "' + ExtractFileExt(FN) + '" not supported!');
      end;

  case Ft of
    dfREC, dfDTA, dfText:
      begin
        DocFile.CreateNewDocument('');
        DF := DocFile.Document.DataFiles.NewDataFile;
        DocFile.Document.Relations.NewMasterRelation.Datafile := DF;

        I := TEpiImport.Create;
        I.OnFeedbackImport := @ImportFeedback;
        I.OnProgress := @ImportProgress;
        I.OnControlItemPosition := @ImportPositionItems;
        FLastEpiCtrl := nil;

        try
          case Ft of
            dfREC:  I.ImportRec(FN, DF);
            dfDTA:  I.ImportStata(FN, DocFile.Document, DF);

            dfText:
              begin
                if ImpClipBrd then
                  Str := TStringStream.Create(Clipboard.AsText)
                else
                  Str := TFileStream.Create(FN, fmOpenRead);

                Setting := TEpiTextImportSetting.Create;
                Setting.OutputDatafile := DF;
                Setting.ImportSteam := Str;

                if ST.Options.HasOption('d', Opt) then
                  Setting.Delimiter := Opt.Expr.AsString[1];

                if ST.Options.HasOption('q', Opt) then
                  if Opt.Expr.AsString = '' then
                    Setting.QuoteCharacter := #0
                  else
                    Setting.QuoteCharacter := Opt.Expr.AsString[1];

                if ST.HasOption('vn', Opt) then
                  if Opt.Expr.AsBoolean then
                    Setting.FirstLineIsHeader := eflTrue
                  else
                    Setting.FirstLineIsHeader := eflFalse;

                I.ImportTxt(Setting);
                Str.Free;
                Setting.Free;
              end;
          end;
        except
          Result := dfrError;
          DoError(Exception(ExceptObject).Message);
          Exit;
        end;
        TAnaDocumentFile(DocFile).ImportedFilename := FN;
      end;

    dfEPZ, dfEPX:
      begin
        if (not DocFile.OpenFile(FN)) then
          begin
            result := dfrError;
//            DocFile.OnWarning := OldWarning;
            exit;
          end;

        if DocFile.Document.Admin.Initialized and
           (not DocFile.AuthedUser.Groups.HasRights([earExport]))
        then
          begin
            S := DocFile.AuthedUser.Login;
            FreeAndNil(DocFile);
            result := dfrError;
            DoError('User "' + S + '" is not authorized to open this project for analysis!');
          end;
      end
  else
    FreeAndNil(DocFile);
    Result := dfrError;
    DoError('Filetype "' + ExtractFileExt(FN) + '" not supported!');
  end;

  if not (Assigned(DocFile.Document)) then
  begin
    FreeAndNil(DocFile);
    Exit(dfrError);
  end;

  Result := dfrSuccess;
end;

function TaDM.OpenPgmFile(var FN: UTF8String): TDMFileResult;
begin
  if (FN = '') then
  begin
    OpenDialog1.Filter := GetEpiDialogFilter([dfPGM]);

    if OpenDialog1.Execute then
    begin
      FN := OpenDialog1.FileName;
      DoDialogFilename(FN);
    end
    else
      Exit(dfrCanceled);
  end;

  result := dfrSuccess;
end;

function TaDM.OpenDirectory(Var Dir: UTF8String): TDMFileResult;
begin
  if (Dir <> '') then
    SelectDirectoryDialog1.InitialDir := Dir;

  result := dfrSuccess;

  if (SelectDirectoryDialog1.Execute) then
  begin
    Dir := SelectDirectoryDialog1.FileName;
    DoDialogFilename(Dir);
  end
  else
    result := dfrCanceled;
end;

procedure TaDM.DoDialogFilename(const S: String);
begin
  if Assigned(OnDialogFilename) then
    OnDialogFilename(S);
end;

end.

