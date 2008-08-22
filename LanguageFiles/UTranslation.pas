unit UTranslation;

interface

uses
  comctrls,menus,extctrls,stdctrls,buttons,windows,sysutils,classes,forms;

{$R English.res}

TYPE
TTranslator=class(TObject)
  private
    FCurLanguage: String;
    FCurLangAbbriv: string;
    FLangFilePath: String;
    FLanStr: TStringList;
    FUsesEnglish: boolean;
    FReturnOrigtext: boolean;
    procedure LoadLanguage;
    procedure SetLanguage(value: string);
    procedure SetLangFilePath(value: string);
  published
    constructor Create;
    destructor  Destroy;  override;
    procedure   TranslateForm(aForm: TForm);
    function    Translate(code:integer; origtext:string):string;
    Property    Language:string read FCurLanguage write SetLanguage;
    Property    LangFilePath:string read FLangFilePath write SetLangFilePath;
    Property    ReturnOrigtext: boolean read FReturnOrigText write FReturnOrigText;
    property    LanguageAbbriv: string read FCurLangAbbriv;
  end;

var
  OTranslator: TTranslator;

implementation

uses UcmdProcessor;
{TTranslator}

constructor TTranslator.Create;
begin
  inherited create;
  FLanStr := TStringList.Create;
  FUsesEnglish := true;
  FCurLanguage := 'English';
  FCurLangAbbriv := 'EN';
  FLangFilePath := ExtractFileDir(Application.ExeName);
  FReturnOrigText := true;
  LoadLanguage;
end;

destructor TTranslator.Destroy;
begin
  FLanStr.Free;
  inherited destroy;
end;

procedure TTranslator.LoadLanguage;
var
  s:string;
begin
  if uppercase(FCurLanguage) = 'ENGLISH' then FUsesEnglish := True;

  IF FUsesEnglish THEN
    begin
      FLanStr.Clear;
      FCurLanguage:='English';
      FCurLangAbbriv:='EN';
    end
  ELSE
    BEGIN
      s:=FLangFilePath+'\'+FCurLanguage+'.ea.lang.txt';
      TRY
        FLanStr.LoadFromFile(s);

      EXCEPT
        FUsesEnglish:=True;
        FCurLanguage:='ENGLISH';
        dm.SetOptionValue('LANGUAGE', 'ENGLISH', true);
        FLanStr.Clear;
        dm.error('Could not open languagefile %s',[s], 118001,-1);
        //Next line did not send message, therefore the former line (JL 28dec 2007)
        exception.Create(Format('Could not open languagefile %s',[s]));

        //Setlanguage('English');
      END;
      FCurLangAbbriv:=Translate(105,'en');
     {TODO MIB aug 2007: add check of languagefileversion and compare with application version}
    END;  //if not usesEnglish
end;

procedure TTranslator.SetLanguage(value: string);
begin
  FCurLanguage:=value;
  FUsesEnglish:=false;
  LoadLanguage;
end;

procedure TTranslator.SetLangFilePath(value: string);
begin
  FLangFilePath:=value;
  LoadLanguage;
end;

function TTranslator.Translate(code:integer; origtext:string):string;
VAR
  a: Array[0..255] OF Char;
BEGIN
  IF (FUsesEnglish) THEN
    BEGIN
      IF LoadString(hInstance,code,a,SizeOf(a))<>0 THEN
        BEGIN
          Result:=StrPas(a);
          WHILE Pos('~',Result)>0
          DO Result[Pos('~',Result)]:=#13;
        END
      ELSE
        begin
          if {not} FReturnOrigText then result:=origtext else Result:='**'+IntToStr(code)+'**';
        end;
    END
  ELSE
    BEGIN
      Result:=FLanStr.Values[IntToStr(code)];
      IF Result='' THEN
        begin
          if {not} FReturnOrigText then result:=origtext else Result:='**'+IntToStr(code)+'**';
        end
      ELSE
        BEGIN
          WHILE Pos('~',Result)>0
          DO Result[Pos('~',Result)]:=#13;
        END;
    END;
    result := result;
END;

Procedure TTranslator.TranslateForm(aForm: TForm);
VAR
  n,n2,aTag: Integer;
  s: String;
  a: Array[0..255] OF Char;
  AComp: TComponent;
BEGIN
  IF AForm.Tag<>0 THEN AForm.Caption:=Translate(AForm.Tag,AForm.caption);
  FOR n:=0 TO AForm.ComponentCount-1 DO
    BEGIN
      aTag:=AForm.Components[n].Tag;
      IF aTag=0 then CONTINUE;

      // Try if local text available first and use as original text.
      if LoadString(hInstance,aTag,a,SizeOf(a)) <> 0 then
        s:=Translate(aTag,StrPas(a))
      else
        s:=Translate(aTag, '');
      (* only for components with Tag <> 0 *)
      if trim(s)='' then CONTINUE;
      AComp:=AForm.Components[n];

      IF AComp is TSpeedButton THEN
        BEGIN
          //IF aTag>10000 THEN (AComp AS TSpeedButton).Hint:=s
          //ELSE (AComp AS TSpeedButton).Caption:=s;
          (AComp AS TSpeedButton).Caption:=s;
        END
      ELSE IF AComp is TLabel       THEN (AComp AS TLabel).Caption:=s
      ELSE IF AComp is TStaticText  THEN (AComp AS TStaticText).Caption:=s
      ELSE IF AComp is TPanel       THEN (AComp AS TPanel).Caption:=s
      ELSE IF AComp is TMenuItem    THEN (AComp AS TMenuItem).Caption:=s
      ELSE IF AComp is TGroupBox    THEN (AComp AS TGroupBox).Caption:=s
      ELSE IF AComp is TCheckBox    THEN (AComp AS TCheckBox).Caption:=s
      ELSE IF AComp is TRadioButton THEN (AComp AS TRadioButton).Caption:=s
      ELSE IF AComp is TButton      THEN (AComp AS TButton).Caption:=s
      ELSE IF AComp is TBitBtn      THEN (AComp AS TBitBtn).Caption:=s
      ELSE IF AComp is TPageControl THEN
        BEGIN
          //Translate all pages
          FOR n2:=0 TO (AComp AS TPageControl).PageCount-1 DO
            IF (AComp AS TPageControl).Pages[n2].Tag<>0
            THEN (AComp AS TPageControl).Pages[n2].Caption:=Translate( (AComp AS TPageControl).Pages[n2].Tag,(AComp AS TPageControl).Pages[n2].Caption );
        END
      ELSE IF AComp is TRadioGroup THEN
        BEGIN
          (AComp AS TRadiogroup).Caption:=s;
          FOR n2:=0 TO (AComp AS TRadiogroup).Items.Count-1 DO
            (AComp AS TRadiogroup).Items[n2]:=Translate(aTag+1+n2,(AComp AS TRadiogroup).Items[n2]);
        END
      ELSE IF AComp is TComboBox THEN
        BEGIN
          FOR n2:=0 TO (AComp AS TComboBox).Items.Count-1 DO
            (AComp AS TComboBox).Items[n2]:=Translate(aTag+n2,(AComp AS TComboBox).Items[n2]);
        END;
    END;  //for
END;

end.
