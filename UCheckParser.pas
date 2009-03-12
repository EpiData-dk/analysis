unit UCheckParser;

interface

uses SysUtils,windows, classes, smutils,prExpr,UCheckToken,AAPasTok,uepifile;

Type
TCkBlockType=(btField,btLabel,btConsist,btBeforeFile,btAfterFile,btBeforeRecord,btAfterRecord,btIllegalBlock);

TCheckParser=class(TSMParser)
 private
    fEpiFile: TEpiFile;
    function CheckBlockType: TCkBlockType;
    function ParseLabelBlock: boolean;
    function ParseLabelValue(const pLabName:String): boolean;
    function ParseLabelPair(blk: TLabelValueList): boolean;
    function ParseFieldBlock: boolean;
    function ParseFieldCommand(Fld: TEpiField): boolean;
    function ParseFieldLegalBlock(Fld: TEpiField): boolean;
public
 constructor Create(pEpiFile: TEpiFile);
 destructor Destroy;override;
 Function ParseFile:boolean;
 class procedure Error(Token:TSMToken;const ErrorMsg: string='Unknown Error');override;
 property EpiFile: TEpiFile read fEpiFile write fEpiFile;
end;

implementation

uses Ucmdprocessor;
{ TCheckParser }

constructor TCheckParser.Create(pEpiFile: TEpiFile);
var
 pFilename:string;
begin
  fEpiFile:=pEpiFile;
  pFileName:=ChangeFileExt(EpiFile.FileName,'.CHK');
  if not fileexists(pFilename) then abort;
  inherited CreateToken(TSMCheckTokenizer.CreateFile(pFilename),nil);
end;

function TCheckParser.ParseFile: boolean;
begin
result:=true;
while true do
begin
  NextToken;
  case Currenttoken.TokenType of
    opComment: continue;
    OpEndofFile: break;
    opEndofLine:continue;
  else
  begin
   case CheckBlockType of
      btLabel: ParseLabelBlock;
      btField: ParseFieldBlock;
     btIllegalBlock:;
   end;//case
 end//begin
 end;//case
end;//while
end;

function TCheckParser.CheckBlockType:TCkBlockType;
begin
 result:=btIllegalBlock;
 if (Currenttoken.TokenType=opIdentifier) then
 begin
 if epifile.Fields.Find(Currenttoken.Token)<> nil then
 begin
     result:=btField;
 end
 else
 if (Currenttoken.Token='LABELBLOCK')then //string block
 begin
      Accept([opEndofLine]);
      result:=btLabel;
 end;
 end;//if opidentifier
end; //function


function TCheckParser.ParseFieldBlock: boolean;
var
 Fld: TEpiField;
Begin
  result:=false;
  fld :=epifile.Fields.Find(Currenttoken.Token);
  if fld = nil then exit;
  Accept([opEndofLine]);
  NextToken;
  If Currenttoken.TokenSubType = opEnd then exit;
  While ParseFieldCommand(fld)  do;
  If Currenttoken.TokenSubType <> opend then
    Error(Currenttoken,'End expected');
  Accept([opEndofLine,opEndofFile]);
End;


function TCheckParser.ParseFieldCommand(Fld: TEpiField): boolean;
Begin
  result:=true;
  NextToken;
  case Currenttoken.TokenType of
  opEndOFFile:begin result:=false;exit;end;
  opKeyword:
  begin
     if Currenttoken.TokenSubType=opEnd then begin result:=false;exit;end;
  end;
  opIdentifier:
  begin
    if Currenttoken.TokenSubType=opEnd then begin result:=false;exit;end;
    if PrevToken.Token='COMMENT' then
    begin
       if Currenttoken.token<>'LEGAL' then error(Currenttoken,'LEGAL is required');
       ParseFieldLegalBlock(fld);
    end//'=comment'
    else if PrevToken.Token='MUSTENTER' then
       fld.MustEnter := true
    else if PrevToken.Token='NOENTER' then
       fld.ReadOnly  := true
    else if PrevToken.Token='REPEAT' then
       fld.Repeated  := true
    else if PrevToken.Token='MISSINGVALUE' then
       dm.info('Missingvalue NOT supported yet', [], 22001)
    else
      while not(NextToken.TokenType in [opEndofLine,opEndofFile]) do;
  end//opidentifier
  else
     while not(NextToken.TokenType in [opEndofLine,opEndofFile]) do;
  end;//case
end;


//         fld.ValueLabel:=TLabelValueList(EpiFile.LabelBlocks.objects[idx]);

function TCheckParser.ParseFieldLegalBlock(Fld: TEpiField): boolean;
var
 Vallst,old :TLabelValueList;
 labname :string;
 Idx : integer;
 aTok : TSMToken;
 afld :TEpiField;
 found : boolean;
Begin
   NextToken;
   If Currenttoken.TokenType = opEndofLine  then
   begin
//ugly hack, but quick
       aTok.TokenType:= opIdentifier;
       labname:='__'+fld.Fieldname;
       CurrentToken := aTok;
       ParseLabelValue(labname);
       if EpiFile.LabelBlocks.Find(labname,idx) then
       begin
           fld.ValueLabelName := Labname;
           fld.ValueLabel:=TLabelValueList(EpiFile.LabelBlocks.objects[idx]);
       end;
   end
   else
   if Currenttoken.token='USE' then
   begin
     Accept([opIdentifier],'Label/field name is required');
     labname := Currenttoken.Token;
     if EpiFile.LabelBlocks.Find(labname,idx) then
       fld.ValueLabelName := Labname
     else
      begin
        afld:=EpiFile.Fields.Find(labname);
        if aFld<> nil then
           fld.ValueLabelName := aFld.ValueLabelName
        else
           error(Currenttoken,format('%s not name or label block',[labname]))
      end;
     Accept([opEndofLine]);
   end //comment legal use
   else
       error(Currenttoken,format('filename not supported in Label block',[Currenttoken.token]))
End;


function TCheckParser.ParseLabelBlock: boolean;
Begin
  NextToken;
  If Currenttoken.TokenSubType = opEnd then exit;
  While ParseLabelValue('')  do;
  If Currenttoken.TokenSubType <> opend then
    Error(Currenttoken,'End expected');
  Accept([opEndofLine,opEndofFile]);
End;

function TCheckParser.ParseLabelValue(const pLabName:String): boolean;
var
 Vallst,old :TLabelValueList;
 labname :string;
 Idx : integer;
Begin
  result:=true;
//  NextToken;
  case Currenttoken.TokenType of
  opEndOFFile:begin result:=false;exit;end;
  opKeyword:
  begin
     if Currenttoken.TokenSubType=opEnd then begin result:=false;exit;end;
  end;
  opIdentifier:
  begin
    if Currenttoken.TokenSubType=opEnd then begin result:=false;exit;end;
    if (Currenttoken.Token='LABEL') or (pLabname<>'') then
    begin
       if (pLabname<>'') then
          labname:=pLabname
       else
       begin
         Accept([opIdentifier]);
         labname := Currenttoken.Token;
         Accept([opEndofLine]);
       end;
       Vallst :=TLabelValueList.Create;
       Vallst.LabelName:=labname;
       While ParseLabelPair(Vallst) do;
       if Epifile.LabelBlocks.find(labname,idx) then
       begin
          old:=TLabelValueList(Epifile.LabelBlocks.objects[idx]);
          old.free;
          Epifile.LabelBlocks.objects[idx]:=vallst;
       end
       else
        Epifile.LabelBlocks.AddObject(labname,vallst);
      If Currenttoken.TokenSubType <> opend then
          Error(Currenttoken,'End expected');
      Accept([opEndofLine]);
      if (pLabname='') then NextToken;
    end;//'=label'
  end//opidentifier
  else
     while not(NextToken.TokenType in [opEndofLine,opEndofFile]) do;
  end;//case
end;

function TCheckParser.ParseLabelPair(blk:TLabelValueList):boolean;
var
 aCode, alabel : string;
begin
  result:=true;
  NextToken;
  case Currenttoken.TokenType of
  opEndOFFile:begin result:=false;exit;end;
  opKeyword:
  begin
     if Currenttoken.TokenSubType=opEnd then begin result:=false;exit;end;
  end;
  opIdentifier,opString,opnumber,opPeriod:
  begin
    if Currenttoken.TokenSubType=opEnd then begin result:=false;exit;end;
    aCode := Currenttoken.token;
    Accept([opIdentifier,opString]);
    aLabel :=Currenttoken.token;
    Accept([opEndofLine]);
    blk.AddPair(aCode,aLabel);
  end//opidentifier
  else
     while not(NextToken.TokenType in [opEndofLine,opEndofFile]) do;
  end;//case
end;

destructor TCheckParser.Destroy;
begin
 inherited;
end;


class procedure TCheckParser.Error(Token: TSMToken; const ErrorMsg: string);
begin
 inherited Error(Token,'Error in check file '+ ErrorMsg);
end;

end.
