unit UAnaToken;

interface

uses AAPasTok,sysutils,UExpToken;

const
opRead=opUserDefined		+1;
opBROWSE=opUserDefined		+2;
opDESCRIBE=opUserDefined	+3;
opIF=opUserDefined		+4;
opLET=opUserDefined		+5;
opDEFINE=opUserDefined		+6;
opSORT=opUserDefined		+7;
opSELECT=opUserDefined		+8;
opRECODE=opUserDefined		+9;
opUPDATE=opUserDefined		+10;
opRUN=opUserDefined		+11;
opFREQ=opUserDefined		+12;
opTABLES=opUserDefined		+13;
opMEANS=opUserDefined		+14;
opREGRESS=opUserDefined		+15;
opShortSTABles=opUserDefined		+16;
opSTABLES=opUserDefined		+17;
opSET=opUserDefined		+18;
opList=opUserDefined		+19;
OpQUIT=opUserDefined		+20;
opBegin=opUserDefined		+21;
opECHO=opUserDefined		+22;
opCls=opUserDefined		+23;
opCount=opUserDefined		+24;
opMemory=opUserDefined		+25;
opVariables=opUserDefined	+26;
opPie=opUserDefined		+27;
opBar=opUserDefined		+28;
opHistogram=opUserDefined	+29;
opLine=opUserDefined		+30;
opSavepgm=opUserDefined		+31;
opType=opUserDefined		+32;
opAssign=opUserDefined		+33;
opGenerate=opUserDefined	+34;
opRelate=opUserDefined		+35;
opDIR=opUserDefined		+36;
opERASE=opUserDefined		+37;
opthen=opUserDefined		+38;
opRENAME=opUserDefined		+39;
opCD=opUserDefined		+40;
opDOS=opUserDefined		+41;
opTitle=opUserDefined		+42;
opNewPage=opUserDefined		+43;
opIMMEDIATE=opUserDefined	+44;
opCorrelate=opUserDefined	+45;
opKWallis=opUserDefined		+46;
opShortTABLES=opUserDefined		+47;
opLOGOPEN=opUserDefined		+48;
opLOGCLOSE=opUserDefined	+49;
opVar=opUserDefined		+50;
opHelp=opUserDefined		+51;
opgen=opUserDefined		+52;
opDROP=opUserDefined		+53;

opResultList=opUserDefined	+54;
oploop=opUserDefined		+55;
opImif=opUserDefined		+56;
opDO=opUserDefined		+57;
opElse=opUserDefined		+58;
opEndIf=opUserDefined		+59;
opVIEW=opUserDefined		+60;
opEdit=opUserDefined		+61;
opAssert=opUserDefined		+62;
opRUNTEST=opUserDefined		+63;
opEnd=opUserDefined		+64;
opScatter=opUserDefined		+65;
opPCHART=opUserDefined		+66;
opIchart=opUserDefined		+67;
opBox=opUserDefined		+68;
opBoxPlot=opUserDefined		+69;
opRunChart=opUserDefined	+70;
opSaveData=opUserDefined	+71;
opErasePng=opUserDefined	+72;
opCOPYFILE=opUserDefined	+73;
opAppend=opUserDefined		+74;
opKeep=opUserDefined		+75;
opTemp=opUserDefined		+76;

opShortDescribe=opUserDefined	+77;
opShortHistogram=opUserDefined	+78;
opShortMeans=opUserDefined	+79;
opShortScatter=opUserDefined	+80;
opShortCorrelate=opUserDefined	+81;
opExit=opUserDefined		+82;
opShow=opUserDefined		+83;
opCLh=opUserDefined		+84;
opVersion=opUserDefined		+85;
opMkDir=opUserDefined		+86;
opAggregate=opUserDefined	+87;
opShortAggregate=opUserDefined	+88;
opLabel=opUserDefined		+89;
opLabelData=opUserDefined	+90;
opCloseHelp=opUserDefined	+91;
opPrintViewer=opUserDefined	+92;
opLabelValue=opUserDefined	+93;
opMissingValue=opUserDefined	+94;
opXChart=opUserDefined		+95;
opEval = opUserDefined 		+96;
opEpiCurve = opUserDefined 	+97;
opCDFPlot = opUserDefined 	+98;
opDotPlot = opUserDefined 	+99;
opPareto =  opUserDefined 	+100;
opTableDialog = opUserDefined + 101;
opMerge = opUserDefined + 102;
opSaveIniScreen =opUserDefined + 103;
opHelpView = opUserDefined + 104;
opCIPlot =  opUserDefined + 105;
opLifeTable =  opUserDefined + 106;
opShortLifeTable =  opUserDefined + 107;

 // last command always for token list below to go from opRead..OpClose:
opCLOSE = opUserDefined		+108;

 const
 EpiGraphicCmd=[opPie, opBar, opHistogram, opLine, opScatter, opPchart, opIchart, opBox, opBoxPlot, opPareto, opEpiCurve,
                opXchart, opCDFPlot,opdotplot, opCIplot];

 EpiClearResultCmds= [{opRead,opEXECUTE,opRETURN,opCLOSE,}opFREQ,opTABLES,
                     opMEANS,opDescribe, opREGRESS,opShortSTABLES,opSTABLES, opCorrelate{,opCount},
                     opShortMeans, opShortCorrelate, opShortDescribe,
                     opKWallis ,opShortTables, opdir, opIchart, opPchart, opRunchart,
                     opEpiCurve,opCDFPlot,opDotPlot,opTableDialog, opLifeTable, opShortLifeTable];

 EpiShowSelectCmds= [opDESCRIBE, opSORT, opFREQ, opTABLES, opMEANS, opREGRESS, opShortSTABles, opSTABLES, opList,
                    opCount, opPie, opBar, opHistogram, opLine, opGenerate, opCorrelate, opKWallis, opShortTABLES, opScatter, opPCHART,
                    opIchart, opBox, opBoxPlot, opRunChart, opSaveData, opShortDescribe, opShortHistogram, opShortMeans, opShortScatter, opShortCorrelate,
                    opAggregate, opShortAggregate, opXChart, opEpiCurve, opCDFPlot, opDotPlot, opPareto, opCIPlot, opLifeTable, opShortLifeTable];

 GlobaloptSyntax='NOECHO';
 MaxStringSize=80;
 SPC_OPTIONS =   ' B T NEGLCL EXP EXV EXZ NT ';
 GRAPH_OPTIONS = ' BW BY TEXT EDIT XLABEL N TI SUB SAVE LEGEND NOLEGEND FRAME HGRID VGRID XHIDE YHIDE' +
                 ' XINV YINV XLOG YLOG NOXTICK NOYTICK XMIN XMAX YMIN YMAX XTEXT YTEXT XLINE YLINE' +
                 ' XLINED YLINED YVALUE XINC YINC X90 X45 XA NT FN REPLACE SIZEX SIZEY NG POSYTEXT';
 SORT_OPTIONS =  ' SA SR SC SD SLA SLD SRAT SCAT SRDT SCDT SRD SCD SRA SCA ';
 STAT_OPTIONS =  ' T GAM EX CI CT O RR AR OA OBS ADV EXP ';
 PCT_OPTIONS =   ' PCT R TP C RP CP ';
 FMT_OPTIONS =   ' E0 E1 E2 E3 E4 D0 D1 D2 D3 ';
 Gnrl_Options =  ' Q W V VL VN VNL ';
 AGGR_STAT_OPTIONS =  ' MEAN MCI SD SV MEDIAN SUM MIN MAX P5 P10 P25 P50 P75 P90 P95 P99 ISR IQR IDR DES MV ';

 Type

TSMAnaTokenizer=class(TSMExpTokenizer)
  private
    procedure GetTokenClass(var aToken: TSMToken);override;
protected
   procedure ppInitKeywords;override;
public
   function GetTokenName(index:TSMTokenType): string;override;
   procedure Initialize; override;
end;

implementation
//modify the following array to add more keyword

const
TokensList: array[opRead..opClose] of String =(
{1-3}           'READ','BROWSE', 'DESCRIBE',
{4-10}          'IF', 'LET', 'DEFINE', 'SORT', 'SELECT', 'RECODE', 'UPDATE',
{11-17}         'RUN', 'FREQ', 'TABLES','MEANS', 'REGRESS', 'STAB','STATTABLES',
{18-23}         'SET', 'LIST', 'QUIT', 'BEGIN', 'ECHO', 'CLS',
{24-30}         'COUNT', 'MEMORY', 'VARIABLES', 'PIE', 'BAR', 'HISTOGRAM', 'LINE',
{31-35}         'SAVEPGM','TYPE', 'ASSIGN', 'GENERATE', 'RELATE',
{36-42}         'DIR','ERASE','THEN','RENAME','CD','DOS','TITLE',
{43-44}         'NEWPAGE','IMMEDIATE',
{45-52}         'CORRELATE','KWALLIS','TAB','LOGOPEN','LOGCLOSE','VAR','HELP','GEN',
{53-60}         'DROP','RESULT','LOOP','IMIF','DO','ELSE','ENDIF', 'VIEW',
{61-66}         'EDIT', 'ASSERT','RUNTEST', 'END', 'SCATTER','PCHART',
{67-73}         'ICHART','BOX','BOXPLOT','RUNCHART','SAVEDATA', 'ERASEPNG','COPYFILE',
{74-82}         'APPEND','KEEP','TEMP', 'DES', 'HIS', 'MEA', 'SCA', 'COR',
{83-87}         'EXIT', 'SHOW', 'CLH', 'VERSION', 'MKDIR',
{88-90}         'AGGREGATE', 'AGG', 'LABEL',
{91-95}         'LABELDATA', 'CLOSEHELP', 'PRINTVIEWER', 'LABELVALUE', 'MISSINGVALUE',
{96-100}        'XCHART', 'EVAL', 'EPICURVE', 'CDFPLOT','DOTPLOT','PARETO','TABDIALOG',
                'MERGE','SAVEINISCREEN','HELPVIEW', 'CIPLOT', 'LIFETABLE', 'LTAB',

                // last command always for token list below to go from opRead..OpClose:
                'CLOSE');

{ TSMExpTokenizer }
{
constructor TSMAnaTokenizer.Create(aInStm: TaaInCharStream);
begin
  inherited Create(aInStm);
  DequoteStrings:=true;
end;
}

function TSMAnaTokenizer.GetTokenName(index: TSMTokenType): string;
begin
  Result:=inherited GetTokenName(index);
  if result<> '' then exit;
  if (index<low(TokensList)) or (index>high(TokensList)) then exit;
    result:= TokensList[index];
end;


procedure TSMAnaTokenizer.Initialize;
begin
  inherited;
  DequoteStrings := true;
end;

procedure TSMAnaTokenizer.ppInitKeywords;
var
  i : TSMTokenType;
begin
  inherited;
  for i :=low(TokensList) to high(TokensList) do
    Keywords.Insert(TokensList[i], pointer(i));
end;

procedure TSMAnaTokenizer.GetTokenClass(var aToken: TSMToken);
var
  DummyObj : pointer;
begin
   aToken.TokenSubType:=opInvalidToken;
   aToken.TokenType:=opInvalidToken;
   aToken.TokenGroup  := opInvalidToken;
//   aToken.Token:=UpperCase(aToken.Token);
   if Keywords.Find(AnsiUppercase(aToken.Token), DummyObj) then
   begin  //word operators (e.g. and, div, shr etc) will be found here
      if isOperator(TSMTokenType(DummyObj)) then
         aToken.TokenType := TSMTokenType(DummyObj)
      else
      begin
        aToken.TokenGroup  := opKeyword;
        aToken.TokenSubType :=TSMTokenType(DummyObj);
      end;
   end;
//operators (single char or words) will be processed further here
  if isOperator(aToken.TokenType)  then
  begin
     aToken.TokenGroup :=opOperator;
     aToken.TokenSubType :=aToken.TokenType;
  end;
end;


end.
