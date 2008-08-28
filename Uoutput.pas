unit Uoutput;

interface

uses sysutils, windows, classes,Graphics, ansDataTypes,UstringConst, CStrings, dialogs, uabout;


resourcestring
  SListIndexError = 'List index out of bounds (%d)';

const
  cFiller=#160;
  Tab =#9;
  CRLF = #13#10;
  CloseHTML='</BODY></HTML>';
  objTable=1;
  objPara=2;
  MAXSTREAMSIZE= 1024 * 60;

type
TCVAlignment = (caTop,caMiddle,caBottom);
THAlignment = (caLeftJustify,caCenter,caRightJustify);

TTableOptions=class(Tpersistent)
public
   FBorder : Integer;
   FCellPadding : Integer;
   FHeader : string;
   FTableAlignment : TAlignment;
   FCellVAlignment : TCVAlignment;
   FCellAlignment : THAlignment;
   FHeaderAlignment : THAlignment;
   FTableString : String;
   FWrap : Boolean;
   FRowColor : TColor;
   constructor Create;
   procedure SetToDefault;
//   procedure assign;
end;

TParaOptions=class(Tpersistent)
public
   FBorder : Integer;
   FAlignment : THAlignment;
   FWrap : Boolean;
   FFontColor : TColor;
   fOClass : string;
   constructor Create;
   procedure SetToDefault;
end;

TStatOutputObj=class;
TStatTable=class;
TStatPara=class;

TStatOutputList=class(Tpersistent)
private
// fObjList : TList;
 fobjcount: integer;
    fParaOptions: TParaOptions;
    function GetAnObject(index: integer): TStatOutputObj;
    function Getobjcount: integer;
    procedure DestroyCols;
public
 constructor Create;
 destructor Destroy;override;
 function NewTable(cols:integer;rows:integer=-1):TStatTable;
 function NewPara(const pText:string=''):TStatPara;
 procedure InternalInsertObj(index: integer; obj: TStatOutputObj);
 property objcount : integer read Getobjcount;
 property Objects[index:integer]:TStatOutputObj read GetAnObject;
 property ParaOptions :TParaOptions read fParaOptions;
end;

TstatColumn=class(Tpersistent)
private
    fTable: TStatTable;
public
 constructor Create(pTable:TStatTable);
 destructor Destroy;override;
 property Table:TStatTable read fTable write fTable;
end;

TStatOutputObj=class(Tpersistent)
private
   fOutput: TStatOutputList;
   fObjSeq: integer;
   fdone: boolean;
   function GetObjectType: integer;virtual;
   procedure SetObjSeq(const Value: integer);virtual;
   function GetObjSeq: integer;virtual;
public
 constructor Create(pOutput:TStatOutputList);
 property Output:TStatOutputList read fOutput;
 property ObjectType : integer read GetObjectType;
 property ObjSeq     : integer read GetObjSeq write SetObjSeq;
 property done :boolean read fdone write fdone;
end;

TStatPara=class(TStatOutputObj)
  private
    ftext: string;
    foptions: TParaOptions;
   procedure Settext(const Value: string);
   function GetObjectType: integer;override;
public
 constructor CreatePara(pOutput:TStatOutputList;options:TParaOptions;const ptext:string='');
// destructor Destroy;override;
 property text :string read ftext write Settext;
 property options:TParaOptions read foptions;
end;

// Tabletypes for different outputs.
// NEVER USE sttInitial - used only during create to indicate newly initialise
// TStatTable. Whenever creating a new TStatTable give it some other value!
TStatTableType = (sttPercents, sttGraph, sttSystem, sttNormal, sttFreq, sttInitial, sttStat,  sttFVStat,
                   sttCTStat1,sttCTStat2,sttCTStat3);

TStatTable=class(TStatOutputObj)
private
    fdata     : Tstringlist;
    fcolCount : integer;
    fRowCount : integer;
    fAuto     : boolean;
    fCaption  : string;
    fFooter   : string;
    fObjSeq   : integer;
    fTableType: TStatTableType;
    function Getcell(Acol, Arow: integer): string;
    procedure SetCell(Acol, Arow: integer; const Value: string);
    procedure SetCaption(const Value: string);
    function GetObjectType: integer;override;
    procedure SetTableType(Value: TStatTableType);
  public
    constructor CreateTable(pOutput:TStatOutputList;cols:integer;rows:integer=-1);
    destructor Destroy;override;
    function InsertColumn(x: integer): boolean;
    procedure DeleteColumn(index: integer);
    function AddColumn: boolean;
    property Cell[col,row:integer]:string read Getcell write SetCell;
    procedure InsertRow(x: integer);
    procedure AddRow;
    property Caption : string read fCaption write SetCaption;
    property Footer  : string read fFooter write fFooter;
    property RowCount:integer read fRowCount;
    property ColCount:integer read fcolCount;
    property TableType: TStatTableType read fTableType write SetTableType;
    // property TableOptions:TTableOptions read GetTableOptions;
  end;

THTMLMaker=class(Tpersistent)
private
  fOutput:        TStatOutputList;
  fStream:        TMemoryStream;
  fLogFile:       TFileStream;
  fParaOptions:   TParaOptions;
  fTableOptions:  TTableOptions;
  fLogging:       boolean;
  fFileMode:      word;
  fLogFileName: string;
  fLogFileHeader: boolean;
  fCSSFileName: string;
  fCharset: string;
  FTitle: string;
  fLogType: TEpiOutputType;
  function InternalOutputTable(xtab: TStatTable;srow,erow:integer;LogType:TEpiOutputType;foot:string=''): string;
  function InternalOutputPercents(xtab: TStatTable; row, col: integer): string;
  function InternalOutputSystem(xtab: TStatTable; row, col: integer): string;
  function InternalOutputNormal(xtab: TStatTable; row, col: integer): string;
  function InternalOutputFreq(xtab: TStatTable; row, col: integer): string;
  function InternalOutputStat(xtab: TStatTable; row, col: integer): string;
  function InternalOutputStatHeader(xtab: TStatTable): string;
  //  function InternalOutputPercents(): string;
  procedure SetLogFileName(const Value: string);
  procedure SetCSSFileName(const Value: string);
  procedure SetCharset(const Value: string);
  function FormatOutPutStr(S: String): string;
  function formatHTMLString(const S: String): string;
public
  constructor Create(pOutput:TStatOutputList);
  destructor Destroy;override;
  function OutputTable(xtab:TStatTable;foot:string='') : string;
//  function OutputTabRows(xtab: TStatTable; srow, erow: integer): string;
  function outputPara(xPara: TStatPara): string;
 // function OutputAll(NewOnly:boolean=false):boolean;
  procedure EndOutput;
  procedure StartOutput;
  procedure write(const S: String;const logS:string=''; StreamOnly: boolean = false);
  procedure writeLn(const S: String;const logS:string=''; StreamOnly: boolean = false);
  function striphtml(html: string): string;
 // function  FormatOutPutStr(S: String):string;
  procedure Remark(aText: String);
  procedure SaveToStream(Stream: TStream);
  procedure AddToStream(Stream: TStream);
  procedure clear;
  procedure SaveToFile(const FileName: string);
  function GetStream: TStream;
  property Output:TStatOutputList read fOutput;
  property TableOptions:TTableOptions read fTableOptions;
  property ParaOptions :TParaOptions read fParaOptions;
  property Logging : boolean read fLogging write fLogging;
  Property LogFileName:string read fLogFileName write SetLogFileName;
  property Title : string read FTitle write FTitle;
  Property CSSFileName:string read fCSSFileName write SetCSSFileName;
  Property Charset:string read fCharset write SetCharset;

  property LogType: TEpiOutputType read fLogType write fLogType;
  property FileMode: word read fFileMode write fFileMode;
end;



implementation

Uses UcmdProcessor;
{ TTableOptions }

constructor TTableOptions.Create;
begin
  inherited;
  SetToDefault;
end;

procedure TTableOptions.SetToDefault;
begin
  FBorder := 1;
  FCellPadding:=5;
  FHeader := '';
  FTableAlignment := taLeftJustify;
  FCellAlignment := caLeftJustify;
  FCellVAlignment := caTop;
  FHeaderAlignment := caCenter;
  FWrap := False;
  FTableString := '';
  FRowColor := clblack;
end;



{ TStatOutput }

constructor TStatOutputList.Create;
begin
   inherited create;
//   fObjList := TList.create;
   fParaOptions:= TParaOptions.create;
end;

destructor TStatOutputList.Destroy;
begin
  DestroyCols;
//  fObjList.clear;
//  fObjList.free;
  fParaOptions.free;
  inherited;
end;

function TStatOutputList.NewTable(cols, rows: integer): TStatTable;
begin
  result:= TStatTable.CreateTable(self,cols,rows);
//  InternalInsertObj(objcount,result);
end;

procedure TStatOutputList.DestroyCols;
var
  obj: TStattable;
begin
{  while fObjList <> nil do
  begin
    obj := fObjList.Last;
    fObjList.Remove(obj);
    obj.Destroy;
  end;}
end;

procedure TStatOutputList.InternalInsertObj(index:integer;obj: TStatOutputObj);
begin
//  fObjList.insert(index,obj);
end;

function TStatOutputList.GetAnObject(index: integer): TStatOutputObj;
begin
//  Result := fObjList[Index];
end;

function TStatOutputList.Getobjcount: integer;
begin
//  result:=fObjlist.count;
end;

function TStatOutputList.NewPara(const pText: string): TStatPara;
begin
  result:=TStatPara.createPara(self,ParaOptions,pText);
//  InternalInsertObj(objcount,result);
end;

{ TStatTable }

constructor TStatTable.CreateTable(pOutput: TStatOutputList; cols, rows: integer);
var
  i, vcol,vrow : integer;
begin
  inherited Create(pOutput);
//  fcolsList := TList.Create;
//  fTableOptions:=TTableOptions.create;
  fTableType := sttInitial;
  fAuto:=rows<1;
  vrow:=rows;
  if vrow < 0 then vrow:=1;
  vcol:=cols;
  if vcol< 0 then vcol:=1;
  frowcount:=vrow;
  fdata := Tstringlist.Create;
  fdata.Capacity:=vcol*vrow;
  for i:= 0 to vcol-1 do
     AddColumn;
end;

destructor TStatTable.Destroy;
var
 i : integer;
begin
// DestroyCols;
// fcolsList.Clear;
// fcolsList.free;
 if Assigned(fdata) then fdata.Clear;
 fdata.free;
// fTableOptions.free;
end;

function TStatTable.AddColumn:boolean;
begin
  Result:=InsertColumn(fcolcount);
end;

function TStatTable.InsertColumn(x:integer):boolean;
var
  I:integer;
begin
  inc(fcolcount);
  for i:=1 to frowcount do
  begin
    fdata.Insert(fcolcount*i-(fcolcount-x),' ');
  end;
end;

procedure TStatTable.InsertRow(x:integer);
var i:integer;
begin
  inc(fRowCount);
  for i:=1 to colcount do
  begin
    fdata.Insert(colcount*x,'.');
  end;
end;


procedure TStatTable.AddRow;
begin
  InsertRow(frowcount);
end;

procedure TStatTable.SetTableType(value: TStatTableType);
begin
  if (value = sttInitial) then
    raise Exception.Create('TableType ''sttInitial'' is reserved.')
  else
    fTableType := value;
end;

{
function TStatTable.InternalNewCol:TStatColumn;
begin
  result:= TStatColumn.create(self);
end;


procedure TStatTable.InternalInsertCol(index:integer;Acol: TStatColumn);
begin
//  fcolsList.insert(index,Acol);
  Acol.Table:= Self;
end;
}

procedure TStatTable.DeleteColumn(index:integer);
var
  i:integer;
begin
  for i:=1 to RowCount do
  begin
    fdata.Delete((i-1)*colcount+(index-1));
  end;
  dec(fcolCount);
end;

{
procedure TStatTable.DestroyCols;
var
  col: TStatColumn;
begin
  while fcolsList <> nil do
  begin
    col := fcolsList.Last;
    InternalRemoveCol(col);
    col.Destroy;
  end;
end;


function TStatTable.GetColumn(Index: Integer): TStatColumn;
begin
//  if fcolsList = nil then TList.Error(SListIndexError, Index);
//  Result := fcolsList[Index];
end;

function TStatTable.GetColumnCount: Integer;
begin
  if fcolsList <> nil then
    Result := fcolsList.Count else
    Result := 0;
end;
}

function TStatTable.Getcell(Acol, Arow: integer): string;
begin
 if ((Acol>0) and (Acol<=colcount)) and ((Arow>0) and (Arow<=rowcount)) then
 begin
   result:=fdata[(Arow-1)*colcount+Acol-1];
 end;
end;


procedure TStatTable.SetCell(Acol, Arow: integer; const Value: string);
begin
 if ((Acol>0) and (Acol<=colcount)) and ((Arow>0) and (Arow<=rowcount)) then
 begin
   fdata[(Arow-1)*colcount+Acol-1]:=value;
 end;
end;

procedure TStatTable.SetCaption(const Value: string);
begin
  fCaption := Value;
end;
{
function TStatTable.GetTableOptions: TTableOptions;
begin
  result:=fTableOptions;
end;
}

function TStatTable.GetObjectType: integer;
begin
  result:=objTable;
end;

{ TstatColumn }

constructor TstatColumn.Create(pTable: TStatTable);
begin
 inherited create;
 fTable:=pTable;
end;

destructor TstatColumn.Destroy;
begin
 fTable :=nil;
 inherited;
end;


constructor THTMLMaker.Create(pOutput: TStatOutputList);
begin
  inherited Create;
  fOutput:=pOutput;
  fStream := TMemoryStream.Create;
  fTableOptions:=TTableOptions.Create;
  LogType:= EpiOTHTML;
  fFileMode := fmCreate;
  fLogFileHeader := false;
//  StartOutput;
end;


destructor THTMLMaker.Destroy;
begin
  fTableOptions.free;
  if fLogFile<> nil then
    fLogFile.free;
 inherited destroy;
end;


function GetColorStr(AColor : TColor) : string;
VAR
  ColorStr : string;
Begin
  ColorStr := IntToHex(AColor, 6);
  Result := '"#'+Copy(ColorStr,5,2)+Copy(ColorStr,3,2)+Copy(ColorStr,1,2)+'"';
End;

function THTMLMaker.striphtml(html: string): string;
 var i: integer;

   function exchangehtml(thml: string;tag: string;add : string):string;
   begin
      i := length(trim(tag))  ;
      while pos(tag,html) > 0 do
         html := copy(html,1,pos(tag,html)-1)  + add
                + copy(html,(pos(tag,html)+i),length(html));
      result:= html;
   end;
 begin
    // add all tags to be removed here:
    html :=  exchangehtml(html,'<br>',CRLF);
    html :=  exchangehtml(html,'<small>','');
    html :=  exchangehtml(html,'</small>','');
    html := exchangehtml(html,'<sup>','');
    html := exchangehtml(html,'</sup>','');
    html := exchangehtml(html,'<sub>','');
    html := exchangehtml(html,'</sub>','');
    html := exchangehtml(html,'&nbsp;',' ');
    html := exchangehtml(html,'&lt;','<');
    html := exchangehtml(html,'&gt;','>');
    html := exchangehtml(html,'<h1>','');
    html := exchangehtml(html,'<h2>','');
    html := exchangehtml(html,'<h3>','');
    html := exchangehtml(html,'<h4>','');
    html := exchangehtml(html,'<h5>','');
    html := exchangehtml(html,'</h1>','');
    html := exchangehtml(html,'</h2>','');
    html := exchangehtml(html,'</h3>','');
    html := exchangehtml(html,'</h4>','');
    html := exchangehtml(html,'</h5>','');
    html := exchangehtml(html,'<img src=','file: ');
    html := exchangehtml(html,'<a href=','file: ');
    html := exchangehtml(html,'">','" ');
    html := exchangehtml(html,'</a>',' ');
    result:=html;
end;

function THTMLMaker.InternalOutputPercents(xtab: TStatTable; row, col: integer): string;
const
  output: array[1..4,1..5] of string =
        (('columnvariable', 'firstrow',    'firstrowpct', 'rowtotalh',  'rowtotalpcth' ),
         ('firstcol1',      'cell1',       'cellpct1',    'rowtotal1',   'rowtotalpct1'  ),
         ('firstcol2',      'cell2',       'cellpct2',    'rowtotal2',   'rowtotalpct2'  ),
         ('columntotalh',   'columntotal', 'columnpct',   'grandtotal', 'grandtotalpct'));
var
  r, c: integer;
begin
  result := '';
  c := (col mod 2) + 2;
  r := (row mod 2) + 2;
  if (col = 1) then c := 1;
  if (col = xtab.ColCount-1) then c := 4;
  if (col = xtab.ColCount) then c := 5;
  if (row = 1) then r := 1;
  if (row = xtab.RowCount) then r := 4;
  result := '<TD class=' + output[r, c] + '>';
end;

function THTMLMaker.InternalOutputNormal(xtab: TStatTable; row, col: integer): string;
const
  output: array[1..4,1..3] of string =
        (('columnvariable', 'firstrow',    'rowtotalh' ),
         ('firstcol1',      'cell1',       'rowtotal1'  ),
         ('firstcol2',      'cell2',       'rowtotal2'  ),
         ('columntotalh',   'columntotal', 'grandtotal'));
var
  r, c: integer;
begin
  result := '';
  c := 2;
  r := (row mod 2) + 2;
  if (col = 1) then c := 1;
  if (col = xtab.ColCount) then c := 3;
  if (row = 1) then r := 1;
  if (row = xtab.RowCount) then r := 4;
  result := '<TD class=' + output[r, c] + '>';
end;

function THTMLMaker.InternalOutputStat(xtab: TStatTable; row, col: integer): string;
const
  output: array[1..4,1..3] of string =
        (('columnvariable', 'firstrow',    'firstrow' ),
         ('firstcol1',      'cell1',       'cell1'  ),
         ('firstcol2',      'cell2',       'cell2'  ),
         ('firstcol2',      'cell2',       'cell2'  ));
var
  r, c: integer;
begin
  result := '';
  c := 2;
  r := (row mod 2) + 2;
  if (col = 1) then c := 1;
  if (col = xtab.ColCount) then c := 3;
  if (row = 1) then r := 1;
  if (row = xtab.RowCount) then r := 4;
  result := '<TD class=' + output[r, c] + '>';
end;

function THTMLMaker.InternalOutputStatHeader(xtab: TStatTable): string;
begin
  Result := '<TR><TD class=firstcol1 colspan=2>&nbsp</TD>'
          + '<TD class=firstrow colspan=2><center>' + xtab.Cell[3,1] + '</center></TD>';
  Case xtab.TableType of
    sttCTStat1:
      Result := Result + '<TD class=firstcol1>&nbsp</TD>' +
            '<TD class=firstrow colspan=2><center>' + xtab.cell[5,1] + '</center></TD>';// +
    sttCTStat2:
      Result := Result + '<TD class=firstcol1 colspan=2>&nbsp;</TD>' +
            '<TD class=firstrow colspan=2><center>' + xtab.cell[5,1] + '</center></TD>';// +
    sttCTStat3:
      Result := Result + '<TD class=firstcol1 colspan=3>&nbsp;</TD>' +
            '<TD class=firstrow colspan=2><center>' + xtab.cell[5,1] + '</center></TD>';// +
  end;
  Result := Result + '</TR>';

{  if xtab.TableType = sttOAStat1 then
       + '<TD class=firstrow colspan=2>' + xtab.Cell[3,1] + '</TD><TD class=firstcol1>&nbsp;</TD>'
       + '<TD class=firstrow colspan=2>' + xtab.cell[5,1] + '</TD><TD class=firstcol1 colspan=2> </TD></TR><TR>'
  else if xtab.TableType = sttCCStat then
       + '<TD class=firstrow colspan=2><center>' + xtab.Cell[5,1] + '</center></TD><TD class=firstcol1 colspan=2> </TD></TR><TR>'
  else
       + '<TD class=firstrow colspan=2>' + xtab.Cell[3,1] + '</TD><TD class=firstcol1 colspan=2> </TD>'
       + '<TD class=firstrow colspan=2>' + xtab.Cell[5,1] + '</TD><TD class=firstcol1 colspan=3> </TD></TR><TR>';
}
end;

function THTMLMaker.InternalOutputFreq(xtab: TStatTable; row, col: integer): string;
const
  output: array[1..4,1..3] of string =
        (('columnvariable', 'firstrow',    'rowtotalh' ),
         ('firstcol1',      'cell1',       'rowtotal'  ),
         ('firstcol2',      'cell2',       'rowtotal'  ),
         ('columntotalh',   'columntotal', 'grandtotal'));
var
  r, c: integer;
begin
  result := '';
  c := 2;
  r := (row mod 2) + 2;
  if (col = 1) then c := 1;
  if (col = xtab.ColCount) then c := 3;
  if (row = 1) then r := 1;
  if (row = xtab.RowCount) then r := 4;
  result := '<TD class=' + output[r, c] + '>';
end;

function THTMLMaker.InternalOutputSystem(xtab: TStatTable; row, col: integer): string;
const
  output: array[1..4,1..3] of string =
        (('columnvariable', 'firstrow',    'rowtotalh' ),
         ('firstcol1',      'cell1',       'rowtotal'  ),
         ('firstcol2',      'cell2',       'rowtotal'  ),
         ('columntotalh',   'columntotal', 'grandtotal'));
var
  r, c: integer;
begin
  result := '';
  c := 2;
  r := (row mod 2) + 2;
  if (col = 1) then c := 1;
  if (col = xtab.ColCount) then c := 3;
  if (row = 1) then r := 1;
  if (row = xtab.RowCount) then r := 4;
  result := '<TD class=' + output[r, c] + '>';
end;

function THTMLMaker.InternalOutputTable(xtab:TStatTable; srow, erow: integer; LogType: TEpiOutputType;
                                        foot:string=''): string;
VAR
  AlignTitle, AlignTable, AlignCellV,
  CellPadding, Wrapped, CellStart, TableCells,
  TableRow,  TableString, RowColor : string;
  i,j,c,row,col : integer;
  Closeoutput :boolean;
  opt :TEpiOption;
  tableclass : string;
Begin
  if (xtab = nil) then exit;
  if erow =MaxInt then
  begin
    erow:=xtab.rowcount;
    Closeoutput:=true;
  end;

  // TODO -oTorsten: Remove footer "conversion" when 'foot' no longer used.  
  if (Foot <> '') and (xTab.Footer = '')  then
    xTab.Footer := Foot;

  if LogType=EpiOTHTML then
  begin
    if srow=1 then
    begin
      case xtab.TableType of
        sttNormal, sttFVStat,
        sttCTStat1,sttCTStat2,sttCTStat3: tableclass := '';
        sttPercents: tableclass := '';
        sttSystem: tableclass := ' SYSTEM';
        sttGraph: tableclass := ' GRAPH';
        sttFreq: tableclass := ' FREQ';
        sttStat: tableclass := ' STAT';
        sttInitial: tableclass := '';
      end;
      if (DM.GetOptionValue('TABLE DESIGN' + tableclass, Opt)) then
        tableclass := lowercase(Opt.value)
      else
        tableclass := 'line';

      TableString := '<TABLE cellspacing=0 class=' + tableclass  + ' >'+CRLF;

      IF xtab.Caption <> '' THEN
        TableString := TableString+'<CAPTION class=caption>'+format('%s',[xtab.Caption]) +'</CAPTION>'+CRLF;
    end;

    IF xTab.Footer = 'GraphSubTables' THEN              // this is a pseudo crosstable for a graph
    begin
      TableString := TableString + '<CAPTION class=caption>&nbsp;</CAPTION>'+CRLF;
      xTab.Footer := '';
    end;

    for i:= srow to erow do
    begin
      TableRow := TableRow+'<TR>';

      IF (i MOD 2 = 0) then row := 1 else row := 2;

      if (i = 1) and (xtab.TableType in [sttCTStat1, sttCTStat2, sttCTStat3]) then
        TableRow := InternalOutputStatHeader(xtab)
      else begin
        FOR j := 1 TO xtab.colcount DO
        begin
          case xtab.TableType of
            sttNormal: CellStart := InternalOutputNormal(xtab, i, j);
            sttPercents: CellStart := InternalOutputPercents(xtab, i, j);
            sttSystem: CellStart := InternalOutputSystem(xtab, i, j);
            sttGraph, sttStat: CellStart := InternalOutputStat(xtab, i, j);
            sttCTStat1,sttCTStat2,sttCTStat3 : Cellstart:= InternalOutputStat(xtab, i-1, j);
            sttFreq: CellStart := InternalOutputFreq(xtab, i, j);
            sttInitial: CellStart := '<TD>';
          end;

          if xtab.cell[j,i] <> ''
            THEN TableCells := TableCells + CellStart + xtab.cell[j,i]+'</TD>'
            ELSE TableCells := TableCells + CellStart +'&nbsp</TD>';
        end;
      end;
      TableRow := TableRow+TableCells+CRLF;
      TableCells := '';
    end;

     // footer to table ?
    if  xTab.Footer <> '' then
    begin
      TableRow := TableRow
                   + format('<tr><TD class=cellfoot colspan= %d >',[xtab.colcount])
                   + xTab.Footer + '</TD>' + CRLF ;
    end;

    if Closeoutput then
      Result := TableString + TableRow+ '</TABLE>'+ CRLF
    else
      Result := TableString + TableRow + CRLF;
    //HTML output
  end else begin
    //txt output
    for i:= srow to erow do
    begin
      FOR j := 1 TO xtab.colcount DO
      Begin
        if TableCells <> '' THEN
          TableCells := TableCells+Tab+xtab.cell[j,i]
        else
          TableCells := xtab.cell[j,i]
      end;
      TableRow := TableRow+TableCells+CRLF;
      TableCells := '';
    End;
    if xTab.Footer <> '' then  TableRow := TableRow + CRLF + xTab.Footer;
    Result := striphtml(TableRow);
  end;
End;


function THTMLMaker.OutputTable(xtab: TStatTable; foot: string=''): string;
var
 s, s1 : string;
begin
  s := InternalOutputTable(xtab,1,MaxInt,EpiOTHTML, foot);
  s:= s + ' ';
  if LogType=EpiOTText then
  begin
    s1 := InternalOutputTable(xtab,1,MaxInt,EpiOTText,foot);
    Writeln(s,s1)
  end
  else
    Writeln(s);
end;

{function THTMLMaker.OutputTabRows(xtab: TStatTable;srow,erow:integer): string;
var
 s : string;
begin
   s := InternalOutputTable(xtab,srow,erow,EpiOTHTML,'',);
   Write(s);
   end;
}

function THTMLMaker.outputPara(xPara:TStatPara):string;
var
s, align :string;
begin
   s :=ColorToString(xPara.options.FFontColor);
   if length(s) > 2 then
      s:= copy(s,3,MAxInt);
   case Xpara.options.fAlignment of
   caCenter: align:= 'align=center';
   caRightJustify : align:= 'align=right'
   else align:=''
   end;
   if Xpara.options.fOClass ='' then
      s:=format('<P>',[align,s])
   else
      s:=format('<P class="%s">',[Xpara.options.fOClass]);
   s := s+xPara.text+'</P>';
//   s := s+FormatOutPutStr(xPara.text)+'</P>';
{   Writeln(s);
   OutPutStr(xPara.text);}
 if LogType=EpiOTText then
   begin
    // remove html tags <br> in text
   xpara.text :=  striphtml(Xpara.text);
   xpara.text :=  striphtml(Xpara.text);
   Writeln(s,xpara.text)
    end
 else
    Writeln(formatHTMLString(s));
end;


function THTMLMaker.formatHTMLString(const S:String):string;
begin
  Result:=StringReplace(s,cFiller,'&nbsp;',[rfReplaceAll]);
end;

Procedure THTMLMaker.write(const S:String;const logS:string=''; StreamOnly: boolean = false);
Begin
   if fStream.Size > MAXSTREAMSIZE then
      Clear;
  fStream.WriteBuffer(Pointer(S)^, length(s));
  if (fLogFile <> nil) and (not StreamOnly) then
  begin
    if LogType=EpiOTHTML then
      fLogFile.WriteBuffer(Pointer(S)^, length(s))
    else
      if LogS <> CRLF then
        fLogFile.WriteBuffer(Pointer(LogS)^, length(Logs))
  end;
End;


Procedure THTMLMaker.writeLn(const S:String;const logS:string=''; StreamOnly: boolean = false);
Begin
  write(S+CRLF,logS+CRLF, StreamOnly);
//  write(CRLF);
End;

function  THTMLMaker.FormatOutPutStr(S: String): string;
Var I,len  : integer;
Begin
 result := '';
 len :=Length(S);
 If len > 0 Then
    For I:=1 To len Do
       Case S[I] Of
          '&': result := result + '&amp;';
          '<': result := result + '&lt;';
          '>': result := result + '&gt;';
          #9,#32: result := result + '&nbsp;';
//          VK_SPACE
       Else
          result := result + S[I];
       End;{Case}
End;

Procedure THTMLMaker.Remark(aText:String);
Begin
      Write('<!-- ');
//      WriteString(aText);
      write(aText);
      WriteLn(' -->');
End;


Procedure THTMLMaker.StartOutput;
var
 s : string;
 opt: TEpiOption;
 StyleSheet: TStringList;
 b : Boolean;
Begin
//      If FDocType <> '' Then
//      WriteLn('<!DOCTYPE ' + FDocType + '>');
  // avoid writing header information twice in the same outputfile!

  //if fFileMode = fmOpenReadWrite then exit;
//  showmessage('past fmOpenReadWrite');
  FStream.Clear;
  WriteLn('<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.01 Transitional//EN">','',fLogFileHeader);
  WriteLn('<HTML>','',fLogFileHeader);
  Remark('This file generated by Epidata Analysis ' + GetBuildInfoAsString);
  WriteLn('<Head>','',fLogFileHeader);
  If CSSFileName <> '' Then
  Begin
    if Assigned(dm) and (dm.GetOptionValue('STYLE SHEET EXTERNAL', opt)) and (opt.Value = 'ON') then
      s := format('<link href="%s" rel="stylesheet" type="text/css">',[CSSFileName])
    else
    try
      StyleSheet := TStringList.create;
      StyleSheet.LoadFromFile(CSSFileName);
      s := '<STYLE>' + #13 + '<!--' + #13;
      s := s + StyleSheet.Text;
      s := s + '-->' + #13 + '</STYLE>' + #13;
    except
      if assigned(dm) then
        dm.error('Style sheet file not found', [], 107001)
      else
        raise Exception.Create('Style sheet file not found');
    end;
    WriteLn(s,'',fLogFileHeader);
  End;
  writeln('<meta name="author" content="' + GetBuildInfoAsString + '">','',fLogFileHeader);
  writeln('<meta name="Copyright" content="EpiData Association, Denmark">','',fLogFileHeader);
  writeln('<meta name="No_Payment" content="EpiData Analysis is freeware">','',fLogFileHeader);
  writeln('<meta name="Update_from:" content="Http://www.epidata.dk">','',fLogFileHeader);
  writeln('<meta name="Disclaimer" content="Http://www.epidata.dk/disclaim.htm">','',fLogFileHeader);

  If ansiuppercase(FCharSet) <> '' Then
  Begin
    Write('<meta http-equiv="Content-Type" ','',fLogFileHeader);
    WriteLn('content="text/html; charset=' + FCharSet+ '">','',fLogFileHeader);
     // Chinese works with <meta http-equiv="Content-Type" content="text/html; charset=gb2312">
    // Chinese works with <font face="Arial Unicode MS">
  End;



  WriteLn('<TITLE>' + Title + '</TITLE>','',fLogFileHeader);
  WriteLn('</HEAD>','',fLogFileHeader);
  Write('<BODY class=body','',fLogFileHeader);
  WriteLn('>','',fLogFileHeader);
  fLogFileHeader := true;
End;

Procedure THTMLMaker.EndOutput;
Begin
    WriteLn(CloseHTML)
End;



function THTMLMaker.GetStream: TStream;
begin
 result:=fstream;
end;

procedure THTMLMaker.SaveToStream(Stream: TStream);
var
  s : string;
begin
//  s :=CloseHTML;
  Stream.CopyFrom(fstream,0);
  Stream.WriteBuffer(Pointer(S)^, length(s));
end;

procedure THTMLMaker.AddToStream(Stream: TStream);
begin
  Stream.CopyFrom(fstream,0);
end;


procedure THTMLMaker.SaveToFile(const FileName: string);
var
  Stream: TStream;
begin
  Stream := TFileStream.Create(FileName, fmCreate);
  try
    SaveToStream(Stream);
  finally
    Stream.Free;
  end;
end;
{
function THTMLMaker.OutputAll(NewOnly:boolean=false): boolean;
var
  i, tseq : integer;
  obj : TStatOutputObj;
begin
  tseq :=0;
  for i := 0 to foutput.objcount-1 do
  begin
   obj := foutput.Objects[i];
   if NewOnly and obj.done then
   begin
    case obj.ObjectType of
    objTable :
     begin
       inc(tseq);
     end;
   end;//case
   continue;
   end
   else
   begin
   case obj.ObjectType of
     objTable :
     begin
       inc(tseq);
       obj.ObjSeq:=tseq;
       outputTable(TstatTable(obj));
     end;
     objPara:
       outputPara(TstatPara(obj));
   end;//case
     obj.done:=true;
   end;
  end;
end;
}
{ TStatPara }

constructor TStatPara.CreatePara(pOutput: TStatOutputList;options:TParaOptions;const ptext: string);
begin
  inherited Create(pOutput);
  Text:=pText;
  fOptions:=Options;
end;

function TStatPara.GetObjectType: integer;
begin
  result:=objPara;
end;

procedure TStatPara.Settext(const Value: string);
begin
  ftext := Value;
end;

{ TStatOutputObj }

constructor TStatOutputObj.Create(pOutput: TStatOutputList);
begin
  inherited Create;
  fOutput:=pOutput;
//  fObjSeq :=
end;

function TStatOutputObj.GetObjectType: integer;
begin
  result:=-1;
end;

function TStatOutputObj.GetObjSeq: integer;
begin
  result:=fObjSeq;
end;

procedure TStatOutputObj.SetObjSeq(const Value: integer);
begin
  fObjSeq := Value;
end;

{ TParaOptions }

constructor TParaOptions.Create;
begin
 inherited Create;
 SetToDefault;
end;

procedure TParaOptions.SetToDefault;
begin
  FBorder := 1;
  FAlignment := caLeftJustify;
  FWrap := False;
  FFontColor := clYellow;
end;


procedure THTMLMaker.clear;
begin
 FStream.Clear;
 StartOutput;
end;

procedure THTMLMaker.SetLogFileName(const Value: string);
var
  lLogFile   : TFileStream;
  fn         : string;
begin
  fn := StrRemoveSurroundingQuotes(value);
  if fn='' then
  begin
    fLogFileName := fn;
    if fLogFile<> nil then
    begin
      EndOutput;
      fLogFile.free;
      fLogfile:=nil;
    end;
    exit;
  end;
  case fFileMode of
    fmCreate:        lLogFile := TFileStream.Create(fn, fmCreate);
    fmOpenWrite:     lLogFile := TFileStream.Create(fn, fmOpenWrite);
    fmOpenRead:      lLogFile := TFileStream.Create(fn, fmOpenRead);
    fmOpenReadWrite: lLogFile := TFileStream.Create(fn, fmOpenReadWrite);
  end;
  if fLogFile<> nil then
  begin
      fLogFile.free;
      fLogfile:=nil;
   end;
  fLogFile:= lLogFile;
  if fLogFile.Size > 0 then
    fLogFile.Seek(0, soFromEnd);
  fLogFileHeader := false;
  if fFileMode =fmOpenReadWrite then
    fLogFileHeader := true; // no need to recreate HTML headers within document
  fLogFileName := Expandfilename(fn);
end;

procedure THTMLMaker.SetCSSFileName(const Value: string);
begin
  if fCSSFileName = Value then exit;
  fCSSFileName := Value;
  clear;
end;

procedure THTMLMaker.SetCharset(const Value: string);
begin
  if fCharset = Value then exit;
  fCharset := Value;
  clear;
end;

end.
