unit Uformats;

interface
uses sysutils,UEpidataTypes,ansDataTypes, uDateUtils, cstrings, Uoutput ;

function SetNumberFormatString(const fmt:string):boolean;

function EpiCIFormat(OddsRatio,LL,UL:EpiFloat; const efmt,cifmt,head:string;const sum: integer):string;

function Epiformat(value:EpiFloat;const fmt:string=''):string;overload;
function Epiformat(value:EpiInt;const fmt:string='%d'):string;overload;
function Epiformat(const value:string;const fmt:string=''):string;overload;

function EpiPercentFormat(const value:string;const fmt:string=''):string; overload;
function EpiPercentFormat(value:EpiFloat; const fmt:string=''):string; overload;


implementation

const DefaultNumberFormat:string = '%8.2f';


function SetNumberFormatString(const fmt:string):boolean;
var
  defaultnumberformatstring: string;
begin
    if fmt<>'' then
       DefaultNumberFormatString:=fmt
     else
       DefaultNumberFormatString:= '%8.2f';
end;


function Epiformat(const value:string; const fmt:string=''):string;overload;
var
  s : string;
begin
  result:='';
  if value='' then exit;
  s := fmt;
    if s='' then s:='%s';
  result:=format(s,[Value]);
end;

function Epiformat(value:EpiFloat;const fmt:string=''):string;overload;
var
  s : string;
begin
  result:='.';
  if value = NA_FLOAT then exit;
  s := fmt;
  if s<>'' then
  begin
   if upcase(s[1])='P' then  // show as percentage format
   begin
      s := '%5.'+ copy(s,2,1)+'f';
      Value:= Value*100;
   end;
  end else
    s:=DefaultNumberFormat;
  result:=format(s,[Value]);
  if copy(trimleft(result),1,4) = '9999' then result := 'inf.' ;  //'&#8734;';
  if copy(trimleft(result),1,5) = '-9999' then result := '-inf.' ; // -&#8734;';
end;

function Epiformat(value:EpiInt;const fmt:string='%d'):string;overload;
var
  s : string;
begin
  result:='.';
  if value=NA_INT then exit;
  s := AnsiUppercase(fmt);
  if s='' then s:='%d'
  else if s='%DMY' then
      begin Result:=EpiDateToStr(Value,dfDMY);exit;end
  else if s='%MDY' then
      begin Result:=EpiDateToStr(Value,dfMDY);exit;end
  else if s='%YMD' then
      begin Result:=EpiDateToStr(Value,dfYMD);exit;end
  else if upcase(s[1])='P' then
     begin
        s := copy(s,2,length(s));
        s := '%'+ s +'f'+'%%';
        Value:= Value*100;
     end;
  result:=format(s,[Value]);
end;

function EpiPercentFormat(const value: string; const fmt:string=''):string; overload;
begin
  result:= value;
  if value = '' then exit;
  if fmt[1] <> 'P' then exit;
  //s := '%8.' + fmt[2] + 'f';
  result:= fmt[3] + Value + copy(fmt, 4, length(fmt));
end;

function EpiPercentFormat(value:EpiFloat; const fmt:string=''):string; overload;
var
  s : string;
begin
  result := floattostr(value);
  if fmt[1] <> 'P' then exit;
  s := '%8.' + fmt[2] + 'f';
  result:= fmt[3] + trim(format(s,[Value])) + copy(fmt, 4, length(fmt));
end;

function EpiCIFormat(OddsRatio,LL,UL:EpiFloat; const efmt,cifmt,head: string; const sum: integer):string;
var
  s : string;
  function deletep(var s: string): string;
    begin
      result := s;
      if result[1] = '(' then result := copy(result,2,length(result)-1);
      result := trim(result);
      if result[length(result)] = ')' then result := copy(result,1,length(result)-1);
    end;
begin
  s := cifmt;

  // add the parenthesis from CI format to
  result:= s[1];
  if abs(ll) = 9999.0 then result := result+ ' inf'
  else result:= result + trim(format(efmt,[LL]));
  if length(s)> 2 then result := result + copy(s,3,length(s)-2) else result := result + '&nbsp;';
  if abs(ul) = 9999.0 then result := result+ ' inf'
  else result:= result + trim(format(efmt,[UL]));
  if length(s) > 1 then result:= result + s[2];

  if sum = 2 then  // format for footer of table:
  begin
    s := deletep(result);
    result := copy(head,pos('>',head)+1,8);
    if pos('<',result) > 0 then result := copy(head,1,pos('<',head)-1);
    result := ' ('+ deletep(result) + ': ' + s + ')';
  end;
end;

end.
