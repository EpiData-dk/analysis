program Lang2rc;

{ Lang2rc is a small consoleapplication that makes a ressourcefile (*.rc)
  from a EpiData language file.
  The language file must contain lines with code=string. All other types
  of lines are ignored.

  SYNTAX: Lang2rc languagefilename [rc-filename]

  If rc-filename is not given, then the resulting rc-file will have
  same name as then languagefile but with the extention .rc

  Michael Bruus
  August 2007

}


{$APPTYPE CONSOLE}

uses
  SysUtils,Classes;

const
  Ofs=0;

VAR
  LanStr,UdStr: TStringList;
  n,t: Integer;
  langfile: string;
  rcfile: string;
  s:string;
  flags: TReplaceFlags;

begin
  if ParamCount=0 then
    begin
      writeln('Syntax is Lang2rc {languagefilename} [{ressourcefilename}]');
      halt(1);
    end;
  langfile:=ParamStr(1);
  rcfile:=ChangeFileExt(langfile,'.rc');
  if ParamCount>1 then rcfile:=ParamStr(2);
  TRY
    LanStr:=TStringList.Create;
    Udstr:=TStringList.Create;
    UdStr.Append('STRINGTABLE'#13'{');
    try
      LanStr.LoadFromFile(langfile);
    except
      writeln('Error: Languagefile ' + langfile + ' not found');
      halt(1);
    end;
    flags:=[rfReplaceAll];
    FOR n:=0 TO LanStr.Count-1 DO
      BEGIN
        IF LanStr.Names[n]='' THEN Continue;
        t:=StrToInt(LanStr.Names[n])+Ofs;
        s:=LanStr.Values[LanStr.Names[n]];
        s:=StringReplace(s,'"','""',flags);
        s:=IntToStr(t)+', "'+s+'"';
        UdStr.Append(s);
      END;
    UdStr.Append('}');
    try
      UdStr.SaveToFile(rcfile);
    except
      writeln('Error saving ressourcefile as '+rcfile);
      halt(1);
    end;
  FINALLY
    UdStr.Free;
    LanStr.Free;
  END;

end.
 