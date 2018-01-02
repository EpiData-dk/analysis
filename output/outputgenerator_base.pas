unit outputgenerator_base;

{$codepage UTF-8}
{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, outputcreator;

type

  { TOutputGeneratorBase }

  TOutputGeneratorBase = class
  private
    FCreator: TOutputCreator;
    FStream: TStream;

  { text }
  protected
    type
      TTextFormat = (tfNormal, tfLink, tfBold, tfUnderline, tfItalic, tfSubscript, tfSuperscript);
  protected
    function ProcessIText(IText: IOutputText): boolean;
    procedure TextOut(Const S: UTF8String; TextFormat: TTextFormat); virtual; abstract;

  { loop objects }
  protected
    procedure LoopOutput; virtual;
    procedure DoOutputLine(Line: TOutputLine); virtual;
    procedure DoOutputTable(Table: TOutputTable); virtual;
  protected
    procedure WriteToStream(Const S: UTF8String);
    property Creator: TOutputCreator read FCreator;
  public
    constructor Create(ACreator: TOutputCreator; AOutputStream: TStream = nil); virtual;
    procedure GenerateReport; virtual; overload;
    procedure GenerateReport(out Text: UTF8String); overload;
    property Stream: TStream read FStream;
  end;

  TOutputGeneratorClass = class of TOutputGeneratorBase;


implementation

uses
  LazUTF8;

{ TOutputGeneratorBase }

procedure TOutputGeneratorBase.DoOutputLine(Line: TOutputLine);
begin
  //
end;

procedure TOutputGeneratorBase.DoOutputTable(Table: TOutputTable);
begin
  //
end;

procedure TOutputGeneratorBase.WriteToStream(const S: UTF8String);
begin
  Stream.Write(S[1], Length(S));
end;

function TOutputGeneratorBase.ProcessIText(IText: IOutputText): boolean;
var
  S, T: UTF8String;
  I, j: Integer;
  L: PtrInt;
  tt: TTextFormat;
begin
  S := IText.Text;

  I := 1;
  L := Length(S);
  while I <= L do
  begin
    if ((S[i] = '{') and ((I + 1) <= L) and (S[i+1] = '{'))
       or
       ((S[i] = '}') and ((I + 1) <= L) and (S[i+1] = '}'))
    then
      begin
        TextOut(S[i], tfNormal);
        Inc(I, 2);
        Continue;
      end;

    if (S[i] = '{') then
      begin
        inc(i);
        if S[i] <> '\' then exit; // ERROR

        inc(i);
        case S[i] of
          'i': tt := tfItalic;
          'b': tt := tfBold;
          'l': tt := tfLink;
          'u': tt := tfUnderline;
          's': tt := tfSubscript;
          'S': tt := tfSuperscript;
        else
          tt := tfNormal;
        end;

        inc(i, 2);    // empty space
                                                             //  123456
        j := i;                                                  {\b s}
        while (j <= L) and                                   //  abcdef
              (S[j] <> '}')
        do
          Inc(j);


        T := Copy(S, i, j - i);
        TextOut(T, tt);
        i := j + 1;

        Continue;
      end;

    j := i;
    while (j <= L) and
          (S[j] <> '{') and
          (S[j] <> '}')
    do
      inc(j);

    T := Copy(S, i, j - i);
    TextOut(T, tfNormal);
    i := j;
  end;
end;

procedure TOutputGeneratorBase.LoopOutput;
var
  i: Integer;
  O: TOutputBase;
begin
  for i := 0 to Creator.Count - 1 do
  begin
    O := Creator.Items[i];

    case O.OutputType of
      otTable: DoOutputTable(TOutputTable(O));
      otLine:  DoOutputLine(TOutputLine(O));
    end;
  end;
end;

constructor TOutputGeneratorBase.Create(ACreator: TOutputCreator;
  AOutputStream: TStream);
begin
  FCreator := ACreator;

  FStream := AOutputStream;
  if (not Assigned(FStream)) then
    FStream := TMemoryStream.Create;
end;

procedure TOutputGeneratorBase.GenerateReport;
begin
  LoopOutput;
end;

procedure TOutputGeneratorBase.GenerateReport(out Text: UTF8String);
var
  AStream: TStream;
  T1, T2: TDateTime;
begin
  AStream := TStringStream.Create('');

  T1 := now;
  GenerateReport;
  T2 := now;

  Stream.Position := 0;
  AStream.CopyFrom(Stream, Stream.Size);
  Text := TStringStream(AStream).DataString;
  FreeAndNil(AStream);
end;

end.

