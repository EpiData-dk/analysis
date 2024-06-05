unit outputgenerator_console;

{$codepage UTF8}
{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, outputgenerator_base, outputcreator, epicustombase,
  outputgenerator_txt;

type

  { TOutputGeneratorConsole }

  TOutputGeneratorConsole = class(TOutputGeneratorTXT)
  private
    FLastOutputItemIndex: integer;
    procedure RedrawRequest(Sender: TObject);
  protected
    procedure LoopOutput; override;
    procedure WriteToStream(const S: UTF8String); override;
  public
    constructor Create(ACreator: TOutputCreator; AOutputStream: TStream = nil); override;
  end;

implementation

{ TOutputGeneratorConsole }

procedure TOutputGeneratorConsole.RedrawRequest(Sender: TObject);
begin
  GenerateReport;
end;

procedure TOutputGeneratorConsole.LoopOutput;
var
  i: Integer;
  O: TOutputBase;
begin
  for i := FLastOutputItemIndex to Creator.Count - 1 do
  begin
    O := Creator.Items[i];

    case O.OutputType of
      otTable: DoOutputTable(TOutputTable(O));
      otLine:  DoOutputLine(TOutputLine(O));
    end;
  end;
  FLastOutputItemIndex := Creator.Count;
end;

procedure TOutputGeneratorConsole.WriteToStream(const S: UTF8String);
begin
  Write(S);
end;

constructor TOutputGeneratorConsole.Create(ACreator: TOutputCreator;
  AOutputStream: TStream);
begin
  inherited Create(ACreator, AOutputStream);
  ACreator.OnRedrawRequest := @RedrawRequest;
  FLastOutputItemIndex := 0;
end;

end.

