unit save_output;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, executor, outputcreator, ast, epidatafilestypes;

type

  { TSaveOutput }

  TSaveOutput = class
  private
    FExecutor: TExecutor;
    FOutputCreator: TOutputCreator;
  public
    constructor Create(AExecutor: TExecutor; AOutputCreator: TOutputCreator);
    destructor Destroy; override;
    procedure ExecSaveOutput(ST: TCustomStringCommand);
  end;

implementation

uses
  datamodule, Dialogs, ana_globals, LazUTF8, outputgenerator_base,
  outputgenerator_html, outputgenerator_txt, LazFileUtils;

{ TSaveOutput }

constructor TSaveOutput.Create(AExecutor: TExecutor;
  AOutputCreator: TOutputCreator);
begin
  FExecutor := AExecutor;
  FOutputCreator := AOutputCreator;
end;

destructor TSaveOutput.Destroy;
begin
  inherited Destroy;
end;

procedure TSaveOutput.ExecSaveOutput(ST: TCustomStringCommand);
var
  FN: EpiString;
  SD: TSaveDialog;
  Opt: TOption;
  OutputType: UTF8String;
  OutputGenerator: TOutputGeneratorBase;
  FS: TFileStream;
begin
  FN := '';

  if (ST.HasOption('output', Opt)) and
     (Assigned(Opt.Expr))
  then
    begin
      OutputType := UTF8LowerString(Opt.Expr.AsString);
      if (OutputType <> 'html') and
         (OutputType <> 'text')
      then
        begin
          FExecutor.Error('Unknown output format "' + Opt.Expr.AsString + '"');
          ST.ExecResult := csrFailed;
          Exit;
        end;
    end
  else
    OutputType := UTF8LowerString(FExecutor.SetOptions[ANA_SO_OUTPUT_SAVE_FORMAT].Value);

  if Assigned(ST.StringExpr) then
    FN := ST.StringExpr.AsString;

  if (FN = '') then
    begin
      SD := TSaveDialog.Create(nil);
      SD.Options := aDM.SaveDialog1.Options;
      SD.InitialDir := aDM.SaveDialog1.InitialDir;

      case OutputType of
        'html':
          begin
            SD.Filter := 'HTML Output (*.html)|*.html';
            SD.DefaultExt := '.html';
          end;
        'text':
          begin
            SD.Filter := 'Text Output (*.txt)|*.txt';
            SD.DefaultExt := '.txt';
          end;
      end;

      if (not SD.Execute) then
        begin
          FOutputCreator.DoInfoAll('Save cancelled');
          ST.ExecResult := csrFailed;
          Exit;
        end;

      FN := SD.FileName;

      // TODO: Really bad use of aDM...
      if assigned(aDM.OnDialogFilename) then
        aDM.OnDialogFilename(FN);

      if FileExistsUTF8(FN) then
        ST.Options.Add(TOption.Create(TVariable.Create('replace', FExecutor), nil));

      SD.Free;
    end;

  if (FileExistsUTF8(FN)) and
     (not ST.HasOption('replace'))
  then
    begin
      FExecutor.Error('File exists.' + LineEnding + 'Add !replace or erase file:' + LineEnding + FN);
      ST.ExecResult := csrFailed;
      Exit;
    end;

  FS := nil;
  try
    FS := TFileStream.Create(FN, fmCreate);

    if OutputType = 'html' then
      begin
        OutputGenerator := TOutputGeneratorHTML.Create(FOutputCreator, FS);
        TOutputGeneratorHTML(OutputGenerator).CSSFileName := FExecutor.SetOptionValue[ANA_SO_OUTPUT_CSS_FILE];
      end
    else
      OutputGenerator := TOutputGeneratorTXT.Create(FOutputCreator, FS);
    OutputGenerator.GenerateReport;
    OutputGenerator.Free;
  except
    on E: Exception do
      begin
        FExecutor.Error('Unable to save output!' + LineEnding +
                        E.Message);
        ST.ExecResult := csrFailed;
        FS.Free;
        Exit;
      end;
  end;
  FS.Free;

  FOutputCreator.DoInfoShort('Output saved: ' + FN);
  ST.ExecResult := csrSuccess;
end;

end.

