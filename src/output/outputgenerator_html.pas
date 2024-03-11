unit outputgenerator_html;

{$codepage UTF8}
{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, outputgenerator_base, epireport_generator_html, outputcreator;

type

  { TOutputGeneratorHTML }

  TOutputGeneratorHTML = class(TOutputGeneratorBase)
  private
    FCSSFileName: UTF8String;
    FEmbedCSSFile: boolean;
    FHtmlText: UTF8String;
    procedure SetCSSFileName(AValue: UTF8String);
    function  TextToHtml(IText: IOutputText): UTF8String;
  protected
    procedure TextOut(const S: UTF8String; TextFormat: TTextFormat); override;
  protected
    procedure DoOutputTable(Table: TOutputTable); override;
    procedure DoOutputLine(Line: TOutputLine); override;
  protected
    function GetDialogFilter: UTF8String; override;
  public
    procedure GenerateReport; override;
    property CSSFileName: UTF8String read FCSSFileName write SetCSSFileName;
    property EmbedCSSFile: boolean read FEmbedCSSFile write FEmbedCSSFile;
  end;

const
  HTML_OUTPUT_CSS =
        '<STYLE type="text/css">' + LineEnding +
        '<!--' + LineEnding +
        '/* EpiData Reporting Minimalistic style sheet - white background' + LineEnding +
        '   v1.0' + LineEnding +
        '   Use the design table.system as a template for a new design. To be safe, define all styles for a design.' + LineEnding +
        '   Note that a style followed by a comma will take the attributes at the end of the group, so do not sort this file.' + LineEnding +
        '*/' + LineEnding +
        '' + LineEnding +
        '  .body {color: black; background-color: white;  font-size: 1.0em; font-weight: normal}' + LineEnding +
        '' + LineEnding +
        '   p {color: black ;font-size: 1.0em; font-family: proportional,monospace; font-weight: normal; margin: 0em }' + LineEnding +
        '  h1 {color: blue; font-size: 1.25em; font-family: proportional,monospace; font-weight: bold}' + LineEnding +
        '  h2 {color: blue; font-size: 1.20em; font-family: proportional,monospace; font-weight: bold}' + LineEnding +
        '  h3 {color: blue; font-size: 1.15em; font-family: proportional,monospace; font-weight: bold}' + LineEnding +
        '  hr.line {  border-top: 1px solid black; }' + LineEnding +
        '  .small {color: black; font-size: 0.85em; font-family: proportional,monospace; font-weight: normal}' + LineEnding +
        '' + LineEnding +
        '  .command {color: black; font-size: 0.85em; font-weight: normal; font-family: monospace}' + LineEnding +
        '  .warning {color: black; font-size: 0.85em; font-weight: normal; font-family: monospace}' + LineEnding +
        '  .info {color: green; font-size: 1.0em; font-weight: normal; font-family: monospace ;}' + LineEnding +
        '  .error {color: red; font-family: monospace}' + LineEnding +
        '' + LineEnding +
        'table.simple  {color: black; font-size: 1.0em; font-family: proportional,monospace; font-weight: normal; border-left: solid 2px black; border-right: solid 2px black; border-bottom: solid 2px black; border-spacing: 0; margin-top: 1.25cm; }' + LineEnding +
        'table.simple th,' + LineEnding +
        'table.simple tr {padding: 0.2em}' + LineEnding +
        'table.simple td {text-align: right; vertical-align: top; padding: 0.2em}' + LineEnding +
        'table.simple .cell {text-align: left; vertical-align: top; padding: 0.2em;}' + LineEnding +
        '' + LineEnding +
        'table.simple .cellfoot {border-top: solid 2px black; font-size: 0.8em; text-align: left;}' + LineEnding +
        'table.simple .caption {font-size: 1.1em; font-weight: bold; border-bottom: 2px solid black; text-align: center;}' + LineEnding +
        '' + LineEnding +
        'table.simple .firstrow {font-weight: bold; text-align: center; padding-right: 0.4em }' + LineEnding +
        'table.simple .firstcol {font-weight: bold; text-align: right; padding-right: 0.4em}' + LineEnding +
        '-->' + LineEnding +
        '</STYLE>' + LineEnding;

implementation

uses
  strutils, LazFileUtils;

{ TOutputGeneratorHTML }

function TOutputGeneratorHTML.TextToHtml(IText: IOutputText): UTF8String;
begin
  FHtmlText := '';
  ProcessIText(IText);
  Result := FHtmlText;
end;

procedure TOutputGeneratorHTML.SetCSSFileName(AValue: UTF8String);
begin
  if FCSSFileName = AValue then Exit;

  FCSSFileName := '';
  if (FileExistsUTF8(FCSSFileName)) then
    Exit;

  FCSSFileName := AValue;
end;

procedure TOutputGeneratorHTML.TextOut(const S: UTF8String;
  TextFormat: TTextFormat);
var
  T: String;
begin
  T := StringsReplace(S, [LineEnding], ['<br>'], [rfReplaceAll]);

  case TextFormat of
    tfNormal:
      FHtmlText += T;

    tfLink:
      FHtmlText += '<a href="">' + T + '</a>';

    tfBold:
      FHtmlText += '<b>' + T + '</b>';

    tfUnderline:
      FHtmlText += '<u>' + T + '</u>';

    tfItalic:
      FHtmlText += '<i>' + T + '</i>';

    tfSubscript:
      FHtmlText += '<sub>' + T + '</sub>';

    tfSuperscript:
      FHtmlText += '<sup>' + T + '</sup>';
  end;
end;

procedure TOutputGeneratorHTML.DoOutputTable(Table: TOutputTable);
var
  C: TOutputTableCell;
  B: TOutputTableCellBorder;
  Row, Col: Integer;
  S, Style: UTF8String;
begin
  WriteToStream(
    '<table cellspacing=0>' + LineEnding +
//    '<table cellspacing=0 class=simple>' + LineEnding +
    '<caption class=caption>' + Table.Header.Text + '</caption>' + LineEnding
  );

  for Row := 0 to Table.RowCount - 1 do
  begin
    WriteToStream('<tr>');

    for Col := 0 to Table.ColCount - 1 do
    begin
      WriteToStream('<td');

      // Class
      Style := '';

      // Alignment
      C := Table.Cell[Col, Row];
      case C.Alignment of
        taLeftJustify:  Style += ' text-align: left;';
        taRightJustify: Style += ' text-align: right;';
        taCenter:       Style += ' text-align: center;'
      end;

      // Borders
      if C.Borders = cbAll then
        Style += ' border-width: 1px; border-style: solid;'
      else
        for B in C.Borders do
          case B of
            cbLeft:   Style += ' border-left-width: 1px;   border-left-style: solid;';
            cbTop:    Style += ' border-top-width: 1px;    border-top-style: solid;';
            cbRight:  Style += ' border-right-width: 1px;  border-right-style: solid;';
            cbBottom: Style += ' border-bottom-width: 1px; border-bottom-style: solid;';
          end;

      if Style <> '' then
        WriteToStream(' style="' + Style + '"');

      if C.Text <> '' then
        WriteToStream('>' + TextToHtml(C) + '</td>' + LineEnding)
      else
        WriteToStream('>&nbsp;</td>' + LineEnding);
    end;

    WriteToStream('</tr>' + LineEnding);
  end;

    if (Table.Footer <> nil) then
    begin
      S := TextToHtml(Table.Footer);
      WriteToStream('<tfoot><td colspan="' + Table.ColCount.ToString + '">' + S + '&nbsp;</td></tfoot>'); // most appropriate style ?
    end;

    WriteToStream('</tbody>');

  WriteToStream('</table>');

end;

procedure TOutputGeneratorHTML.DoOutputLine(Line: TOutputLine);
var
  cl: UTF8String;
begin
  case Line.LineType of
    oltNormal:  cl := 'line';
    oltCommand: cl := 'command';
    oltError:   cl := 'error';
    oltWarning: cl := 'warning';
    oltInfo:    cl := 'info';
  end;

  WriteToStream('<hr class="' + cl + '">' + TextToHtml(Line)); // Requested by Jens (Email of 2020-05-06)
//  WriteToStream('<p class="' + cl + '">' + TextToHtml(Line) + '</p>');
end;

function TOutputGeneratorHTML.GetDialogFilter: UTF8String;
begin
  result := 'HTML Output|*.html';
end;

procedure TOutputGeneratorHTML.GenerateReport;
var
  i: Integer;
  CSSStrings: TStringList;
begin
  WriteToStream(
    '<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.01 Transitional//EN">' + LineEnding +
    '<HTML>' + LineEnding +
    '<Head>' + LineEnding +
    '' + LineEnding +
    '<meta name="Copyright" content="EpiData Association, Denmark">' + LineEnding +
    '<meta name="No_Payment" content="EpiData Analysis is freeware">' + LineEnding +
    '<meta name="Update_from:" content="Http://www.epidata.dk">' + LineEnding +
    '<meta name="Disclaimer" content="Http://www.epidata.dk/disclaim.htm">' + LineEnding +
    '<meta http-equiv="Content-Type" content="text/html; charset=UTF-8">' + LineEnding +
    '' + LineEnding
  );

  if (FCSSFileName <> '') then
    begin
      if (FEmbedCSSFile) then
        begin
          CSSStrings := TStringList.Create;
          CSSStrings.LoadFromFile(FCSSFileName);
          WriteToStream(CSSStrings.Text);
        end
      else
        WriteToStream('<link rel="stylesheet" href="' + FCSSFileName +'">');
    end
  else
    WriteToStream(HTML_OUTPUT_CSS);
  WriteToStream(
    '<TITLE>  </TITLE>' + LineEnding +
    '</HEAD>' + LineEnding +
    '<BODY class=body>' + LineEnding
  );

  inherited GenerateReport;

  WriteToStream(
    '</BODY>' + LineEnding +
    '</HTML>'
  );
end;

end.

