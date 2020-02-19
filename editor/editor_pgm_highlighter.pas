unit editor_pgm_highlighter;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, SynHighlighterAny;

type

  { TPGMHighLighter }

  TPGMHighLighter = class(TSynAnySyn)

  public
    constructor Create(AOwner: TComponent); override;
  end;

implementation

uses
  Graphics;

const
  PGM_CONSTANTS: array[1..3] of string = (
    'FALSE',
    'PI',
    'TRUE'
  );

  PGM_KEYWORDS: array[1..19] of string = (
    'AND',
    'BEGIN',
    'DIV',
    'DO',
    'DOWNTO',
    'ELSE',
    'END',
    'FOR',
    'IF',
    'IN',
    'MOD',
    'NOT',
    'OR',
    'SELECT',
    'SHL',
    'SHR',
    'THEN',
    'TO',
    'XOR'
  );

  PGM_COMMANDS: array[1..31] of string = (
    'APPEND',
    'ASSERT',
    'BROWSE',
    'CD',
    'CHECK',
    'CLH',
    'CLOSE',
    'CLS',
    'COUNT',
    'DESCRIBE',
    'DIR',
    'DROP',
    'EDIT',
    'FREQ',
    'LIST',
    'LS',
    'MEANS',
    'MERGE',
    'NEW',
    'QUIT',
    'READ',
    'RECODE',
    'REORDER',
    'RESET',
    'RUN',
    'RUNTEST',
    'SAVE',
    'SET',
    'SORT',
    'USE',
    'VERSION'
  );

{ TPGMHighLighter }

constructor TPGMHighLighter.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  ActiveDot := false;
  CommentAttri.Foreground := clBlue;
  Comments := [csCStyle];
  Constants.AddStrings(PGM_CONSTANTS);
  DetectPreprocessor := false;
  DollarVariables := false;
  Entity := false;
  IdentifierChars := '0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ_abcdefghijklmnopqrstuvwxyz';
  KeyWords.AddStrings(PGM_KEYWORDS);
  Markup := false;
  NumberAttri.Foreground := clNavy;
  Objects.AddStrings(PGM_COMMANDS);
  StringAttri.Foreground := clBlue;
  StringDelim := sdDoubleQuote;
  SymbolAttri.Foreground := clRed;
end;

end.

