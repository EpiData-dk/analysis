unit parser_types;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Token, GOLDParser;

type

  TLexError            = procedure(Sender: TObject; ErrorToken: TToken) of object;
  TSyntaxError         = procedure(Sender: TObject; ErrorToken: TToken; TokenTable: TTokenStack) of object;
  TCommentError        = procedure(Sender: TObject; ErrorToken: TToken) of object;
  TASTBuildError       = procedure(Sender: TObject; Const Msg: UTF8String; ErrorToken: TToken) of object;
  TParserResponseEvent = procedure(Sender: TObject; Const Response: TGoldParserMessage;
                                   var StopParsing, handled: boolean) of object;

implementation

end.

