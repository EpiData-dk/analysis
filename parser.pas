unit parser;

{$codepage UTF-8}
{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, executor, ast, GOLDParser, Token, parser_types;

type

  { TParser }

  TParser = class
  private
    FGoldParser: TGOLDParser;
    FExecutor: IEpiScriptExecutor;
  public
    constructor Create(Executor: TExecutor);
    function ParseFile(Const Fn: UTF8String; out TheProgram: TProgram): boolean; virtual;
    function ParseText(Const S: UTF8String; out TheProgram: TProgram): boolean; virtual;
    property GoldParser: TGOLDParser read FGoldParser;
  // Events
  private
    FOnCommentError: TCommentError;
    FOnLexError: TLexError;
    FOnParseResponse: TParserResponseEvent;
    FOnSyntaxError: TSyntaxError;
    FOnASTBuildError: TASTBuildError;
    procedure SetOnCommentError(AValue: TCommentError);
    procedure SetOnLexError(AValue: TLexError);
    procedure SetOnSyntaxError(AValue: TSyntaxError);
    procedure SetOnASTBuildError(AValue: TASTBuildError);
  protected
    procedure DoOnCommentError();
    procedure DoOnLexError();
    procedure DoOnSyntaxError();
    procedure DoOnASTBuildError(Const MSG: UTF8String; ErrorToken: TToken);
    function  DoOnParseResponse(Const Response: TGoldParserMessage; var StopParsing: boolean): Boolean;
  public
    property OnParseResponse: TParserResponseEvent read FOnParseResponse write FOnParseResponse;
    property OnLexError: TLexError read FOnLexError write SetOnLexError;
    property OnSyntaxError: TSyntaxError read FOnSyntaxError write SetOnSyntaxError;
    property OnCommentError: TCommentError read FOnCommentError write SetOnCommentError;
    property OnASTBuildError: TASTBuildError read FOnASTBuildError write SetOnASTBuildError;
  end;

  { TOptionListParser }

  TOptionListParser = class(TParser)
  private
    FInternalResult: Boolean;
    FInternalOptionList: TOptionList;
  private
    procedure InternalParseResponse(Sender: TObject; const Response: TGoldParserMessage; var StopParsing, handled: boolean);
    procedure InternalLexError(Sender: TObject; ErrorToken: TToken);
    procedure InternalCommentError(Sender: TObject; ErrorToken: TToken);
    procedure InternalSyntaxError(Sender: TObject; ErrorToken: TToken;
      TokenTable: TTokenStack);
  public
    constructor Create(Executor: IEpiScriptExecutor);
    function ParseText(const S: UTF8String; out OptionList: TOptionList): boolean; overload;
  end;

implementation

uses
  LazUTF8Classes, ast_builder, Symbol;

{$I EpiDataAnalysisGrammar.inc}
{$I EpiDataAnalysisOptionListGrammar.inc}

{ TParser }

constructor TParser.Create(Executor: TExecutor);
var
  MS: TMemoryStream;
begin
  FExecutor := Executor;

  MS := TMemoryStream.Create;
  MS.Write(EpiDataAnalysisGrammar[0], SizeOf(EpiDataAnalysisGrammar));
  MS.Position := 0;

  FGoldParser := TGOLDParser.Create;
  FGoldParser.LoadCompiledGrammar(MS);

  MS.Free;
end;

function TParser.ParseFile(const Fn: UTF8String; out TheProgram: TProgram
  ): boolean;
var
  SL: TStringListUTF8;
begin
  SL := TStringListUTF8.Create;
  SL.LoadFromFile(Fn);
  result := ParseText(SL.Text, TheProgram);
  SL.Free;
end;

function TParser.ParseText(const S: UTF8String; out TheProgram: TProgram
  ): boolean;
var
  ABuild: TASTBuilder;
  Done, Handled: Boolean;
  Response: TGoldParserMessage;
  FunctionList: TFunctionList;
begin
  Done := false;
  result := false;

  FGoldParser.OpenTextString(S);
  while (not Done) do
  begin
    Response := FGoldParser.Parse;

    if DoOnParseResponse(Response, Done) then
      Continue;

    case Response of
      gpMsgAccept:
        begin
          ABuild := TASTBuilder.Create(FExecutor);
          result := ABuild.BuildAST(FGoldParser.CurrentReduction, TheProgram);

          if not result then
            begin
              DoOnASTBuildError(ABuild.ErrorMsg, ABuild.ErrorToken);
              result := false;
              ABuild.Free;
              exit;
            end;

          ABuild.Free;
          Done := true;
        end;

      gpMsgCommentError:
        begin
          DoOnCommentError();
          Done := true;
        end;

      gpMsgNotLoadedError:
        Done := true;

      gpMsgLexicalError:
        begin
          DoOnLexError();
          Done := true;
        end;

      gpMsgSyntaxError:
        begin
          DoOnSyntaxError();
          Done := true;
        end;

      gpMsgInternalError:
        Done := true;
    end;
  end;
end;

procedure TParser.SetOnCommentError(AValue: TCommentError);
begin
  if FOnCommentError = AValue then Exit;
  FOnCommentError := AValue;
end;

procedure TParser.SetOnLexError(AValue: TLexError);
begin
  if FOnLexError = AValue then Exit;
  FOnLexError := AValue;
end;

procedure TParser.SetOnSyntaxError(AValue: TSyntaxError);
begin
  if FOnSyntaxError = AValue then Exit;
  FOnSyntaxError := AValue;
end;

procedure TParser.SetOnASTBuildError(AValue: TASTBuildError);
begin
  if FOnASTBuildError = AValue then Exit;
  FOnASTBuildError := AValue;
end;

procedure TParser.DoOnCommentError();
begin
  if Assigned(OnCommentError) then
    OnCommentError(Self, FGoldParser.CurrentToken);
end;

procedure TParser.DoOnLexError();
begin
  if Assigned(OnLexError) then
    OnLexError(Self, FGoldParser.CurrentToken);
end;

procedure TParser.DoOnSyntaxError();
begin
  if Assigned(OnSyntaxError) then
    OnSyntaxError(Self, FGoldParser.CurrentToken, FGoldParser.TokenTable);
end;

procedure TParser.DoOnASTBuildError(const MSG: UTF8String; ErrorToken: TToken);
begin
  if Assigned(OnASTBuildError) then
    OnASTBuildError(Self, Msg, ErrorToken);
end;

function TParser.DoOnParseResponse(const Response: TGoldParserMessage;
  var StopParsing: boolean): Boolean;
begin
  result := false;
  if Assigned(OnParseResponse) then
    OnParseResponse(Self, Response, StopParsing, Result);
end;

{ TOptionListParser }

procedure TOptionListParser.InternalParseResponse(Sender: TObject;
  const Response: TGoldParserMessage; var StopParsing, handled: boolean);
var
  ABuild: TASTBuilder;
  result: Boolean;
begin
  if (Response = gpMsgAccept) then
    begin
      handled := true;
      StopParsing := true;

      ABuild := TASTBuilder.Create(FExecutor);
      result := ABuild.BuildOptionList(FGoldParser.CurrentReduction, FInternalOptionList);

      if not result then
        begin
          DoOnASTBuildError(ABuild.ErrorMsg, ABuild.ErrorToken);
          FInternalResult := false;
          ABuild.Free;
        end;

      FInternalResult := true;
      ABuild.Free;
    end;
end;

procedure TOptionListParser.InternalLexError(Sender: TObject; ErrorToken: TToken
  );
begin
  FInternalResult := false;
end;

procedure TOptionListParser.InternalCommentError(Sender: TObject;
  ErrorToken: TToken);
begin
  FInternalResult := false;
end;

procedure TOptionListParser.InternalSyntaxError(Sender: TObject;
  ErrorToken: TToken; TokenTable: TTokenStack);
begin
  FInternalResult := false;
end;

constructor TOptionListParser.Create(Executor: IEpiScriptExecutor);
var
  MS: TMemoryStream;
begin
  FInternalResult := true;
  FExecutor := Executor;

  MS := TMemoryStream.Create;
  MS.Write(EpiDataAnalysisOptionListGrammar[0], SizeOf(EpiDataAnalysisOptionListGrammar));
  MS.Position := 0;

  FGoldParser := TGOLDParser.Create;
  FGoldParser.LoadCompiledGrammar(MS);

  MS.Free;

  OnParseResponse := @InternalParseResponse;
  OnCommentError  := @InternalCommentError;
  OnLexError      := @InternalLexError;
  OnSyntaxError   := @InternalSyntaxError;
end;

function TOptionListParser.ParseText(const S: UTF8String; out
  OptionList: TOptionList): boolean;
var
  Dummy: TProgram;
begin
  inherited ParseText(S, Dummy);
  OptionList := FInternalOptionList;
  Result := FInternalResult;
end;

end.

