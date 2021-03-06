unit Symbol;

{$MODE Delphi}

{
'================================================================================
' Class Name:
'      Symbol
'
' Instancing:
'      Public; Non-creatable  (VB Setting: 2- PublicNotCreatable)
'
' Purpose:
'       This class is used to store of the nonterminals used by the Deterministic
'       Finite Automata (DFA) and LALR Parser. Symbols can be either
'       terminals (which represent a class of tokens - such as identifiers) or
'       nonterminals (which represent the rules and structures of the grammar).
'       Terminal symbols fall into several catagories for use by the GOLD Parser
'       Engine which are enumerated below.
'
' Author(s):
'      Devin Cook
'      GOLDParser@DevinCook.com
'
' Dependacies:
'      (None)
'
'================================================================================
 Conversion to Delphi:
      Beany
      Beany@cloud.demon.nl

 Conversion status: Done, not tested
}

interface

uses
   Classes, contnrs;


//const

type
  TGoldSymbolKind = (

   SymbolTypeNonterminal,     // Normal nonterminal
   SymbolTypeTerminal,        // Normal terminal
   SymbolTypeWhitespace,      // This Whitespace symbols is a special terminal
                              // that is automatically ignored the the parsing engine.
                              // Any text accepted as whitespace is considered
                              // to be inconsequential and "meaningless".
   SymbolTypeEnd,             // The End symbol is generated when the tokenizer
                              // reaches the end of the source text.
   SymbolTypeCommentStart,    // This type of symbol designates the start of a block quote.
   SymbolTypeCommentEnd,      // This type of symbol designates the end of a block quote.
   SymbolTypeCommentLine,     // When the engine reads a token that is recognized as
                              // a line comment, the remaining characters on the line
                              // are automatically ignored by the parser.
   SymbolTypeError            // The Error symbol is a general-purpose means
                              // of representing characters that were not recognized
                              // by the tokenizer. In other words, when the tokenizer
                              // reads a series of characters that is not accepted
                              // by the DFA engine, a token of this type is created.
   );

type

   kQuotedChars = set of Char;


   { TSymbol }

   TSymbol = class
   private
      FName: UTF8String;
      FKind: TGoldSymbolKind;
      FTableIndex: Integer;
      function GetText: UTF8String;
      function PatternFormat(Source: UTF8String): UTF8String;
   public
      constructor Create(aTableIndex : integer; aName : UTF8String; aKind : TGoldSymbolKind);
      property Kind: TGoldSymbolKind read FKind;
      // Returns an enumerated data type that denotes
      // the class of symbols that the object belongs to.
      property TableIndex: Integer read FTableIndex;
      // Returns the index of the symbol in the GOLDParser object's Symbol Table.
      property Name: UTF8String read FName;
      // Returns the name of the symbol.
      property Text: UTF8String read GetText;
      // Returns the text representation of the symbol.
      // In the case of nonterminals, the name is delimited by angle brackets,
      // special terminals are delimited by parenthesis
      // and terminals are delimited by single quotes (if special characters are present).
   end;

  TSymbolTable = class
  private
    FList : TObjectList;
    function GetCount: integer;
    function GetItem(Index: integer): TSymbol;
    procedure SetItem(Index: integer; const Value: TSymbol);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Add(Value : TObject);
    procedure Clear;

    property Count : integer read GetCount;
    property Items[Index : integer] : TSymbol read GetItem write SetItem; default;
  end;

implementation

   const pkQuotedChars = ['|', '-', '+', '*', '?', '(', ')', '[', ']', '{', '}', '<', '>', '!'];

//In test condition:
//The number of Symbols created = the number of symbols destroyed.
constructor TSymbol.Create(aTableIndex: integer; aName: UTF8String;
  aKind: TGoldSymbolKind);
begin
  inherited Create;
  FTableIndex := aTableIndex;
  FName := aName;
  FKind := aKind;
end;

function TSymbol.GetText: UTF8String;
begin
  case Kind of
    SymbolTypeNonterminal: Result := '<' + Name + '>';
    SymbolTypeTerminal: Result := PatternFormat(Name);
  else
    Result := '(' + Name + ')';
  end;
end;

function TSymbol.PatternFormat(Source: UTF8String): UTF8String;
var i : integer;
begin
  for i := 1 to Length(Source) do
    if Source[i] = '''' then  Result := Result + ''''''
    else if (Source[i] in pkQuotedChars) or (Source[i] = '"')
         then Result := Result + '''' + Source[i] + '''';
end;

{ TSymbolTable }

procedure TSymbolTable.Add(Value: TObject);
begin
  FList.Add(Value);
end;

procedure TSymbolTable.Clear;
begin
  FList.Clear;
end;

constructor TSymbolTable.Create;
begin
  inherited;
  FList := TObjectList.Create(True);
end;

destructor TSymbolTable.Destroy;
begin
  FList.Free;
  inherited;
end;

function TSymbolTable.GetCount: integer;
begin
  Result := Flist.Count;
end;

function TSymbolTable.GetItem(Index: integer): TSymbol;
begin
  // TODO: testing -1 as "starting" symbol.
  if Index = -1 then
    result := nil
  else
    Result := FList[Index] as TSymbol;
end;

procedure TSymbolTable.SetItem(Index: integer; const Value: TSymbol);
begin
  if Index >= Count then FList.Count := Index + 1;
  if Assigned(Items[Index]) then Items[Index].Free;
  FList[Index] := Value;
end;

end.
