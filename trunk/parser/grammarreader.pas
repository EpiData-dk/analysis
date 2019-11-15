unit GrammarReader;

{$MODE Delphi}

interface

uses
   Classes, SysUtils, Dialogs, Variables, Symbol, Rule, FAState, LRAction,
   GOLDParser;

const

   EntryContentEmpty = 69;
   EntryContentInteger = 73;
   EntryContentString = 83;
   EntryContentBoolean = 66;
   EntryContentByte = 98;

   RecordIdProperty     = 112;   //p   (version 5.0 addtition)
   RecordIdParameters   =  80;   //P

   RecordIdTableCounts  =  84;   //T
   RecordIdTableCounts5 = 116;   //t   (version 5.0 addtition)

   RecordIdInitial      =  73;   //I

   RecordIdSymbols      =  83;   //S

   RecordIdGroups       = 103;   //g   (version 5.0 addtition)

   RecordIdCharSets     =  67;   //C
   RecordIdCharSets5    =  99;   //c   (version 5.0 addtition)

   RecordIdRules        =  82;   //R

   RecordIdDFAStates    =  68;   //D

   RecordIdLRTables     =  76;   //L

   RecordIdComment      =  33;   //!

type
  TGOLDVersion = (
    gvCGT,       // 'GOLD Parser Tables/v1.0';
    gvEGT        // 'GOLD Parser Tables/v5.0';
  );

  { TGrammarReader }

  TGrammarReader = class
  private
    FBufferPos: Integer;
    FBuffer: UTF8String;
    FCurrentRecord: Variant;
    FEntryPos: Integer;
    FEntryCount: Integer;
    FStartSymbol: TSymbol;
    FParser : TGoldParser;
    FVersion: TGOLDVersion;
    function ReadUniString: UTF8String;
    function ReadInt16: Integer;
    function ReadByte: Char;
    function ReadEntry: Variant;
    function OpenFile(const FileName: UTF8String): boolean;
    function OpenStream(const Stream : TStream) : boolean;
  protected
    function DoLoadTables : boolean;
  public
    constructor Create(aParser : TGoldParser);
    destructor Destroy; override;
    function GetNextRecord: Boolean;
    function RetrieveNext: Variant;
    function LoadTables(const FileName : UTF8String): boolean; overload;
    function LoadTables(const Stream : TStream): boolean; overload;
    property Buffer: UTF8String read FBuffer;
    property StartSymbol: TSymbol read FStartSymbol write FStartSymbol;
    property Parser : TGOLDParser read FParser;
    property Version: TGOLDVersion read FVersion;
  end;

implementation

uses
  Variants, LazUTF8Classes, LazUTF8, strutils;
  
const
   GoldHeaderV1: UTF8String = 'GOLD Parser Tables/v1.0';
   GoldHeaderV5: UTF8String = 'GOLD Parser Tables/v5.0';

constructor TGrammarReader.Create(aParser : TGoldParser);
begin
  inherited Create;
  FParser := aParser;
end;

destructor TGrammarReader.Destroy;
begin
  inherited Destroy;
end;

function TGrammarReader.OpenFile(const FileName: UTF8String): boolean;
var FS : TFileStream;
begin
  try
    FS := TFileStreamUTF8.Create(Filename, fmOpenRead);
    try
      SetLength(FBuffer, FS.Size);
      FS.ReadBuffer(FBuffer[1], FS.Size);
      Result := True;
      FBufferPos := 1;
    finally
      FS.Free;
    end;
  except
    Result := False;
    FBufferPos := -1;
  end;
end;

function TGrammarReader.GetNextRecord: Boolean;
var TypeOfRecord: char;
    i, Entries : Integer;
begin
  Result := False;
  TypeOfRecord := ReadByte;
   //Structure below is ready for future expansion
  case TypeOfRecord of
  'M': begin
         //Read the number of entry's
         Entries := ReadInt16;
         VarClear(FCurrentRecord);
         FCurrentRecord := VarArrayCreate([1, Entries], varVariant);
         FEntryCount := Entries;
         FEntryPos := 1;
         for i := 1 to Entries do FCurrentRecord[i] := ReadEntry;
         Result := True;
       end;
  end;
end;

function TGrammarReader.ReadUniString: UTF8String;
var
  uchr: UnicodeChar;
  ustr: UnicodeString;
begin
  ustr := '';
  uchr := UnicodeChar(ReadInt16);
  while (uchr <> #0) do begin
    ustr := ustr + uchr;
    uchr := UnicodeChar(ReadInt16);
  end;
  result := UTF16ToUTF8(ustr);
end;

function TGrammarReader.ReadInt16: Integer;
begin
  Result := ord(FBuffer[FBufferPos]) + ord(FBuffer[FBufferPos + 1]) * 256;
  FBufferPos := FBufferPos + 2;
end;

function TGrammarReader.ReadByte: Char;
begin
  Result := FBuffer[FBufferPos];
  Inc(FBufferPos);
end;

function TGrammarReader.ReadEntry: Variant;
var EntryType: Char;
begin
  EntryType := ReadByte;
  case Ord(EntryType) of
  EntryContentEmpty     :  Result := varEmpty;
  EntryContentInteger   :  Result := ReadInt16;
  EntryContentBoolean   :  begin
                             Result := ReadByte;
                             if Result = #1 then Result := True else Result := False;
                           end;
  EntryContentString    :  Result := ReadUniString;
  EntryContentByte      :  Result := ReadByte;
  end;
end;

function TGrammarReader.RetrieveNext: Variant;
begin
  if FEntryPos <= FEntryCount then begin
    Result := FCurrentRecord[FEntryPos];
    Inc(FEntryPos);
  end else Result := varEmpty;
end;

function TGrammarReader.DoLoadTables: boolean;
var Id : UTF8String;
    iDummy1, iDummy2, iDummy3, i , j: integer;
    strDummy : UTF8String;
    NewSymbol : TSymbol;
    NewRule : TRule;
    NewFAState : TFAState;
    bAccept : boolean;
    NewActionTable : TLRActionTable;
    iDummySymb: TGoldSymbolKind;
begin
  FStartSymbol := TSymbol.Create(-1, 'START SYMBOL', SymbolTypeNonterminal);

  Parser.VariableList.Add('Name', '');
  Parser.VariableList.Add('Version', '');
  Parser.VariableList.Add('Author', '');
  Parser.VariableList.Add('About', '');
  Parser.VariableList.Add('Case Sensitive', '');
  Parser.VariableList.Add('Start Symbol', '');

  Result := False;

  strDummy := ReadUniString;

  if strDummy = GoldHeaderV1 then
    FVersion := gvCGT
  else if strDummy = GoldHeaderV5 then
    FVersion := gvEGT
  else
    Exit;

  while (FBufferPos < Length(FBuffer)) do begin
    Result := GetNextRecord;
    Id := UTF8String(RetrieveNext);
    case Ord(Id[1]) of
      RecordIdProperty: begin
        case RetrieveNext of
          0, // Name
          1, // Version
          2, // Author
          3: // About
             begin
               strDummy := RetrieveNext;
               Parser.VariableList.Value[strDummy] := RetrieveNext;

             end;

          4, // CharacterSet - always "unicode"
          5, // CharacterMapping - not used
          6, // Generated by
          7: // Generated date
             begin
               RetrieveNext;
               RetrieveNext;
             end;
        end;
      end;

      RecordIdParameters : begin
        Parser.VariableList.Value['Name'] := RetrieveNext;
        Parser.VariableList.Value['Version'] := RetrieveNext;
        Parser.VariableList.Value['Author'] := RetrieveNext;
        Parser.VariableList.Value['About'] := RetrieveNext;
        Parser.VariableList.Value['Case Sensitive'] := RetrieveNext;
        Parser.VariableList.Value['Start Symbol'] := RetrieveNext;
      end;

      RecordIdTableCounts,
      RecordIdTableCounts5: begin
        RetrieveNext; // for i := 0 to RetrieveNext - 1 do Parser.SymbolTable.Add(nil);
        for i := 0 to RetrieveNext do Parser.CharacterSetTable.Add('');
        RetrieveNext; // for i := 0 to RetrieveNext do Parser.RuleTable.Add(nil);
        RetrieveNext; // for i := 0 to RetrieveNext do Parser.DFA.Add(nil);
        RetrieveNext; // for i := 0 to RetrieveNext do Parser.ActionTable.Add(nil, 0, 0);

        if (Version = gvEGT) then
          RetrieveNext;  // GroupTable counts
      end;

      RecordIdInitial : begin
        Parser.InitialDFAState := RetrieveNext;
        Parser.InitialLALRState := RetrieveNext;
      end;

      RecordIdSymbols : begin
        iDummy1 := RetrieveNext;
        strDummy := RetrieveNext;
        iDummySymb := TGoldSymbolKind(RetrieveNext);

        NewSymbol := TSymbol.Create(iDummy1, strDummy, iDummySymb);
        Parser.SymbolTable.Items[NewSymbol.TableIndex] := NewSymbol;
      end;

      RecordIdGroups: begin
        // TODO: NEeds implementation
      end;

      RecordIdCharSets: begin
        iDummy1 := RetrieveNext;
        Parser.CharacterSetTable.Strings[iDummy1] := RetrieveNext;
      end;

      RecordIdCharSets5: begin
        iDummy1 := RetrieveNext;  // Index
        iDummy2 := RetrieveNext;  // Unicode Plane
        iDummy3 := RetrieveNext;  // Range cound
        RetrieveNext;             // Reserved = empty.

        strDummy := '';
        for i := 0 to iDummy3 - 1 do
        begin
          iDummy2 := RetrieveNext;
          for j := iDummy2 to RetrieveNext do
            strDummy := strDummy + UTF16ToUTF8(WideChar(j));
        end;
        Parser.CharacterSetTable.Strings[iDummy1] := strDummy;
      end;

      RecordIdRules : begin
        iDummy1 := RetrieveNext;
        NewRule := TRule.Create(iDummy1, Parser.SymbolTable.Items[RetrieveNext]);
        RetrieveNext;
        while FEntryPos <= FEntryCount do
          NewRule.AddItem(Parser.SymbolTable.Items[RetrieveNext]);
        Parser.RuleTable.Items[NewRule.TableIndex] := NewRule;
      end;

      RecordIdDFAStates : begin
        NewFAState := TFAState.Create;
        iDummy1 := RetrieveNext;
        bAccept := RetrieveNext;
        if bAccept then NewFAState.AcceptSymbol := RetrieveNext
        else begin
          NewFAState.AcceptSymbol := -1;
          RetrieveNext;
        end;
        RetrieveNext;
        while FEntryPos <= FEntryCount do begin
          strDummy := RetrieveNext;
          iDummy2 := RetrieveNext;
          NewFAState.AddEdge(strDummy, iDummy2);
          RetrieveNext;
        end;
        Parser.DFA.Items[iDummy1] := NewFAState;
      end;

      RecordIdLRTables : begin
        NewActionTable := TLRActionTable.Create;
        i := RetrieveNext;
        RetrieveNext;
        while FEntryPos <= FEntryCount do begin
          iDummy1 := RetrieveNext;
          iDummy2 := RetrieveNext;
          iDummy3 := RetrieveNext;
          NewActionTable.Add(Parser.SymbolTable[iDummy1], iDummy2, iDummy3);
          RetrieveNext;
        end;
        Parser.ActionTables.Items[i] := NewActionTable;
      end;

    end;
  end;
end;

function TGrammarReader.LoadTables(const Stream: TStream): boolean;
begin
  Result := OpenStream(Stream) and DoLoadTables;
end;

function TGrammarReader.LoadTables(const FileName: UTF8String): boolean;
begin
  Result := OpenFile(FileName) and DoLoadTables;
end;

function TGrammarReader.OpenStream(const Stream: TStream): boolean;
begin
  try
    SetLength(FBuffer, Stream.Size);
    Stream.ReadBuffer(FBuffer[1], Stream.Size);

    Result := True;
    FBufferPos := 1;
  except
    Result := False;
    FBufferPos := -1;
  end;
end;

end.

