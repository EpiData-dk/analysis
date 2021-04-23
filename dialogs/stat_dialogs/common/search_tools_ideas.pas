from entry -- unit epitools_search;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, epidatafiles;

type

  TSearchBinOp = (boAnd, boOr);
  TMatchCriteria = (
    mcEq,
    mcNEq,
    mcLEq,
    mcLT,
    mcGT,
    mcGEq,
    mcBegin,
    mcEnd,
    mcContains,
    mcIsSysMissing,
    mcIsMaxMissing,
    mcIs2ndMaXMissing
  );
  TMatchCriterias = set of TMatchCriteria;

const
  MatchCriteriaCaption: Array[TMatchCriteria] of string =  (
    '=',
    '<>',
    '<=',
    '<',
    '>',
    '>=',
    'Begins',
    'Ends',
    'Contains',
    'System Missing',
    'Max Missing',
    '2nd Max Missing'
  );

  MatchCriteriaAll = [mcEq, mcNEq, mcLEq, mcLT, mcGT, mcGEq, mcBegin, mcEnd, mcContains, mcIsSysMissing, mcIsMaxMissing, mcIs2ndMaXMissing];
  MatchCriteriaDefault = MatchCriteriaAll - [mcBegin, mcEnd, mcContains];
  MatchCriteriaStrings = MatchCriteriaAll - [mcLEq, mcLT, mcGT, mcGEq];

  MatchCriteriaNoTextSearch = [mcIsSysMissing, mcIsMaxMissing, mcIs2ndMaXMissing];

type

  { TEpiSearchCondition }

  TEpiSearchCondition = class
  private
    FBinOp: TSearchBinOp;
    FCaseSensitive: Boolean;
    FField: TEpiField;
    FMatchCriteria: TMatchCriteria;
    FText: string;
    procedure SetBinOp(const AValue: TSearchBinOp);
    procedure SetCaseSensitive(AValue: Boolean);
    procedure SetField(const AValue: TEpiField);
    procedure SetMatchCriteria(const AValue: TMatchCriteria);
    procedure SetText(const AValue: string);
  public
    constructor Create;
    property BinOp: TSearchBinOp read FBinOp write SetBinOp;
    property Field: TEpiField read FField write SetField;
    property MatchCriteria: TMatchCriteria read FMatchCriteria write SetMatchCriteria;
    property CaseSensitive: Boolean read FCaseSensitive write SetCaseSensitive;
    property Text: string read FText write SetText;
  end;


  TEpiSearchDirection = (sdForward, sdBackward);
  TEpiSearch = class
  private
    FDataFile: TEpiDataFile;
    FDirection: TEpiSearchDirection;
    FList: TList;
    FOrigin: TSeekOrigin;
    function GetConditionCount: integer;
    function GetSearchCondiction(const index: integer): TEpiSearchCondition;
  public
    constructor Create;
    property List: TList read FList;
    property ConditionCount: integer read GetConditionCount;
    property SearchCondiction[Const index: integer]: TEpiSearchCondition read GetSearchCondiction;
    property Origin: TSeekOrigin read FOrigin write FOrigin;
    property Direction: TEpiSearchDirection read FDirection write FDirection;
    property DataFile: TEpiDataFile read FDataFile write FDataFile;
  end;



function SearchFindNext(Const Search: TEpiSearch; Const Index: integer): integer;
function SearchFindList(Const Search: TEpiSearch; CurIndex: integer): TBoundArray;


implementation

uses
  Math, LazUTF8, epidatafilestypes, epiglobals, epiconvertutils;

function SearchFindNext(const Search: TEpiSearch; const Index: integer): integer;
var
  Match: Boolean;
  SC: TEpiSearchCondition;
  TmpRes: Boolean;
  i: Integer;
  S1: String;
  S2: String;

  function Eq(Const SC: TEpiSearchCondition; Const Idx: integer): boolean;
  var
    TheDate: EpiDate;
    TheTime: EpiTime;
    Msg: String;
  begin
    case SC.Field.FieldType of
      ftBoolean: Result := ((SC.Field.AsBoolean[Idx] = 0) and (SC.Text[1] in BooleanNoChars)) or
                           ((SC.Field.AsBoolean[Idx] = 1) and (SC.Text[1] in BooleanYesChars));
      ftInteger,
      ftAutoInc: Result := SC.Field.AsInteger[Idx] = StrToInt64(SC.Text);
      ftFloat:   Result := SameValue(SC.Field.AsFloat[Idx], StrToFloat(SC.Text), 0.0);
      ftDMYDate,
      ftMDYDate,
      ftYMDDate,
      ftDMYAuto,
      ftMDYAuto,
      ftYMDAuto:
        begin
          Result := EpiStrToDate(SC.Text, DefaultFormatSettings.DateSeparator, SC.Field.FieldType, TheDate, Msg) and
                    (SC.Field.AsDate[Idx] = TheDate);
        end;
      ftTime,
      ftTimeAuto:
        begin
          Result := EpiStrToTime(SC.Text, DefaultFormatSettings.TimeSeparator, TheTime, Msg) and
                    (SC.Field.AsTime[Idx] = TheTime);
        end;
      ftString,
      ftUpperString:
        if SC.CaseSensitive then
          Result := UTF8CompareStr(SC.Field.AsString[Idx], SC.Text) = 0
        else
          Result := UTF8CompareText(SC.Field.AsString[Idx], SC.Text) = 0
    end;
  end;

  function LT(Const SC: TEpiSearchCondition; Const Idx: integer): boolean;
  var
    TheDate: EpiDate;
    TheTime: EpiTime;
    Msg: String;
  begin
    case SC.Field.FieldType of
      ftBoolean,
      ftInteger,
      ftAutoInc: Result := SC.Field.AsInteger[Idx] < StrToInt64(SC.Text);
      ftFloat:   Result := (SC.Field.AsFloat[Idx] < StrToFloat(SC.Text)) and
                           (Not SameValue(SC.Field.AsFloat[Idx], StrToFloat(SC.Text), 0.0));
      ftDMYDate,
      ftMDYDate,
      ftYMDDate,
      ftDMYAuto,
      ftMDYAuto,
      ftYMDAuto:
        begin
          Result := EpiStrToDate(SC.Text, DefaultFormatSettings.DateSeparator, SC.Field.FieldType, TheDate, Msg) and
                    (SC.Field.AsDate[Idx] < TheDate);
        end;
      ftTime,
      ftTimeAuto:
        begin
          Result := EpiStrToTime(SC.Text, DefaultFormatSettings.TimeSeparator, TheTime, Msg) and
                    (SC.Field.AsTime[Idx] < TheTime);
        end;
      ftString,
      ftUpperString:
        if SC.CaseSensitive then
          Result := UTF8CompareStr(SC.Field.AsString[Idx], SC.Text) < 0
        else
          Result := UTF8CompareText(SC.Field.AsString[Idx], SC.Text) = 0
    end;
  end;

begin
  Result := Index;

  while true do
  begin
    Match := true;

    if Result < 0 then exit(-1);
    if Result >= Search.DataFile.Size then exit(-1);

    For i := 0 to Search.ConditionCount - 1 do
    begin
      SC := Search.SearchCondiction[i];
      case SC.MatchCriteria of
        mcEq:  TmpRes := Eq(SC, Result);
        mcNEq: TmpRes := not (Eq(SC, Result));
        mcLEq: TmpRes := LT(SC, Result) or EQ(SC, Result);
        mcLT:  TmpRes := LT(SC, Result);
        mcGT:  TmpRes := not (LT(SC, Result) or Eq(SC, Result));
        mcGEq: TmpRes := not (LT(SC, Result));

        mcBegin,
        mcEnd,
        mcContains:
          begin
            If not (SC.Field.FieldType in StringFieldTypes) then
            begin
              TmpRes := False;
              Continue;
            end;

            S1 := SC.Text;
            S2 := SC.Field.AsString[Result];
            if not SC.CaseSensitive then
            begin
              S1 := UTF8LowerCase(S1);
              S2 := UTF8LowerCase(S2);
            end;

            Case SC.MatchCriteria of
              mcBegin:
                TmpRes := UTF8Pos(S1, S2) = 1;
              mcEnd:
                TmpRes := UTF8Pos(S1, S2) = UTF8Length(S2) - UTF8Length(S1);
              mcContains:
                TmpRes := UTF8Pos(S1, S2) >= 1;
            end;
          end;

        mcIsSysMissing:
          TmpRes := SC.Field.IsMissing[Result];

        mcIsMaxMissing:
          TmpRes := SC.Field.IsMaxMissingValue[Result];

        mcIs2ndMaXMissing:
          TmpRes := SC.Field.Is2MaxMissingValue[Result];
      end; // Case MatchCriteria of;

      case SC.BinOp of
        boAnd: Match := Match and TmpRes;
        boOr:  Match := Match or TmpRes;
      end;
    end; // For i := 0 to Search.ConditionCount - 1 do

    if Match then exit;

    case Search.Direction of
      sdForward:  Inc(Result);
      sdBackward: Dec(Result);
    end;
  end;
end;

function SearchFindList(const Search: TEpiSearch; CurIndex: integer): TBoundArray;
var
  L: Integer;
begin
  L := 0;

  case Search.Origin of
    soBeginning: CurIndex := 0;
    soEnd:       CurIndex := Search.DataFile.Size - 1;
  end;

  while true do
  begin
    CurIndex := SearchFindNext(Search, CurIndex);
    if CurIndex = -1 then exit;

    SetLength(Result, L + 1);
    Result[L] := CurIndex;
    Inc(L);

    case Search.Direction of
      sdForward:  Inc(CurIndex);
      sdBackward: Dec(CurIndex);
    end;
  end;
end;

{ TEpiSearchCondition }

procedure TEpiSearchCondition.SetField(const AValue: TEpiField);
begin
  if FField = AValue then exit;
  FField := AValue;
end;

procedure TEpiSearchCondition.SetBinOp(const AValue: TSearchBinOp);
begin
  if FBinOp = AValue then exit;
  FBinOp := AValue;
end;

procedure TEpiSearchCondition.SetCaseSensitive(AValue: Boolean);
begin
  if FCaseSensitive = AValue then Exit;
  FCaseSensitive := AValue;
end;

procedure TEpiSearchCondition.SetMatchCriteria(const AValue: TMatchCriteria);
begin
  if FMatchCriteria = AValue then exit;
  FMatchCriteria := AValue;
end;

procedure TEpiSearchCondition.SetText(const AValue: string);
begin
  if FText = AValue then exit;
  FText := AValue;
end;

constructor TEpiSearchCondition.Create;
begin
  FBinOp          := boAnd;
  FCaseSensitive  := true;
  FField          := nil;
  FMatchCriteria  := mcEq;
  FText           := '';
end;

{ TEpiSearch }

function TEpiSearch.GetSearchCondiction(const index: integer): TEpiSearchCondition;
begin
  result := TEpiSearchCondition(List[Index]);
end;

function TEpiSearch.GetConditionCount: integer;
begin
  result := FList.Count;
end;

constructor TEpiSearch.Create;
begin
  FList := TList.Create;
end;

end.
