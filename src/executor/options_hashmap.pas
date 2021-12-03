unit options_hashmap;

{$codepage UTF-8}
{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ast_types, epidatafilestypes, gmap, LazMethodList,
  result_variables;

type

  ESetOption = class(Exception);

  { TSetOption }

  TSetOption=class
  private
    FOnChange: TNotifyEvent;
    FName: UTF8String;
    FValue: UTF8String;
    FASTType: TASTResultType;
    FLegalValues: TStrings;
    FWarningMessage: UTF8String;
    procedure SetASTType(AValue: TASTResultType);
  protected
    procedure DoError(Const Msg: UTF8String); virtual;
    procedure DoWarning(Const Msg: UTF8String); virtual;
    procedure SetValue(AValue: UTF8String); virtual;
  private
    FLowRange: UTF8String;
    FHighRange: UTF8String;
    procedure DoErrorLegals(Const AValue: UTF8String);
    procedure DoErrorType(Const AValue: UTF8String);
    procedure DoErrorRange(Const AValue: UTF8String);
    procedure SetLowRange(AValue: UTF8String);
    procedure SetHighRange(AValue: UTF8String);
  public
    constructor Create(const AValue: UTF8String; AAstType: TASTResultType); virtual;
    property Name: UTF8String read FName;
    property Value: UTF8String read FValue write SetValue;
    property ASTType: TASTResultType read FASTType;
    property LegalValues: TStrings read FLegalValues;
    property LowRange: UTF8String read FLowRange write SetLowRange;
    property HighRange: UTF8String read FHighRange write SetHighRange;
    property WarningMessage: UTF8String read FWarningMessage write FWarningMessage;

  // Handlers
  private
    FChangeHandlers: TMethodList;
    procedure DoOnChange;
  public
    procedure AddOnChangeHandler(Event: TNotifyEvent);
    procedure RemoveOnChangeHandler(Event: TNotifyEvent);
  end;

  { TOptionCompare }

  TOptionCompare = class
    class function c(KeyA, KeyB: UTF8String): boolean;
  end;

  TCustomSetOptionsMap = specialize TMap<UTF8String, TSetOption, TOptionCompare>;

  { TSetOptionsMap }

  TSetOptionsMap = class(TCustomSetOptionsMap)
    procedure Insert(key: UTF8String; value: TSetOption);
  end;

  TTypesAndFlagsRec = record
    ResultTypes: TASTResultTypes;
    ExecutorVariableTypes: TExecutorVariableTypes;
    Flags: TExecutorVariableFlags;
  end;
  PMapValues = ^TTypesAndFlagsRec;

  TCustomStatementOptionsMap = specialize TMap<UTF8String, PMapValues, TOptionCompare>;

  { TStatementOptionsMap }

  TStatementOptionsMap = class(TCustomStatementOptionsMap)
  public
    procedure Insert(Key: UTF8String; ResultTypes: TASTResultTypes;
      ExecutorVariableTypes: TExecutorVariableTypes = ExecutorVariableTypesData;
      Flags: TExecutorVariableFlags = [evfInternal, evfAsValue]);
    procedure Insert(MasterKey: UTF8String; SubKeys: array of UTF8String;
      ResultTypes: TASTResultTypes;
      ExecutorVariableTypes: TExecutorVariableTypes = ExecutorVariableTypesData;
      Flags: TExecutorVariableFlags = [evfInternal, evfAsValue]);
    function HasOption(Const Key: UTF8String): Boolean;
    function GetTypesAndFlags(Const Key: UTF8String): PMapValues;
    function GetResultTypes(Const Key: UTF8String): TASTResultTypes;
    function GetExecutorVariableTypes(Const Key: UTF8String): TExecutorVariableTypes;
    function GetExecutorVariableFlags(Const Key: UTF8String): TExecutorVariableFlags;
  end;

  function TypesAndFlags(
    AResultTypes: TASTResultTypes = [];
    AExecutorVariableTypes: TExecutorVariableTypes = [];
    AFlags: TExecutorVariableFlags = [evfInternal, evfAsValue]
  ): TTypesAndFlagsRec;

implementation

uses
  LazUTF8, LazUTF8Classes, epiconvertutils;

function TypesAndFlags(AResultTypes: TASTResultTypes;
  AExecutorVariableTypes: TExecutorVariableTypes; AFlags: TExecutorVariableFlags
  ): TTypesAndFlagsRec;
begin
  result.ResultTypes := AResultTypes;
  result.ExecutorVariableTypes := AExecutorVariableTypes;
  result.Flags := AFlags;
end;

{ TSetOptionsMap }

procedure TSetOptionsMap.Insert(key: UTF8String; value: TSetOption);
begin
  inherited Insert(key, value);
  value.FName := key;
end;

{ TStatementOptionsMap }

procedure TStatementOptionsMap.Insert(Key: UTF8String;
  ResultTypes: TASTResultTypes; ExecutorVariableTypes: TExecutorVariableTypes;
  Flags: TExecutorVariableFlags);
var
  MapValues: PMapValues;
begin
  MapValues := New(PMapValues);
  MapValues^.ResultTypes           := ResultTypes;
  MapValues^.ExecutorVariableTypes := ExecutorVariableTypes;
  MapValues^.Flags                 := Flags;

  Inherited Insert(Key, MapValues);
end;

procedure TStatementOptionsMap.Insert(MasterKey: UTF8String;
  SubKeys: array of UTF8String; ResultTypes: TASTResultTypes;
  ExecutorVariableTypes: TExecutorVariableTypes; Flags: TExecutorVariableFlags);
var
  Value: PMapValues;
  Key: UTF8String;
begin
  Insert(MasterKey, ResultTypes, ExecutorVariableTypes, Flags);

  // TryGetValue(MasterKey, Value);
  // TODO : Make a connection between the master key and sub keys
  for Key in SubKeys do
    Insert(Key, ResultTypes, ExecutorVariableTypes, Flags);
end;

function TStatementOptionsMap.HasOption(const Key: UTF8String): Boolean;
var
  Value: PMapValues;
begin
  result := TryGetValue(Key, Value);
end;

function TStatementOptionsMap.GetTypesAndFlags(const Key: UTF8String
  ): PMapValues;
begin
  TryGetValue(Key, Result);
end;

function TStatementOptionsMap.GetResultTypes(const Key: UTF8String
  ): TASTResultTypes;
var
  Value: PMapValues;
begin
  result := [];
  if TryGetValue(Key, Value) then
    result := Value^.ResultTypes;
end;

function TStatementOptionsMap.GetExecutorVariableTypes(const Key: UTF8String
  ): TExecutorVariableTypes;
var
  Value: PMapValues;
begin
  result := [];
  if TryGetValue(Key, Value) then
    result := Value^.ExecutorVariableTypes;
end;

function TStatementOptionsMap.GetExecutorVariableFlags(const Key: UTF8String
  ): TExecutorVariableFlags;
var
  Value: PMapValues;
begin
  result := [];
  if TryGetValue(Key, Value) then
    result := Value^.Flags;
end;

{ TOptionCompare }

class function TOptionCompare.c(KeyA, KeyB: UTF8String): boolean;
begin
  result := UTF8CompareText(KeyA, KeyB) < 0;
end;

{ TSetOption }

procedure TSetOption.SetValue(AValue: UTF8String);
var
  Msg: string;
  ValDate,  LowDate,  HighDate: EpiDate;
  ValFloat, LowFloat, HighFloat: EpiFloat;
  ValInt,   LowInt,   HighInt: EpiInteger;
  ValTime,  LowTime,  HighTime: EpiTime;
begin
  if (FValue = AValue) then Exit;

  if (LegalValues.Count > 0) then
  begin
    if (LegalValues.IndexOf(AValue) < 0) then
      DoErrorLegals(AValue);
  end;

  if (LegalValues.Count = 0) then
  begin
    case ASTType of
      rtInteger:
        begin
          if (not TryStrToInt64(AValue, ValInt)) then
            DoErrorType(AValue);

          if (LowRange <> '') and
             (HighRange <> '')
          then
            begin
              LowInt := StrToInt(LowRange);
              HighInt := StrToInt(HighRange);

              if (ValInt < LowInt) or (ValInt > HighInt) then
                DoErrorRange(AValue);
            end;
        end;

      rtDate:
        begin
          if (not EpiStrToDateGuess(AValue, ValDate, Msg)) then
            DoErrorType(AValue);

          if (LowRange <> '') and
             (HighRange <> '')
          then
            begin
            EpiStrToDateGuess(LowRange, LowDate, Msg);
            EpiStrToDateGuess(HighRange, HighDate, Msg);

            if (ValDate < LowDate) or (ValDate > HighDate) then
              DoErrorRange(AValue);
          end;
        end;

      rtFloat:
        begin
          if (not TryStrToFloat(AValue, ValFloat)) then
            DoErrorType(AValue);

          if (LowRange <> '') and
             (HighRange <> '')
          then
            begin
              LowFloat := StrToFloat(LowRange);
              HighFloat := StrToFloat(HighRange);

              if (ValFloat < LowFloat) or (ValFloat > HighFloat) then
                DoErrorRange(AValue);
            end;
        end;

      rtTime:
        begin
          if (not EpiStrToTimeGues(AValue, ValTime, Msg)) then
            DoErrorType(AValue);

          if (LowRange <> '') and
             (HighRange <> '')
          then
            begin
              EpiStrToTimeGues(LowRange, LowTime, Msg);
              EpiStrToTimeGues(HighRange, HighTime, Msg);

              if (ValTime < LowTime) or (ValTime > HighTime) then
                DoErrorRange(AValue);
            end;
        end;
    end;
  end;

  if ASTType = rtBoolean then
    FValue := UTF8UpperString(AValue)
  else
    FValue := AValue;

  DoOnChange;
end;

procedure TSetOption.SetASTType(AValue: TASTResultType);
begin
  if FASTType = AValue then Exit;
  FASTType := AValue;
end;

procedure TSetOption.DoError(const Msg: UTF8String);
begin
  raise ESetOption.Create(Msg);
end;

procedure TSetOption.DoWarning(const Msg: UTF8String);
begin
  FWarningMessage := Msg;
end;

procedure TSetOption.DoErrorLegals(const AValue: UTF8String);
var
  S, T: String;
begin
  T := '';

  for S in LegalValues do
    T := T + ', ' + S;
  Delete(T, 1, 2);

  T := 'Not a legal Set Option: ' + AValue + LineEnding +
       'Possible values are: ' + T;

  DoError(T);
end;

procedure TSetOption.DoErrorType(const AValue: UTF8String);
var
  T: String;
begin
  T := '"' + AValue + '" is not the correct type!' + LineEnding +
       'Expected type: "' + ASTResultTypeString[ASTType];

  DoError(T);
end;

procedure TSetOption.DoErrorRange(const AValue: UTF8String);
var
  T: UTF8String;
begin
  T := '"' + AValue + '" is not within correct range!' + LineEnding +
       'Values must be within: ' + LowRange + ' - ' + HighRange;

  DoError(T);
end;

procedure TSetOption.SetHighRange(AValue: UTF8String);
begin
  if FHighRange = AValue then Exit;
  FHighRange := AValue;
end;

procedure TSetOption.SetLowRange(AValue: UTF8String);
begin
  if FLowRange = AValue then Exit;
  FLowRange := AValue;
end;

constructor TSetOption.Create(const AValue: UTF8String; AAstType: TASTResultType
  );
begin
  FChangeHandlers := TMethodList.Create;

  FLegalValues := TStringListUTF8.Create;
  TStringListUTF8(FLegalValues).CaseSensitive := false;
  TStringListUTF8(FLegalValues).Sorted := true;

  if AAstType = rtBoolean then
  begin
    if (AValue = 'YES') or (AValue = 'NO') then
    begin
      FLegalValues.Add('YES');
      FLegalValues.Add('NO');
    end else begin
      FLegalValues.Add('ON');
      FLegalValues.Add('OFF');
    end;
  end;

  FValue := AValue;
  FASTType := AAstType;
end;

procedure TSetOption.DoOnChange;
var
  I: Integer;
  M: TNotifyEvent;
begin
  I := FChangeHandlers.Count;
  while FChangeHandlers.NextDownIndex(I) do
  begin
    M := TNotifyEvent(FChangeHandlers.Items[I]);
    M(Self);
  end;
end;

procedure TSetOption.AddOnChangeHandler(Event: TNotifyEvent);
begin
  FChangeHandlers.Add(TMethod(Event));
end;

procedure TSetOption.RemoveOnChangeHandler(Event: TNotifyEvent);
begin
  FChangeHandlers.Remove(TMethod(Event));
end;

end.

