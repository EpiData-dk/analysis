unit recode;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ast, executor, epidatafiles, outputcreator;

type

  { TRecode }

  TRecode = class(TObject)
  private
    FExecutor: TExecutor;
    FOutputCreator: TOutputCreator;
    function FindStartValue(Options: TOptionList; FromVariable: TEpiField): ASTFloat;
    procedure DoRecode(ST: TRecodeCommand);

  public
    constructor Create(AExecutor: TExecutor; AOutputCreator: TOutputCreator);
    destructor Destroy; override;
    procedure ExecRecode(ST: TRecodeCommand);
  end;

implementation

uses
  Math, epidatafilestypes;

{ TRecode }

function TRecode.FindStartValue(Options: TOptionList; FromVariable: TEpiField
  ): ASTFloat;
var
  Opt: TOption;
  i: Integer;
begin
  // With no '!min' specified always start at 0
  if (not (Options.HasOption('min'))) then
    begin
      Result := 0;
      Exit;
    end;

  // A specific value was given, return it
  if (Options.HasOption('min', Opt)) and
     (Assigned(Opt.Expr))
  then
    begin
      Result := Opt.Expr.AsFloat;
      Exit;
    end;

  // Only '!min' was given, find the lowest value.
  Result := MaxExtended;
  for i := 0 to FromVariable.Size - 1 do
    Result := Math.Min(Result, FromVariable.AsFloat[i]);
end;

procedure TRecode.DoRecode(ST: TRecodeCommand);
var
  FromVariable, ToVariable: TEpiField;
  MinValue, MaxValue, GroupByValue: ASTFloat;
  Value: Extended;
  i, Group: Integer;
  Opt: TOption;
begin
  FromVariable := FExecutor.DataFile.Fields.FieldByName[ST.FromVariable.Ident];
  ToVariable   := FExecutor.DataFile.MainSection.NewField(ftInteger);
  ToVariable.Name := ST.ToVariable.Ident;

  // Determin start value:
  MinValue     := FindStartValue(ST.Options, FromVariable);
  MaxValue     := Math.MaxExtended;
  if (ST.HasOption('max', Opt)) then
    MaxValue := Opt.Expr.AsFloat;

  GroupByValue := ST.Options.Option['by'].Expr.AsFloat;

  for i := 0 to FromVariable.Size - 1 do
    begin
      Value := FromVariable.AsFloat[i];
      if (Value > MaxValue) or (Value < MinValue) then
        Continue;

      Value := Value - MinValue;
      Value := Value / GroupByValue;

      Group := floor(Value);

      ToVariable.AsInteger[i] := Group;
    end;
end;

constructor TRecode.Create(AExecutor: TExecutor; AOutputCreator: TOutputCreator
  );
begin
  FExecutor := AExecutor;
  FOutputCreator := AOutputCreator;
end;

destructor TRecode.Destroy;
begin
  inherited Destroy;
end;

procedure TRecode.ExecRecode(ST: TRecodeCommand);
begin
  // To Variable must not exist.
  if (Assigned(FExecutor.GetExecVariable(ST.ToVariable.Ident))) then
    begin
      FExecutor.Error('The "to" variable already exists!');
      Exit;
    end;

  if (not ST.HasOption('by')) then
    begin
      FExecutor.Error('The options "by" is missing and must be specified!');
      Exit;
    end;

  DoRecode(ST);

  ST.ExecResult := csrSuccess;
end;

end.

