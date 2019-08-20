unit recode;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ast, executor, epidatafiles, outputcreator, epivaluelabels;

type

  { TRecode }

  TRecode = class(TObject)
  private
    // Aux. methods
    function GetValueLabelName(ToVariable: TCustomVariable): UTF8String;
    function CreateToField(ToVariable: TCustomVariable): TEpiField;
    function CreateValueLabelSet(ST: TRecodeCommand): TEpiValueLabelSet;
  private
    FExecutor: TExecutor;
    FOutputCreator: TOutputCreator;
    function FindStartValue(Options: TOptionList; FromVariable: TEpiField): ASTFloat;
    procedure DoByRecode(ST: TRecodeCommand);
    procedure DoIntervalRecode(ST: TRecodeCommand);
  public
    constructor Create(AExecutor: TExecutor; AOutputCreator: TOutputCreator);
    destructor Destroy; override;
    procedure ExecRecode(ST: TRecodeCommand);
  end;

implementation

uses
  Math, epidatafilestypes;

{ TRecode }

function DoSortVlSet(Item1, Item2: Pointer): Integer;
begin
  Result := TEpiIntValueLabel(Item1).Value - TEpiIntValueLabel(Item2).Value;
end;

function TRecode.GetValueLabelName(ToVariable: TCustomVariable): UTF8String;
begin
  result := 'lbl_' + ToVariable.Ident;
end;

function TRecode.CreateToField(ToVariable: TCustomVariable): TEpiField;
var
  VariableName: UTF8String;
  Section: TEpiSection;
  OldV: TEpiField;
  lTop, lLeft: Integer;
begin
  VariableName := ToVariable.Ident;

  Result := FExecutor.DataFile.Fields.FieldByName[VariableName];
  if (Assigned(Result)) then
    FreeAndNil(Result);

  Section := FExecutor.DataFile.MainSection;
  if Section.Fields.Count > 0 then
    begin
      OldV  := Section.Fields[Section.Fields.Count - 1];
      lTop  := OldV.Top + 35;
      lLeft := OldV.Left;
    end
  else
    begin
      lTop  := 20;
      lLeft := 80;
    end;

  Result := FExecutor.DataFile.NewField(ftInteger);
  Result.Name := VariableName;
  Result.Top := lTop;
  Result.Length := lLeft;
end;

function TRecode.CreateValueLabelSet(ST: TRecodeCommand): TEpiValueLabelSet;
var
  ValueLabelName: UTF8String;
begin
  ValueLabelName := GetValueLabelName(ST.ToVariable);
  Result := FExecutor.Document.ValueLabelSets.GetValueLabelSetByName(ValueLabelName);

  if (Assigned(Result)) then
    FreeAndNil(Result);

  if (ST.HasOption('nvl')) then
    Exit(nil);

  Result := FExecutor.Document.ValueLabelSets.NewValueLabelSet(ftInteger);
  Result.Name := ValueLabelName;
end;

function TRecode.FindStartValue(Options: TOptionList; FromVariable: TEpiField
  ): ASTFloat;
var
  Opt: TOption;
  i: Integer;
begin
  // A specific value was given, return it
  if (Options.HasOption('min', Opt)) then
    begin
      Result := Opt.Expr.AsFloat;
      Exit;
    end;

  // With no '!min' specified always start at 0
  Result := 0;
end;

procedure TRecode.DoByRecode(ST: TRecodeCommand);
var
  FromVariable, ToVariable: TEpiField;
  MinValue, MaxValue, MissingValue: ASTFloat;
  Value: Extended;
  i, Group, IntGroupOffset: Integer;
  Opt: TOption;
  IsIntegerGroups, HasMissingValue: Boolean;
  IntervalValue: ASTInteger;
  VLSet: TEpiValueLabelSet;
  ValueLabel: TEpiCustomValueLabel;

  procedure CreateValueLabel(IsMissingValue: Boolean = false);
  begin
    if (not VLSet.ValueLabelExists[Group]) then
      begin
        ValueLabel := VLSet.NewValueLabel;
        ValueLabel.IsMissingValue := IsMissingValue;

        TEpiIntValueLabel(ValueLabel).Value := Group;

        if (IsIntegerGroups) then
          Group := (Group - IntGroupOffset) * IntervalValue;
        ValueLabel.TheLabel.Text := Format('%d - %d', [Group, Group + IntervalValue - 1]);
      end;
  end;

begin
  FromVariable := FExecutor.DataFile.Fields.FieldByName[ST.FromVariable.Ident];
  ToVariable   := CreateToField(ST.ToVariable);
  VLSet        := CreateValueLabelSet(ST);
  ToVariable.ValueLabelSet := VLSet;

  // Determin start value:
  MinValue      := FindStartValue(ST.Options, FromVariable);
  MaxValue      := Math.MaxFloat;
  if (ST.HasOption('max', Opt)) then
    MaxValue := Opt.Expr.AsFloat;

  IntervalValue   := ST.Options.Option['by'].Expr.AsInteger;
  IsIntegerGroups := ST.HasOption('i', Opt);
  IntGroupOffset  := 0;
  if ((IsIntegerGroups) and
      (Assigned(Opt.Expr)))
  then
    IntGroupOffset := Opt.Expr.AsInteger;

  HasMissingValue := ST.HasOption('m', Opt);
  if HasMissingValue then
    MissingValue := Opt.Expr.AsFloat;

  for i := 0 to FromVariable.Size - 1 do
    begin
      if (FromVariable.IsMissing[i]) then
        Continue;

      Value := FromVariable.AsFloat[i];

      if ((HasMissingValue) and
          (SameValue(Value, MissingValue)))
      then
        begin
          ToVariable.AsInteger[i] := Trunc(Value);
          CreateValueLabel(true);
          Continue;
        end;

      if (Value > MaxValue) or (Value < MinValue) then
        Continue;

      Group := Floor(Value / IntervalValue);
      if (IsIntegerGroups) then
        Group := Group + IntGroupOffset
      else
        Group := Group * IntervalValue;

      ToVariable.AsInteger[i] := Group;

      CreateValueLabel();
    end;

  VLSet.Sorted := true;
  VLSet.OnSort := @DoSortVlSet;
  VLSet.Sort;
  VLSet.OnSort := nil;
end;

procedure TRecode.DoIntervalRecode(ST: TRecodeCommand);
var
  FromVariable, ToVariable: TEpiField;
  VLSet: TEpiValueLabelSet;
  RI: TRecodeInterval;
  IntVL: TEpiIntValueLabel;
  i: Integer;
  Value: Extended;
begin
  FromVariable := FExecutor.DataFile.Fields.FieldByName[ST.FromVariable.Ident];
  ToVariable   := CreateToField(ST.ToVariable);
  VLSet        := CreateValueLabelSet(ST);
  ToVariable.ValueLabelSet := VLSet;


  for RI in ST.RecodeIntervalList do
    begin
      IntVL               := TEpiIntValueLabel(VLSet.NewValueLabel);
      IntVL.Value         := RI.LabelValue.AsInteger;
      IntVL.TheLabel.Text := RI.ValueLabel.AsString;
    end;

  for i := 0 to FromVariable.Size - 1 do
    begin
      Value := FromVariable.AsFloat[i];

      for RI in ST.RecodeIntervalList do
        begin
          if (Value >= RI.FromValue.AsFloat) and
             (Value < RI.ToValue.AsFloat)
          then
            ToVariable.AsInteger[i] := RI.LabelValue.AsInteger;
        end;
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
var
  IsReplaceable: Boolean;
  ValueLabelName: UTF8String;
begin
  IsReplaceable := ST.HasOption('replace');

  // To Variable must not exist.
  if ((Assigned(FExecutor.GetExecVariable(ST.ToVariable.Ident))) and
      (not IsReplaceable))
  then
    begin
      FExecutor.Error('The "to" variable already exists! Use !replace to overwrite existing variable.');
      Exit;
    end;

  ValueLabelName := GetValueLabelName(ST.ToVariable);
  if ((Assigned(FExecutor.GetExecVariable(ValueLabelName))) and
      (not IsReplaceable))
  then
    begin
      FExecutor.Error('The valuelabel "' + ValueLabelName + '" already exists! Use !replace to overwrite existing valuelabel.');
      Exit;
    end;

  if ((ST.HasOption('by')) and
      (ST.RecodeIntervalList.Count > 0))
  then
    begin
      FExecutor.Error('The option !by cannot be used together with interval recoding!');
      Exit;
    end;

  if ((not ST.HasOption('by')) and
      (ST.RecodeIntervalList.Count = 0))
  then
    begin
      FExecutor.Error('Either use option !by or use interval recoding!');
      Exit;
    end;

  if (ST.HasOption('by')) then
    DoByRecode(ST)
  else
    DoIntervalRecode(ST);

  ST.ExecResult := csrSuccess;
end;

end.
