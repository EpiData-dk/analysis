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
    function CreateToField(ST: TRecodeCommand): TEpiField;
    function CreateValueLabelSet(ST: TRecodeCommand): TEpiValueLabelSet;
  private
    FExecutor: TExecutor;
    FOutputCreator: TOutputCreator;
  private
    function FindStartValue(Options: TOptionList): ASTFloat;
    function DoByRecode(ST: TRecodeCommand; PreparedDataFile: TEpiDataFile): boolean;
  private
    function SanityCheckIntervals(Intervals: TRecodeIntervalList): boolean;
    function DoIntervalRecode(ST: TRecodeCommand; PreparedDataFile: TEpiDataFile): boolean;
  public
    constructor Create(AExecutor: TExecutor; AOutputCreator: TOutputCreator);
    destructor Destroy; override;
    procedure ExecRecode(ST: TRecodeCommand; PreparedDataFile: TEpiDataFile);
  end;

implementation

uses
  Math, epidatafilestypes, ana_globals;

{ TRecode }

function DoSortVlSet(Item1, Item2: Pointer): Integer;
begin
  Result := TEpiIntValueLabel(Item1).Value - TEpiIntValueLabel(Item2).Value;
end;

function TRecode.GetValueLabelName(ToVariable: TCustomVariable): UTF8String;
begin
  result := '_' + ToVariable.Ident;
end;

function TRecode.CreateToField(ST: TRecodeCommand): TEpiField;
var
  VariableName: UTF8String;
  Section: TEpiSection;
  OldV: TEpiField;
  lTop, lLeft: Integer;
  Opt: TOption;
begin
  VariableName := ST.ToVariable.Ident;

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

  if (ST.HasOption('label', Opt)) then
    Result.Question.Text := Opt.Expr.AsString;
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

function TRecode.FindStartValue(Options: TOptionList): ASTFloat;
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

function TRecode.DoByRecode(ST: TRecodeCommand; PreparedDataFile: TEpiDataFile
  ): boolean;
var
  FromVariable, ToVariable, ObsNo: TEpiField;
  MinValue, MaxValue: ASTFloat;
  Value: Extended;
  i, Group, IntGroupOffset: Integer;
  Opt: TOption;
  IsIntegerGroups, HasMissingValue: Boolean;
  IntervalValue, MissingValue: ASTInteger;
  VLSet: TEpiValueLabelSet;
  ValueLabel: TEpiCustomValueLabel;
  Idx: Int64;

  procedure CreateValueLabel();
  begin
    if (Assigned(VLSet)) and
       (not (VLSet.ValueLabelExists[Group]))
    then
      begin
        ValueLabel := VLSet.NewValueLabel;
        TEpiIntValueLabel(ValueLabel).Value := Group;

        if (IsIntegerGroups) then
          Group := (Group - IntGroupOffset) * IntervalValue;
        ValueLabel.TheLabel.Text := Format('%d - %d', [Group, Group + IntervalValue - 1]);
      end;
  end;

  procedure CreateMissingValueLabel();
  begin
    if (Assigned(VLSet)) and
       (HasMissingValue) and
       (not VLSet.ValueLabelExists[MissingValue])
    then
      begin
        ValueLabel := VLSet.NewValueLabel;
        ValueLabel.IsMissingValue := true;
        TEpiIntValueLabel(ValueLabel).Value := MissingValue;
        ValueLabel.TheLabel.Text := 'missing';
      end;
  end;

begin
  FromVariable := FExecutor.DataFile.Fields.FieldByName[ST.FromVariable.Ident];
  ToVariable   := CreateToField(ST);
  VLSet        := CreateValueLabelSet(ST);
  ToVariable.ValueLabelSet := VLSet;
  ObsNo        := PreparedDataFile.Fields.FieldByName[ANA_EXEC_PREPAREDS_OBSNO_FIELD];

  // Determin start value:
  MinValue      := FindStartValue(ST.Options);
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
    MissingValue := Opt.Expr.AsInteger;

  for i := 0 to ObsNo.Size - 1 do
    begin
      Idx := ObsNo.AsInteger[i];
      Value := FromVariable.AsFloat[Idx];

      if (FromVariable.IsMissing[Idx]) or
         (Value > MaxValue) or
         (Value < MinValue)
      then
        begin
          if (not HasMissingValue) then
            Continue;

          ToVariable.AsInteger[Idx] := MissingValue;
          CreateMissingValueLabel();
          Continue;
        end;

      Group := Floor(Value / IntervalValue);
      if (IsIntegerGroups) then
        Group := Group + IntGroupOffset
      else
        Group := Group * IntervalValue;

      ToVariable.AsInteger[Idx] := Group;

      CreateValueLabel();
    end;

  if (Assigned(VLSet)) then
    begin
      VLSet.Sorted := true;
      VLSet.OnSort := @DoSortVlSet;
      VLSet.Sort;
      VLSet.OnSort := nil;
    end;

  Result := true;
end;

function CompareIntervals(const Item1, Item2: TRecodeInterval): Integer;
begin
  Result := CompareValue(Item1.FromValue.AsFloat, Item2.FromValue.AsFloat);
  if (Result = EqualsValue) then
    Result := CompareValue(Item1.ToValue.AsFloat, Item2.ToValue.AsFloat);
end;

function TRecode.SanityCheckIntervals(Intervals: TRecodeIntervalList): boolean;
var
  First, Second: TRecodeInterval;
  i: Integer;
begin
  Intervals.Sort(@CompareIntervals);

  Result := False;
  for i := 0 to Intervals.Count - 2 do
    begin
      First := Intervals[i];
      Second := Intervals[i + 1];

      if (Second.FromValue.AsFloat < First.ToValue.AsFloat) then
        begin
          FExecutor.Error(
            Format('Intervals overlap: (%s - %s), (%s - %s)',
                   [First.FromValue.AsString, First.ToValue.AsString,
                    Second.FromValue.AsString, Second.ToValue.AsString])
          );
          Exit;
        end;

      if (First.FromValue.AsFloat > First.ToValue.AsFloat) then
        begin
          FExecutor.Error(
            Format('Interval invalid: (%s - %s)',
                   [First.FromValue.AsString, First.ToValue.AsString])
          );
          Exit;
        end;
    end;

  First := Intervals[Intervals.Count - 1];
  if (First.FromValue.AsFloat > First.ToValue.AsFloat) then
    begin
      FExecutor.Error(
        Format('Interval invalid: (%s - %s)',
               [First.FromValue.AsString, First.ToValue.AsString])
      );
      Exit;
    end;

  Result := True;
end;

function TRecode.DoIntervalRecode(ST: TRecodeCommand; PreparedDataFile: TEpiDataFile): boolean;
var
  FromVariable, ToVariable, ObsNo: TEpiField;
  VLSet: TEpiValueLabelSet;
  RI: TRecodeInterval;
  IntVL: TEpiIntValueLabel;
  i: Integer;
  Value: Extended;
  Idx: Int64;
  Opt: TOption;
  HasMissingValue, UseIntergerGroups: Boolean;
  MissingValue: ASTInteger;

  procedure CreateMissingValueLabel();
  var
    ValueLabel: TEpiCustomValueLabel;
  begin
    if (Assigned(VLSet)) and
       (HasMissingValue) and
       (not VLSet.ValueLabelExists[MissingValue])
    then
      begin
        ValueLabel := VLSet.NewValueLabel;
        ValueLabel.IsMissingValue := true;
        TEpiIntValueLabel(ValueLabel).Value := MissingValue;
        ValueLabel.TheLabel.Text := 'missing';
      end;
  end;

  procedure CreateValueLabels(RecodeIntervalList: TRecodeIntervalList);
  var
    RI: TRecodeInterval;
  begin
    UseIntergerGroups := ST.HasOption('i');

    for RI in RecodeIntervalList do
      begin
        IntVL := TEpiIntValueLabel(VLSet.NewValueLabel);

        if Assigned(RI.ValueLabel) then
          IntVL.TheLabel.Text := RI.ValueLabel.AsString
        else
          IntVL.TheLabel.Text := Format('%s - %s', [RI.FromValue.AsString, RI.ToValue.AsString]);

        if Assigned(RI.LabelValue) then
          IntVL.Value         := RI.LabelValue.AsInteger
        else if (UseIntergerGroups) then
          IntVL.Value         := RecodeIntervalList.IndexOf(RI) + 1
        else
          IntVL.Value         := RI.FromValue.AsInteger;
      end;
  end;

begin
  Result := false;

  if (not SanityCheckIntervals(ST.RecodeIntervalList)) then
    Exit;

  HasMissingValue := ST.HasOption('m', Opt);
  if HasMissingValue then
    MissingValue := Opt.Expr.AsInteger;

  FromVariable := FExecutor.DataFile.Fields.FieldByName[ST.FromVariable.Ident];
  ToVariable   := CreateToField(ST);
  VLSet        := CreateValueLabelSet(ST);
  ToVariable.ValueLabelSet := VLSet;
  ObsNo        := PreparedDataFile.Fields.FieldByName[ANA_EXEC_PREPAREDS_OBSNO_FIELD];

  if (Assigned(VLSet)) then
    CreateValueLabels(ST.RecodeIntervalList);

  UseIntergerGroups := ST.HasOption('i');

  for i := 0 to ObsNo.Size - 1 do
    begin
      Idx := ObsNo.AsInteger[i];
      Value := FromVariable.AsFloat[Idx];

      for RI in ST.RecodeIntervalList do
        begin
          if (Value >= RI.FromValue.AsFloat) and
             (Value < RI.ToValue.AsFloat)
          then
            if Assigned(RI.LabelValue) then
              ToVariable.AsInteger[Idx] := RI.LabelValue.AsInteger
            else if (UseIntergerGroups) then
              ToVariable.AsInteger[Idx] := ST.RecodeIntervalList.IndexOf(RI) + 1
            else
              ToVariable.AsInteger[Idx] := RI.FromValue.AsInteger;
        end;

      if ((ToVariable.IsMissing[Idx]) and
          (HasMissingValue))
      then
        begin
          ToVariable.AsInteger[Idx] := MissingValue;
          CreateMissingValueLabel();
        end;
    end;

  Result := true;
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

procedure TRecode.ExecRecode(ST: TRecodeCommand; PreparedDataFile: TEpiDataFile
  );
var
  IsReplaceable, Res: Boolean;
  ValueLabelName: UTF8String;
begin
  ST.ExecResult := csrFailed;
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
    Res := DoByRecode(ST, PreparedDataFile)
  else
    Res := DoIntervalRecode(ST, PreparedDataFile);

  if (Res) then
    ST.ExecResult := csrSuccess;
end;

end.

