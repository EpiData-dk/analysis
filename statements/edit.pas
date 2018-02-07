unit edit;

{$codepage UTF-8}
{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ast, executor, epidocument, epidatafiles, epicustombase,
  outputcreator;

type

  { TExecEdit }

  TExecEdit = class
  private
    FExecutor: TExecutor;
    FOutputCreator: TOutputCreator;
  protected
    procedure DoEditProject(ST: TEditProject);
    procedure DoEditValuelabel(ST: TEditValueLabel);
    procedure DoEditDataset(ST: TEditDataset);
    procedure DoEditData(ST: TEditData);
  public
    constructor Create(AExecutor: TExecutor; AOutputCreator: TOutputCreator);
    destructor Destroy; override;
    procedure ExecEdit(EditCommand: TCustomCrudCommand);
  end;

implementation

uses
  epidatafilestypes, epivaluelabels, epimiscutils, epifields_helper,
  epidatafilerelations_helper, epidatafilerelations;

{ TExecEdit }

procedure TExecEdit.DoEditProject(ST: TEditProject);
var
  Opt: TOption;
begin
  if ST.HasOption('title', Opt) then
    FExecutor.Document.Study.Title.Text := Opt.Expr.AsString;

  ST.ExecResult := csrSuccess;
end;

procedure TExecEdit.DoEditValuelabel(ST: TEditValueLabel);
var
  VLPairs: TValueLabelPairs;
  i, BeforeCount: Integer;
  VLSet: TEpiValueLabelSet;
  VL: TEpiCustomValueLabel;
  Opt: TOption;
  BeforeMissing: LongInt;
begin
  VLSet := FExecutor.Document.ValueLabelSets.GetValueLabelSetByName(ST.Variable.Ident);
  BeforeCount := VLSet.Count;
  BeforeMissing := VLSet.MissingCount;

  VLPairs := ST.ValueLabelPairs;
  if Assigned(VLPairs) then
    for i := 0 to VLPairs.Count -1 do
      begin
        if VLSet.ValueLabelExists[VLPairs.Values[i].AsString] then
          VL := VLSet.ValueLabel[VLPairs.Values[i].AsString]
        else
          VL := VLSet.NewValueLabel;

        VL.TheLabel.Text := VLPairs.Text[i].AsString;
        case VLSet.LabelType of
          ftInteger: TEpiIntValueLabel(VL).Value    := VLPairs.Values[i].AsInteger;
          ftFloat:   TEpiFloatValueLabel(VL).Value  := VLPairs.Values[i].AsFloat;
          ftString:  TEpiStringValueLabel(VL).Value := VLPairs.Values[i].AsString;
          ftDMYDate,
          ftMDYDate,
          ftYMDDate: TEpiDateValueLabel(VL).Value   := VLPairs.Values[i].AsDate;
          ftTime:    TEpiTimeValueLabel(VL).Value   := VLPairs.Values[i].AsTime;
        end;
      end;

  for i := 0 to ST.OptionList.Count - 1 do
    begin
      Opt := ST.OptionList.Options[i];
      if (Opt.Ident <> 'd') then continue;

      VL := VLSet.ValueLabel[Opt.Expr.AsString];
      Vl.Free;
    end;

  for i := 0 to ST.OptionList.Count - 1 do
    begin
      Opt := ST.OptionList.Options[i];
      if (Opt.Ident <> 'm') then continue;

      VLSet.ValueLabel[Opt.Expr.AsString].IsMissingValue := true;
    end;

  for i := 0 to ST.OptionList.Count - 1 do
    begin
      Opt := ST.OptionList.Options[i];
      if (Opt.Ident <> 'nom') then continue;

      VLSet.ValueLabel[Opt.Expr.AsString].IsMissingValue := false;
    end;

  if ST.HasOption('r', Opt) then
    VLSet.Name := Opt.Expr.AsIdent;

  FOutputCreator.DoInfoAll('Edited valuelabel: ' + VLSet.Name + LineEnding +
    Format('before: (%d (value, label) pairs, %d missing)', [BeforeCount, BeforeMissing]) + LineEnding +
    Format('after:  (%d (value, label) pairs, %d missing)', [VLSet.Count, VLSet.MissingCount])
  );

  ST.ExecResult := csrSuccess;
end;

procedure TExecEdit.DoEditDataset(ST: TEditDataset);
var
  Opt: TOption;
  DF: TEpiDataFile;
  DR: TEpiDetailRelation;
  MR: TEpiMasterRelation;

  procedure RecurseMoveToTop(Rel: TEpiMasterRelation);
  var
    NewRel: TEpiMasterRelation;
  begin
    NewRel := FExecutor.Document.Relations.NewMasterRelation;
    NewRel.Datafile := Rel.Datafile;

    for NewRel in Rel.DetailRelations do
      RecurseMoveToTop(NewRel);

    Rel.Datafile := nil;
    Rel.Free;
  end;

begin
  DF := FExecutor.Document.DataFiles.GetDataFileByName(ST.Variable.Ident);
  MR := FExecutor.Document.Relations.MasterRelationFromDatafileName(DF.Name);
  DR := TEpiDetailRelation(MR);

  if (ST.HasOption('size', Opt)) then
    DF.Size := Opt.Expr.AsInteger;

  if (ST.HasOption('label', Opt)) then
    DF.Caption.Text := Opt.Expr.AsString;

  if (St.HasOption('childobs', Opt)) then
    DR.MaxRecordCount := Opt.Expr.AsInteger;

  if (St.HasOption('afterobs', Opt)) then
    DF.AfterRecordState := TEpiDataFileAfterRecordState(Opt.Expr.AsInteger);

  if (St.HasOption('statusbar', Opt)) then
    DF.StatusbarContentString := Opt.Expr.AsString;

  if ST.HasOption('r', Opt) then
    DF.Name := Opt.Expr.AsIdent;

  if ST.HasOption('noparent') then
    begin
      if (not (MR is TEpiDetailRelation)) then
        FOutputCreator.DoWarning('"' + DF.Name + '" is NOT a related dataset. Ignoring option !noparent')
      else
        RecurseMoveToTop(MR);
    end;

  ST.ExecResult := csrSuccess;
end;

procedure TExecEdit.DoEditData(ST: TEditData);
var
  SelectVec: TEpiIntField;
  DF: TEpiDataFile;
  i: Integer;
begin
  SelectVec := FExecutor.SelectVector;
  DF := FExecutor.DataFile;


  if (ST.HasOption('md')) then
    begin
      for i := 0 to SelectVec.Size - 1 do
        DF.Deleted[SelectVec.AsInteger[i]] := true;

      FOutputCreator.DoInfoAll(IntToStr(SelectVec.Size) + ' obs. marked for deletion');
    end;

  if (ST.HasOption('nomd')) then
    begin
      for i := 0 to SelectVec.Size - 1 do
        DF.Deleted[SelectVec.AsInteger[i]] := false;

      FOutputCreator.DoInfoAll(IntToStr(SelectVec.Size) + ' obs. removed marked for deletion');
    end;

  if (ST.HasOption('mv')) then
    begin
      for i := 0 to SelectVec.Size - 1 do
        DF.Verified[SelectVec.AsInteger[i]] := true;

      FOutputCreator.DoInfoAll(IntToStr(SelectVec.Size) + ' obs. marked verified');
    end;

  if (ST.HasOption('nomv')) then
    begin
      for i := 0 to SelectVec.Size - 1 do
        DF.Verified[SelectVec.AsInteger[i]] := false;

      FOutputCreator.DoInfoAll(IntToStr(SelectVec.Size) + ' obs. removed verified mark');
    end;
end;

constructor TExecEdit.Create(AExecutor: TExecutor;
  AOutputCreator: TOutputCreator);
begin
  FExecutor := AExecutor;
  FOutputCreator := AOutputCreator;
end;

destructor TExecEdit.Destroy;
begin
  inherited Destroy;
end;

procedure TExecEdit.ExecEdit(EditCommand: TCustomCrudCommand);
begin
  case EditCommand.SubCommand of
    ccDataset:
      DoEditDataset(TEditDataset(EditCommand));

    ccProject:
      DoEditProject(TEditProject(EditCommand));

    ccValuelabel:
      DoEditValuelabel(TEditValueLabel(EditCommand));

    ccData:
      DoEditData(TEditData(EditCommand));
  end;
end;

end.

