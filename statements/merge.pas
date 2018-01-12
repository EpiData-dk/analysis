unit merge;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, executor, ast, epiopenfile, epidocument, outputcreator,
  epidatafiles, epidatafilerelations, datamodule;

type

  { TMerge }

  TMerge = class
  private
    type
      TMergeOption = (
        moNone,
        moCombine,
        moUpdate,
        moReplace
      );
  private
    FExecutor: TExecutor;
    FOutputCreator: TOutputCreator;
    function FieldsFromStrings(S: TStrings; DF: TEpiDataFile): TEpiFields;
    procedure TransferData(DestField, SrcField: TEpiField; DestIdx, SrcIdx: Integer; MergeOption: TMergeOption = moReplace);
    procedure CopyStructure(DstDatafile, SrcDatafile: TEpiDataFile);
  protected
    function InternalOpenFile(ST: TCustomMergeCommand; out
      Docfile: TEpiDocumentFile): boolean;
    procedure InternalAppend(ST: TAppendCommand; AppendDocFile: TEpiDocumentFile);
    procedure InternalMerge(ST: TMergeCommand; MergeDF: TEpiDataFile; VarNames: TStrings);
  public
    constructor Create(AExecutor: TExecutor; AOutputCreator: TOutputCreator); virtual;
    procedure DoAppend(ST: TAppendCommand);
    procedure DoMerge(ST: TMergeCommand);
  end;


implementation

uses
  epidatafilestypes, epidatafilerelations_helper, LazUTF8, epitools_integritycheck,
  epicustombase, math, epivaluelabels, ast_types, Token, LazFileUtils;

const
  MERGE_CUSTOM_DATA = 'MERGE_CUSTOM_DATA';

{ TMerge }

function TMerge.FieldsFromStrings(S: TStrings; DF: TEpiDataFile): TEpiFields;
var
  St: String;
begin
  result := TEpiFields.Create(nil);
  result.ItemOwner := false;

  for St in S do
    result.AddItem(DF.Fields.FieldByName[St]);
end;

procedure TMerge.TransferData(DestField, SrcField: TEpiField; DestIdx,
  SrcIdx: Integer; MergeOption: TMergeOption);
var
  WasInMain: Boolean;
begin
  // If MERGE_CUSTOM_DATA < 1 then the field was in the main datafile
  WasInMain := PtrInt(DestField.FindCustomData(MERGE_CUSTOM_DATA)) < 1;

  case MergeOption of
    moNone:
      if WasInMain then Exit;

    moCombine:
      if WasInMain and (not DestField.IsMissing[DestIdx]) then Exit;

    moUpdate:
      if WasInMain and (SrcField.IsMissing[SrcIdx]) then Exit;

    moReplace:
      ;
  end;

  Case DestField.FieldType of
    ftBoolean:
      DestField.AsBoolean[DestIdx] := SrcField.AsBoolean[SrcIdx];

    ftInteger,
    ftAutoInc:
      DestField.AsInteger[DestIdx] := SrcField.AsInteger[SrcIdx];

    ftFloat:
      DestField.AsFloat[DestIdx] := SrcField.AsFloat[SrcIdx];

    ftDMYDate,
    ftMDYDate,
    ftYMDDate,
    ftDMYAuto,
    ftMDYAuto,
    ftYMDAuto:
      DestField.AsDate[DestIdx] := SrcField.AsDate[SrcIdx];

    ftTime,
    ftTimeAuto:
      DestField.AsTime[DestIdx] := SrcField.AsTime[SrcIdx];

    ftUpperString,
    ftString,
    ftMemo:
      DestField.AsString[DestIdx] := SrcField.AsString[SrcIdx];
  end;
end;

procedure TMerge.CopyStructure(DstDatafile, SrcDatafile: TEpiDataFile);
var
  F: TEpiField;
  RefMap: TEpiReferenceMap;
  OffsetTop: Integer;
  H: TEpiHeading;
  S: TEpiCustomControlItem;

  procedure CloneItem(SrcItem: TEpiCustomControlItem; Dst: TEpiCustomControlItemList; Offset: Integer);
  var
    DstItem: TEpiCustomControlItem;
    F: TEpiField absolute DstItem;
    VLSet: TEpiValueLabelSet;
  begin
    if DstDatafile.ControlItems.ItemExistsByName(SrcItem.Name) then
      Exit;

    DstItem := TEpiCustomControlItem(SrcItem.Clone(Dst, RefMap));
    DstItem.Top := DstItem.Top + Offset;
    Dst.AddItem(DstItem);
    if (DstItem is TEpiField) then
    begin
      F.Size := DstDatafile.Size;
      F.ResetData;
      DstItem.AddCustomData(MERGE_CUSTOM_DATA, TObject(1));

      if Assigned(TEpiField(SrcItem).ValueLabelSet) and
         (not DstDatafile.ValueLabels.ItemExistsByName(TEpiField(SrcItem).ValueLabelSet.Name))
      then
        begin
          VLSet := TEpiValueLabelSet(TEpiField(SrcItem).ValueLabelSet.Clone(DstDatafile.ValueLabels, RefMap));
          DstDatafile.ValueLabels.AddItem(VLSet);
          F.ValueLabelSet := VLSet;
        end;
    end;
  end;

begin
  RefMap := TEpiReferenceMap.Create;

  OffsetTop := TEpiCustomControlItem(DstDatafile.ControlItems[DstDatafile.ControlItems.Count - 1]).Top + 40;

  for F in SrcDatafile.MainSection.Fields do
    CloneItem(F, DstDatafile.MainSection.Fields, OffsetTop);

  for H in SrcDatafile.MainSection.Headings do
    CloneItem(H, DstDatafile.MainSection.Headings, OffsetTop);

  for S in SrcDatafile.Sections do
    if S <> SrcDatafile.MainSection then
      CloneItem(S, DstDatafile.Sections, OffsetTop);

  RefMap.FixupReferences;
  RefMap.Free;
end;

function TMerge.InternalOpenFile(ST: TCustomMergeCommand; out
  Docfile: TEpiDocumentFile): boolean;
var
  Opt: TOption;
  ReadCmd: TCustomStringCommand;
  OptList: TOptionList;
  S, FN: EpiString;
const
  ReadOptionNames: array[0..4] of EpiString = (
    'd', 'q', 'h', 'pw', 'login'
  );
begin
  FN := '';
  if (ST.HasOption('fn', Opt)) and
     (Assigned(Opt.Expr))
  then
    begin
      FN := Opt.Expr.AsString;

      if (not FileExistsUTF8(FN)) then
      begin
        FExecutor.Error('File does not exist: ' + FN);
        ST.ExecResult := csrFailed;
        Exit(false);
      end;
    end;

  OptList := TOptionList.Create;

  for S in ReadOptionNames do
    if ST.HasOption(S, Opt) then
      OptList.Add(Opt);

  ReadCmd := TCustomStringCommand.Create(
    TStringLiteral.Create(Fn),
    OptList,
    'read'
  );

  result := false;
  case aDM.OpenFile(ReadCmd, DocFile) of
    dfrCanceled:
      begin
        FOutputCreator.DoInfoAll('Read cancelled!');
        FExecutor.Cancelled := true;
        ST.ExecResult := csrFailed;
        Exit;
      end;

    dfrError:
      begin
        FExecutor.Error('Error loading file: ' + ST.Filename);
        ST.ExecResult := csrFailed;
        Exit;
      end;

    dfrSuccess:
      begin
        ST.Filename := '';
        result := true;
      end;
  end;
end;

procedure TMerge.InternalAppend(ST: TAppendCommand;
  AppendDocFile: TEpiDocumentFile);
var
  Opt: TOption;
  DFs: TEpiDataFiles;
  AppendDoc: TEpiDocument;
  DF, AppendDF: TEpiDataFile;
  Strict: Boolean;
  LocalFields: TEpiFields;
  F, AppendF: TEpiField;
  StartIdx, i: Integer;

begin
  AppendDoc := AppendDocFile.Document;

  if ST.HasOption('ds', Opt) then
    begin
      DFs := TEpiDataFiles.Create(nil);
      DFs.AddItem(FExecutor.Document.DataFiles.GetItemByName(Opt.Expr.AsIdent));
    end
  else
    DFs := FExecutor.Document.Relations.GetOrderedDataFiles;

  Strict := ST.HasOption('strict');

  // First sanity checks, so we don't manipulate data and half-way find an error!
  for DF in DFs do
    begin
      AppendDF := AppendDoc.DataFiles.GetDataFileByName(DF.Name);

      if (not Assigned(AppendDF)) then
        begin
          if (not Strict) then
            Continue;

          FExecutor.Error(
            Format(
              'Dataset "%s" not found in append project!',
              [DF.Name]
            )
          );
          ST.ExecResult := csrFailed;
          Exit;
        end;

      if ST.VariableList.Count > 0 then
        LocalFields := FieldsFromStrings(ST.VariableList.GetIdentsAsList, DF)
      else
        LocalFields := DF.Fields;

      for F in LocalFields do
        begin
          AppendF := AppendDF.Fields.FieldByName[F.Name];

          if (not Assigned(AppendF)) or
             ((F.FieldType <> AppendF.FieldType) and Strict)
          then
            begin
              if (not Strict) then
                Continue;

              FExecutor.Error(
                Format(
                  'Variable "%s" not found in dataset "%s" append project!',
                  [F.Name, AppendDF.Name]
                )
              );
              ST.ExecResult := csrFailed;
              Exit;
            end;
        end;
    end;

  FOutputCreator.DoInfoAll('Appending data...');
  FOutputCreator.RequestRedraw;

  for DF in DFs do
    begin
      AppendDF := AppendDoc.DataFiles.GetDataFileByName(DF.Name);

      if (not Assigned(AppendDF)) then
        Continue;

      StartIdx := DF.Size;
      DF.Size := DF.Size + AppendDF.Size;

      if ST.VariableList.Count > 0 then
        LocalFields := FieldsFromStrings(ST.VariableList.GetIdentsAsList, DF)
      else
        LocalFields := DF.Fields;


      for F in LocalFields do
        begin
          AppendF := AppendDF.Fields.FieldByName[F.Name];

          if (not Assigned(AppendF)) then
            Continue;

          for i := 0 to AppendDF.Size - 1 do
            TransferData(F, AppendF, StartIdx + i, i);
        end;
    end;
  FOutputCreator.DoInfoAll('Complete');
  FOutputCreator.DoWarning('Data may be in an inconsisten state if the dataset(s) use keys or has related data!' + LineEnding +
                           'Use the data validator tool to check for inconsistencies!');
end;

procedure TMerge.InternalMerge(ST: TMergeCommand; MergeDF: TEpiDataFile;
  VarNames: TStrings);
var
  DFvar, V: TCustomVariable;
  MainDF: TEpiDataFile;
  MR, DR: TEpiMasterRelation;
  MergeF, MainF, CombineVar: TEpiField;
  i, MainDFBeforeSize, MainRunner, MergeRunner,
  DestIdx, SrcIdx: Integer;
  MainKeyFields, MergeKeyFields: TEpiFields;
  KeyChecker: TEpiIntegrityChecker;
  TmpRecsA, TmpRecsB: TBoundArray;
  MergeOpt: TMergeOption;
  PrevCompare: TValueRelationship;
  TableLookup: Boolean;
  Opts: TOptionList;
  NewST: TCustomNew;
  VLList: TValueLabelPairs;

  function CompareKeys(MainIdx, MergeIdx: Integer): TValueRelationship;
  var
    MainField, MergeField: TEpiField;
    I: Integer;
  begin
    Result := 0;

    for i := 0 to MainKeyFields.Count - 1 do
      begin
        MainField := MainKeyFields[i];
        MergeField := MergeKeyFields[i];

        case MainField.FieldType of
          ftBoolean:
            result := CompareValue(MainField.AsBoolean[MainIdx], MergeField.AsBoolean[MergeIdx]);

          ftInteger,
          ftAutoInc:
            result := CompareValue(MainField.AsInteger[MainIdx], MergeField.AsInteger[MergeIdx]);

          ftFloat:
            result := CompareValue(MainField.AsFloat[MainIdx], MergeField.AsFloat[MergeIdx], 0.0);

          ftDMYDate,
          ftMDYDate,
          ftYMDDate,
          ftDMYAuto,
          ftMDYAuto,
          ftYMDAuto:
            result := CompareValue(MainField.AsDate[MainIdx],  MergeField.AsDate[MergeIdx]);

          ftTime,
          ftTimeAuto:
            result := CompareValue(MainField.AsTime[MainIdx], MergeField.AsTime[MergeIdx], 0.0);

          ftUpperString,
          ftString,
          ftMemo:
            result := Sign(UTF8CompareStr(MainField.AsString[MainIdx], MergeField.AsString[MergeIdx]));
        end;
      end;
  end;

begin
  MainDF  := FExecutor.DataFile;

  MainKeyFields  := FieldsFromStrings(Varnames, MainDF);
  MergeKeyFields := FieldsFromStrings(Varnames, MergeDF);

  KeyChecker := TEpiIntegrityChecker.Create;
  // If we are performing a table lookup, then check that the "table" passes the index integrity check.
  if (ST.HasOption('table')) then
    if (not KeyChecker.IndexIntegrity(MergeDF, TmpRecsA, TmpRecsB, true, MergeKeyFields)) then
      begin
        FExecutor.Error(
          Format('External lookup table dataset "%s" has a non-unique key combination', [MainDF.Name]) + LineEnding +
          'Please use the integrity tool to get a complete list of non-unique values'
        );
        ST.ExecResult := csrFailed;
        Exit;
      end;

  if (not ST.HasOption('table')) and
     (not KeyChecker.IndexIntegrity(MainDF, TmpRecsA, TmpRecsB, true, MainKeyFields))
  then
    begin
      FExecutor.Error(
        Format('Internal dataset "%s" has a non-unique key combination', [MergeDF.Name]) + LineEnding +
        'Please use the integrity tool to get a complete list of non-unique values'
      );
      ST.ExecResult := csrFailed;
      Exit;
    end;
  KeyChecker.Free;

  CopyStructure(MainDF, MergeDF);

  MainDF.SortRecords(MainKeyFields);
  MergeDF.SortRecords(MergeKeyFields);

  MergeOpt := moNone;
  if ST.HasOption('combine') then
    MergeOpt := moCombine;
  if ST.HasOption('update') then
    MergeOpt := moUpdate;
  if ST.HasOption('replace') then
    MergeOpt := moReplace;


  CombineVar := MainDF.Fields.FieldByName['mergevar'];
  if (not Assigned(CombineVar)) then
    begin
      Opts := TOptionList.Create;
      Opts.Add(TOption.Create(TVariable.Create('label', FExecutor),  TStringLiteral.Create('Source of information for each observation')));
      Opts.Add(TOption.Create(TVariable.Create('l', FExecutor), TIntegerLiteral.Create(1)));
      NewST := TNewVariable.Create(TIntegerLiteral.Create(1), ftInteger, TVariable.Create('mergevar', FExecutor), Opts);
      NewST.AssignToken(TToken.Create(ST.LineNo, ST.ColNo, ST.ByteNo));

      FExecutor.ExecStatement(NewST);
      NewST.Free;
      Opts.Free;

      if (not MainDf.ValueLabels.ItemExistsByName('_mergevar_lbl')) then
        begin
          VLList := TValueLabelPairs.Create(rtInteger);
          VLList.AddPair(TIntegerLiteral.Create(1), 'In main dataset only');
          VLList.AddPair(TIntegerLiteral.Create(2), 'In merged dataset only');
          VLList.AddPair(TIntegerLiteral.Create(3), 'In both datasets');
          Opts := TOptionList.Create;

          NewST := TNewValuelabel.Create(VLList, ftInteger, TVariable.Create('_mergevar_lbl', FExecutor), Opts);
          NewST.AssignToken(TToken.Create(ST.LineNo, ST.ColNo, ST.ByteNo));

          FExecutor.ExecStatement(NewST);
          NewST.Free;
          VLList.Free;
          Opts.Free;
        end;

      CombineVar := MainDF.Fields.FieldByName['mergevar'];
      CombineVar.ValueLabelSet := MainDF.ValueLabels.GetValueLabelSetByName('_mergevar_lbl');
    end;

  TableLookup := ST.HasOption('table');

  MainDFBeforeSize := MainDF.Size;

  MainRunner := 0;
  MergeRunner := 0;
  PrevCompare := EqualsValue;

  while (MainRunner < MainDFBeforeSize) and
        (MergeRunner < MergeDF.Size)
  do
    begin
      case CompareKeys(MainRunner, MergeRunner) of
        LessThanValue:
          begin
            Inc(MainRunner);
            PrevCompare := LessThanValue;
          end;

        EqualsValue:
          begin
            if (PrevCompare = EqualsValue) and
               (not TableLookup)
            then
              begin
                // In this case we have to expand on the records, since we are having
                // additional key values from the merge dataset
                DestIdx := MainDF.NewRecords();

                for MergeF in MergeDF.Fields do
                  begin
                    MainF := MainDF.Fields.FieldByName[MergeF.Name];


                    if Assigned(MainF.FindCustomData(MERGE_CUSTOM_DATA)) or
                       (MergeOpt in [moUpdate, moReplace])
                    then
                      // This is a field from the merge dataset
                      TransferData(MainF, MergeF, DestIdx, MergeRunner, moCombine)
                    else
                      // this field was in the original dataset also
                      TransferData(MainF, MainF,  DestIdx, MainRunner,  moCombine)
                  end;
              end
            else
              begin
                DestIdx := MainRunner;

                for MergeF in MergeDF.Fields do
                  begin
                    MainF := MainDF.Fields.FieldByName[MergeF.Name];
                    if MainDF.KeyFields.FieldExists(MainF) then
                      TransferData(MainF, MergeF, DestIdx, MergeRunner, moCombine)
                    else
                      TransferData(MainF, MergeF, DestIdx, MergeRunner, MergeOpt);
                  end;
              end;

            CombineVar.AsInteger[DestIdx] := 3;

            PrevCompare := EqualsValue;
            if TableLookup then
              Inc(MainRunner)
            else
              Inc(MergeRunner);
          end;

        GreaterThanValue:
          begin
            if (not TableLookup) then
              begin
                DestIdx := MainDF.NewRecords();

                for MergeF in MergeDF.Fields do
                  begin
                    MainF := MainDF.Fields.FieldByName[MergeF.Name];
                    if MainDF.KeyFields.FieldExists(MainF) then
                      TransferData(MainF, MergeF, DestIdx, MergeRunner, moCombine)
                    else
                      TransferData(MainF, MergeF, DestIdx, MergeRunner, MergeOpt);
                  end;

                CombineVar.AsInteger[DestIdx] := 2;
              end;


            PrevCompare := GreaterThanValue;
            Inc(MergeRunner);
          end;
      end;
    end;

  if (not TableLookup) then
    while (MergeRunner < MergeDF.Size) do
      begin
        DestIdx := MainDF.NewRecords();

        for MergeF in MergeDF.Fields do
          begin
            MainF := MainDF.Fields.FieldByName[MergeF.Name];
            if MainDF.KeyFields.FieldExists(MainF) then
              TransferData(MainF, MergeF, DestIdx, MergeRunner, moCombine)
            else
              TransferData(MainF, MergeF, DestIdx, MergeRunner, MergeOpt);
          end;

        Inc(MergeRunner);
      end;

  MainDF.SortRecords(MainKeyFields);

  MainKeyFields.Free;
  MergeKeyFields.Free;
end;

constructor TMerge.Create(AExecutor: TExecutor; AOutputCreator: TOutputCreator);
begin
  FExecutor := AExecutor;
  FOutputCreator := AOutputCreator;
end;

procedure TMerge.DoAppend(ST: TAppendCommand);
var
  AppendDocFile: TEpiDocumentFile;
  LastModified: TDateTime;
  DF: TEpiDataFile;
  TmpDoc: TEpiDocument;
begin
  if (not InternalOpenFile(ST, AppendDocFile)) then
    Exit;

  LastModified := TDateTime(0);

  TmpDoc := AppendDocFile.Document;
  for DF in TmpDoc.DataFiles do
    begin
      LastModified := Max(LastModified, DF.RecModifiedDate);
      LastModified := Max(LastModified, DF.StructureModifiedDate);
    end;

  FOutputCreator.DoInfoAll('Append file: ' + ExpandFileNameUTF8(AppendDocFile.FileName));
  FOutputCreator.DoInfoAll(
    'Cycle: ' + IntToStr(TmpDoc.CycleNo) +
    ' Datasets: ' + IntToStr(TmpDoc.DataFiles.Count) +
    ' Modified: ' + DateTimeToStr(LastModified)
  );

  InternalAppend(ST, AppendDocFile);

  AppendDocFile.Free;
end;

procedure TMerge.DoMerge(ST: TMergeCommand);
var
  MergeDocFile: TEpiDocumentFile;
  Datafiles: TEpiDataFiles;
  MR: TEpiMasterRelation;
  MergeDF, DF: TEpiDataFile;
  MergeF, MainF: TEpiField;
  VarNames: TStrings;
  Opt: TOption;
  V: TCustomVariable;
  i: Integer;
  LastModified: TDateTime;
  TmpDoc: TEpiDocument;

  function DoDatafilesCheck(Datafiles: TEpiDataFiles): boolean;
  var
    Opt: TOption;
    DF: TEpiDataFile;
    S: String;
  begin
    result := true;

    if (ST.HasOption('ds', Opt)) and
       (not Datafiles.ItemExistsByName(Opt.Ident))
    then
      begin
        S := '';
        for DF in Datafiles do
          S := DF.Name + ', ';

        FExecutor.Error(
          Format('Dataset id "%s" not found!', [Opt.Ident]) + LineEnding +
          'Possible datasets are: ' + S
        );
        ST.ExecResult := csrFailed;
        result := false;
      end;
  end;

begin
  MergeDocFile := nil;

  if (ST.HasOption('fn')) and
     (not InternalOpenFile(ST, MergeDocFile))
  then
    Exit;

  if Assigned(MergeDocFile) then
    begin
      LastModified := TDateTime(0);

      TmpDoc := MergeDocFile.Document;
      for DF in TmpDoc.DataFiles do
        begin
          LastModified := Max(LastModified, DF.RecModifiedDate);
          LastModified := Max(LastModified, DF.StructureModifiedDate);
        end;

      FOutputCreator.DoInfoAll('Merge file: ' + ExpandFileNameUTF8(MergeDocFile.FileName));
      FOutputCreator.DoInfoAll(
        'Cycle: ' + IntToStr(TmpDoc.CycleNo) +
        ' Datasets: ' + IntToStr(TmpDoc.DataFiles.Count) +
        ' Modified: ' + DateTimeToStr(LastModified)
      );

      if (MergeDocFile.Document.DataFiles.Count > 1) and
         (not ST.HasOption('ds'))
      then
        begin
          FExecutor.Error(
            'External file has more than one dataset' + LineEnding +
            'Use the option !ds := <id> to specify which on to use!'
          );
          ST.ExecResult := csrFailed;
          Exit;
        end;

      if (ST.VariableList.Count = 0)
      then
        begin
          FExecutor.Error('Key variables must be used when merging with an external dataset!');
          ST.ExecResult := csrFailed;
          Exit;
        end;

      if (not DoDatafilesCheck(MergeDocFile.Document.DataFiles)) then
        Exit;

      if (ST.HasOption('ds', Opt)) then
        MergeDF := MergeDocFile.Document.DataFiles.GetDataFileByName(Opt.Expr.AsIdent)
      else
        MergeDF := MergeDocFile.Document.DataFiles[0];

      for i := 0 to ST.VariableList.Count - 1 do
        begin
          V := ST.VariableList[i];
          MergeF := MergeDF.Fields.FieldByName[V.Ident];

          if not Assigned(MergeF) then
          begin
            FExecutor.Error(Format('Variable "%s" not found in external dataset!', [V.Ident]));
            ST.ExecResult := csrFailed;
            Exit;
          end;

          MainF := FExecutor.DataFile.Fields.FieldByName[V.Ident];
          if (MainF.FieldType <> MergeF.FieldType) then
          begin
            FExecutor.Error(Format('Variable "%s" in external dataset has a different type than in the internal dataset', [V.Ident]));
            ST.ExecResult := csrFailed;
            Exit;
          end;
        end;

      VarNames := ST.VariableList.GetIdentsAsList;
    end // IF ASSIGNED(MERGEDOC)
  else
    begin
      if (ST.VariableList.Count > 0) and
         (not ST.HasOption('table'))
      then
        begin
          FExecutor.Error('Key variables cannot be used when merging with a related dataset!');
          ST.ExecResult := csrFailed;
          Exit;
        end;

      if (ST.VariableList.Count = 0) and
         (ST.HasOption('table'))
      then
        begin
          FExecutor.Error('Key variables must be used when performing a tablelookup!');
          ST.ExecResult := csrFailed;
          Exit;
        end;

      MR := FExecutor.Document.Relations.MasterRelationFromDatafileName(FExecutor.DataFile.Name);
      if (MR.DetailRelations.Count > 1) and
         (not ST.HasOption('ds'))
      then
        begin
          FExecutor.Error(
            'No related dataset specified.' + LineEnding +
            'Use the option !ds := <id> to specify which on to use!'
          );
          ST.ExecResult := csrFailed;
          Exit;
        end;

      if (not ST.HasOption('table')) then
        begin
          if (MR.DetailRelations.Count = 0)
          then
            begin
              FExecutor.Error('No related dataset to perform a merge with!');
              ST.ExecResult := csrFailed;
              Exit;
            end;
        end
      else
        begin
          if (MR.DetailRelations.Count = 0) and
             (not ST.HasOption('ds'))
          then
            begin
              FExecutor.Error('No related dataset to perform a merge with!');
              ST.ExecResult := csrFailed;
              Exit;
            end;

        end;

      if (not DoDatafilesCheck(FExecutor.Document.DataFiles)) then
        Exit;

      if (ST.HasOption('ds', Opt)) then
        MergeDF := FExecutor.Document.DataFiles.GetDataFileByName(Opt.Expr.AsIdent)
      else
        MergeDF := MR.DetailRelation[0].Datafile;

      if (ST.HasOption('table')) then
        VarNames := ST.VariableList.GetIdentsAsList
      else
        begin
          VarNames := TStringList.Create;
          for MainF in FExecutor.DataFile.KeyFields do
            VarNames.Add(MainF.Name);
        end;
    end;

  InternalMerge(ST, MergeDF, VarNames);

  if (not ST.HasOption('table')) and
     (FExecutor.Document.DataFiles.IndexOf(MergeDF) >= 0)
  then
    MergeDF.Free;

  MergeDocFile.Free;
end;

end.

