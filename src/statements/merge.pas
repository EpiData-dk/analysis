unit merge;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, executor, ast, epiopenfile, epidocument, outputcreator,
  epidatafiles, epidatafilerelations, datamodule, epidocument_helper;

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
    function MergableFieldsAreCompatible(MainField, MergeField: TEpiField;
      Options: TOptionList; out ErrorMessage: UTF8String): boolean;
  protected
    function InternalCheckAndOpenFile(ST: TCustomMergeCommand; out
      Docfile: TEpiDocumentFile): boolean;
    procedure InternalAppend(ST: TAppendCommand; AppendDocFile: TEpiDocumentFile);
    function InternalMerge(ST: TMergeCommand; MergeDF: TEpiDataFile; VarNames: TStrings): TEpiDataFile;
  public
    constructor Create(AExecutor: TExecutor; AOutputCreator: TOutputCreator); virtual;
    procedure DoAppend(ST: TAppendCommand);
    function  DoMerge(ST: TMergeCommand): TEpiDataFile;
  end;


implementation

uses
  epidatafilestypes, epidatafilerelations_helper, LazUTF8, epitools_integritycheck,
  epicustombase, math, epivaluelabels, ast_types, Token, LazFileUtils, epidatafileutils,
  epifields_helper, epimiscutils;

const
  MERGE_CUSTOM_DATA = 'MERGE_CUSTOM_DATA';

type

  { TInternalNewValuelabel }

  TInternalNewValuelabel = class(TNewValuelabel)
  protected
    function GetExecFlags: TCustomStatementExecutionFlags; override;
  end;

{ TInternalNewValuelabel }

function TInternalNewValuelabel.GetExecFlags: TCustomStatementExecutionFlags;
begin
  Result := inherited GetExecFlags + [sefInternal];
end;

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
    DstItem, Item: TEpiCustomControlItem;
    F: TEpiField absolute DstItem;
    VLSet: TEpiValueLabelSet;
  begin
    if DstDatafile.ControlItems.ItemExistsByName(SrcItem.Name) then
      Exit;

    if (SrcItem is TEpiSection) then
      begin
        DstItem := TEpiSection.Create(Dst);
        TEpiSection(DstItem).Top :=    TEpiSection(SrcItem).Top;
        TEpiSection(DstItem).Left :=   TEpiSection(SrcItem).Left;
        TEpiSection(DstItem).Width :=  TEpiSection(SrcItem).Width;
        TEpiSection(DstItem).Height := TEpiSection(SrcItem).Height;

        DstItem.Name                := SrcItem.Name;
        DstItem.Top := DstItem.Top + Offset;
        Dst.AddItem(DstItem);

        for Item in TEpiSection(SrcItem).Fields do
          CloneItem(Item, TEpiSection(DstItem).Fields, 0);

        for Item in TEpiSection(SrcItem).Headings do
          CloneItem(Item, TEpiSection(DstItem).Headings, 0);
      end
    else
      begin
        DstItem := TEpiCustomControlItem(SrcItem.Clone(Dst, RefMap));
        DstItem.Top := DstItem.Top + Offset;
        Dst.AddItem(DstItem);
      end;

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

      // Remove any "Relates" on a field - they are not valid anymore.
      if (Assigned(F.Relates)) then
        begin
          F.Relates.Free;
          F.Relates := nil;
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

function TMerge.MergableFieldsAreCompatible(MainField, MergeField: TEpiField;
  Options: TOptionList; out ErrorMessage: UTF8String): boolean;
begin
  ErrorMessage := '';

  if (Options.HasOption('ignorecase')) and
     (MainField.FieldType in StringFieldTypes) and
     (MergeField.FieldType in StringFieldTypes)
  then
    Result := not (MainField.FieldType = ftMemo) xor (MergeField.FieldType = ftMemo)
  else
    Result := MainField.FieldType = MergeField.FieldType;

  if (not Result) then
    ErrorMessage := Format('Type conflict for variable "%s": External dataset: (%s). Internal dataset (%s)',
           [MainField.Name, EpiTypeNames[MergeField.FieldType], EpiTypeNames[MainField.FieldType]]);
end;

function TMerge.InternalCheckAndOpenFile(ST: TCustomMergeCommand; out Docfile: TEpiDocumentFile): boolean;
var
  Opt: TOption;
  ReadCmd: TCustomStringCommand;
  OptList: TOptionList;
  S, FN: EpiString;
  DSName: String;
  MR: TEpiMasterRelation;
const
  ReadOptionNames: array[0..4] of EpiString = (
    'd', 'q', 'pw', 'login', 'force'
  );

  function CompareTreeStructure(Const RelationListA, RelationListB: TEpiDatafileRelationList): boolean;
  var
    i: Integer;
    MRA: TEpiMasterRelation;
    MRB: TEpiMasterRelation;
  begin
    result := (RelationListA.Count = RelationListB.Count);
    if not Result then exit;

    for i := 0 to RelationListA.Count - 1 do
    begin
      MRA := RelationListA.MasterRelation[i];
      MRB := RelationListB.MasterRelation[i];

      Result :=
        (MRA.Datafile.Name = MRB.Datafile.Name) and
        CompareTreeStructure(MRA.DetailRelations, MRB.DetailRelations);

      if not Result then exit;
    end;
  end;

begin
  Result := false;

  if St.HasOption('fn', Opt) then
    begin
      FN := '';
      if (Assigned(Opt.Expr)) then
        begin
          FN := OPt.Expr.AsString;

          if (not FileExistsUTF8(FN)) then
            begin
              FExecutor.Error('File does not exist: ' + FN);
              St.ExecResult := csrFailed;
              Exit;
            end;
        end;

      OptList := TOptionList.Create;

      for S in ReadOptionNames do
        if ST.HasOption(S, Opt) then
          OptList.Add(Opt);

      FExecutor.SetSaveOptions(OptList);
      ReadCMD := TReadCommand.Create(TStringLiteral.Create(FN), OptList);

      case aDM.OpenFile(ReadCMD, Docfile) of
        dfrCanceled:
          begin
            FOutputCreator.DoInfoAll('Read cancelled!');
            FExecutor.Cancelled := true;
            ST.ExecResult := csrFailed;
            FExecutor.SetSaveOptions(Nil);
            Exit;
          end;

        dfrError:
          begin
            FExecutor.Error('Error loading file: ' + ST.Filename);
            ST.ExecResult := csrFailed;
            FExecutor.SetSaveOptions(Nil);
            Exit;
          end;

        dfrSuccess:
          ST.Filename := '';
      end;
      FExecutor.SetSaveOptions(Nil);
    end
  else
    Docfile := FExecutor.DocFile;

  if (FExecutor.Document.DataFiles.Count = 0) then
    begin
      FExecutor.Error('At least one internal datafile must be present!');
      St.ExecResult := csrFailed;
      Exit;
    end;

  if (ST is TMergeCommand) then
    Exit(true);

  DSName := '';
  if St.HasOption('ds', Opt) then
    DSName := Opt.Expr.AsIdent;

  if (not St.HasOption('ds')) and (not St.HasOption('fn'))
  then
    begin
      FExecutor.Error('At least one of the options !fn or !ds must be used');
      St.ExecResult := csrFailed;
      Exit;
    end;

  if (DSName = '') then
    begin
      if (not CompareTreeStructure(FExecutor.Document.Relations, Docfile.Document.Relations))   then
        begin
          FExecutor.Error('The two projects do not share the same structure of related datasets!');
          St.ExecResult := csrFailed;
          Exit;
        end;
    end
  else
    if (Docfile.Document.DataFiles.Count > 1) and
       (DSName = '')
    then
      begin
        FExecutor.Error('The project "' + Docfile.FileName + '" has more than one dataset, and !ds is not specified');
        St.ExecResult := csrFailed;
        Exit;
      end;

  if (DSName <> '') and
     (not Docfile.Document.DataFiles.ItemExistsByName(DSName))
  then
    begin
      FExecutor.Error('Dataset "' + DSName + '" not found in: ' + Docfile.FileName);
      St.ExecResult := csrFailed;
      Exit;
    end;

  Result := true;
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
  DFName: String;

  procedure AppendDataset(Src, Dst: TEpiDataFile; DstFields: TEpiFields);
  var
    DstF, SrcF: TEpiField;
    I: integer;
  begin
    StartIdx := Dst.Size;
    Dst.Size := Dst.Size + Src.Size;

    for DstF in DstFields do
      begin
        SrcF := Src.Fields.FieldByName[DstF.Name];

        if (not Assigned(SrcF)) then Continue;

        for i := 0 to Src.Size - 1 do
          TransferData(DstF, SrcF, StartIdx + i, i);
      end;

    for i := 0 to Src.Size - 1 do
      begin
        if Src.Deleted[i] then Dst.Deleted[StartIdx + i] := True;
        if Src.Verified[i] then Dst.Verified[StartIdx + i] := True;
      end;
  end;

begin
  AppendDoc := AppendDocFile.Document;

  FOutputCreator.DoInfoAll('Appending data...');
  FOutputCreator.RequestRedraw;

  if ((FExecutor.Document.DataFiles.Count > 1) and (AppendDoc.DataFiles.Count > 1)) and
     (not ST.HasOption('ds'))
  then
    begin
      if Assigned(St.VariableList) and
         (St.VariableList.Count > 0)
      then
        begin
          FExecutor.Error('When appending whole projects, it is not possible to select individual variables!');
          St.ExecResult := csrFailed;
          Exit;
        end;

      for DF in FExecutor.Document.DataFiles do
        begin
          AppendDF := AppendDoc.DataFiles.GetDataFileByName(DF.Name);
          AppendDataset(AppendDF, DF, DF.Fields);
        end;
    end
  else
    begin
      DF := FExecutor.DataFile;
      if ST.VariableList.Count > 0 then
        LocalFields := FieldsFromStrings(ST.VariableList.GetIdentsAsList, DF)
      else
        LocalFields := DF.Fields;

      if ST.HasOption('ds', Opt) then
        AppendDF := AppendDoc.DataFiles.GetDataFileByName(Opt.Expr.AsIdent)
      else
        AppendDF := AppendDoc.DataFiles[0];

      AppendDataset(AppendDF, DF, LocalFields);
    end;

  FOutputCreator.DoInfoAll('Complete');
  FOutputCreator.DoWarning('Check data for inconsistencies after appending!' + LineEnding +
                           'Apply commands:  Check key / check relate / browse UPPER variables!');
end;

function TMerge.InternalMerge(ST: TMergeCommand; MergeDF: TEpiDataFile;
  VarNames: TStrings): TEpiDataFile;
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
  TableLookup, CaseSensitive: Boolean;
  Opts: TOptionList;
  NewST: TCustomNew;
  VLList: TValueLabelPairs;
  RefMap: TEpiReferenceMap;
  Opt: TOption;
  S: UTF8String;
  NewCaption: String;

  function CompareKeys(MainIdx, MergeIdx: Integer; CaseSensitive: Boolean): TValueRelationship;
  var
    MainField, MergeField: TEpiField;
    I: Integer;
  begin
    Result := 0;

    for i := 0 to MainKeyFields.Count - 1 do
      begin
        MainField := MainKeyFields[i];
        MergeField := MergeKeyFields[i];

        CompareFieldRecords(Result, MainField, MergeField, MainIdx, MergeIdx, CaseSensitive);
        if (Result <> 0) then
          Exit;
      end;
  end;

begin
  Result := nil;

  RefMap := TEpiReferenceMap.Create;
  MainDF := TEpiDataFile(FExecutor.DataFile.Clone(nil, RefMap));

  if (ST.HasOption('label', Opt)) then
    NewCaption := Opt.Expr.AsString
  else
    NewCaption := 'Merged datasets: ' + MainDF.Name + ' and ' + MergeDF.Name;

  if (ST.HasOption('r', Opt)) then
    begin
      S := Opt.Expr.AsIdent;
      if (not FExecutor.Document.DataFiles.ValidateRename(S, false)) then
        begin
          FExecutor.Error('Name already used: ' + S);
          ST.ExecResult := csrFailed;
          MainDF.Free;
          RefMap.Free;
          Exit;
        end;
      MainDF.Name := S;
    end
  else
    begin
      i := 1;
      S := 'mds' ;
      while (not FExecutor.Document.DataFiles.ValidateRename(S + IntToStr(i), false)) do
        inc(i);
      MainDF.Name := S + IntToStr(i);
    end;
  FExecutor.Document.DataFiles.AddItem(MainDF);
  RefMap.FixupReferences;
  RefMap.Free;
  MainDF.Relates.ClearAndFree;
  MainDF.Caption.Text := NewCaption;
  FExecutor.Document.Relations.NewMasterRelation.Datafile := MainDF;

  MainKeyFields  := FieldsFromStrings(Varnames, MainDF);
  MergeKeyFields := FieldsFromStrings(Varnames, MergeDF);

  CaseSensitive := not ST.HasOption('ignorecase');

  KeyChecker := TEpiIntegrityChecker.Create;
  // If we are performing a table lookup, then check that the "table" passes the index integrity check.
  if (ST.HasOption('table')) then
    if (not KeyChecker.IndexIntegrity(MergeDF, TmpRecsA, TmpRecsB, true, MergeKeyFields, CaseSensitive)) then
      begin
        FExecutor.Error(
          Format('External lookup table dataset "%s" has a non-unique key combination', [MergeDF.Name]) + LineEnding +
          'Please use the integrity tool to get a complete list of non-unique values'
        );
        ST.ExecResult := csrFailed;
        Exit;
      end;

  if (not ST.HasOption('table')) and
     (not KeyChecker.IndexIntegrity(MainDF, TmpRecsA, TmpRecsB, true, MainKeyFields, CaseSensitive))
  then
    begin
      FExecutor.Error(
        Format('Internal dataset "%s" has a non-unique key combination', [MainDF.Name]) + LineEnding +
        'Please use the integrity tool to get a complete list of non-unique values'
      );
      ST.ExecResult := csrFailed;
      Exit;
    end;
  KeyChecker.Free;

  if (MainDF.Fields.ItemExistsByName('mergevar')) then
    begin
      FExecutor.Error(
        '"mergevar" already exists in the main dataset!' + LineEnding +
        'Drop or rename the variable before using "merge"'
      );
      MainDF.Free;
      ST.ExecResult := csrFailed;
      Exit;
    end;

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

  if (not MainDf.ValueLabels.ItemExistsByName('_mergevar_lbl')) then
    begin
      VLList := TValueLabelPairs.Create(rtInteger);
      VLList.AddPair(TIntegerLiteral.Create(1), TStringLiteral.Create('In main dataset only'));
      VLList.AddPair(TIntegerLiteral.Create(2), TStringLiteral.Create('In merged dataset only'));
      VLList.AddPair(TIntegerLiteral.Create(3), TStringLiteral.Create('In both datasets'));
      Opts := TOptionList.Create;

      NewST := TInternalNewValuelabel.Create(VLList, ftInteger, TVariable.Create('_mergevar_lbl', FExecutor), Opts);
      NewST.AssignToken(TToken.Create(ST.LineNo, ST.ColNo, ST.ByteNo));
      NewST.ExecFlags;

      FExecutor.ExecStatement(NewST);
      NewST.Free;
      VLList.Free;
      Opts.Free;
    end;

  CombineVar := MainDF.NewField(ftInteger);
  CombineVar.Name := 'mergevar';
  CombineVar.Question.Text := 'Source of information for each observation';
  CombineVar.Length := 1;
  CombineVar.ValueLabelSet := MainDF.ValueLabels.GetValueLabelSetByName('_mergevar_lbl');
  CombineVar.Left := MainDF.Field[MainDF.Fields.Count - 1].Left;
  CombineVar.Top  := MainDF.Field[MainDF.Fields.Count - 1].Top + 30;

  for i := 0 to CombineVar.Size - 1 do
    CombineVar.AsInteger[i] := 1;

  TableLookup := ST.HasOption('table');

  MainDFBeforeSize := MainDF.Size;

  MainRunner := 0;
  MergeRunner := 0;
  PrevCompare := LessThanValue;

  while (MainRunner < MainDFBeforeSize) and
        (MergeRunner < MergeDF.Size)
  do
    begin
      case CompareKeys(MainRunner, MergeRunner, CaseSensitive) of
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

                // First transfer data from existing fields into new records.
                for MainF in MainDF.Fields do
                  TransferData(MainF, MainF, DestIdx, MainRunner, moReplace);

                for MergeF in MergeDF.Fields do
                  begin
                    MainF := MainDF.Fields.FieldByName[MergeF.Name];
                    TransferData(MainF, MergeF,  DestIdx, MergeRunner,  MergeOpt);
                  end;
              end
            else
              begin
                DestIdx := MainRunner;

                for MergeF in MergeDF.Fields do
                  begin
                    MainF := MainDF.Fields.FieldByName[MergeF.Name];
                    if MainF.IsKeyfield then
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
                    TransferData(MainF, MergeF, DestIdx, MergeRunner, moReplace)
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
            TransferData(MainF, MergeF, DestIdx, MergeRunner, moReplace)
          end;

        CombineVar.AsInteger[DestIdx] := 2;
        Inc(MergeRunner);
      end;

  MainDF.SortRecords(MainKeyFields);
  Result := MainDF;

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
  if (not InternalCheckAndOpenFile(ST, AppendDocFile)) then
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

function TMerge.DoMerge(ST: TMergeCommand): TEpiDataFile;
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
  S, ErrorMessage: UTF8String;
  VariableCheck: Boolean;

  function DoDatafilesCheck(Datafiles: TEpiDataFiles): boolean;
  var
    Opt: TOption;
    DF: TEpiDataFile;
    S: String;
  begin
    result := true;

    if (ST.HasOption('ds', Opt)) and
       (not Datafiles.ItemExistsByName(Opt.Expr.AsIdent))
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
  Result := nil;
  MergeDocFile := nil;

  if (not InternalCheckAndOpenFile(ST, MergeDocFile)) then
    Exit(nil);

  if (MergeDocFile <> FExecutor.DocFile) then
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
          MergeDocFile.Free;
          Exit;
        end;

      if (ST.VariableList.Count = 0)
      then
        begin
          FExecutor.Error('Key variables must be used when merging with an external dataset!');
          ST.ExecResult := csrFailed;
          MergeDocFile.Free;
          Exit;
        end;

      if (not DoDatafilesCheck(MergeDocFile.Document.DataFiles)) then
        begin
          ST.ExecResult := csrFailed;
          MergeDocFile.Free;
          Exit;
        end;

      if (ST.HasOption('ds', Opt)) then
        MergeDF := MergeDocFile.Document.DataFiles.GetDataFileByName(Opt.Expr.AsIdent)
      else
        MergeDF := MergeDocFile.Document.DataFiles[0];

      VariableCheck := True;
      for i := 0 to ST.VariableList.Count - 1 do
        begin
          V := ST.VariableList[i];
          MergeF := MergeDF.Fields.FieldByName[V.Ident];

          if not Assigned(MergeF) then
          begin
            FExecutor.Error(Format('Variable "%s" not found in external dataset!', [V.Ident]));
            VariableCheck := False;
            Continue;
          end;

          MainF := FExecutor.DataFile.Fields.FieldByName[V.Ident];
          if not MergableFieldsAreCompatible(MainF, MergeF, ST.Options, ErrorMessage) then
          begin
            FExecutor.Error(ErrorMessage);
            VariableCheck := False;
          end;
        end;

      if (not VariableCheck) then
        begin
          ST.ExecResult := csrFailed;
          MergeDocFile.Free;
          Exit;
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

  Result := InternalMerge(ST, MergeDF, VarNames);

  if (Assigned(Result)) then
    begin
      S := 'Successfully merged ' + MergeDF.Name + ' into ' + FExecutor.DataFile.Name + LineEnding +
           'Resulting dataset name: ' + Result.Name;

      if (MergeDocFile.Document.IsEncrypted()) and
         (not FExecutor.Document.IsEncrypted())
      then
        S := S + LineEnding +
             'Warning: Merged dataset is UN-encrypted';

      FOutputCreator.DoNormal(S);
    end;

  if (MergeDocFile <> FExecutor.DocFile) then
    MergeDocFile.Free;
end;

end.

