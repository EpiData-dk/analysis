unit UMerge;

interface

uses SysUtils, UVectors, classes, UCommands;

// ============================================================================
// Declaration of:
// Main methodes, functions and procedures.
// ============================================================================

type

  TMerge = class
  private
    // For internally available methods!
    procedure TransferData(DestVec, SrcVec: TEpiVector; IndexA, IndexB: Integer);
    function CompareRelates(ListA, ListB: TEpiVectors; IndexA, IndexB: integer): integer;
    function LevelChanged(List: TEpiVectors; index: integer):boolean;
    function Missing(Vector: TEpiVector; Index: integer): boolean;
    procedure CopyStructure(Dataframe, Relateframe: TEpiDataframe; Varnames: TStrings);
    procedure InternalMerge(Dataframe, Relateframe: TEpiDataframe; Varnames: TStrings; Cmd: TCommand);
    procedure InternalAppend(Dataframe, Relateframe: TEpiDataframe; Varnames: TStrings; Cmd: TCommand);
  protected
    //
  public
    // For Externaly available methods.
    procedure DoAppend(Dataframe, Relateframe: TEpiDataframe; Varnames: TStrings; Cmd: TCommand);
    procedure DoMerge(Dataframe, Relateframe: TEpiDataframe; Varnames: TStrings; Cmd: TCommand);
  end;


var
  OMerge: TMerge;

implementation

uses UCmdProcessor, UDebug, Math, UAnaToken, UEpiDatatypes, AnsDatatypes,
     UEpiFile;

const
  UnitName = 'Udocument';

// ============================================================================
// Public methodes.
// ============================================================================

procedure TMerge.DoMerge(Dataframe, Relateframe: TEpiDataframe; Varnames: TStrings; Cmd: TCommand);
const
  procname = 'DoMerge';
  procversion = '1.0.0.0';
begin
  ODebug.IncIndent;
  ODebug.Add(UnitName + ':' + procname + ' - ' + procversion, 1);
  try
    InternalMerge(Dataframe, Relateframe, Dataframe.GetVectorNames(Varnames), Cmd);
  finally
    ODebug.DecIndent;
  end;
end;

procedure TMerge.DoAppend(Dataframe, Relateframe: TEpiDataframe; Varnames: TStrings; Cmd: TCommand);
const
  procname = 'DoAppend';
  procversion = '1.0.0.0';
begin
  ODebug.IncIndent;
  ODebug.Add(UnitName + ':' + procname + ' - ' + procversion, 1);
  try
    InternalAppend(Dataframe, Relateframe, Dataframe.GetVectorNames(Varnames), Cmd);
    dm.AddResult('$appendfilename', EpiTyString, Relateframe.FileName, 0, 0);
  finally
    ODebug.DecIndent;
  end;
end;


// ============================================================================
// Private/Protected methodes.
// ============================================================================
type
  TUpdateState = (mNoUpdate, mUpdate, mUpdateAll);

procedure TMerge.InternalMerge(Dataframe, Relateframe: TEpiDataframe; Varnames: TStrings; Cmd: TCommand);
var
  i, OrgRowCounter, RelRowCounter,
  FileACounter, FileBCounter, FileABCounter,                 
  TempCounter,
  CompareResult: integer;
  OrgVec, RelVec, MergeVec: TEpiVector;
  OrgList, RelList: TEpiVectors;
  TempDF: TEpiDataframe;
  ValLabels: TLabelValueList;
  UpdateState: TUpdateState;
const
  procname = 'InternalMerge';
  procversion = '1.0.0.0';
begin
  ODebug.IncIndent;
  ODebug.Add(UnitName + ':' + procname + ' - ' + procversion, 1);
  OrgVec := nil;
  RelVec := nil;

  // Check for valid merge of the two dataframes.
  for i := 0 to Varnames.Count -1 do
  begin
    OrgVec := Dataframe.VectorByName[varnames[i]];
    RelVec := Relateframe.FindVector(Varnames[i]);
    if not Assigned(RelVec) then
      Dm.Error('%s not found in %s', [Varnames[i], Relateframe.FileName] , 115001);
    if (OrgVec.DataType <> RelVec.DataType)
         and (not((OrgVec.DataType = 6 ) and (RelVec.DataType = 5)))
         and (not((OrgVec.DataType = 5 ) and (RelVec.DataType = 6)))
      then
      Dm.Error('Incompatible key variables.<br>Current file: %s of type %s<br> Merge file: %s of type %s',
			   [OrgVec.Name, GetFieldTypeName(OrgVec.DataType), RelVec.Name, GetFieldTypeName(RelVec.DataType)],
			   115002);
  end;
  OrgList := Dataframe.GetVectorListByName(Varnames);
  RelList := Relateframe.GetVectorListByName(Varnames);
  Dataframe.Sort(OrgList);
  Relateframe.Sort(RelList);
  // If not a lookup-merge then both files must have unique id's.
  if (Cmd.ParamByName['TABLE'] = nil) then
    for i := 2 to Dataframe.RowCount do
     if not LevelChanged(OrgList, i) then
       dm.Error('Non-unique key in: %s', [Dataframe.FileName], 115003);
  for i := 2 to Relateframe.RowCount do
   if not LevelChanged(RelList, i) then
     dm.Error('Non-unique key in: %s', [Relateframe.FileName], 115003);
  // Done checking.

  dm.AddResult('$mergedfilename', EpiTyString, Relateframe.FileName, 0, 0);
  for i := 0 to Varnames.Count -1 do
    dm.AddResult('$key' + IntToStr(i+1), EpiTyString, varnames[i], 10, 0);


  // Copy structure from related dataframe to original dataframe.
  CopyStructure(Dataframe, Relateframe, Varnames);

  MergeVec := Dataframe.FindVector('MergeVar');
  if not Assigned(MergeVec) then
  begin
    MergeVec := TEpiIntVector.Create('MergeVar', Dataframe.RowCount, Dataframe.CheckProperties);
    ValLabels := TLabelValueList.Create();
    ValLAbels.LabelName := 'mergelbl';
    ValLabels.AddPair('1', 'Only in memory (Original)');
    ValLabels.AddPair('2', 'Only in external file');
    ValLabels.AddPair('3', 'In both');
    Dataframe.CheckProperties.ValueLabels.AddObject('mergelbl', ValLabels);
    MergeVec.CheckProperties.ValueLabelSet := ValLabels;
    MergeVec.FieldDataType := EpiTyInteger;
    MergeVec.FieldDataSize := 1;
    MergeVec.VariableLabel := 'Source of information for each observation';
    Dataframe.Vectors.Add(MergeVec);
  END;
  for i := 1 to Dataframe.RowCount do
    MergeVec.AsInteger[i] := 1;

  if Cmd.ParamByName['TABLE'] = nil then
  begin
    TempDF := TEpiDataframe.CreateTemp(Dataframe.RowCount + Relateframe.RowCount);
    CopyStructure(TempDF, Dataframe, nil);
  end;

  UpdateState := mNoUpdate;
  if (Cmd.ParamByName['UPDATE'] <> nil) then
    UpdateState := mUpdate;
  if (Cmd.ParamByName['UPDATEALL'] <> nil) then
    UpdateState := mUpdateAll;

  FileACounter := 0;
  FileBCounter := 0;
  FileABCounter := 0;
  TempCounter := 1;
  RelRowCounter := 1;
  for OrgRowCounter := 1 to Dataframe.RowCount do
  begin
    CompareResult := CompareRelates(OrgList, RelList, OrgRowCounter, RelRowCounter);
    while CompareResult > 0 do
    // Original dataframe at current position has a (combined) higher value in the idvar(s).
    // Advance position in related dataframe until we meet a match or relateframe has higher (combined) value.
    begin
      if Cmd.ParamByName['TABLE'] = nil then
      begin
        for i := 0 to Relateframe.VectorCount-1 do
        begin
          TransferData(TempDf.VectorByName[RelateFrame.Vectors[i].Name], RelateFrame.Vectors[i],
                       TempCounter, RelRowCounter);
        end;
        TempDf.VectorByName['MergeVar'].AsInteger[TempCounter] := 2;
      end;
      Inc(RelRowCounter);
      Inc(TempCounter);
      Inc(FileBCounter);
      if RelRowCounter > Relateframe.RowCount then
      begin
        FileACounter := FileACounter + (Dataframe.RowCount - OrgRowCounter);
        break;
      end;
      CompareResult := CompareRelates(OrgList, RelList, OrgRowCounter, RelRowCounter);
    end;
    if CompareResult = 0 then
    begin
      // Current position in original dataframe and relatedataframe are equal.
      // Lookup into related dataframe and copy values in record.
      for i := 0 to Relateframe.VectorCount-1 do
      begin
        // If original vector has a missing value overwrite in any case.
        if Missing(Dataframe.VectorByName[RelateFrame.Vectors[i].Name], OrgRowCounter) then
          TransferData(Dataframe.VectorByName[RelateFrame.Vectors[i].Name], RelateFrame.Vectors[i],
                       OrgRowCounter, RelRowCounter)
        else begin
          case UpdateState of
            mNoUpdate: ; // Do nothing as requested.
            mUpdate: if (not Missing(RelateFrame.Vectors[i], RelRowCounter)) then
                       TransferData(Dataframe.VectorByName[RelateFrame.Vectors[i].Name], RelateFrame.Vectors[i],
                                    OrgRowCounter, RelRowCounter);
            mUpdateAll: TransferData(Dataframe.VectorByName[RelateFrame.Vectors[i].Name], RelateFrame.Vectors[i],
                                     OrgRowCounter, RelRowCounter);
          end;
        end;
      end;
      MergeVec.AsInteger[OrgRowCounter] := 3;
      if Cmd.ParamByName['TABLE'] = nil then
        Inc(RelRowCounter);
      Inc(FileABCounter);
    end else begin
      Inc(FileACounter);
    end;
    if RelRowCounter > Relateframe.RowCount then
    begin
      FileACounter := FileACounter + (Dataframe.RowCount - OrgRowCounter);
      break;
    end;
  end;
  // Add remaining records in related dataframe (if any).
  while (RelRowCounter <= Relateframe.RowCount) do
  begin
    if Cmd.ParamByName['TABLE'] = nil then
    begin
      for i := 0 to Relateframe.VectorCount-1 do
      begin
        TransferData(TempDf.VectorByName[RelateFrame.Vectors[i].Name], RelateFrame.Vectors[i],
                     TempCounter, RelRowCounter);
      end;
      TempDf.VectorByName['MergeVar'].AsInteger[TempCounter] := 2;
    end;
    Inc(RelRowCounter);
    Inc(TempCounter);
    Inc(FileBCounter);
  end;

  if Cmd.ParamByName['TABLE'] = nil then
  begin
    TempDF.Vectors.Capacity := TempCounter -1;
    TempDF.RowCount := TempDF.Vectors.Capacity;
    InternalAppend(Dataframe, TempDF, nil, Cmd);
  end;
  
  dm.AddResult('$merge1', EpiTyInteger, FileACounter, 6, 0);
  dm.AddResult('$merge2', EpiTyInteger, FileBCounter, 6, 0);
  dm.AddResult('$merge3', EpiTyInteger, FileABCounter, 6, 0);

  if Assigned(TempDF) then FreeAndNil(TempDF);
  ODebug.DecIndent;
end;

procedure TMerge.InternalAppend(Dataframe, Relateframe: TEpiDataframe; Varnames: TStrings; Cmd: TCommand);
var
  i, j, RowPos: integer;
  OrgVec, RelVec: TEpiVector;
const
  procname = 'InternalAppend';
  procversion = '1.0.0.0';
begin
  ODebug.IncIndent;
  ODebug.Add(UnitName + ':' + procname + ' - ' + procversion, 1);

  RowPos := Dataframe.RowCount;
  Dataframe.Vectors.Capacity := Dataframe.RowCount + Relateframe.RowCount;
  Dataframe.RowCount := Dataframe.Vectors.Capacity;

  for i := 0 to Dataframe.VectorCount -1 do
    for j := RowPos+1 to Dataframe.RowCount do
      Dataframe.Vectors[i].IsMissing[j] := true;

  if Varnames = nil then
    Varnames := Relateframe.GetVectorNames(nil); 

  for i := 0 to Varnames.Count -1 do
  begin
    OrgVec := Dataframe.FindVector(Varnames[i]);
    if not Assigned(OrgVec) then continue; 
    RelVec := Relateframe.FindVector(Varnames[i]);
    if not Assigned(RelVec) then continue; 

    for j := 1 to RelateFrame.RowCount do
      TransferData(OrgVec, RelVec, RowPos + j, j);
  end;
  for i := RowPos+1 to Dataframe.RowCount do
  begin
    Dataframe.Selected[i] := true;
    Dataframe.Deleted[i] := false;
    Dataframe.Verified[i] := false;
  end;
  Dataframe.RebuildRecnumber;

  ODebug.DecIndent;
end;

function TMerge.LevelChanged(List: TEpiVectors; index: integer):boolean;
var
  i:  integer;
begin
   result:=true;
   for i:= 0 to List.Count-1 do
     if List[i].compare(index-1,index)<>0 then exit;
   result:=false;
end;

function TMerge.Missing(Vector: TEpiVector; Index: integer): boolean;
begin
  result := (Vector.IsMissing[Index] or Vector.IsMissingValue[Index]); 
end;

procedure TMerge.TransferData(DestVec, SrcVec: TEpiVector; IndexA, IndexB: Integer);
const
  procname = 'TransferData';
  procversion = '1.0.0.0';
begin
  ODebug.IncIndent;
  ODebug.Add(UnitName + ':' + procname + ' - ' + procversion, 3);
  case DestVec.DataType of
    EpiTyDate,
    EpiTyInteger:   DestVec.AsInteger[IndexA] := SrcVec.AsInteger[IndexB];
    EpiTyFloat:     DestVec.AsFloat[IndexA]   := SrcVec.AsFloat[IndexB];
    EpiTyString,
    EpiTyBoolean,
    EpiTyUppercase: DestVec.AsString[IndexA]  := SrcVec.AsString[IndexB];
  end;
  ODebug.DecIndent;
end;

function TMerge.CompareRelates(ListA, ListB: TEpiVectors; IndexA, IndexB: integer): integer;
var
  i: integer;

  function CompareFloats(ValA, ValB: EpiFloat): integer;
  begin
    if ValA < ValB then result := -1
    else if ValA = ValB then result := 0
    else result := 1;
  end;

const
  procname = 'CompareRelates';
  procversion = '1.0.0.0';
begin
  ODebug.Add(UnitName + ':' + procname + ' - ' + procversion, 3);
  result := 0;
  for i := 0 to ListA.Count-1 do
  begin
    case ListA[i].DataType of
      EpiTyDate,
      EpiTyInteger: result := ListA.Vectors[i].AsInteger[IndexA] - ListB.Vectors[i].AsInteger[IndexB];
      EpiTyFloat: result := CompareFloats(ListA.Vectors[i].AsFloat[IndexA],ListB.Vectors[i].AsFloat[IndexB]);
      EpiTyString, EpiTyBoolean,
      EpiTyUppercase: result := AnsiCompareStr(ListA.Vectors[i].AsString[IndexA],ListB.Vectors[i].AsString[IndexB]);
    end;
    if result <> 0 then exit;
  end;
end;

procedure TMerge.CopyStructure(Dataframe, Relateframe: TEpiDataframe; Varnames: TStrings);
var
  i, j: integer;
  NewVec: TEpiVector;
  VarName: string;
const
  procname = 'CopyStructure';
  procversion = '1.0.0.0';
begin
  ODebug.IncIndent;
  ODebug.Add(UnitName + ':' + procname + ' - ' + procversion, 3);
  // Create the new structure in the original dataframe.
  for i := 0 to Relateframe.VectorCount -1 do
  begin
    VarName := RelateFrame.Vectors[i].Name;
    // Do not copy vectors used to relate dataframes.
    if (Assigned(Varnames)) and (Varnames.IndexOf(VarName) > -1) then continue;
    // Do not copy variables with same name.
    if Dataframe.FindVector(Varname) <> nil then continue;
    NewVec := RelateFrame.Vectors[i].Clone(Dataframe, true);
    NewVec.Length := Dataframe.RowCount;
    NewVec.Name := VarName;
    for j := 1 to NewVec.Length do
      NewVec.IsMissing[j] := true;
    Dataframe.Vectors.Add(NewVec);
  end;
  ODebug.DecIndent;
end;

end.
