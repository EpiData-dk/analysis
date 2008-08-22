unit Udocument;

interface

uses SysUtils, UVectors, classes, ansDataTypes, UVectorOp, UOutput, UCommands, Windows, UAnaToken,
     UEpiDataTypes, Uepifile, UDebug;

// ============================================================================
// Declaration of:
// Main methodes, functions and procedures.
// ============================================================================

type

  TDocument = class
  private
    function included(varname: string; varnames: tstringlist): boolean;
    function ExpandVarList(DF: TEpiDataframe; Varlist: IValue; Complement: boolean = false): TStringList;
    procedure OutResults();
    procedure OutAll(Cmd: TCommand);
    function DoDrop(cmd: TCommand): boolean;
    procedure DoFormat(cmd: TCommand);
    procedure DoClear();
    function DoRename(cmd: TCommand): boolean;
  protected
    //
  public
    function DoVariables(cmd:TCommand): boolean;
    function DoLabel(Df: TEpiDataframe; Varname, Varlabel: string): boolean;
    function DoValueLabel(Df: TEpiDataframe; Varnames: TStrings; cmd: TCommand): boolean;
    function DoMissingValue(Df: TEpiDataframe; Varnames: TStrings; cmd: TCommand): boolean;
    function DoLabelData(Df: TEpiDataframe; name: String): boolean;
    class function AppendDateType(Vector: TEpiVector; VarType: string): string; overload;
  end;


var
  ODocument: TDocument;

implementation

uses UCmdProcessor, UCheckProps, GeneralUtils;

const
  UnitName = 'Udocument';

// ============================================================================
// Public methodes, functions and procedures.
// ============================================================================


class Function TDocument.AppendDateType(Vector: TEpiVector; VarType: string): string;
var
  s: string;
begin
  result := VarType;
  if not (vector is TEpiDateVector) then exit;
  s := vector.FieldFormat;
  delete(s, 1, 1);
  result := result + ' ' + s;
end;

function TDocument.DoVariables(cmd:TCommand): boolean;
var
 Param   : IValue;

begin
  result := false;
  if (cmd.ParameterCount> 0) then //has sub commands
  begin
    Param := cmd.ParamByName['SUBCOMMAND'];
    if param<> nil then
    begin
      if Param.AsString='RESULT' then
      begin
        OutResults();
        exit;
      end;
           if not dm.CheckDataOpen() then exit; //dm.CheckDataOpen();
      if (Param.AsString='DROP') or (Param.AsString='KEEP') then
        result := DoDrop(Cmd)
      else if Param.AsString='FORMAT' then
        DoFormat(Cmd)
      else if Param.AsString='CLEAR' then
        DoClear()
      else if Param.AsString='RENAME' then
        DoRename(cmd);
    end;
  end else
    OutAll(cmd);
end;

function TDocument.DoLabel(Df: TEpiDataframe; Varname, Varlabel: string): boolean;
var
  V: TEpiVector;
begin
  V := df.VectorByName[Varname];
  V.VariableLabel := Varlabel;
  // TODO -o Torsten : Modification updates should be handles internally in dataframe.
  df.Modified := true;
  dm.NotifyInterface(EpiVectorListChanged,integer(dm.dataframe),0);
end;

function TDocument.DoLabelData(Df: TEpiDataframe; name: string): boolean;
begin
  df.DataLabel := Name;
  // TODO -o Torsten : Modification updates should be handles internally in dataframe.
  df.Modified := true;
  dm.NotifyInterface(EpiVectorListChanged,integer(dm.dataframe),0);
end;

function TDocument.DoValueLabel(Df: TEpiDataframe; Varnames: TStrings; cmd: TCommand): boolean;
var
  Vector: TEpiVector;
  i, j: integer;
  Param: IValue;
  ValueLabelSet: TLabelValueList;
  Clear: Boolean;
  Labels: TStringList;
  Lab: string;
const
  procname = 'DoValueLabel';
  procversion = '1.0.0.0';
begin
  ODebug.IncIndent;
  ODebug.Add(UnitName + ':' + procname + ' - ' + procversion, 1);
  result := false;
  clear := false;
  try
    Labels := TStringList.Create;
    for i := Cmd.ParameterCount -1 downto 0 do
    begin
      Param := Cmd.ParameterList.Items[i];
      if (AnsiUpperCase(Param.VarName) = 'VARLIST') then continue;
      if (AnsiUpperCase(Param.VarName) = 'CLEAR') then
      begin
        Clear := true;
        continue;
      end;
      Labels.AddObject(Param.VarName, TString.Create(Param.AsString));
      Cmd.ParameterList.RemoveVar(Param);
    end;
    for i := 0 to Varnames.Count-1 do
    begin
      Vector := Df.VectorByName[Varnames[i]];
      Df.Modified := true;
      ValueLabelSet := Vector.CheckProperties.ValueLabelSet;
      if not Vector.HasValueLabels() then
      begin
        if (not assigned(df.CheckProperties.ValueLabels)) then df.CheckProperties.ValueLabels:=TLabelBlocksList.Create;
        ValueLabelSet := TLabelValueList.Create();
        ValueLabelSet.LabelName := AnsiLowerCase(Vector.Name) + 'lbl';
        Vector.CheckProperties.ValueLabelSet:=ValueLabelSet;
        df.CheckProperties.ValueLabels.AddObject(ValueLabelSet.LabelName,ValueLabelSet);
      end;
      if Clear then
        ValueLabelSet.clear();
      if Vector.DataType = EpiTyBoolean then
      begin
        for j := 0 to Labels.Count-1 do
        begin
          if Labels[j][1] in BooleanYesChars then lab := string(GetTrueString)
          else lab :=  string(GetFalseString);
          if Vector.CheckProperties.ValueLabelSet.Labels[lab] <> lab then
            Vector.CheckProperties.ValueLabelSet.Labels[lab] := TString(Labels.Objects[j]).Str
          else
            Vector.CheckProperties.ValueLabelSet.AddPair(lab, TString(Labels.Objects[j]).Str);
        end;
      end else begin
        for j := 0 to Labels.Count-1 do
          if Vector.CheckProperties.ValueLabelSet.Labels[Labels[j]] <> Labels[j] then
            Vector.CheckProperties.ValueLabelSet.Labels[Labels[j]] := TString(Labels.Objects[j]).Str
          else
            Vector.CheckProperties.ValueLabelSet.AddPair(Labels[j], TString(Labels.Objects[j]).Str);
      end;
    end; // For i
  finally
    If Assigned(Labels) then FreeAndNil(Labels);
    ODebug.DecIndent;
  end;
end;

function TDocument.DoMissingValue(Df: TEpiDataframe; Varnames: TStrings; cmd: TCommand): boolean;
var
  Vector: TEpiVector;
  CheckProperties: TVectorChkProp;
  count, i, j, k:  integer;
  clear, cont: boolean;
  Param: IValue;
begin
  result := false;
  clear := (Cmd.ParamByName['CLEAR'] <> nil) or
           (cmd.ParamByName['C'] <> nil);

  for i := 0 to varnames.Count -1 do
  begin
    CheckProperties := Df.VectorByName[Varnames[i]].CheckProperties;

    if not Assigned(CheckProperties) then
      dm.Error('Cannot create Missing values: CheckProperties does not exist!', [], 111001);

    count := 0;
    for j:=0 to MAXDEFINEDMISSINGVALUES do
      if CheckProperties.MissingValues[j]<>'' then
        if clear then
          CheckProperties.MissingValues[j] := ''
        else
          inc(count);
    if clear then
    begin
      count := 0;
      df.Modified := true;
    end;
    for j := 0 to Cmd.ParameterCount -1 do
    begin
      Param := Cmd.ParameterList.Items[j];
      if (AnsiUpperCase(Param.VarName) = 'VARLIST') or
         (AnsiUpperCase(Param.VarName) = 'CLEAR') then continue;
      cont := false;
      for k := 0 to MAXDEFINEDMISSINGVALUES do
        if CheckProperties.MissingValues[k] = Param.VarName then cont := true;
      if cont then continue;
      if count = MAXDEFINEDMISSINGVALUES+1 then dm.Error('max 3 missing values, add [/c  or /clear]', [], 111002);
      CheckProperties.MissingValues[count] := Param.VarName;
      inc(count);
      df.Modified := true;
    end;
  end; // for i
end;


// ============================================================================
// Private methodes, functions and procedures.
// ============================================================================


function TDocument.included(varname: string; varnames: tstringlist): boolean;
var
  i: integer;
begin
  result := true;
  for i := 0 to varnames.Count -1 do
    if varnames[i] = varname then exit;
  result := false;
end;

function TDocument.ExpandVarList(DF: TEpiDataframe; Varlist: IValue; Complement: boolean = false): TStringList;
var
  Vectorlist: TEpiVectors;
  List: TStringList;
  i: integer;
begin
  try
    Result := TStringList.Create;
    Vectorlist := df.GetVectorListByName(TStringList(Varlist.AsInteger));
    Vectorlist.GetVectorNames(Result);
    if not Complement then exit;
    List := Result;
    Result := TStringList.Create;
    Vectorlist := df.Vectors;
    for i := 0 to Vectorlist.Count -1 do
      if {(not Vectorlist[i].Internal) and} (not included(Vectorlist[i].Name, List)) then
        Result.Add(Vectorlist[i].Name);
  except
    dm.Info('Invalid variable name(s)', [], 211001);
    result := nil;
  end;
end;

procedure TDocument.OutResults();
var
 i, co : integer;
 tab1  : TStatTable;
begin
  // Output System variables
  co:= dm.Executor.SystemVariables.Count;
  if co>0 then
  begin
    tab1 := dm.OutputList.NewTable(2,1);
    tab1.TableType := sttSystem;
    tab1.Cell[1,1] := 'System Variables';
    tab1.Cell[2,1] := 'Value';
    for i:= 0 to co-1 do
    begin
       tab1.AddRow;
       tab1.Cell[1,i+2] := dm.Executor.SystemVariables.items[i].Varname;
       tab1.Cell[2,i+2] := dm.Executor.SystemVariables.items[i].AsString;
    end;
    dm.CodeMaker.OutputTable(tab1,'');
    dm.Sendoutput;
  end;

  // Output Global variables
  co:=dm.Executor.GlobalVariables.Count;
  if co>0 then
  begin
    tab1 := dm.OutputList.NewTable(2,1);
    tab1.TableType := sttSystem;
    tab1.Cell[1,1] := 'Global Variables';
    tab1.Cell[2,1] := 'Value';
    for i:= 0 to co-1 do
    begin
      tab1.AddRow;
      tab1.Cell[1,i+2] := dm.Executor.GlobalVariables.items[i].Varname;
      tab1.Cell[2,i+2] := dm.Executor.GlobalVariables.items[i].AsString;
    end;
    dm.CodeMaker.OutputTable(tab1,'');
    dm.Sendoutput;
  end;

  // Output Result variables
  co :=dm.Executor.ResultVariables.Count;
  if co > 0 then
  begin
    tab1 := dm.OutputList.NewTable(2,1);
    tab1.TableType := sttSystem;
    tab1.Cell[1,1] := 'Temporary Variables' ;
    tab1.Cell[2,1] := 'Value';
    for i:= 0 to co-1 do
    begin
      tab1.AddRow;
      tab1.Cell[1,i+2] := dm.Executor.ResultVariables.items[i].Varname;
      tab1.Cell[2,i+2] := dm.Executor.ResultVariables.items[i].AsString;
    end;
    dm.CodeMaker.OutputTable(tab1,'');
    dm.Sendoutput;
  end;
  exit;
end;

procedure TDocument.OutAll(Cmd: TCommand);
var
  i,j,k, co,n : integer;
  s       : string;
  tab1    : TStatTable;
  Vec: TEpiVector;
// userdata: TEpiInfoCustomUserData;

  function iInc(var i: integer): integer;
  begin
    inc(i);
    result := i;
  end;

begin
  k := 0;
       if not dm.CheckDataOpen() then exit; //dm.CheckDataOpen();
  co := dm.dataframe.VectorCount;
  tab1 := dm.OutputList.NewTable(7{8},co+1);
  tab1.TableType := sttSystem;
  tab1.caption := 'File: ' + dm.dataframe.FileName + '<br>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;' + dm.dataframe.DataLabel   ;
  tab1.Cell[iInc(k),1] := 'Name';
  tab1.Cell[iInc(k),1] := 'Type';
  tab1.Cell[iInc(k),1] := 'Length';
  tab1.Cell[iInc(k),1] := 'Decimals';
  tab1.Cell[iInc(k),1] := 'Label';
  tab1.Cell[iInc(k),1] := 'Value label';
  tab1.Cell[iInc(k),1] := 'Missing';
  for i:= 0 to co-1 do
  begin
    k := 0;
    tab1.Cell[iInc(k),i+2] := dm.dataframe.vectors[i].Name;
    tab1.Cell[iInc(k),i+2] := AppendDateType(dm.dataframe.vectors[i], GetFieldTypeName(dm.dataframe.vectors[i].FieldDataType));
    tab1.Cell[iInc(k),i+2] := inttostr(dm.dataframe.vectors[i].FieldDataSize);
    tab1.Cell[iInc(k),i+2] := inttostr(dm.dataframe.vectors[i].FieldDataDecimals);
    // TODO : Complete Variable should be written - opVariable
    tab1.Cell[iInc(k),i+2] := AnsiLowerCase(dm.dataframe.vectors[i].GetVariableLabel(Cmd.ParameterList));
    Inc(k);
    vec := dm.dataframe.vectors[i];
    if vec.HasValueLabels() then
    with vec.CheckProperties do
    begin
      for j := 0 to ValueLabelSet.Count-1 do
      begin
        tab1.Cell[k,i+2] := tab1.Cell[k,i+2] + ValueLabelSet[j] + ' = ' + PChar(ValueLabelSet.Objects[j]);
        if (j < ValueLabelSet.Count-1) then tab1.Cell[k,i+2] := tab1.Cell[k,i+2] + '<br>';
      end;
    end;
    s:='';
    for n:=0 to MAXDEFINEDMISSINGVALUES do
      if vec.CheckProperties.MissingValues[n]<>'' then s := s + vec.CheckProperties.MissingValues[n]+' ';
    tab1.Cell[iInc(k),i+2] := s;
  end;
  dm.CodeMaker.OutputTable(tab1, format('Fields: %d Records: %d' ,[dm.dataframe.vectorcount,dm.dataframe.selectedrowcount]));
  dm.Sendoutput;
end;

function TDocument.DoDrop(cmd: TCommand): boolean;
var
  list : TStringList;
  param, param2: IValue;

begin
  result := true;
  Param := cmd.ParamByName['VARLIST'];
  param2 := cmd.ParamByName['SUBCOMMAND'];
  if (Param = nil) or (Param2 = nil) then exit;
  list := ExpandVarList(dm.dataframe, param, Param2.AsString = 'KEEP');
  if not assigned(list) then exit;
  dm.Executor.DropVars(list,EpiVarlocal);
  dm.dataframe.DropVectors(list);
  if dm.dataframe.VectorCount = 0 then
  begin
    dm.info('No variables left - closing dataframe', [], 211002);
    dm.CloseFile;
    result := false;
    exit;
  end;
  dm.UpdateBrowse(opDROP);
end;

procedure TDocument.DoFormat(cmd: TCommand);
var
  Param   : IValue;
  list : TStringList;
  s: string;

begin
  Param := cmd.ParamByName['VARLIST'];
  if Param = nil then exit;
  list := TStringList(Param.AsInteger);
  Param := cmd.ParamByName['FORMAT'];
  if Param = nil then exit;
  s :=Param.AsString;
  dm.dataframe.FormatVectors(list,s);
end;

procedure TDocument.DoClear();
var
  list : TStringList;

begin
  list := TStringList.Create;
  dm.Executor.GlobalVariables.GetVarNames(list);
  dm.Executor.DropVars(list,EpiVarGlobal);
  dm.Executor.ResultVariables.GetVarNames(list);
  dm.Executor.DropVars(list,EpiVarResult);
end;

function TDocument.DoRename(Cmd: TCommand): boolean;
var
  vec: TEpiVector;
  oldname, newname: string;
begin
  oldname := cmd.ParamByName['VARNAME'].AsString;
  vec := dm.dataframe.VectorByName[oldname];
  newname := cmd.ParamByName['NEWVARNAME'].AsString;
  if dm.dataframe.FindVector(newname) <> nil then
    dm.Error('Cannot rename: Variable exists: %s', [newname],  111003);
  vec.Name := newname;
  dm.NotifyInterface(EpiVectorListChanged,integer(dm.dataframe),0);
  dm.UpdateBrowse(opRename);
  dm.Info('Variable %s renamed to: %s', [oldname,newname], 211003);
end;

end.
