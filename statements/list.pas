unit list;

{$codepage UTF-8}
{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fgl, ast, executor, epidocument, outputcreator,
  epidatafilerelations;

type

  { TExecList }

  TExecList = class
  private
    FST: TListCommand;
    FExecutor: TExecutor;
    FOutputCreator: TOutputCreator;
    procedure DoListData(ST: TListCommand);
    procedure DoListVar;
    procedure DoListDataSets;
    procedure DoListValueLabels;
    procedure DoListResults;
    procedure DoListGlobals;
    procedure FillDataSetTable(const Relation: TEpiMasterRelation;
      const Depth: Cardinal; const Index: Cardinal; var aContinue: boolean;
      Data: Pointer = nil);
  public
    constructor Create(AExecutor: TExecutor; AOutputCreator: TOutputCreator);
    destructor Destroy; override;
    procedure ExecList(ST: TCustomCrudCommand);
  end;

implementation

uses
  Forms, Controls, result_variables, epidatafiles, epidatafilerelations_helper,
  epivaluelabels, LazUTF8, epimiscutils, strutils, epifields_helper, options_utils;

{ TExecList }

procedure TExecList.DoListData(ST: TListCommand);
var
  T: TOutputTable;
  F: TEpiField;
  i, j, Sz: Integer;
  Gvt: TEpiGetValueLabelType;
  L: TOutputLine;
  Fields: TEpiFields;
  VGvt: TEpiGetVariableLabelType;
  DF: TEpiDataFile;
  VarList: TStrings;
  V: TCustomVariable;
begin
  Gvt := ValueLabelTypeFromOptionList(ST.OptionList, FExecutor.SetOptions, sovBrowser);
  VGvt := VariableLabelTypeFromOptionList(ST.OptionList, FExecutor.SetOptions, sovBrowser);

  VarList := ST.Variables.GetIdentsAsList;
  if ST.HasOption('del') then
    DF := FExecutor.PrepareDatafile(VarList, nil, [pdoInvertDelete])
  else
    DF := FExecutor.PrepareDatafile(VarList, nil);

  Fields := TEpiFields.Create(nil);
  Fields.UniqueNames := false;
  Fields.Sorted := false;

  if (not Assigned(FST.Variables)) or
     (FST.Variables.Count = 0)
  then
    Fields.Assign(FExecutor.SortedFields)
  else
    for V in FST.Variables do
      Fields.AddItem(FExecutor.SortedFields.FieldByName[V.Ident]);

//  Fields := DF.Fields;

  T := FOutputCreator.AddTable;
  T.ColCount := Fields.Count + 1;
  T.RowCount := DF.Size + 1;

  T.Cell[0,0].Text := '{\b Obs. No}';
  for i := 0 to DF.Size -1 do
    T.Cell[0, i + 1].Text := IntToStr(i + 1);
  T.SetColAlignment(0, taLeftJustify);

  i := 1;
  for F in Fields do
    begin
      T.Cell[i, 0].Text := '{\b ' + F.GetVariableLabel(VGvt) + '}';

      for j := 0 to DF.Size -1 do
        T.Cell[i, j + 1].Text := F.GetValueLabel(j, Gvt);

      T.SetColAlignment(i, taLeftJustify);

      Inc(i);
    end;

  FOutputCreator.DoInfoAll('Note: Browse is faster than List');
  ST.ExecResult := csrSuccess;

  Fields.Free;
end;

procedure TExecList.DoListVar;
var
  i, j: Integer;
  S: UTF8String;
  T: TOutputTable;
  F: TEpiField;
  VL: TEpiCustomValueLabel;
  Fields: TEpiFields;
  V: TCustomVariable;
begin
  Fields := TEpiFields.Create(nil);
  Fields.UniqueNames := false;
  Fields.Sorted := false;

  if (not Assigned(FST.Variables)) or
     (FST.Variables.Count = 0)
  then
    Fields.Assign(FExecutor.SortedFields)
  else
    for V in FST.Variables do
      Fields.AddItem(FExecutor.SortedFields.FieldByName[V.Ident]);

  T := FOutputCreator.AddTable;
  T.ColCount := 7;
  T.RowCount := 1 + Fields.Count;

  T.Cell[0,0].Text := '{\b Name}';
  T.Cell[1,0].Text := '{\b Type}';
  T.Cell[2,0].Text := '{\b Length}';
  T.Cell[3,0].Text := '{\b Decimal}';
  T.Cell[4,0].Text := '{\b Label}';
  T.Cell[5,0].Text := '{\b Valuelabels}';
  T.Cell[6,0].Text := '{\b Missing}';
  T.SetRowAlignment(0, taLeftJustify);


  for i := 0 to Fields.Count - 1 do
    begin
      F := Fields[i];

      T.Cell[0, i + 1].Text := F.Name + IfThen(F.IsKeyfield, '*', '');
      T.Cell[1, i + 1].Text := EpiTypeNames[F.FieldType];
      T.Cell[2, i + 1].Text := IntToStr(F.Length);
      T.Cell[3, i + 1].Text := IntToStr(F.Decimals);
      T.Cell[4, i + 1].Text := F.Question.Text;

      S := '';
      if Assigned(F.ValueLabelSet) then
        for j := 0 to F.ValueLabelSet.Count - 1 do
          begin
            VL := F.ValueLabelSet[j];
            if VL.IsMissingValue then continue;
            S := S + VL.ValueAsString + ' = ' + VL.TheLabel.Text + LineEnding;
          end;
      S := UTF8Trim(S);
      T.Cell[5, i + 1].Text := S;

      S := '';
      if Assigned(F.ValueLabelSet) then
        for j := 0 to F.ValueLabelSet.Count - 1 do
          begin
            VL := F.ValueLabelSet[j];
            if (not VL.IsMissingValue) then continue;
            S := S + VL.ValueAsString + ' = ' + VL.TheLabel.Text + LineEnding;
          end;
      S := UTF8Trim(S);

      T.Cell[6, i + 1].Text := S;

      T.SetRowAlignment(i + 1, taLeftJustify);
    end;

  Fields.Free;
end;

procedure TExecList.DoListDataSets;
var
  T: TOutputTable;
  Idx: Integer;
begin
  T := FOutputCreator.AddTable;
  T.ColCount := 6;
  T.RowCount := 1;

  Idx := 0;
  T.Cell[PostInc(Idx), 0].Text := '{\b Name}';
  T.Cell[PostInc(Idx), 0].Text := '{\b Vars}';
  T.Cell[PostInc(Idx), 0].Text := '{\b Obs}';
  T.Cell[PostInc(Idx), 0].Text := '{\b Relation}';
  T.Cell[PostInc(Idx), 0].Text := '{\b Label}';
  T.Cell[PostInc(Idx), 0].Text := '{\b Key}';
  T.SetRowBorders(0, [cbTop, cbBottom]);

  FExecutor.Document.Relations.OrderedWalk(@FillDataSetTable, T);
  T.SetRowBorders(T.RowCount -1, [cbBottom]);
  T.SetColAlignment(0, taLeftJustify);
  T.SetColAlignment(1, taRightJustify);
  T.SetColAlignment(2, taRightJustify);
  T.SetColAlignment(3, taLeftJustify);
  T.SetColAlignment(4, taLeftJustify);
  T.SetColAlignment(5, taLeftJustify);
end;

procedure TExecList.DoListValueLabels;
var
  T: TOutputTable;
  VL: TEpiValueLabelSet;
  i, j: Integer;
  VLs: TExecutorValuelabelsets;
  AList: TStrings;
begin
  VLs := FExecutor.Valuelabels;

  if Assigned(FST.Variables) then
    AList := FSt.Variables.GetIdentsAsList
  else
    AList := TStringList.Create;

  if (AList.Count = 0) then
    for i := 0 to FExecutor.Valuelabels.Count - 1 do
      AList.Add(VLs.Data[i].Ident);

  for i := 0 to AList.Count - 1 do
    begin
      VL := VLs[AList[i]].Valuelabelset;

      T := FOutputCreator.AddTable;
      T.ColCount := 3;
      T.RowCount := Vl.Count + 1;

      T.Header.Text := '{\b ' + VL.Name + '} (' + EpiTypeNames[Vl.LabelType] + ')';

      T.SetColAlignment(0, taLeftJustify);
      T.SetColAlignment(1, taLeftJustify);

      T.Cell[0,0].Text := '{\b Value}';
      T.Cell[1,0].Text := '{\b Label}';
      T.Cell[2,0].Text := '{\b Missing}';
      T.Cell[2,0].Alignment := taLeftJustify;

      for j := 0 to VL.Count - 1 do
        begin
          T.Cell[0, j + 1].Text := VL[j].ValueAsString;
          T.Cell[1, j + 1].Text := VL[j].TheLabel.Text;
          T.Cell[2, j + 1].Text := IfThen(VL[j].IsMissingValue, '*', '');
          T.Cell[2, j + 1].Alignment := taCenter;
        end;
    end;
end;

procedure TExecList.DoListResults;
var
  T: TOutputTable;
  Results: TExecutorDataVariables;
  i: Integer;
  FV: TCustomExecutorDataVariable;
  AList: TStringList;
  V: TCustomVariable;
begin
  Results := FExecutor.Results;
  AList := TStringList.Create;

  if (not Assigned(FST.Variables)) or
     (FST.Variables.Count = 0)
  then
    for i := 0 to FExecutor.Results.Count - 1 do
      AList.AddObject(Results.Keys[i], Results.Data[i])
  else
    for V in FST.Variables do
      AList.AddObject(V.Ident, Results.KeyData[V.Ident]);

  T := FOutputCreator.AddTable;
  T.ColCount := 4;
  T.RowCount := AList.Count + 1;

  T.Cell[0,0].Text := 'Name';
  T.SetColAlignment(0, taLeftJustify);

  T.Cell[1,0].Text := 'Type';
  T.SetColAlignment(1, taLeftJustify);

  T.Cell[2,0].Text := 'Datatype';
  T.SetColAlignment(2, taLeftJustify);

  T.Cell[3,0].Text := 'Value/Size';
  T.SetColAlignment(3, taLeftJustify);

  for i := 0 to AList.Count - 1 do
    begin
      FV := TCustomExecutorDataVariable(AList.Objects[i]);
      T.Cell[0, i + 1].Text := FV.Ident;
      T.Cell[2, i + 1].Text := EpiTypeNames[FV.DataType];
      case FV.VarType of
        evtResultConst:
          begin
            T.Cell[1, i + 1].Text := 'const';
            T.Cell[3, i + 1].Text := FV.AsStringVector[0];
          end;

        evtResultVector:
          begin
            T.Cell[1, i + 1].Text := 'vector';
            T.Cell[3, i + 1].Text := Format('[%d]', [TExecVarVector(FV).Length]);
          end;

        evtResultMatrix:
          begin
            T.Cell[1, i + 1].Text := 'matrix';
            T.Cell[3, i + 1].Text := Format('[%d][%d]', [TExecVarMatrix(FV).Cols, TExecVarMatrix(FV).Rows]);
          end;
      end;
    end;

  AList.Free;
end;

procedure TExecList.DoListGlobals;
var
  T: TOutputTable;
  Consts: TExecutorDataVariables;
  i: Integer;
  FV: TCustomExecutorDataVariable;
  AList: TStringList;
  V: TCustomVariable;
begin
  Consts := FExecutor.Consts;
  AList := TStringList.Create;

  if (not Assigned(FST.Variables)) or
     (FST.Variables.Count = 0)
  then
    for i := 0 to Consts.Count - 1 do
      AList.AddObject(Consts.Keys[i], Consts.Data[i])
  else
    for V in FST.Variables do
      AList.AddObject(V.Ident, Consts.KeyData[V.Ident]);


  T := FOutputCreator.AddTable;
  T.ColCount := 4;
  T.RowCount := AList.Count + 1;

  T.Cell[0,0].Text := 'Name';
  T.Cell[1,0].Text := 'Type';
  T.Cell[2,0].Text := 'Datatype';
  T.Cell[3,0].Text := 'Value';

  for i := 0 to AList.Count - 1 do
    begin
      FV := TCustomExecutorDataVariable(AList.Objects[i]);
      T.Cell[0, i + 1].Text := FV.Ident;
      T.Cell[2, i + 1].Text := EpiTypeNames[FV.DataType];

      case FV.VarType of
        evtGlobal:
          begin
            T.Cell[1, i + 1].Text := 'const';
            T.Cell[3, i + 1].Text := FV.AsStringVector[0];
          end;

        evtGlobalVector:
          begin
            T.Cell[1, i + 1].Text := 'vector';
            T.Cell[3, i + 1].Text := Format('[%d]', [TExecVarGlobalVector(FV).Length]);
          end;
      end;
    end;

  AList.Free;
end;

procedure TExecList.FillDataSetTable(const Relation: TEpiMasterRelation;
  const Depth: Cardinal; const Index: Cardinal; var aContinue: boolean;
  Data: Pointer = nil);
var
  T: TOutputTable;
  Row, Idx: Integer;
  S: UTF8String;
  DF: TEpiDataFile;
  F: TEpiField;
  Gvt: TEpiGetVariableLabelType;
begin
  Gvt := VariableLabelTypeFromOptionList(FST.OptionList, FExecutor.SetOptions);

  T := TOutputTable(Data);
  Row := T.RowCount;

  T.RowCount := Row + 1;

  Idx := 0;

  // Name
  T.Cell[PostInc(Idx), Row].Text := Relation.Datafile.Name + IfThen(Relation.Datafile = FExecutor.DataFile, '*', '');

  // Var
  T.Cell[PostInc(Idx), Row].Text := IntToStr(Relation.Datafile.Fields.Count);

  // Obs
  T.Cell[PostInc(Idx), Row].Text := IntToStr(Relation.Datafile.Size);

  // Relation
  S := '';
  if Depth > 0 then
    S := DupeString('-', Depth) + ' ';
  if (Relation is TEpiDetailRelation) then
    if (TEpiDetailRelation(Relation).MaxRecordCount > 0) then
      S := S + '1:' + IntToStr(TEpiDetailRelation(Relation).MaxRecordCount)
    else
      S := S + '1:' + char($E2) + char($88) + char($9E); // unicode infinity symbol (UTF-8 encoded)
  T.Cell[PostInc(Idx), Row].Text := S;

  // Label
  T.Cell[PostInc(Idx), Row].Text := Relation.Datafile.Caption.Text;

  // Key
  S := '';
  DF := Relation.Datafile;
  for F in DF.KeyFields do
    S := S + ' + (' + F.GetVariableLabel(Gvt) + ')';
  Delete(S, 1, 3);
  T.Cell[PostInc(Idx), Row].Text := S;
end;

constructor TExecList.Create(AExecutor: TExecutor;
  AOutputCreator: TOutputCreator);
begin
  FExecutor := AExecutor;
  FOutputCreator := AOutputCreator;
end;

destructor TExecList.Destroy;
begin
  inherited Destroy;
end;

procedure TExecList.ExecList(ST: TCustomCrudCommand);
begin
  FST := TListCommand(ST);
  case ST.SubCommand of
    ccData:
      DoListData(TListCommand(ST));

    ccVariable:
      DoListVar;

    ccDataset:
      DoListDatasets;

    ccValuelabel:
      DoListValueLabels;

    ccResult:
      DoListResults;

    ccGlobal:
      DoListGlobals;
  end;
  ST.ExecResult := csrSuccess;
end;

end.


