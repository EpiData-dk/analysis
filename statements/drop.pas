unit drop;

{$codepage UTF-8}
{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fgl, ast, executor, epidocument, epidatafiles,
  epicustombase, epidatafilestypes, epivaluelabels, outputcreator;

type

  { TExecDrop }

  TExecDrop = class
  private
    FExecutor: TExecutor;
    FOutputCreator: TOutputCreator;
  private
    // Drop data things
    FDropDataExpr: TExpr;
    FReverseSelectVector: TEpiField;
    function  DropPackFunction(Sender: TEpiDataFile; Index: Integer; Data: Pointer): boolean;
  protected
    procedure DoDropData(ST: TDropCommand);
    procedure DoDroptVar(ST: TDropCommand);
    procedure DoDropGlobal(ST: TDropCommand);
    procedure DoDropDataset(ST: TDropCommand);
    procedure DoDropValuelabel(ST: TDropCommand);
  public
    constructor Create(AExecutor: TExecutor; AOutputCreator: TOutputCreator);
    destructor Destroy; override;
    procedure ExecDrop(ST: TDropCommand);
  end;

implementation

uses
  ast_types, result_variables, epidatafilerelations, epidatafilerelations_helper;

{ TExecDrop }

function TExecDrop.DropPackFunction(Sender: TEpiDataFile; Index: Integer;
  Data: Pointer): boolean;
begin
  result := (not FReverseSelectVector.IsMissing[Index]);
end;

procedure TExecDrop.DoDropData(ST: TDropCommand);
var
  i, Len: Integer;
  lSelectVector: TEpiIntField;
  OnlyDeleted: Boolean;
begin
  // First build a reverse index for the pack command.
  FReverseSelectVector      := TEpiField.CreateField(nil, ftInteger);
  FReverseSelectVector.Size := FExecutor.DataFile.Size;

  lSelectVector := FExecutor.SelectVector;

  OnlyDeleted := ST.HasOption('del');
  if OnlyDeleted then
    begin
      for i := 0 to lSelectVector.Size - 1 do
        if FExecutor.DataFile.Deleted[lSelectVector.AsInteger[i]] then
          FReverseSelectVector.AsInteger[lSelectVector.AsInteger[i]] := i;
    end
  else
    for i := 0 to lSelectVector.Size - 1 do
      FReverseSelectVector.AsInteger[lSelectVector.AsInteger[i]] := i;

  Len := FExecutor.DataFile.Size;
  FExecutor.DataFile.Pack(@DropPackFunction);
  Len := Len - FExecutor.DataFile.Size;
  FReverseSelectVector.Free;

  if Len = 1 then
    FOutputCreator.DoInfoAll(Format('(%d observation dropped)', [Len]))
  else
    FOutputCreator.DoInfoAll(Format('(%d observations dropped)', [Len]));
end;

procedure TExecDrop.DoDroptVar(ST: TDropCommand);
var
  Idx: LongInt;
  V: TCustomVariable;
  S, T: String;
  i: Integer;
  Fields: TEpiFields;
  F: TEpiField;
begin
  S := '';
  T := '';

  for V in ST.Variables do
    begin
      F := FExecutor.DataFile.Fields.FieldByName[V.Ident];

      if (Assigned(F)) then
        begin
          S := S + ', ' + V.Ident;
          F.Free;
        end
      else
        T := T + ', ' + V.Ident;
    end;

  if S <> '' then
    begin
      Delete(S, 1, 2);
      FOutputCreator.DoInfoAll(S + ' was dropped!');
    end;

  if T <> '' then
    begin
      Delete(T, 1, 2);
      FOutputCreator.DoWarning(T + ' not found!');
    end;

  ST.ExecResult := csrSuccess;

  FExecutor.UpdateFieldResultVar;
end;

procedure TExecDrop.DoDropGlobal(ST: TDropCommand);
var
  V: TCustomVariable;
  i, Idx: Integer;
  S, T: String;
  Opt: TOption;
  Ft: TEpiFieldType;
  FTs: TEpiFieldTypes;
  lConsts: TExecutorDataVariables;
begin
  S := '';
  T := '';

  if ST.HasOption('all', Opt) then
    begin
      FTs := AllFieldTypes;

      if Assigned(Opt.Expr) then
      begin
        S :=  Opt.Expr.AsString;
        if (EpiFieldTypeFromString(S, Ft)) then
          FTs := [Ft];
      end;

      lConsts := FExecutor.Consts;
      for i := lConsts.Count - 1 downto 0 do
      begin
        if (lConsts.Data[i].DataType in FTs) then
          begin
            S := S + lConsts.Data[i].Ident + ', ';
            lConsts.Data[i].Free;
            lConsts.Delete(i);
          end;
      end;
    end
  else
    begin
      for V in ST.Variables do
        begin

          if FExecutor.Consts.Find(V.Ident, Idx) then
            begin
              S := S + ', ' + V.Ident ;
              FExecutor.Consts.KeyData[V.Ident].Free;
              FExecutor.Consts.Remove(V.Ident);
            end
          else
            T := T + ', ' + V.Ident;
        end;
    end;

  if S <> '' then
    begin
      Delete(S, 1, 2);
      FOutputCreator.DoInfoAll(S + ' was dropped!');
    end;

  if T <> '' then
    begin
      Delete(T, 1, 2);
      FOutputCreator.DoInfoAll(T + ' not found!');
    end;
end;

procedure TExecDrop.DoDropDataset(ST: TDropCommand);

  function DropRecusive(Relation: TEpiMasterRelation): string;
  var
    Rel: TEpiMasterRelation;
  begin
    result := Relation.Datafile.Name;
    for Rel in Relation.DetailRelations do
      Result := Result + ', ' + DropRecusive(Rel);

    // This auto destroys the relation....
    Relation.Datafile.Free;
  end;

var
  V: TCustomVariable;
  Rel: TEpiMasterRelation;
  S, T: String;
begin
  S := '';
  T := '';

  for V in ST.Variables do
    if (not Assigned(FExecutor.Document.Relations.MasterRelationFromDatafileName(V.Ident))) then
      T := T + ', ' + V.Ident;

  for V in ST.Variables do
  begin
    Rel := FExecutor.Document.Relations.MasterRelationFromDatafileName(V.Ident);
    if (Assigned(Rel)) then
      S := S + DropRecusive(Rel);
  end;

  if (S <> '') then
    FOutputCreator.DoInfoAll(S + ' was dropped!');

  if (T <> '') then
    begin
      Delete(T, 1, 2);
      FOutputCreator.DoInfoAll(T + ' not found!');
    end;

  ST.ExecResult := csrSuccess;
  FExecutor.UpdateDatasetResultVar;
end;

procedure TExecDrop.DoDropValuelabel(ST: TDropCommand);
var
  S, T: String;
  V: TCustomVariable;
  VLSet: TEpiValueLabelSet;
begin
  S := '';
  T := '';

  for V in ST.Variables do
  begin
    VLSet := FExecutor.Document.ValueLabelSets.GetValueLabelSetByName(V.Ident);

    if Assigned(VLSet) then
      begin
        S := S + ', ' + VLSet.Name;
        VLSet.Free;
      end
    else
     T := T + ', ' + V.Ident;
  end;

  if (S <> '') then
    begin
      Delete(S, 1, 2);
      FOutputCreator.DoInfoAll(S + ' was dropped!');
    end;

  if (T <> '') then
    begin
      Delete(T, 1, 2);
      FOutputCreator.DoInfoAll(T + ' not found!');
    end;

  ST.ExecResult := csrSuccess;
  FExecutor.UpdateValuelabelsResultVar;
end;

constructor TExecDrop.Create(AExecutor: TExecutor;
  AOutputCreator: TOutputCreator);
begin
  FExecutor := AExecutor;
  FOutputCreator := AOutputCreator;
end;

destructor TExecDrop.Destroy;
begin
  inherited Destroy;
end;

procedure TExecDrop.ExecDrop(ST: TDropCommand);
begin
  case ST.SubCommand of
    ccData:
      DoDropData(ST);

    ccVariable:
      DoDroptVar(ST);

    ccDataset:
      DoDropDataset(ST);

    ccValuelabel:
      DoDropValuelabel(ST);

    ccGlobal:
      DoDropGlobal(ST);
  end;
end;

end.

