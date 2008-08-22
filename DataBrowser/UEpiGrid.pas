unit UEpiGrid;

interface

uses Windows, SysUtils, Classes, Grids, Uvectors,ansDatatypes,UEpidataTypes;

Type

  TOnCellChange = procedure(msg: string) of object;

{**********************************************************************
 * Ideas for a new grid - this has problems when using update command *
 * since some all vectors must be shown and rows not shown will have  *
 * heigth 0.                                                          *
 *                                                                    *
 * When accessing the dataframe, let the rows and columns have a      *
 * special property that indicates the index into the dataframe.      *
 * This is better than using the row/column number since then the     *
 * whole dataframe must be "shown"                                    *
 **********************************************************************

}
  TEpiGrid = class(TDrawGrid)
  private
  //  FData: Pointer;
    FUpdating: Boolean;
    FUseLabels: Boolean; 
    FNeedsUpdating: Boolean;
    FEditUpdate: Integer;
    FEditing: boolean;
    FDataFrame: TEpiDataFrame;
    fEditText : string;
    fReadOnly: boolean;
    FHistory: TStringList;
    FId: string;
//    FIdCol: integer;
    FOldValue: String;
    fOnCellChange: TOnCellChange;
    procedure DisableEditUpdate;
    procedure EnableEditUpdate;
    procedure Initialize;
    procedure GridDblClick(Sender: TObject);
    procedure Update(ACol, ARow: Integer); reintroduce;
    procedure SetUpdateState(Updating: Boolean);
    function GetCells(ACol, ARow: Integer): string;
    procedure SetCells(ACol, ARow: Integer; const Value: string);
    procedure SetDataFrame(const Value: TEpiDataFrame);
    procedure KeyPress(var Key: Char);override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure UpdateRowCount;
    procedure SerReadOnly(const Value: boolean);
    procedure SetFID(const Value: string);
    function GetIDColNo(const id: string): integer;
  protected
    procedure ColumnMoved(FromIndex, ToIndex: Longint); override;
    procedure DrawCell(ACol, ARow: Longint; ARect: TRect;
      AState: TGridDrawState); override;
    function GetEditText(ACol, ARow: Longint): string; override;
    procedure SetEditText(ACol, ARow: Longint; const Value: string); override;
    procedure RowMoved(FromIndex, ToIndex: Longint); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property Cells[ACol, ARow: Integer]: string read GetCells write SetCells;
    property DataFrame: TEpiDataFrame read FDataFrame write SetDataFrame;
    property ReadOnly:boolean read  fReadOnly write SerReadOnly;
    property History: TStringList read FHistory;
    property ID: String read fid write SetFID;
    property OnCellChange: TOnCellChange read fOnCellChange write fOnCellChange;
    property UseLabels: Boolean read FUseLabels write FUseLabels;
  end;

implementation

uses Controls;

constructor TEpiGrid.Create(AOwner: TComponent);
begin
  FEditing := false;
  FUseLabels := true;
  OnDblClick := GridDblClick;
  FHistory := TStringList.Create();
  inherited Create(AOwner);
end;

destructor TEpiGrid.Destroy;
begin
  FHistory.Free;
  inherited Destroy;
end;

procedure TEpiGrid.ColumnMoved(FromIndex, ToIndex: Longint);
begin
  Invalidate;
  inherited ColumnMoved(FromIndex, ToIndex);
end;

procedure TEpiGrid.RowMoved(FromIndex, ToIndex: Longint);
begin
  Invalidate;
  inherited RowMoved(FromIndex, ToIndex);
end;

function TEpiGrid.GetEditText(ACol, ARow: Longint): string;
begin
  Result := Cells[ACol, ARow];
//  if Assigned(FOnGetEditText) then FOnGetEditText(Self, ACol, ARow, Result);
end;

procedure TEpiGrid.SetEditText(ACol, ARow: Longint; const Value: string);
begin
  DisableEditUpdate;
  try
    FEditText := Value;
  finally
    EnableEditUpdate;
  end;
  inherited SetEditText(ACol, ARow, Value);
end;

procedure TEpiGrid.DrawCell(ACol, ARow: Longint; ARect: TRect;
  AState: TGridDrawState);
begin
{  if Arow> 0 then
    if not DataFrame.Selected[ARow] then
    begin
        RowHeights[ARow]:=0;
        exit;
     end
     else
        RowHeights[ARow]:=DefaultRowHeight; }
  if DefaultDrawing then
    Canvas.TextRect(ARect, ARect.Left+1, ARect.Top+2, Cells[ACol, ARow]);
  inherited DrawCell(ACol, ARow, ARect, AState);
end;

procedure TEpiGrid.DisableEditUpdate;
begin
  Inc(FEditUpdate);
end;

procedure TEpiGrid.EnableEditUpdate;
begin
  Dec(FEditUpdate);
end;

procedure TEpiGrid.UpdateRowCount;
var
 rc : integer;
begin
 rc:=FDataFrame.RowCount;
 if rc=0 then inc(rc,2)
 else inc(rc,1);
 rowcount := rc;
 if rc> 1 then fixedrows:=1;
end;

procedure TEpiGrid.Initialize;
var
 i, co,rc,j, dw, rw : integer;

begin
  if FDataFrame = nil then exit;
  co := FDataFrame.VectorCount;
  colcount:=co+1;
  UpdateRowCount;
  fixedcols :=1;
  dw := Round(Canvas.TextHeight('W')*1.5);
  for i := 0 to RowCount -1 do
    RowHeights[i] := dw;
  for i:= 1 to co do
  begin
    dw :=Round(canvas.TextWidth(DataFrame.Vectors[i-1].name)*1.3);
    if dw <canvas.TextWidth('WWW') then dw:=canvas.TextWidth('WWW');
    rc := 5;
    if rc> FDataFrame.RowCount then rc:= FDataFrame.RowCount;
    for j:= 1 to rc do
    begin
       rw := Round(canvas.TextWidth(cells[i,j])*1.3);
       if dw < rw then dw:= rw;
    end;
    colwidths[i]:=dw;
  end;
end;

procedure TEpiGrid.SetUpdateState(Updating: Boolean);
begin
  FUpdating := Updating;
  if not Updating and FNeedsUpdating then
  begin
//    InvalidateGrid;
    FNeedsUpdating := False;
  end;
end;

procedure TEpiGrid.Update(ACol, ARow: Integer);
begin
  if not FUpdating then InvalidateCell(ACol, ARow)
  else FNeedsUpdating := True;
  if (ACol = Col) and (ARow = Row) and (FEditUpdate = 0) then InvalidateEditor;
end;

function TEpiGrid.GetCells(ACol, ARow: Integer): string;
begin
 result:='';
 if Acol= 0 then
 begin
   if Arow = 0 then exit;
   result:= inttostr(ARow);
   exit;
 end;
 if Arow= 0 then
 begin
     result:=Trim(DataFrame.Vectors[ACol-1].name);
     exit;
 end;
 if (FUseLabels) then
   result:=Trim(DataFrame.Vectors[Acol-1].GetValueLabel(DataFrame.Vectors[Acol-1].asString[ARow]))
 else
   result:=Trim(DataFrame.Vectors[Acol-1].asString[ARow]);
end;


procedure TEpiGrid.SetCells(ACol, ARow: Integer; const Value: string);
var
 s: string;
 v : TEpiVector;
begin
  v := DataFrame.Vectors[Acol-1];
  if value = ' ' then
    s := value
  else
   s :=trim(value);
  v.IsMissing[aRow]:=true;
  if s <> '' then
  begin
  case v.DataType of
    EpiTyBoolean:
      if s[1] in BooleanYesChars then v.asInteger[ARow] :=1
      else if s[1] in BooleanNoChars then v.asInteger[ARow] :=0;
    EpiTyInteger,EpiTyFloat,
    EpiTyUppercase,EpiTyString:v.asString[ARow] := s;
    EpiTyDate: {if length(s)>9 then}
      try
         v.asString[ARow]:= s;
      except
      end;
  end;
  end;
  Update(ACol, ARow);
end;

procedure TEpiGrid.SetDataFrame(const Value: TEpiDataFrame);
begin
  if Assigned(FDataFrame) then FreeAndNil(FDataframe);
  FDataFrame := Value;
  Initialize;
end;

procedure TEpiGrid.KeyPress(var Key: Char);
begin
//  FIsESCKey := False;
  if (goEditing in Options) and (Key = #13) then
  // when entering the section, we assumes the user wants to change content.
  begin
    if not FEditing then
    begin
      FEditText := cells[col,row];
      FOldValue := FEditText;
    end else begin
      cells[col,row] := FEditText;
      if (FEditText <> FOldValue) and Assigned(fOnCellChange) then
      case FDataFrame.Vectors[col-1].DataType of
        EpiTyString, EpiTyUppercase, EpiTyByte:
          fOnCellChange(Format('if %s = %s then %s = "%s"  //from %s', [FId, cells[GetIDColNo(FId),row], cells[col,0], cells[col,row], FOldValue]));
        EpiTyDate:
          fOnCellChange(Format('if %s = %s then %s = date("%s","%s")  //from %s', [FId, cells[GetIDColNo(FId),row], cells[col,0], cells[col,row], FDataFrame.Vectors[col-1].FieldFormat, FOldValue]));
      else
        fOnCellChange(Format('if %s = %s then %s = %s  //from %s', [FId, cells[GetIDColNo(FId),row], cells[col,0], cells[col,row], FOldValue]));
      end;
    end;
    FEditing := not FEditing;
  end else if (goEditing in Options) and (Key in AlfaNumChars) and not FEditing then
  begin
      FEditText := cells[col,row];
      FOldValue := FEditText;
      FEditing := not FEditing;
  end;
  inherited KeyPress(Key);
end;


procedure TEpiGrid.KeyDown(var Key: Word; Shift: TShiftState);
  procedure ClearSelection;
  begin
{    if (dgMultiSelect in Options) then
    begin
      FBookmarks.Clear;
      FSelecting := False;
    end;}
  end;

  procedure DoSelection(Select: Boolean; Direction: Integer);
  var
    AddAfter: Boolean;
  begin
{    AddAfter := False;
    BeginUpdate;
    try
      if (dgMultiSelect in Options) and FDatalink.Active then
        if Select and (ssShift in Shift) then
        begin
          if not FSelecting then
          begin
            FSelectionAnchor := FBookmarks.CurrentRow;
            FBookmarks.CurrentRowSelected := True;
            FSelecting := True;
            AddAfter := True;
          end
          else
          with FBookmarks do
          begin
            AddAfter := Compare(CurrentRow, FSelectionAnchor) <> -Direction;
            if not AddAfter then
              CurrentRowSelected := False;
          end
        end
        else
          ClearSelection;
      FDatalink.MoveBy(Direction);
      if AddAfter then FBookmarks.CurrentRowSelected := True;
    finally
      EndUpdate;
    end;}
  end;

  procedure NextRow(Select: Boolean);
  begin
{    with FDatalink.Dataset do
    begin
      if (State = dsInsert) and not Modified and not FDatalink.FModified then
        if FDataLink.EOF then Exit else Cancel
      else
        DoSelection(Select, 1);
      if FDataLink.EOF and CanModify and (not ReadOnly) and (dgEditing in Options) then
        Append;
    end;}
   if not Readonly and (Row> Dataframe.RowCount) then
   begin
      Dataframe.RowCount :=Dataframe.RowCount+1;
      UpdateRowCount;
   end;
  end;

  procedure PriorRow(Select: Boolean);
  begin
{    with FDatalink.Dataset do
      if (State = dsInsert) and not Modified and FDataLink.EOF and
        not FDatalink.FModified then
        Cancel
      else
        DoSelection(Select, -1);}
  end;

  procedure Tab(GoForward: Boolean);
  //var
    //ACol, Original: Integer;
  begin
{    ACol := Col;
    Original := ACol;
//    BeginUpdate;    // Prevent highlight flicker on tab to next/prior row
    try
      while True do
      begin
        if GoForward then
          Inc(ACol) else
          Dec(ACol);
        if ACol >= ColCount then
        begin
          NextRow(False);
//          ACol := FIndicatorOffset;
        end
        else if ACol < FIndicatorOffset then
        begin
          PriorRow(False);
          ACol := ColCount - FIndicatorOffset;
        end;
        if ACol = Original then Exit;
        if TabStops[ACol] then
        begin
          MoveCol(ACol, 0);
          Exit;
        end;
      end;
    finally
      EndUpdate;
    end;}
  end;

  function DeletePrompt: Boolean;
  //var
    //Msg: string;
  begin
{    if (FBookmarks.Count > 1) then
      Msg := SDeleteMultipleRecordsQuestion
    else
      Msg := SDeleteRecordQuestion;
    Result := not (dgConfirmDelete in Options) or
      (MessageDlg(Msg, mtConfirmation, mbOKCancel, 0) <> idCancel);}
  end;

const
  RowMovementKeys = [VK_UP, VK_PRIOR, VK_DOWN, VK_NEXT, VK_HOME, VK_END];
var
  NKey: char;
begin
  if UseRightToLeftAlignment then
    if Key = VK_LEFT then
      Key := VK_RIGHT
    else if Key = VK_RIGHT then
      Key := VK_LEFT;
   if ssCtrl in Shift then
   begin
      if (Key in RowMovementKeys) then ClearSelection;
      case Key of
        VK_UP, VK_PRIOR:;// FDataLink.MoveBy(-FDatalink.ActiveRecord);
        VK_DOWN, VK_NEXT:;// FDataLink.MoveBy(FDatalink.BufferCount - FDatalink.ActiveRecord - 1);
        VK_LEFT:;// MoveCol(FIndicatorOffset, 1);
        VK_RIGHT:;// MoveCol(ColCount - 1, -1);
        VK_HOME:;// First;
        VK_END: ;//Last;
        VK_DELETE:;
{          if (not ReadOnly) and not IsEmpty
            and CanModify and DeletePrompt then
              Delete; }
     end
    end
    else
      case Key of
        VK_UP, VK_DOWN:
        begin
          NKey := #13;
          if FEditing then
            KeyPress(NKey);
        end;
(*        VK_LEFT:;
{          if dgRowSelect in Options then
            PriorRow(False) else
            MoveCol(Col - 1, -1);}
        VK_RIGHT:;
{          if dgRowSelect in Options then
            NextRow(False) else
            MoveCol(Col + 1, 1);}
        VK_HOME:;
 {         if (ColCount = FIndicatorOffset+1)
            or (dgRowSelect in Options) then
          begin
            ClearSelection;
            First;
          end
          else
            MoveCol(FIndicatorOffset, 1);}
        VK_END:;
{          if (ColCount = FIndicatorOffset+1)
            or (dgRowSelect in Options) then
          begin
            ClearSelection;
            Last;
          end
          else
            MoveCol(ColCount - 1, -1);}*)
        VK_NEXT:
          begin
{            ClearSelection;
            FDataLink.MoveBy(VisibleRowCount);}
          end;
        VK_PRIOR:
          begin
{            ClearSelection;
            FDataLink.MoveBy(-VisibleRowCount);}
          end;
        VK_INSERT:;
{          if CanModify and (not ReadOnly) and (dgEditing in Options) then
          begin
            ClearSelection;
            Insert;
          end;}
        VK_TAB: if not (ssAlt in Shift) then Tab(not (ssShift in Shift));
        VK_ESCAPE:
          begin
            FEditing := false;
{            FDatalink.Reset;
            ClearSelection;
            if not (dgAlwaysShowEditor in Options) then} HideEditor;
          end;
        VK_F2: EditorMode := True;
      end;
   inherited KeyDown(Key,Shift);
end;


procedure TEpiGrid.SerReadOnly(const Value: boolean);
begin
  fReadOnly := Value;
  if Value then
   Options := Options - [goEditing]
 else
   Options := Options + [goEditing];
end;

procedure TEpiGrid.GridDblClick(Sender: TObject);
var
  key: char;
begin
  key := #13;
  KeyPress(key);
end;

procedure TEpiGrid.SetFID(const Value: string);
begin
  FId := value;
end;

function TEpiGrid.GetIDColNo(const id: string): integer;
begin
  for result := 0 to ColCount -1 do
    if ID = Cells[result,0] then exit;
  result := 0;
end;

end.
