unit UpdateBrowse;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, Grids, UVectors, Menus, ActnList, Buttons;

type
  TUpdateForm = class(TForm)
    Panel1: TPanel;
    Button1: TButton;
    DataGrid: TStringGrid;
    UpdateMenu: TPopupMenu;
    SelectasID1: TMenuItem;
    CopytoClipboard1: TMenuItem;
    Variableinfo1: TMenuItem;
    Sorton1: TMenuItem;
    ActionList1: TActionList;
    AcClose: TAction;
    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;
    BitBtn3: TBitBtn;
    BitBtn4: TBitBtn;
    AcBrowseFirst: TAction;
    AcBrowsePrior: TAction;
    AcBrowseNext: TAction;
    AcBrowseLast: TAction;
    Button2: TButton;
    AcValues: TAction;
    AcSelectAll: TAction;
    procedure DataGridDrawCell(Sender: TObject; ACol, ARow: Integer;
      Rect: TRect; State: TGridDrawState);
    procedure DataGridSetEditText(Sender: TObject; ACol, ARow: Integer;
      const Value: String);
    procedure DataGridKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure DataGridMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure DataGridClick(Sender: TObject);
    procedure DataGridDblClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure DataGridGetEditText(Sender: TObject; ACol, ARow: Integer;
      var Value: String);
    procedure DataGridKeyPress(Sender: TObject; var Key: Char);
    procedure SelectasID1Click(Sender: TObject);
    procedure Sorton1Click(Sender: TObject);
    procedure Variableinfo1Click(Sender: TObject);
    procedure CopytoClipboard1Click(Sender: TObject);
    procedure FormDeactivate(Sender: TObject);
    procedure AcCloseExecute(Sender: TObject);
    procedure AcBrowseExecute(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure DataGridExit(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure DataGridKeyUp(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure AcValuesExecute(Sender: TObject);
    procedure AcSelectAllExecute(Sender: TObject);
  private
    { Private declarations }
    FVectorList: TEpiVectors;
    FOldValue: string;
    FEditText: string;
    FId: string;
    FIdClick: string;
    FBrowse: boolean;
    FUseLabels: boolean;
    procedure Initialize();
    procedure SaveValue(ACol, ARow: Integer);
    procedure RestoreValue(ACol, ARow: Integer);
    function HandleEntry(ACol, ARow: Integer): boolean;
    function ValidateEntry(ACol, ARow: Integer): boolean;
    procedure ToOutput(ACol, ARow: Integer);
    function GetIDColNo(const id: string): integer;
    function GetCell(ACol, ARow: Integer): string;
    procedure SetVectorList(value: TEpiVectors);
  protected
    procedure CreateParams(var Params: TCreateParams); override;
  public
    { Public declarations }
    constructor Create(AOwner: TComponent; VectorList: TEpiVectors; ID: string; Browse: Boolean);
    property VectorList: TEpiVectors read FVectorList write SetVectorList;
  end;

  TUpdate = class(TObject)
  private
    fVarnames: TStrings;
    fBrowsing: boolean;
  protected
    //
  public
    destructor Destroy(); override;
    procedure CreateBrowse(df: TEpiDataFrame);
    procedure CreateUpdate(df: TEpiDataFrame; Vectorlist :TEpiVectors; VectorID: string);
    function UpdateBrowseWindow(df: TEpiDataFrame): boolean;
    function BrowseHasFocus(): boolean;
    procedure CloseBrowse();
    function CurrentVarnames(): TStrings;
    function GetBrowserHandle(): TForm;
    property Browsing: boolean read fBrowsing write fBrowsing;
  end;

var
  OUpdate: TUpdate;

implementation

{$R *.dfm}
uses
  UEpiDataTypes, ansDatatypes, UCmdProcessor, UMain, UDocument, ClipBrd, UIniFile, EpiDataUtils,
  UTranslation;

resourcestring
  SelectString = 'Select %s as ID';
  CpSelectToClpBrd = 'Copy Selection to Clipboard';
  VarInfo = 'Show Variable Info';
  SortOn = 'Sort on %s';
  NoSortOn = 'Cannot sort on %s';

var
  OUpdateForm: TUpdateForm;


{
******************
**    TUpdate   **
******************
}


destructor TUpdate.Destroy();
begin

end;

procedure TUpdate.CreateBrowse(df: TEpiDataFrame);
begin
  if Assigned(fVarnames) then FreeAndNil(fVarnames);
  fVarnames := df.GetVectorNames(nil);

  if Assigned(OUpdateForm) then FreeAndNil(OUpdateForm);
  OUpdateForm := TUpdateForm.Create(Application, df.Vectors, '', true);
  OUpdateForm.Caption := OTranslator.Translate(0, 'Browse');
  OUpdateForm.UpdateMenu.Items[0].Enabled := false;
  OUpdateForm.Show;
  Browsing := true;
end;

procedure TUpdate.CreateUpdate(df: TEpiDataFrame; Vectorlist :TEpiVectors; VectorID: string);
begin
  try
    if Assigned(OUpdateForm) then FreeAndNil(OUpdateForm);
    OUpdateForm := TUpdateForm.Create(Application, vectorlist, VectorID, false);
    OUpdateForm.Caption := OTranslator.Translate(0, 'Update');
    OUpdateForm.Button2.Visible := false;
    OUpdateForm.ShowModal;
  finally
    if Assigned(OUpdateForm) then FreeAndNil(OUpdateForm);
  end;
end;

function TUpdate.UpdateBrowseWindow(df: TEpiDataFrame): boolean;
begin
  if not Assigned(OUpdateForm) then exit;
  if not OUpdateForm.FBrowse then exit;
  if Assigned(fVarnames) then FreeAndNil(fVarnames);
  fVarnames := df.GetVectorNames(nil);
  OUpdateForm.VectorList := df.Vectors;
  result := true;
end;

function TUpdate.BrowseHasFocus(): boolean;
begin
  result := Browsing;
end;

procedure TUpdate.CloseBrowse();
begin
  if Assigned(OUpdateForm) then FreeAndNil(OUpdateForm);
end;

function TUpdate.CurrentVarnames(): TStrings;
begin
  result := fVarNames;
end;

function TUpdate.GetBrowserHandle(): TForm;
begin
  result := OUpdateForm;
end;


{
******************
**  TUpdateForm **
******************
}

constructor TUpdateForm.Create(AOwner: TComponent; VectorList: TEpiVectors; ID: string; Browse: Boolean);
const
  def: TFormDefaults = (Section: 'Browse';
                        Top: 60; Left: 580;
                        Width: 500; Height: 600;
                        Maximize: false);
var
  opt: TEpiOption;
begin
  inherited Create(AOwner);
  FVectorList := VectorList;
  FOldValue := '';
  FId := ID;
  FBrowse := Browse;
  if FBrowse then
  begin
    DataGrid.Options := DataGrid.Options - [goEditing];
    DataGrid.Color := clInfoBk;
    FUseLabels := true;
  end else begin
    DataGrid.Options := DataGrid.Options + [goEditing];
    DataGrid.Color := clWindow;
    FUseLabels := false;
  end;
  dm.GetOptionValue('BROWSER FONT SIZE', opt);
  DataGrid.Canvas.Font.Size := StrToInt(opt.Value);
  DataGrid.Font.Size := StrToInt(opt.Value);
  Initialize();
  OIniFile.LoadForm(self, def);
end;

procedure TUpdateForm.CreateParams(var Params: TCreateParams);
begin
  inherited;
  Params.ExStyle := Params.ExStyle or WS_EX_APPWINDOW;
end;


procedure TUpdateForm.Initialize();
var
  i, j, co, rc, dw, rw: integer;
begin
  if FVectorList = nil then exit;

  DataGrid.ColCount := FVectorList.Count + 1;
  DataGrid.RowCount := FVectorList.DataFrame.RowCount + 1;

  dw := Round(DataGrid.Canvas.TextHeight('W')*1.5);
  for i := 0 to DataGrid.RowCount -1 do
    DataGrid.RowHeights[i] := dw;
  co := FVectorList.Count;
  for i:= 1 to co do
  begin
    dw := Round(DataGrid.canvas.TextWidth(FVectorList[i-1].name)*1.3);
    if dw < DataGrid.Canvas.TextWidth('WWW') then dw := DataGrid.Canvas.TextWidth('WWW');
    rc := 5;
    if rc > FVectorList.DataFrame.RowCount then rc := FVectorList.DataFrame.RowCount;
    for j:= 1 to rc do
    begin
       rw := Round(DataGrid.Canvas.TextWidth(FVectorList[i-1].AsString[j])*1.3);
       if dw < rw then dw:= rw;
    end;
    DataGrid.ColWidths[i] := dw;
  end;
end;

procedure TUpdateForm.SaveValue(ACol, ARow: integer);
begin
  FOldValue := FVectorList[ACol-1].AsString[ARow];
end;

procedure TUpdateForm.RestoreValue(ACol, ARow: integer);
begin
  FVectorList[ACol-1].AsString[ARow] := FOldValue;
end;

function TUpdateForm.HandleEntry(ACol, ARow: Integer): Boolean;
begin
  result := ValidateEntry(ACol, ARow);
  if Result then
    ToOutput(ACol, ARow);
end;

procedure TUpdateForm.ToOutput(ACol, ARow: Integer);
var
  Value, S: string;
  IDVal, VarVal: string;
begin
  Value := FEditText;
  if (Value <> ' ') then
   Value := Trim(Value);
  if (Value <> Trim(FOldValue)) then
  begin
    VarVal := GetCell(ACol, 0);
    if FId = VarVal then
      IDVal := FOldValue
    else
      IDVal := GetCell(GetIDColNo(FId), ARow);
    case FVectorList[ACol-1].DataType of
      EpiTyString, EpiTyUppercase, EpiTyByte:
        S := Format('if %s = %s then %s = "%s"  //from %s', [FId, IDVal, VarVal, Value, FOldValue]);
      EpiTyDate:
        S := Format('if %s = %s then %s = date("%s","%s")  //from %s', [FId, IDVal, VarVal, Value, FVectorList[ACol-1].FieldFormat, FOldValue]);
    else
      S := Format('if %s = %s then %s = %s  //from %s', [FId, IDVal, VarVal, Value, FOldValue]);
    end;
  end;
  aMainForm.CmdEdit.History.Add(s);
  aMainForm.CmdEdit.CurrentCmd := aMainForm.CmdEdit.History.Count -1;
  dm.PrintCommand(s);
end;

function TUpdateForm.ValidateEntry(ACol, ARow: Integer): Boolean;
var
 s, Value: string;
 c: Integer;
 i: EpiInt;
 f: EpiFloat;
 v : TEpiVector;
begin
//  Value := GetCell(ACol, ARow);
  Value := FEditText;
  v := FVectorList[ACol-1];
  if value = ' ' then
    s := value
  else
    s := trim(value);
  c := 1;
  if (s = '.') then
  begin
    v.IsMissing[ARow] := true;
    result := true;
    exit;
  end;
  // Valid Data?
  case v.DataType of
    EpiTyBoolean:
      if s[1] in BooleanChars then c := 0;
    EpiTyInteger:
      Val(s, i, c);
    EpiTyFloat:
      Val(s, f, c);
    EpiTyUppercase,
    EpiTyString:
      c := 0;
    EpiTyDate:
      if mibIsDate(s, VectorType2EpiDataFieldType(v.DataType)) then c := 0;
  end;
  if c > 0 then
  begin
    result := false;
    Application.MessageBox('Invalid Entry', 'Error', MB_OK);
  end else begin
    result := true;
    v.AsString[ARow] := s;
  end;
end;


procedure TUpdateForm.DataGridDrawCell(Sender: TObject; ACol,
  ARow: Integer; Rect: TRect; State: TGridDrawState);
var
  Value: string;
begin
  if ACol = 0 then
  begin
    if ARow = 0 then
      exit
    else
      Value := IntToStr(ARow);
  end else if ARow = 0 then
    Value := FVectorList[ACol-1].Name
  else begin
    if FUseLabels then
      Value := FVectorList[ACol-1].GetValueLabel(VectorList[ACol-1].AsString[ARow])
    else
      Value := FVectorList[ACol-1].AsString[ARow];
  end;
  DataGrid.Canvas.TextRect(Rect, Rect.Left+1, Rect.Top+1, Value);
end;

procedure TUpdateForm.DataGridSetEditText(Sender: TObject; ACol,
  ARow: Integer; const Value: String);
begin
//  FVectorList[ACol-1].AsString[ARow] := Value;
  FEditText := Value;
end;

procedure TUpdateForm.DataGridKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
var
  t: TGridRect;
begin
  if DataGrid.EditorMode then
  begin
    // We are in the edit state:
    if Key = VK_ESCAPE then
    begin
      DataGrid.EditorMode := false;
      RestoreValue(DataGrid.Col, DataGrid.Row);
      exit;
    end;
    Case Key of
      VK_LEFT:
        begin
          if ssCtrl in Shift then
          begin
            if not HandleEntry(DataGrid.Col, DataGrid.Row) then
            begin
              Key := 0;
              Exit;
            end;
            if (DataGrid.Col > 1) then
              DataGrid.Col := DataGrid.Col - 1;
          end;
        end;
      VK_RIGHT:
        begin
          if ssCtrl in Shift then
          begin
            if not HandleEntry(DataGrid.Col, DataGrid.Row) then
            begin
              Key := 0;
              Exit;
            end;
            if (DataGrid.Col < DataGrid.ColCount) then
              DataGrid.Col := DataGrid.Col + 1;
          end;
        end;
      VK_UP,
      VK_DOWN:
        begin
          if not HandleEntry(DataGrid.Col, DataGrid.Row) then
          begin
            Key := 0;
            Exit;
          end;
        end;
    end;
  end else begin
    // No Edit state
    if Key = VK_RETURN then
    begin
      SaveValue(DataGrid.Col, DataGrid.Row);
      DataGridGetEditText(nil, DataGrid.Col, DataGrid.Row, FEditText);
    end;
  end;
end;

procedure TUpdateForm.DataGridMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  col, row: integer;
  p: TPoint;
begin
  Datagrid.MouseToCell(X, Y, col, row);
  if (Col < 0) or (row < 0) then exit;
  if Button = mbLeft then
  begin
    if (Col > 0) and (Col < Datagrid.ColCount) and
       (Row > 0) and (Row < Datagrid.RowCount) then
      SaveValue(col, row);
    if (Col = 0) and (Row = 0) then
    begin
      AcSelectAllExecute(nil);
    end;
  end;
  if Button = mbRight then
  begin
    if Col=0 then
      FIdClick := 'recnumber'
    else
      FIdClick := FVectorList[Col-1].Name;
    UpdateMenu.Items[0].Caption := Format(SelectString, [FIdClick]);
    UpdateMenu.Items[1].Caption := Format(CpSelectToClpBrd, []);
    UpdateMenu.Items[2].Caption := Format(VarInfo, []);
    if FIdClick <> 'recnumber' then
      UpdateMenu.Items[3].Caption := Format(SortOn, [FIdClick])
    else
      UpdateMenu.Items[3].Caption := Format(NoSortOn, [FIdClick]);
    P.X := X; P.Y := Y;
    P := DataGrid.ClientToScreen(P);
    UpdateMenu.Popup(p.X, p.Y);
  end;
end;

procedure TUpdateForm.DataGridClick(Sender: TObject);
begin
  DataGrid.EditorMode := false;
end;

procedure TUpdateForm.DataGridDblClick(Sender: TObject);
begin
  DataGrid.EditorMode := true;
end;

procedure TUpdateForm.FormShow(Sender: TObject);
begin
  DataGrid.SetFocus;
end;

procedure TUpdateForm.DataGridGetEditText(Sender: TObject; ACol,
  ARow: Integer; var Value: String);
begin
  if FUseLabels then
    Value := FVectorList[ACol-1].GetValueLabel(FVectorList[ACol-1].AsString[ARow])
  else
    Value := FVectorList[ACol-1].AsString[ARow];
end;

procedure TUpdateForm.DataGridKeyPress(Sender: TObject; var Key: Char);
begin
  if DataGrid.EditorMode and (Key = #13) then
  begin
    if not HandleEntry(DataGrid.Col, DataGrid.Row) then
    begin
      RestoreValue(DataGrid.Col, DataGrid.Row);
    end else begin
      // Move a row down
      if (goEditing in DataGrid.Options) then
      begin
       if (DataGrid.Row < DataGrid.RowCount-1) then
         DataGrid.Row := DataGrid.Row + 1
       else
         DataGrid.EditorMode := false;
       SaveValue(DataGrid.Col, DataGrid.Row);
      end;
    end;
    Key := #0;
  end;
end;

function TUpdateForm.GetIDColNo(const id: string): integer;
begin
  for result := 1 to DataGrid.ColCount-1 do
    if ID = GetCell(Result, 0) then exit;
  result := 0;
end;

function TUpdateForm.GetCell(ACol, ARow: Integer): string;
begin
  if ACol = 0 then
    result := IntToStr(ARow)
  else if ARow = 0 then
    result := FVectorList[ACol-1].Name
  else
    DataGrid.OnGetEditText(nil, ACol, ARow, result);
end;

procedure TUpdateForm.SetVectorList(value: TEpiVectors);
begin
  if Assigned(FVectorList) then FreeAndNil(FVectorList);
  FVectorList := Value;
  Initialize();
  self.Repaint;
end;

procedure TUpdateForm.SelectasID1Click(Sender: TObject);
begin
  FId := FIdClick;
  if FIdClick = 'recnumber' then
    dm.Info('Recnumber depends on sorting <br> SaveData to keep changes', [], 203002)
  else
    dm.Info('%s selected as ID variable', [FId], 0);
end;

procedure TUpdateForm.Sorton1Click(Sender: TObject);
begin
//  FVectorList.DataFrame.Sort(FIdClick);
  if FIdClick <> 'recnumber' then
  begin
    aMainForm.doCommand('sort ' + FIdClick);
    DataGrid.Repaint;
  end;
end;

procedure TUpdateForm.Variableinfo1Click(Sender: TObject);
var
  v: TEpiVector;
  s: string;
  idx: integer;
begin
  if FIdClick <> 'recnumber' then
  begin
    v := FVectorList.VectorByName(FIdClick);

    s := 'Name: ' + v.Name +  '    Type: ' + ODocument.AppendDateType(v, GetFieldTypeName(v.FieldDataType)) + #13;
    s := s + 'Label: ' + v.GetVariableLabel + #13;
    if Assigned(v.CheckProperties.ValueLabelSet) then
    begin
      s := s + 'Value      Label' + #13;
      for idx := 0 to v.CheckProperties.ValueLabelSet.Count-1 do
        s := s + ' ' + v.CheckProperties.ValueLabelSet[idx] + '         ' + PChar(v.CheckProperties.ValueLabelSet.Objects[idx]) + #13;
    end;
    //messagedlg(s,mtInformation,[mbOK],0,);
    ShowMessage(s);
  end;
end;

procedure TUpdateForm.CopytoClipboard1Click(Sender: TObject);
var
  SelRect: TGridRect;
  i, j: integer;
  s: widestring;
  p: PWideChar;
  Data: THandle;
  DataPtr: Pointer;
begin
  s := '';
  SelRect := DataGrid.Selection;
  // Headings
  for j := SelRect.Left to SelREct.Right do
  begin
    s := s + GetCell(j, 0) + #9;
  end;
  delete(s, length(s), 1);
  s := s + #13#10;

  //TODO : Optimize.... takes to long on "large" datasets.
  for i := SelRect.Top to SelRect.Bottom do
  begin
    for j := SelRect.Left to SelREct.Right do
    begin
      s := s + GetCell(j, i) + #9;
    end;
    delete(s, length(s), 1);
    s := s + #13#10;
  end;

  p := PWideChar(s);
  {Unicode clipboard routine courtesy Mike Lischke}
  Data := GlobalAlloc(GMEM_MOVEABLE + GMEM_DDESHARE, 2* Length(s)-2);
  try
    DataPtr := GlobalLock(Data);
    try
      Move(p^, DataPtr^, 2* Length(s)-2);
      Clipboard.SetAsHandle(CF_UNICODETEXT, Data);
    finally
      GlobalUnlock(Data);
    end;
  except
    GlobalFree(Data);
    raise;
  end;
end;

procedure TUpdateForm.FormDeactivate(Sender: TObject);
var
  opt: TEpiOption;
begin
  OUpdate.Browsing := false;
  if (dm.GetOptionValue('DISPLAY DATABROWSER', opt) and (AnsiUpperCase(opt.Value) = 'OFF'))
     and FBrowse then
    Close();
end;

procedure TUpdateForm.AcCloseExecute(Sender: TObject);
begin
  Close();
end;

procedure TUpdateForm.AcBrowseExecute(Sender: TObject);
begin
  if sender = AcBrowseFirst then
     DataGrid.Row :=1
  else if sender = AcBrowseLast then
     DataGrid.Row := (DataGrid.rowcount - 1)
  else if sender = AcBrowseNext then
  begin
    if DataGrid.row <(DataGrid.rowcount - 1) then
      DataGrid.Row := DataGrid.row + 1;
  end else if sender = AcBrowsePrior then
  begin
    if DataGrid.row > 1 then
      DataGrid.Row := DataGrid.row - 1;
  end
end;

procedure TUpdateForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  OInifile.SaveCurrentForm(Self, 'Browse');
end;

procedure TUpdateForm.DataGridExit(Sender: TObject);
begin
  if (DataGrid.EditorMode) and (not HandleEntry(Datagrid.Col, Datagrid.Row)) then
    DataGRid.SetFocus;
end;

procedure TUpdateForm.FormCreate(Sender: TObject);
begin
  OTranslator.TranslateForm(self);
end;

procedure TUpdateForm.DataGridKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if not DataGrid.EditorMode then
  begin
    // No Edit state
    if Key in [VK_LEFT, VK_RIGHT, VK_UP, VK_DOWN] then
      SaveValue(DataGrid.Col, DataGrid.Row);
//      DataGridGetEditText(nil, DataGrid.Col, DataGrid.Row, FEditText);
    if (Key = $43) and (ssCtrl in Shift) then
      CopytoClipboard1Click(nil);
  end;
end;

procedure TUpdateForm.AcValuesExecute(Sender: TObject);
begin
  FUseLabels := not FUseLabels;
  DataGrid.Repaint;
end;

procedure TUpdateForm.AcSelectAllExecute(Sender: TObject);
var
  t: TGridRect;
begin
  t.Left := 1; t.Top := 1;
  t.Right := DataGrid.ColCount - 1;
  t.Bottom := DataGrid.RowCount - 1;
  DataGrid.Selection := t;
end;

end.
