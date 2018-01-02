unit browse4;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ActnList,
  Menus, Grids, epifields_helper, epidatafiles, executor, ast;

type

  { TBrowseForm4 }

  TBrowseForm4 = class(TForm)
    GotoRecordAction: TAction;
    CascadeWindows: TAction;
    CloseAllAction: TAction;
    CloseBrowserAction: TAction;
    CalcColWidthAction: TAction;
    ActionList1: TActionList;
    MenuItem16: TMenuItem;
    MenuItem17: TMenuItem;
    MenuItem18: TMenuItem;
    MenuItem19: TMenuItem;
    MenuItem20: TMenuItem;
    ShowValueAction: TAction;
    CopyToClipboardAction: TAction;
    SelectAllAction: TAction;
    ShowLabelAction: TAction;
    ShowValuelabelAction: TAction;
    ShowLabelvalueAction: TAction;
    ShowVarLabelAction: TAction;
    ShowVarNameAction: TAction;
    ShowVarLabelNameAction: TAction;
    ShowVarNameLabelAction: TAction;
    PopupMenu1: TPopupMenu;
    ToogleValueLabelsMenuItem: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    MenuItem5: TMenuItem;
    MenuItem6: TMenuItem;
    MenuItem7: TMenuItem;
    MenuItem8: TMenuItem;
    MenuItem9: TMenuItem;
    MenuItem10: TMenuItem;
    MenuItem1: TMenuItem;
    CopyMenuItem: TMenuItem;
    CopyToClipboardWithHeadersAction: TAction;
    MenuItem11: TMenuItem;
    MenuItem12: TMenuItem;
    MenuItem13: TMenuItem;
    ChooseFontAction: TAction;
    FontDialog1: TFontDialog;
    MenuItem14: TMenuItem;
    MenuItem15: TMenuItem;
    procedure CalcColWidthActionExecute(Sender: TObject);
    procedure CascadeWindowsExecute(Sender: TObject);
    procedure CloseBrowserActionExecute(Sender: TObject);
    procedure CloseAllActionExecute(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure GotoRecordActionExecute(Sender: TObject);
    procedure ShowValueActionExecute(Sender: TObject);
    procedure ShowLabelActionExecute(Sender: TObject);
    procedure ShowValuelabelActionExecute(Sender: TObject);
    procedure ShowLabelvalueActionExecute(Sender: TObject);
    procedure ShowVarLabelActionExecute(Sender: TObject);
    procedure ShowVarNameActionExecute(Sender: TObject);
    procedure ShowVarLabelNameActionExecute(Sender: TObject);
    procedure ShowVarNameLabelActionExecute(Sender: TObject);
    procedure CopyToClipboardActionExecute(Sender: TObject);
    procedure CopyToClipboardWithHeadersActionExecute(Sender: TObject);
    procedure SelectAllActionExecute(Sender: TObject);
    procedure ChooseFontActionExecute(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
  private
    FVarLabelType: TEpiGetVariableLabelType;
    procedure SetVarLabelType(VarLabelType: TEpiGetVariableLabelType);
    procedure FontSizeChanged(Sender: TObject);
    procedure FormChanged(Sender: TObject; Form: TCustomForm);

  private
    FValueLabelType: TEpiGetValueLabelType;
    procedure SetValueLabelType(ValueLabelType: TEpiGetValueLabelType);
  private
    // Index into columns used for sorting. 1 = FRowNoField 2+ = User fields
    // positive values = ascending
    // negative values = decending
    // eg. FSortIdx =  6: ascending sort of FField[FSortIdx - 2]
    //     FSortIdx = -4: decending sort of FField[FSortIdx - 2]
    FSortIdx: Integer;
    FGrid: TDrawGrid;
    FDataFile: TEpiDataFile;
    FRowNoField: TEpiField;
    FFields: TEpiFields;
    FExecutor: TExecutor;
    function GetCellText(ACol, ARow: Integer): UTF8String;
    procedure DrawCell(Sender: TObject; aCol, aRow: Integer; aRect: TRect;
      aState: TGridDrawState);
    procedure HeaderClick(Sender: TObject; IsColumn: Boolean; Index: Integer);
    procedure CalcWidhtsAndHeight;
    procedure CopyToClipboard(WithHeaders: Boolean = false);
    procedure UpdateCaption(ASt: TBrowseCommand);
    procedure UpdateShortcuts;
  public
    constructor Create(TheOwner: TComponent; AExecutor: TExecutor);
    destructor Destroy; override;
    procedure Browse(ADataFile: TEpiDataFile; VarNames: TStrings; ASt: TBrowseCommand);
    class procedure RestoreDefaultPos;
  end;

function CreateBrowser(AExecutor: TExecutor): TBrowseForm4;
procedure CasecadeBrowsers;
procedure CloseBrowsers;


implementation

uses
  options_utils, LCLIntf, LCLType, math, Clipbrd, epidatafilestypes,
  ana_globals, main, ana_procs, contnrs, strutils, StdCtrls, Buttons,
  options_fontoptions;

var
  FormList: TComponentList = nil;

procedure CasecaseWindows(LeadForm: TForm; ResetSize: Boolean);
var
  Runner, i: Integer;
  AForm: TForm;
begin
  Runner := 1;
  for i := 0 to FormList.Count -1 do
    begin
      if FormList[i] = LeadForm then continue;

      AForm := TForm(FormList[i]);
      if ResetSize then
        begin
          Aform.Width := 500;
          Aform.Height := 500;
        end;
      Aform.top := LeadForm.Top + (30 * Runner);
      Aform.Left := LeadForm.Left + (30 * Runner);

      AForm.BringToFront;
      Inc(Runner);
    end;
end;

function CreateBrowser(AExecutor: TExecutor): TBrowseForm4;
begin
  if (not Assigned(FormList)) then
    FormList := TComponentList.Create(true);

  Result := TBrowseForm4.Create(Application, AExecutor);
  FormList.Add(Result);
end;

procedure CasecadeBrowsers;
begin
  if (Assigned(FormList)) and
     (FormList.Count > 0)
  then
    begin
      TForm(FormList[0]).BringToFront;
      CasecaseWindows(TForm(FormList[0]), false);
    end;
end;

procedure CloseBrowsers;
var
  i: Integer;
begin
  if (Assigned(FormList)) and
     (FormList.Count > 0)
  then
    for i := FormList.Count - 1 downto 0 do
      TBrowseForm4(FormList[i]).Close;
end;

{$R *.lfm}

{ TBrowseForm4 }

procedure TBrowseForm4.ShowValueActionExecute(Sender: TObject);
begin
  SetValueLabelType(gvtValue);
end;

procedure TBrowseForm4.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if ((Key = VK_RIGHT) and (Shift = [ssModifier])) then
    FGrid.Col := FGrid.ColCount - 1;

  if ((Key = VK_LEFT) and (Shift = [ssModifier])) then
    FGrid.Col := 1;
end;

procedure TBrowseForm4.GotoRecordActionExecute(Sender: TObject);
var
  AForm: TForm;
  Lbl: TLabel;
  Edt: TEdit;
  OkBtn, CBtn: TBitBtn;
  Res: Integer;
  ObsNo: int64;
  ASelection: TGridRect;
begin
  AForm := TForm.Create(Self);
  AForm.Caption := 'Goto Obs.';
  AForm.Position :=  poOwnerFormCenter;

  Lbl := TLabel.Create(AForm);
  Lbl.Parent := AForm;
  Lbl.Caption := 'Goto Obs.';
  Lbl.AnchorParallel(akLeft, 10, AForm);
  Lbl.AnchorParallel(akTop, 10, AForm);

  Edt := TEdit.Create(AForm);
  Edt.Parent := AForm;
  Edt.AnchorParallel(akLeft, 10, AForm);
  Edt.AnchorParallel(akRight, 10, AForm);
  Edt.AnchorToNeighbour(akTop, 10, Lbl);
  Edt.NumbersOnly := true;


  OkBtn := TBitBtn.Create(AForm);
  OkBtn.Kind := bkOK;
  OkBtn.Parent := AForm;
  OkBtn.Anchors := [];
  OkBtn.AnchorParallel(akRight, 10, AForm);
  OkBtn.AnchorToNeighbour(akTop, 10, Edt);
  OkBtn.AutoSize := true;

  CBtn := TBitBtn.Create(AForm);
  CBtn.Kind := bkCancel;
  CBtn.Parent := AForm;
  CBtn.Anchors := [];
  CBtn.AnchorToNeighbour(akRight, 10, OkBtn);
  CBtn.AnchorToNeighbour(akTop, 10, Edt);
  CBtn.AutoSize := true;

  AForm.AutoSize := true;

  Res := AForm.ShowModal;

  if (not TryStrToInt64(Edt.Text, ObsNo)) then
  begin
    ShowMessage('"' + Edt.Text + '" is not a valid integer!');
    AForm.Free;
    Exit;
  end;
  AForm.Free;

  FGrid.TopRow := ObsNo;
  ASelection := FGrid.Selection;
  ASelection.Top := ObsNo;
  FGrid.Selection := ASelection;
  CalcWidhtsAndHeight;
end;

procedure TBrowseForm4.CalcColWidthActionExecute(Sender: TObject);
begin
  CalcWidhtsAndHeight;
end;

procedure TBrowseForm4.CascadeWindowsExecute(Sender: TObject);
begin
  CasecaseWindows(Self, false);
end;

procedure TBrowseForm4.CloseBrowserActionExecute(Sender: TObject);
begin
  Close;
end;

procedure TBrowseForm4.CloseAllActionExecute(Sender: TObject);
begin
  CloseBrowsers;
end;

procedure TBrowseForm4.ShowLabelActionExecute(Sender: TObject);
begin
  SetValueLabelType(gvtLabel);
end;

procedure TBrowseForm4.ShowValuelabelActionExecute(Sender: TObject);
begin
  SetValueLabelType(gvtValueLabel);
end;

procedure TBrowseForm4.ShowLabelvalueActionExecute(Sender: TObject);
begin
  SetValueLabelType(gvtLabelValue);
end;

procedure TBrowseForm4.ShowVarLabelActionExecute(Sender: TObject);
begin
  SetVarLabelType(gvtVarLabel);
end;

procedure TBrowseForm4.ShowVarNameActionExecute(Sender: TObject);
begin
  SetVarLabelType(gvtVarName);
end;

procedure TBrowseForm4.ShowVarLabelNameActionExecute(Sender: TObject);
begin
  SetVarLabelType(gvtVarLabelName);
end;

procedure TBrowseForm4.ShowVarNameLabelActionExecute(Sender: TObject);
begin
  SetVarLabelType(gvtVarNameLabel);
end;

procedure TBrowseForm4.CopyToClipboardActionExecute(Sender: TObject);
begin
  CopyToClipboard(false);
end;

procedure TBrowseForm4.CopyToClipboardWithHeadersActionExecute(Sender: TObject);
begin
  CopyToClipboard(true);
end;

procedure TBrowseForm4.SelectAllActionExecute(Sender: TObject);
begin
  FGrid.Selection := Rect(1, 1, FGrid.ColCount - 1, FGrid.RowCount - 1);
end;

procedure TBrowseForm4.ChooseFontActionExecute(Sender: TObject);
var
  AFont: TFont;
begin
  AFont := FontFromSetOptions(
    ANA_SO_BROWSER_FONT_NAME,
    ANA_SO_BROWSER_FONT_SIZE,
    ANA_SO_BROWSER_FONT_COLOR,
    ANA_SO_BROWSER_FONT_STYLE,
    FExecutor.SetOptions
  );
  FontDialog1.Font.Assign(AFont);

  if FontDialog1.Execute then
    FontToSetOptions(FontDialog1.Font,
      ANA_SO_BROWSER_FONT_NAME,
      ANA_SO_BROWSER_FONT_SIZE,
      ANA_SO_BROWSER_FONT_COLOR,
      ANA_SO_BROWSER_FONT_STYLE,
      FExecutor.SetOptions
    );
end;

procedure TBrowseForm4.FormShow(Sender: TObject);
begin
  LoadFormPosition(Self, Self.ClassName);
end;

procedure TBrowseForm4.FormClose(Sender: TObject; var CloseAction: TCloseAction
  );
begin
  SaveFormPosition(Self, Self.ClassName);
  Application.ReleaseComponent(Self);
end;

procedure TBrowseForm4.SetVarLabelType(VarLabelType: TEpiGetVariableLabelType);
begin
  FVarLabelType := VarLabelType;
  case FVarLabelType of
    gvtVarName:      MenuItem7.Checked  := true;
    gvtVarLabel:     MenuItem8.Checked  := true;
    gvtVarNameLabel: MenuItem9.Checked  := true;
    gvtVarLabelName: MenuItem10.Checked := true;
  end;

  FGrid.Invalidate;
end;

procedure TBrowseForm4.FontSizeChanged(Sender: TObject);
var
  AFont: TFont;
begin
  AFont := FontFromSetOptions(
             ANA_SO_BROWSER_FONT_NAME,
             ANA_SO_BROWSER_FONT_SIZE,
             ANA_SO_BROWSER_FONT_COLOR,
             ANA_SO_BROWSER_FONT_STYLE,
             FExecutor.SetOptions
  );

  FGrid.Font.Assign(AFont);
  FGrid.Color := TFontColorOption(FExecutor.SetOptions[ANA_SO_BROWSER_BG_COLOR]).GetBGRValue;

  CalcWidhtsAndHeight;
  FGrid.Invalidate;
end;

procedure TBrowseForm4.FormChanged(Sender: TObject; Form: TCustomForm);
begin
  if (Form = Self) then
    ActionList1.State := asNormal
  else
    ActionList1.State := asSuspended;
end;

procedure TBrowseForm4.SetValueLabelType(ValueLabelType: TEpiGetValueLabelType);
begin
  FValueLabelType := ValueLabelType;
  case FValueLabelType of
    gvtValue:      MenuItem2.Checked := true;
    gvtLabel:      MenuItem3.Checked := true;
    gvtValueLabel: MenuItem4.Checked := true;
    gvtLabelValue: MenuItem5.Checked := true;
  end;

  FGrid.Invalidate;
end;

function TBrowseForm4.GetCellText(ACol, ARow: Integer): UTF8String;
begin
  Result := '';

  if (ACol = 0) and (ARow = 0) then
    Result := 'Obs';

  if (ARow = 0) and (ACol > 0) then
    Result := FFields[ACol - 1].GetVariableLabel(FVarLabelType);

  if (ACol = 0) and (ARow > 0) then
    begin
      Result := FRowNoField.AsString[ARow - 1];
      if FDataFile.Deleted[ARow - 1] then
        Result := result + '*';
    end;

  if (ACol > 0) and (ARow > 0) then
    Result := FFields[ACol - 1].GetValueLabelFormatted(ARow - 1, FValueLabelType);
end;

procedure TBrowseForm4.DrawCell(Sender: TObject; aCol, aRow: Integer;
  aRect: TRect; aState: TGridDrawState);
var
  TS: TTextStyle;
begin
  TS := FGrid.Canvas.TextStyle;
  TS.Alignment := taRightJustify;

  if (aCol > 0) and
     (FFields[aCol - 1].FieldType in StringFieldTypes)
  then
    TS.Alignment := taLeftJustify;

  dec(ARect.Right, varCellPadding);
  case TS.Alignment of
    taLeftJustify: Inc(ARect.Left, varCellPadding);
    taRightJustify: Dec(ARect.Right, 1);
  end;

  case TS.Layout of
    tlTop: Inc(ARect.Top, varCellPadding);
    tlBottom: Dec(ARect.Bottom, varCellPadding);
  end;

  if ARect.Right<ARect.Left then
    ARect.Right:=ARect.Left;

  if ARect.Left>ARect.Right then
    ARect.Left:=ARect.Right;

  if ARect.Bottom<ARect.Top then
    ARect.Bottom:=ARect.Top;

  if ARect.Top>ARect.Bottom then
    ARect.Top:=ARect.Bottom;

  FGrid.Canvas.TextStyle := TS;
  if (ARect.Left<>ARect.Right) and (ARect.Top<>ARect.Bottom) then
    FGrid.Canvas.TextRect(aRect,ARect.Left,ARect.Top, GetCellText(aCol, aRow));
end;

procedure TBrowseForm4.HeaderClick(Sender: TObject; IsColumn: Boolean;
  Index: Integer);
var
  SortFields: TEpiFields;
begin
  if (not IsColumn) then exit;

  SortFields := TEpiFields.Create(nil);

  if Index = 0 then
    SortFields.AddItem(FRowNoField)
  else
    SortFields.AddItem(FFields[Index - 1]);

  Inc(Index);
  if Index = abs(FSortIdx) then
    FSortIdx := (-1) * FSortIdx
  else
    FSortIdx := Index;

  FDataFile.SortRecords(SortFields, (FSortIdx < 0));
  SortFields.Free;

  FGrid.Invalidate;
end;

procedure TBrowseForm4.CalcWidhtsAndHeight;
var
  F: TEpiField;
  i, CW, j: Integer;
const
  SideBuffer = 6;
begin
  // Calculate Widths and Default Heights

  FGrid.DefaultRowHeight := FGrid.Canvas.TextHeight('W') + 4;
  FGrid.ColWidths[0]     := Max(FGrid.Canvas.TextWidth(GetCellText(0, 0)),
                                FGrid.Canvas.TextWidth(IntToStr(FRowNoField.Size))
                            ) + SideBuffer;

  for i := 0 to FFields.Count - 1 do
    begin
      F := FFields[i];

      CW := FGrid.Canvas.TextWidth(GetCellText(i + 1, 0)) + SideBuffer;

      for j := FGrid.TopRow to Min(F.Size - 1, FGrid.TopRow + 50) do
        CW := Max(CW, FGrid.Canvas.GetTextWidth(GetCellText(i + 1, j + 1)) + SideBuffer);

      FGrid.ColWidths[i + 1] := CW;
    end;
end;

procedure TBrowseForm4.CopyToClipboard(WithHeaders: Boolean);
var
  ARow, ACol, RowStart, RowEnd, ColStart, ColEnd: Integer;
  SS: TStringStream;
  S: String;
  Del: UTF8String;
begin
  Del := FExecutor.SetOptions.GetValue(ANA_SO_CLIPBOARD_DELIMITER).Value;
  SS := TStringStream.Create('');

  RowStart := Min(FGrid.Selection.Top, FGrid.Selection.Bottom);
  RowEnd   := Max(FGrid.Selection.Top, FGrid.Selection.Bottom);

  ColStart := Min(FGrid.Selection.Left, FGrid.Selection.Right);
  ColEnd   := Max(FGrid.Selection.Left, FGrid.Selection.Right);

  if (WithHeaders) then
    begin
      S := '';

      for ACol := ColStart to ColEnd do
        S := S + FFields[ACol - 1].Name + Del;

      Delete(S, Length(S), 1);
      S := S + LineEnding;
      SS.WriteString(S);
    end;

  if (FGrid.RowCount > 1) then
    for ARow := RowStart to RowEnd do
      begin
        S := '';
        for ACol := ColStart to ColEnd do
          S := S + GetCellText(ACol, ARow) + Del;

        Delete(S, Length(S), 1);
        S := S + LineEnding;

        if ARow = RowEnd then
          Delete(S, Length(S) - Length(LineEnding) + 1, Length(LineEnding));
        SS.WriteString(S);
      end;

  Clipboard.AsText := SS.DataString;
  SS.Free;
end;

procedure TBrowseForm4.UpdateCaption(ASt: TBrowseCommand);
var
  S: String;
  Opt: TOption;
begin
  if (Ast.HasOption('caption', Opt)) then
    begin
      Caption := Opt.Expr.AsString;
      Exit;
    end;

  S := ExtractFileName(FExecutor.DocFile.FileName) + ': ';

  if (FExecutor.Document.DataFiles.Count = 1) then
    S := S + FExecutor.Document.Study.Title.Text
  else
    S := S + FDataFile.Name + ' ' + FDataFile.Caption.Text;

  Caption := S + ' [' + DateTimeToStr(Now) + ']';
end;

procedure TBrowseForm4.UpdateShortcuts;
begin
  CloseBrowserAction.ShortCut    := ShortCut(VK_W, [ssModifier]);
  CloseAllAction.ShortCut := ShortCut(VK_Q, [ssModifier]);
end;

constructor TBrowseForm4.Create(TheOwner: TComponent; AExecutor: TExecutor);
begin
  inherited Create(TheOwner);
  FExecutor := AExecutor;

  Screen.AddHandlerActiveFormChanged(@FormChanged);


  FFields := TEpiFields.Create(nil);
  FFields.Sorted := false;
  FFields.UniqueNames := false;

  FRowNoField := TEpiField.CreateField(nil, ftInteger);

  FGrid := TDrawGrid.Create(Self);
  FGrid.Align         := alClient;
  FGrid.PopupMenu     := PopupMenu1;
  FGrid.Options       :=
    [goColSizing, goFixedVertLine, goFixedHorzLine, goVertLine,
     goHorzLine, goRangeSelect, goDrawFocusSelected, goSmoothScroll,
     goHeaderHotTracking, goHeaderPushedLook];

  FGrid.OnDrawCell    := @DrawCell;
  FGrid.OnHeaderClick := @HeaderClick;

  FGrid.Parent        := Self;

  FExecutor.SetOptions.GetValue(ANA_SO_BROWSER_FONT_NAME).AddOnChangeHandler(@FontSizeChanged);
  FExecutor.SetOptions.GetValue(ANA_SO_BROWSER_FONT_SIZE).AddOnChangeHandler(@FontSizeChanged);
  FExecutor.SetOptions.GetValue(ANA_SO_BROWSER_FONT_COLOR).AddOnChangeHandler(@FontSizeChanged);
  FExecutor.SetOptions.GetValue(ANA_SO_BROWSER_FONT_STYLE).AddOnChangeHandler(@FontSizeChanged);
  FExecutor.SetOptions.GetValue(ANA_SO_BROWSER_BG_COLOR).AddOnChangeHandler(@FontSizeChanged);

  UpdateShortcuts;
end;

destructor TBrowseForm4.Destroy;
begin
  Screen.RemoveHandlerActiveFormChanged(@FormChanged);
  FExecutor.SetOptions.GetValue(ANA_SO_BROWSER_FONT_NAME).RemoveOnChangeHandler(@FontSizeChanged);
  FExecutor.SetOptions.GetValue(ANA_SO_BROWSER_FONT_SIZE).RemoveOnChangeHandler(@FontSizeChanged);

  FFields.Free;
  FDataFile.Free;
  inherited Destroy;
end;

procedure TBrowseForm4.Browse(ADataFile: TEpiDataFile; VarNames: TStrings;
  ASt: TBrowseCommand);
var
  S: String;
  i: Integer;
begin
  SetVarLabelType(VariableLabelTypeFromOptionList(ASt.Options, FExecutor.SetOptions));
  SetValueLabelType(ValueLabelTypeFromOptionList(ASt.Options, FExecutor.SetOptions));

  FSortIdx := 1;
  FDataFile := ADataFile;

  FFields.Clear;
  for S in VarNames do
    FFields.AddItem(FDataFile.Fields.FieldByName[S]);

  FRowNoField.Size := FDataFile.Size;
  for i := 0 to FRowNoField.Size - 1 do
    FRowNoField.AsInteger[i] := i + 1;
  FDataFile.MainSection.Fields.AddItem(FRowNoField);

  FGrid.ColCount      := FFields.Count + 1;
  FGrid.RowCount      := FDataFile.Size + 1;

  FontSizeChanged(nil);
  UpdateCaption(ASt);
end;

class procedure TBrowseForm4.RestoreDefaultPos;
var
  AForm: TForm;
  i: Integer;
begin
  if (not Assigned(FormList)) or
     (FormList.Count = 0)
  then
    begin
      AForm := TForm.Create(nil);
      Aform.Width := 500;
      Aform.Height := 500;
      Aform.top := (Screen.Monitors[0].Height - Aform.Height) div 2;
      Aform.Left := (Screen.Monitors[0].Width - Aform.Width) div 2;
      SaveFormPosition(Aform, TBrowseForm4.ClassName);

      AForm.Free;
      Exit;
    end;

  AForm := TForm(FormList[0]);
  Aform.Width := 500;
  Aform.Height := 500;
  Aform.top := (Screen.Monitors[0].Height - Aform.Height) div 2;
  Aform.Left := (Screen.Monitors[0].Width - Aform.Width) div 2;
  SaveFormPosition(Aform, AForm.ClassName);

  CasecaseWindows(AForm, true);
end;

end.

