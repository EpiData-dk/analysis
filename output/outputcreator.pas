unit outputcreator;

{$codepage UTF-8}
{$mode objfpc}{$H+}
{$interfaces corba}

interface

uses
  Classes, SysUtils, contnrs, options_hashmap, epireport_generator_base, epireport_types;

type
  TOutputType = (
    otTable,
    otLine
  );

  TOutputTable = class;

  (*
    {\l link}
    {\b bold}
    {\u underline}
    {\i italic}
    {\s subscript}
    {\S superscript}
  *)

  { IOutputText }

  IOutputText = interface['IOutputText']
    function  GetText: UTF8String;
    procedure SetText(AValue: UTF8String);
    property  Text: UTF8String read GetText write SetText;
  end;


  { TOutputBase }

  TOutputBase = class
  protected
    function GetOutputType: TOutputType; virtual; abstract;
  public
    property OutputType: TOutputType read GetOutputType;
  end;


  { TOutputAlignedText }

  TOutputAlignedText = class(IOutputText)
  private
    FAlignment: TAlignment;
    FText: UTF8String;
    function GetText: UTF8String;
    procedure SetText(AValue: UTF8String);
  public
    constructor Create; virtual;
    property Text: UTF8String read GetText write SetText;
    property Alignment: TAlignment read FAlignment write FAlignment;
  end;

  TOutputTableHeader = class(TOutputAlignedText)
  public
    constructor Create; override;
  end;

  TOutputTableCellBorder = (cbLeft, cbTop, cbRight, cbBottom);
  TOutputTableCellBorders = set of TOutputTableCellBorder;

  { TOutputTableCell }

  TOutputTableCell = class(TOutputAlignedText)
  private
    FTable: TOutputTable;
    FBorders: TOutputTableCellBorders;
    FCol: Integer;
    FRow: Integer;
  public
    constructor Create(ATable: TOutputTable); virtual;
    // HasBorder: will check self and adjacent cell for border information
    function HasBorder(Border: TOutputTableCellBorder): boolean;
    property Borders: TOutputTableCellBorders read FBorders write FBorders;
    property Row: Integer read FRow;
    property Col: Integer read FCol;
    property Table: TOutputTable read FTable;
  end;

  { TOutputTable }

  TOutputTable = class(TOutputBase)
  private
    FCells: Array of Array of TOutputTableCell;
    FColCount: Integer;
    FRowCount: Integer;
    FHeader: TOutputTableHeader;
    FFooter: TOutputTableHeader;
    procedure CreateCells(Area: TRect);
    procedure DestroyCells(Area: TRect);
    function  GetCell(const Col, Row: Integer): TOutputTableCell;
    procedure SetColCount(AValue: Integer);
    procedure SetRowCount(AValue: Integer);
    function  GetHasCell(const Col, Row: Integer): Boolean;
  protected
    function GetOutputType: TOutputType; override;
  public
    constructor Create;
    // Changes all cells in row to given alignment
    procedure SetRowAlignment(Const RowNo: Integer; Align: TAlignment);
    // Changes all cells in col to given alignment
    procedure SetColAlignment(Const ColNo: Integer; Align: TAlignment);
    // Sets borders of all cells in a col.
    procedure SetColBorders(Const ColNo: Integer; Borders: TOutputTableCellBorders);
    // Sets borders of all cells in a row.
    procedure SetRowBorders(Const RowNo: Integer; Borders: TOutputTableCellBorders);
    // Set a box around col/row (removes any existing borders)
    procedure SetColBoxBorder(Const ColNo: Integer);
    procedure SetRowBoxBorder(Const RowNo: Integer);
  public
    property Header: TOutputTableHeader read FHeader;
    property Footer: TOutputTableHeader read FFooter;
    property ColCount: Integer read FColCount write SetColCount;
    property RowCount: Integer read FRowCount write SetRowCount;
    property Cell[Const Col, Row: Integer]: TOutputTableCell read GetCell;
  end;


  TOutputLineType = (
    oltNormal,
    oltCommand,
    oltError,
    oltWarning,
    oltInfo,
    oltDebug
  );

  { TOutputLine }

  TOutputLine = class(TOutputBase, IOutputText)
  private
    FText: UTF8String;
    FLineType: TOutputLineType;
    function GetText: UTF8String;
    procedure SetText(AValue: UTF8String);
  protected
    function GetOutputType: TOutputType; override;
  public
    constructor Create;
    property Text: UTF8String read GetText write SetText;
    property LineType: TOutputLineType read FLineType write FLineType;
  end;


  { TOutputCreator }

  TOutputCreator = class
  private
    FList: TFPObjectList;
    FOnRedrawRequest: TNotifyEvent;
    FSetOptions: TSetOptionsMap;
    function GetItems(const Index: Integer): TOutputBase;
    procedure SetOnRedrawRequest(AValue: TNotifyEvent);
    function  SetOptionActive(Const Name: UTF8String; Const Value: UTF8String = 'ON'): boolean;
  protected
    function AddLine: TOutputLine;
  public
    constructor Create;
    function AddTable: TOutputTable;
    procedure DoNormal(Const Msg: UTF8String);
    procedure DoDebug(Const Msg: UTF8String);
    procedure DoError(Const Msg: UTF8String);
    procedure DoInfoShort(Const Msg: UTF8String);
    procedure DoInfoAll(Const Msg: UTF8String);
    procedure DoWarning(Const Msg: UTF8String);
    procedure DoCommand(Const Msg: UTF8String);
    procedure Clear;
    procedure RequestRedraw;
  protected
    function GetCount: Integer;
    procedure DoRedrawRequest;
  public
    property OnRedrawRequest: TNotifyEvent read FOnRedrawRequest write SetOnRedrawRequest;
    property Count: Integer read GetCount;
    property Items[Const Index: Integer]: TOutputBase read GetItems;
    property SetOptions: TSetOptionsMap read FSetOptions write FSetOptions;
  end;

  { TCoreReportGeneratorToOutputCreator }

  TCoreReportGeneratorToOutputCreator = class(TEpiReportGeneratorBase)
  // Inherited overrides!
  private
    FCurrentTable: TOutputTable;
    function LineFromLines(var Lines: String): string;
  public
    // Lines
    procedure Section(Const Text: string); override;
    procedure Heading(Const Text: string); override;
    procedure Line(Const Text: string); override;

    // Table
    procedure TableHeader(const Text: string; const AColCount, ARowCount: Integer;
      const HeaderOptions: TEpiReportGeneratorTableHeaderOptionSet = [
      thoRowHeader]); override;
    procedure TableFooter(Const Text: string); override;
    procedure TableCell(const Text: string; const Col, Row: Integer;
      const CellAdjust: TEpiReportGeneratorTableCellAdjustment = tcaAutoAdjust;
      Const CellOptions: TEpiReportGeneratorTableCellOptionSet = []
      ); override;

    procedure StartReport(Const Title: string); override;
    procedure EndReport; override;
  private
    FOutputCreator: TOutputCreator;
  public
    constructor Create(AOutputCreator: TOutputCreator);
  end;

const
  cbAll = [cbTop, cbLeft, cbRight, cbBottom];

implementation

uses
  math, ana_globals, LazUTF8, strutils;

{ TCoreReportGeneratorToOutputCreator }

function TCoreReportGeneratorToOutputCreator.LineFromLines(var Lines: String
  ): string;
var
  p: Integer;
begin
  p := Pos(LineEnding, Lines);
  if p = 0 then
  begin
    result:=Lines;
    Lines:='';
  end else begin
    Result := Copy(Lines, 1, p-1);
    delete(Lines, 1, (p - 1) + Length(LineEnding));
  end;
end;

procedure TCoreReportGeneratorToOutputCreator.Section(const Text: string);
begin
  FOutputCreator.DoNormal(Text);
end;

procedure TCoreReportGeneratorToOutputCreator.Heading(const Text: string);
begin
  FOutputCreator.DoNormal(Text);
end;

procedure TCoreReportGeneratorToOutputCreator.Line(const Text: string);
begin
  FOutputCreator.DoNormal(Text);
end;

procedure TCoreReportGeneratorToOutputCreator.TableHeader(const Text: string;
  const AColCount, ARowCount: Integer;
  const HeaderOptions: TEpiReportGeneratorTableHeaderOptionSet);
begin
  inherited TableHeader(Text, AColCount, ARowCount, HeaderOptions);

  FCurrentTable := FOutputCreator.AddTable;
  FCurrentTable.RowCount := ARowCount;
  FCurrentTable.ColCount := AColCount;
  FCurrentTable.Header.Text := Text;

  if (thoColHeader in HeaderOptions) then
    FCurrentTable.SetColBoxBorder(0);
  if (thoRowHeader in HeaderOptions) then
    FCurrentTable.SetRowBoxBorder(0);
end;

procedure TCoreReportGeneratorToOutputCreator.TableFooter(const Text: string);
begin
  FCurrentTable.Footer.Text := Text;

  inherited TableFooter(Text);
end;

procedure TCoreReportGeneratorToOutputCreator.TableCell(const Text: string;
  const Col, Row: Integer;
  const CellAdjust: TEpiReportGeneratorTableCellAdjustment;
  const CellOptions: TEpiReportGeneratorTableCellOptionSet);
var
  Cell: TOutputTableCell;
  CBorders: TOutputTableCellBorders;
begin
  inherited TableCell(Text, Col, Row, CellAdjust, CellOptions);

  Cell := FCurrentTable.Cell[Col, Row];
  Cell.Text := Text;

  case CellAdjust of
    tcaAutoAdjust:  Cell.Alignment := taLeftJustify;
    tcaLeftAdjust:  Cell.Alignment := taLeftJustify;
    tcaCenter:      Cell.Alignment := taCenter;
    tcaRightAdjust: Cell.Alignment := taRightJustify;
  end;

  CBorders := Cell.Borders;
  if (tcoLeftBorder in CellOptions)   then
    Include(CBorders, cbLeft);
  if (tcoRightBorder in CellOptions)  then
    Include(CBorders, cbRight);
  if (tcoTopBorder in CellOptions)    then
    Include(CBorders, cbTop);
  if (tcoBottomBorder in CellOptions) then
    Include(CBorders, cbBottom);
  Cell.Borders := CBorders;
end;

procedure TCoreReportGeneratorToOutputCreator.StartReport(const Title: string);
begin
  //
end;

procedure TCoreReportGeneratorToOutputCreator.EndReport;
begin
  //
end;

constructor TCoreReportGeneratorToOutputCreator.Create(
  AOutputCreator: TOutputCreator);
begin
  inherited Create;
  FOutputCreator := AOutputCreator;
end;

{ TOutputAlignedText }

function TOutputAlignedText.GetText: UTF8String;
begin
  Result := FText;
end;

procedure TOutputAlignedText.SetText(AValue: UTF8String);
begin
  FText := AValue;
end;

constructor TOutputAlignedText.Create;
begin
  FAlignment := taCenter;
end;

{ TOutputTableHeader }

constructor TOutputTableHeader.Create;
begin
  FAlignment := taCenter;
end;

{ TOutputTableCell }

constructor TOutputTableCell.Create(ATable: TOutputTable);
begin
  inherited Create;
  FAlignment := taRightJustify;
  FTable := ATable;
end;

function TOutputTableCell.HasBorder(Border: TOutputTableCellBorder): boolean;
begin
  result := Border in Borders;

  if (Result) or
     (not Assigned(FTable))
  then
    Exit;

  if (Border in [cbLeft]) and
     (FCol > 0)
  then
    result := (cbRight in FTable.Cell[FCol - 1, FRow].Borders);

  if (Border in [cbRight]) and
     (FCol < (FTable.ColCount - 1))
  then
    result := (cbLeft in FTable.Cell[FCol + 1, FRow].Borders);

  if (Border in [cbTop]) and
     (FRow > 0)
  then
    result := (cbBottom in FTable.Cell[FCol, FRow - 1].Borders);

  if (Border in [cbBottom]) and
     (FRow < (FTable.RowCount - 1))
  then
    result := (cbTop in FTable.Cell[FCol, FRow + 1].Borders);
end;

{ TOutputTable }

procedure TOutputTable.CreateCells(Area: TRect);
var
  i, j: LongInt;
  C: TOutputTableCell;
begin
  for i := Max(Area.Left, 0) to Area.Right do
    for j := Max(Area.Top, 0) to Area.Bottom do
      begin
        C := TOutputTableCell.Create(Self);
        C.FCol := i;
        C.FRow := j;
        FCells[i][j] := C;
      end;
end;

procedure TOutputTable.DestroyCells(Area: TRect);
var
  i, j: LongInt;
begin
  for i := Max(Area.Left, 0) to Area.Right do
    for j := Max(Area.Top, 0) to Area.Bottom do
      FCells[i][j].Free;
end;

function TOutputTable.GetCell(const Col, Row: Integer): TOutputTableCell;
begin
  result := nil;

  if (Col >= FColCount) or
     (Row >= FRowCount)
  then
    Exit;

  Result := FCells[Col, Row]
end;

procedure TOutputTable.SetColCount(AValue: Integer);
var
  i: Integer;
begin
  if FColCount = AValue then Exit;

  if (AValue < FColCount) then
    DestroyCells(Rect(AValue - 1, 0, FColCount - 1, FRowCount - 1));

  SetLength(FCells, AValue);
  for i := Low(FCells) to High(FCells) do
    SetLength(FCells[i], RowCount);

  if (AValue > FColCount) then
    CreateCells(Rect(FColCount, 0, AValue - 1, FRowCount - 1));

  FColCount := AValue;
end;

procedure TOutputTable.SetRowCount(AValue: Integer);
var
  i: Integer;
begin
  if FRowCount = AValue then Exit;

  if (AValue < FRowCount) then
    DestroyCells(Rect(0, AValue - 1, FColCount - 1, FRowCount - 1));

  for i := Low(FCells) to High(FCells) do
    SetLength(FCells[i], AValue);

  if (AValue > FRowCount) then
    CreateCells(Rect(0, FRowCount, FColCount - 1, AValue - 1));

  FRowCount := AValue;
end;

function TOutputTable.GetHasCell(const Col, Row: Integer): Boolean;
begin
  result := false;

  if (Col >= FColCount) or
     (Row >= FRowCount)
  then
    Exit;

  result := Assigned(FCells[Col,Row]);
end;

function TOutputTable.GetOutputType: TOutputType;
begin
  result := otTable;
end;

constructor TOutputTable.Create;
begin
  FHeader := TOutputTableHeader.Create;
  FFooter := TOutputTableHeader.Create;
end;

procedure TOutputTable.SetRowAlignment(const RowNo: Integer; Align: TAlignment);
var
  i: Integer;
begin
  if (RowNo < 0) or (RowNo >= RowCount) then exit;

  for i := Low(FCells) to High(FCells) do
    Cell[I, RowNo].Alignment := Align;
end;

procedure TOutputTable.SetColAlignment(const ColNo: Integer; Align: TAlignment);
var
  i: Integer;
begin
  if (ColNo < 0) or (ColNo >= ColCount) then exit;

  for i := Low(FCells[ColNo]) to High(FCells[ColNo]) do
    Cell[ColNo, I].Alignment := Align;
end;

procedure TOutputTable.SetColBorders(const ColNo: Integer;
  Borders: TOutputTableCellBorders);
var
  i: Integer;
begin
  if (ColNo < 0) or (ColNo >= ColCount) then exit;

  for i := Low(FCells[ColNo]) to High(FCells[ColNo]) do
    Cell[ColNo, I].Borders := Borders;
end;

procedure TOutputTable.SetRowBorders(const RowNo: Integer;
  Borders: TOutputTableCellBorders);
var
  i: Integer;
begin
  if (RowNo < 0) or (RowNo >= RowCount) then exit;

  for i := Low(FCells) to High(FCells) do
    Cell[I, RowNo].Borders := Borders;
end;

procedure TOutputTable.SetColBoxBorder(const ColNo: Integer);
begin
  SetColBorders(ColNo, [cbLeft, cbRight]);
  Cell[ColNo, 0].Borders            := Cell[ColNo, 0].Borders            + [cbTop];
  Cell[ColNo, RowCount - 1].Borders := Cell[ColNo, RowCount - 1].Borders + [cbBottom];
end;

procedure TOutputTable.SetRowBoxBorder(const RowNo: Integer);
begin
  SetRowBorders(RowNo, [cbTop, cbBottom]);
  Cell[0, RowNo].Borders            := Cell[0, RowNo].Borders            + [cbLeft];
  Cell[ColCount - 1, RowNo].Borders := Cell[ColCount - 1, RowNo].Borders + [cbRight];
end;

{ TOutputLine }

function TOutputLine.GetText: UTF8String;
begin
  result := FText;
end;

procedure TOutputLine.SetText(AValue: UTF8String);
begin
  FText := AValue;
end;

function TOutputLine.GetOutputType: TOutputType;
begin
  result := otLine;
end;

constructor TOutputLine.Create;
begin

end;

{ TOutputCreator }

function TOutputCreator.GetCount: Integer;
begin
  result := FList.Count;
end;

procedure TOutputCreator.DoRedrawRequest;
begin
  if Assigned(OnRedrawRequest) then
    OnRedrawRequest(Self);
end;

function TOutputCreator.GetItems(const Index: Integer): TOutputBase;
begin
  result := TOutputBase(FList[Index]);
end;

procedure TOutputCreator.SetOnRedrawRequest(AValue: TNotifyEvent);
begin
  if FOnRedrawRequest = AValue then Exit;
  FOnRedrawRequest := AValue;
end;

function TOutputCreator.SetOptionActive(const Name: UTF8String;
  const Value: UTF8String): boolean;
begin
  result := true;

  if Assigned(FSetOptions) then
    result := UTF8CompareText(FSetOptions.GetValue(Name).Value, Value) = 0;
end;

constructor TOutputCreator.Create;
begin
  FList := TFPObjectList.Create(true);
  FList.OwnsObjects := true;
end;

function TOutputCreator.AddTable: TOutputTable;
begin
  result := TOutputTable.Create;
  FList.Add(Result);
end;

procedure TOutputCreator.DoNormal(const Msg: UTF8String);
var
  L: TOutputLine;
begin
  if (not SetOptionActive(ANA_SO_ECHO)) then exit;

  L := AddLine;
  L.LineType := oltNormal;
  L.Text := Msg;
end;

function TOutputCreator.AddLine: TOutputLine;
begin
  result := TOutputLine.Create;
  FList.Add(Result);
end;

procedure TOutputCreator.DoDebug(const Msg: UTF8String);
var
  L: TOutputLine;
begin
  if (not SetOptionActive(ANA_SO_ECHO)) then exit;
  if (not SetOptionActive(ANA_SO_SHOW_DEBUG)) then exit;

  L := AddLine;
  L.LineType := oltDebug;
  L.Text := Msg;
end;

procedure TOutputCreator.DoError(const Msg: UTF8String);
var
  L: TOutputLine;
begin
  if (not SetOptionActive(ANA_SO_ECHO)) then exit;
  if (not SetOptionActive(ANA_SO_SHOW_ERROR)) then exit;

  L := AddLine;
  L.LineType := oltError;
  L.Text := Msg;
end;

procedure TOutputCreator.DoInfoShort(const Msg: UTF8String);
var
  L: TOutputLine;
begin
  if (not SetOptionActive(ANA_SO_ECHO)) then exit;
  if (not SetOptionActive(ANA_SO_SHOW_INFO)) and
     (not SetOptionActive(ANA_SO_SHOW_INFO, 'SHORT'))
  then
    Exit;

  L := AddLine;
  L.LineType := oltInfo;
  L.Text := Msg;
end;

procedure TOutputCreator.DoInfoAll(const Msg: UTF8String);
var
  L: TOutputLine;
begin
  if (not SetOptionActive(ANA_SO_ECHO)) then exit;
  if (not SetOptionActive(ANA_SO_SHOW_INFO)) then exit;

  L := AddLine;
  L.LineType := oltInfo;
  L.Text := Msg;
end;

procedure TOutputCreator.DoWarning(const Msg: UTF8String);
var
  L: TOutputLine;
begin
  if (not SetOptionActive(ANA_SO_ECHO)) then exit;
  if (not SetOptionActive(ANA_SO_SHOW_WARNING)) then exit;

  L := AddLine;
  L.LineType := oltWarning;
  L.Text := Msg;
end;

procedure TOutputCreator.DoCommand(const Msg: UTF8String);
var
  L: TOutputLine;
begin
  if (not SetOptionActive(ANA_SO_ECHO)) then exit;
  if (not SetOptionActive(ANA_SO_SHOW_COMMAND)) then exit;

  L := AddLine;
  L.LineType := oltCommand;
  L.Text := Msg;
end;

procedure TOutputCreator.Clear;
begin
  FList.Clear;
end;

procedure TOutputCreator.RequestRedraw;
begin
  DoRedrawRequest;
end;

end.

