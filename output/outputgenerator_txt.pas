unit outputgenerator_txt;

{$codepage UTF8}
{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, outputgenerator_base, outputcreator, epicustombase;

type

  { TOutputGeneratorTXT }

  TOutputGeneratorTXT = class(TOutputGeneratorBase)
  { Misc }
  private
    function LineFromLines(var Lines: UTF8String; LineNo: Integer = 1): UTF8String;

  { Table }
  private
    type
      TCellCorner = (ccTopLeft, ccTopRight, ccBottomLeft, ccBottomRight);
  private
    function GetCorner(Cell: TOutputTableCell; CellCorner: TCellCorner = ccTopLeft): UTF8String;
    function AdjustedText(O: TOutputAlignedText; Width: Integer; LineNo: Integer = 1): UTF8String;
    function CenterText(Const Txt: UTF8String; Width: Integer): UTF8String;
    function LeftAdjustText(Const Txt: UTF8String; Width: Integer): UTF8String;
    function RightAdjustText(Const Txt: UTF8String; Width: Integer): UTF8String;
    procedure DoOutputTable(Table: TOutputTable); override;

  { Text }
  protected
    FText: UTF8String;
    procedure TextOut(const S: UTF8String; TextFormat: TTextFormat); override;
    function GetText(IText: IOutputText): UTF8String;

  { Line }
  protected
    procedure DoOutputLine(Line: TOutputLine); override;
  end;

implementation

uses
  LazUTF8, strutils, Math;

{ TOutputGeneratorTXT }

procedure TOutputGeneratorTXT.DoOutputLine(Line: TOutputLine);
var
  S: UTF8String;
begin
  S := '';

  case Line.LineType of
    oltNormal: ;
    oltCommand: ;
    oltError:   S := 'ERROR: ';
//    oltWarning: S := 'Warning: ';
//    oltInfo:    S := 'Info: ';
  end;
  WriteToStream(S + GetText(Line) + LineEnding);
end;

function TOutputGeneratorTXT.LineFromLines(var Lines: UTF8String;
  LineNo: Integer): UTF8String;
var
  p, i: Integer;
begin
  for i := 0 to LineNo - 1 do
  begin
    p := Pos(LineEnding, Lines);
    if p = 0 then
    begin
      result := Lines;
      Lines  := '';
    end else begin
      Result := Copy(Lines, 1, p-1);
      delete(Lines, 1, (p - 1) + Length(LineEnding));
    end;
  end;
end;

function TOutputGeneratorTXT.GetCorner(Cell: TOutputTableCell;
  CellCorner: TCellCorner): UTF8String;
type
  Rot = (r0, r90, r180, r270);
const
  Corner2: array[Rot, TCellCorner] of string =
    (('┌','┐','└','┘'),
     ('┐','┌','┘','└'),
     ('┘','└','┐','┌'),
     ('└','┘','┌','┐'));

  Corner3: array[Rot, TOutputTableCellBorder] of string =
    (('├','┬','┤','┴'),
     ('┤','┬','┴','├'),
     ('┤','┴','├','┬'),
     ('├','┴','┬','┤'));
var
  A, B: TOutputTableCell;
  ALeft, BTop: TOutputTableCellBorder;
  Rotation: Rot;

begin
{
        |  A   |
     -- ? ---- ? --

     B  | Cell | B

     -- ? ---- ? --
        |  A   |
}
  case CellCorner of
    ccTopLeft,
    ccTopRight:
      begin
        if (Cell.Row - 1) >= 0 then
          A := Cell.Table.Cell[Cell.Col, Cell.Row - 1]
        else
          A := TOutputTableCell.Create(nil);

        if (CellCorner = ccTopLeft) then
          begin
            Rotation := r0;
            ALeft := cbLeft;
            BTop  := cbTop;

            if (Cell.Col - 1) >= 0 then
              B := Cell.Table.Cell[Cell.Col - 1, Cell.Row]
            else
              B := TOutputTableCell.Create(nil);
          end
        else
          begin
            Rotation := r90;
            ALeft := cbRight;
            BTop  := cbTop;

            if (Cell.Col + 1) < Cell.Table.ColCount then
              B := Cell.Table.Cell[Cell.Col + 1, Cell.Row]
            else
              B := TOutputTableCell.Create(nil);
          end;
      end;
    ccBottomLeft,
    ccBottomRight:
      begin
        if (Cell.Row + 1) < Cell.Table.RowCount then
          A := Cell.Table.Cell[Cell.Col, Cell.Row + 1]
        else
          A := TOutputTableCell.Create(nil);

          if (CellCorner = ccBottomLeft) then
            begin
              Rotation := r270;
              ALeft := cbLeft;
              BTop  := cbBottom;

              if (Cell.Col - 1) >= 0 then
                B := Cell.Table.Cell[Cell.Col - 1, Cell.Row]
              else
                B := TOutputTableCell.Create(nil);
            end
          else
            begin
              Rotation := r180;
              ALeft := cbRight;
              BTop  := cbBottom;

              if (Cell.Col + 1) < Cell.Table.ColCount then
                B := Cell.Table.Cell[Cell.Col + 1, Cell.Row]
              else
                B := TOutputTableCell.Create(nil);
            end;
      end;
  end;

  // Assumed CellCorner = ccTopLeft;
  if Cell.HasBorder(BTop) then
    begin
      // Eliminates: ┤ ┐ ┘ (empty)
      // remains: ├ ┼ └ ┴ ┌ ┬ ─

      if Cell.HasBorder(ALeft) then
        begin
          // Eliminates: └ ┴ ─
          // remains: ┼ ├ ┬ ┌

          if A.HasBorder(ALeft) and B.HasBorder(BTop) then
            result := '┼'
          else if A.HasBorder(ALeft) then
            result := Corner3[Rotation, cbLeft]      // '├'
          else if B.HasBorder(BTop) then
            result := Corner3[Rotation, cbTop]      // '┬'
          else
            result := Corner2[Rotation, ccTopLeft] ; // '┌';
        end
      else
        begin
          // Eliminates: ├ ┼ ┌ ┬
          // remains: └ ┴ ─

          if A.HasBorder(ALeft) and B.HasBorder(BTop) then
            result := Corner3[Rotation, cbBottom]     // '┴'
          else if A.HasBorder(ALeft) then
            result := Corner2[Rotation, ccBottomLeft] // '└'
          else if B.HasBorder(BTop) then
            result := '─'
          else
            result := '─';
        end;
    end
  else
    begin
      // Eliminates: ├ ┼ └ ┴ ┌ ┬
      // remains: ┤ ┐ ┘ │ ─  (empty)

      if Cell.HasBorder(ALeft) then
        begin
          // Eleminates: ┘ ─ (empty)
          // remains: ┤ ┐ │

          if A.HasBorder(ALeft) and B.HasBorder(BTop) then
            result := Corner3[Rotation, cbRight]     //'┤'
          else if A.HasBorder(ALeft) then
            result := '│'
          else if B.HasBorder(BTop) then
            result := Corner2[Rotation, ccTopRight]  //'┐'
          else
            result := ' '; // '│';
        end
      else
        begin
          // Eleminates: ┤ ┐
          // remains: ┘ │ ─ (empty)

          if A.HasBorder(ALeft) and B.HasBorder(BTop) then
            result := Corner2[Rotation, ccBottomRight] //'┘'
          else if A.HasBorder(ALeft) then
            result := '│'
          else if B.HasBorder(BTop) then
            result := '─'
          else
            result := ' ';
        end;
    end;
end;

function TOutputGeneratorTXT.AdjustedText(O: TOutputAlignedText;
  Width: Integer; LineNo: Integer): UTF8String;
begin
  Result := GetText(O);
  Result := LineFromLines(Result, LineNo);

  case O.Alignment of
    taLeftJustify:  Result := LeftAdjustText(Result, Width);
    taRightJustify: Result := RightAdjustText(Result, Width);
    taCenter:       Result := CenterText(Result, Width);
  end;
end;

function TOutputGeneratorTXT.CenterText(const Txt: UTF8String; Width: Integer
  ): UTF8String;
var
  L: PtrInt;
  D: Integer;
  Count: Integer;
begin
  L := UTF8Length(Txt);
  if L >= Width then
  begin
    Result := Txt;
    Exit;
  end;

  // D indicates if an extra space is required at the end of text.
  D := 0;
  if (odd(Width) and not odd(L)) then Inc(D);
  if (not odd(Width) and odd(L)) then Inc(D);

  Count := (Width - UTF8Length(Txt)) div 2;
  Result :=
    // Prefix
    DupeString(' ', Count) +
    // Text
    Txt +
    // Postfix (with possible extra space)
    DupeString(' ', Count + D);
end;

function TOutputGeneratorTXT.LeftAdjustText(const Txt: UTF8String;
  Width: Integer): UTF8String;
begin
  Result := Txt + DupeString(' ', Width - UTF8Length(Txt));
end;

function TOutputGeneratorTXT.RightAdjustText(const Txt: UTF8String;
  Width: Integer): UTF8String;
begin
  Result := DupeString(' ', Width - UTF8Length(Txt)) + Txt;
end;

procedure TOutputGeneratorTXT.DoOutputTable(Table: TOutputTable);
var
  ColWidths, RowHeights: TBoundArray;
  ColWidthTotal: Integer;
  Col, Row, L, H, i: Integer;
  Txt, S: UTF8String;
  C: TOutputTableCell;
begin
  // 1: Find max with of each column/row.
  SetLength(ColWidths, Table.ColCount);
  SetLength(RowHeights, Table.RowCount);
  for Row := Low(RowHeights) to High(RowHeights) do
    RowHeights[Row] := 1;
  ColWidthTotal := 0;


  for Col := 0 to Table.ColCount - 1 do
  begin
    for Row := 0 to Table.RowCount - 1 do
    begin
      L := 0;
      H := 0;

      // Handle multiple lines in one cell.
      C := Table.Cell[Col, Row];
      Txt := GetText(C);
      while Length(Txt) > 0 do
      begin
        Inc(H);
        S := LineFromLines(Txt);
        L := Max(L, UTF8Length(S));
      end;

      ColWidths[Col] := Math.Max(ColWidths[Col], L);
      RowHeights[Row] := Math.Max(RowHeights[Row], H);
    end;

    // Add extra room for 1 border (left side)
    ColWidths[Col] := ColWidths[Col] + 1;
    ColWidthTotal += ColWidths[Col];
  end;

  if Table.Header.Text <> '' then
    begin
      H := 0;
      Txt := GetText(Table.Header);
      while Length(Txt) > 0 do
      begin
        Inc(H);
        LineFromLines(Txt);
      end;

      for i := 1 to H do
        WriteToStream(AdjustedText(Table.Header, ColWidthTotal, i) + LineEnding);
    end;

  for Row := 0 to Table.RowCount -1 do
  begin
    Txt := '';

    // Create top border!
    for Col := 0 to Table.ColCount - 1 do
    begin
      C := Table.Cell[Col, Row];

      // Get the corner first
      Txt += GetCorner(C);

      // paint border line / or create
      if Table.Cell[Col, Row].HasBorder(cbTop) then
        Txt += DupeString('─', (ColWidths[Col] - 1))
      else
        Txt += DupeString(' ', (ColWidths[Col] - 1));


      if (Col = (Table.ColCount - 1)) then
        Txt += GetCorner(C, ccTopRight);
    end;

    if UTF8Trim(Txt) <> '' then
      WriteToStream(Txt + LineEnding);


    // Create the cell content
    for i := 0 to (RowHeights[Row] - 1) do
    begin
      Txt := '';

      for Col := 0 to Table.ColCount - 1 do
      begin
        C := Table.Cell[Col, Row];
        L := 0;

        if C.HasBorder(cbLeft) then
          Txt += '│'
        else
          Txt += ' ';

        Txt += AdjustedText(C, (ColWidths[Col] - 1), i + 1);

        if (Col = (Table.ColCount - 1)) then
          begin
            if C.HasBorder(cbRight) then
              Txt += '│';
          end
        else
          Txt += '';
      end;

      WriteToStream(Txt + LineEnding);
    end;

    // Create bottom border
    if (Row = (Table.RowCount - 1)) then
    begin
      Txt := '';

      for Col := 0 to Table.ColCount - 1 do
      begin
        C := Table.Cell[Col, Row];

        // Get the corner first
        Txt += GetCorner(C, ccBottomLeft);

        // paint border line / or create
        if Table.Cell[Col, Row].HasBorder(cbBottom) then
          Txt += DupeString('─', ColWidths[Col] - 1)
        else
          Txt += DupeString(' ', ColWidths[Col] - 1);


        if (Col = (Table.ColCount - 1)) then
          Txt += GetCorner(C, ccBottomRight);
      end;

      WriteToStream(Txt + LineEnding);
    end;
  end;

  if Table.Footer.Text <> '' then
    begin
      H := 0;
      Txt := GetText(Table.Footer);
      while Length(Txt) > 0 do
      begin
        Inc(H);
        LineFromLines(Txt);
      end;

      for i := 1 to H do
        WriteToStream(AdjustedText(Table.Footer, ColWidthTotal, i) + LineEnding);
    end;


  WriteToStream(LineEnding);
end;

procedure TOutputGeneratorTXT.TextOut(const S: UTF8String;
  TextFormat: TTextFormat);
begin
  FText += S;
end;

function TOutputGeneratorTXT.GetText(IText: IOutputText): UTF8String;
begin
  FText := '';
  ProcessIText(IText);
  Result := FText;
end;

end.

