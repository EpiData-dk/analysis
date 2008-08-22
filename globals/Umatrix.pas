unit UMatrix;

interface

uses
  SysUtils, windows, Classes;

type
  { Array of Double }
  PDoubleArray = ^TDoubleArray;
  TDoubleArray = array [0..255] of Double;
  { Array of pointers }
  TPtrArray = array [0..255] of PDoubleArray;

const
  { Size of Double }
  szDouble = SizeOf(Double);

type
  EMatrixException = class(Exception);

  TMatrix = class;

  { TMatrix - the table RowCount x ColCount  }
  TMatrix = class(TPersistent)
  private
    FRows: TPtrArray;
    FColCount: Longint;
    FRowCount: Longint;
    function  GetCells(ARow, ACol: Integer): Double;
    procedure SetCells(ARow, ACol: Integer; Value: Double);
    procedure SetColCount(Value: Longint);
    procedure SetRowCount  (Value: Longint);
  public
    constructor Create(Arow:integer=3;Acol:integer=3);
    destructor  Destroy;                      override;
    procedure   Assign(Source: TPersistent);  override;
    procedure   AssignValue(Value: Double);
    procedure   AssignToLine(NLine: Byte; Values: Array of Double);
    procedure   AssignToCol (NCol : Byte; Values: Array of Double);
    procedure   AddMatrix(AMatrix: TMatrix);
    procedure   MultMatrix(AMatrix: TMatrix);
    procedure   MultValue(Value: Double);
    procedure   DiagMatrix(Value: Double);
    function    Determinant: Double;
    procedure   Inverse;
    procedure   Transpose;
    procedure   ChangeLines(i, j: Integer);
    procedure   AddLines(i, j: integer; Factor: Double);
    procedure   MultLine(i: Integer; Factor: Double);
    property    Cells[ARow, ACol: Integer]: Double read GetCells write SetCells;default;
  published
    property ColCount: Longint read FColCount write SetColCount;
    property RowCount: Longint read FRowCount write SetRowCount;
  end;


function M_Inverse(AMatrix: TMatrix): TMatrix;
function M_SolveLinearSystem(A, B: TMatrix): TMatrix;
function M_MultMatrix(A, B: TMatrix): TMatrix;
function M_AddMatrix(A, B: TMatrix): TMatrix;
function M_MultValue(A: TMatrix; Value: Double): TMatrix;



implementation


function SolveMatrix(AMatrix: TMatrix; var BMatrix: TMatrix): Double;
var
  i, j: Integer;
  Temp: Double;
  Factor: Integer;
begin
  Result := 1;
  for i := 0 to AMatrix.RowCount - 1 do
  begin
    j := 0;
    while j < AMatrix.RowCount do
      if AMatrix.Cells[i, j] <> 0 then break
                                  else inc(j);
    if j = AMatrix.RowCount then
    begin
      Result := 0;
      Exit;
    end;
    AMatrix.ChangeLines(i, j);
    BMatrix.ChangeLines(i, j);
    if not Odd(AMatrix.ColCount) then Result := -1 * Result;
    for j := i + 1 to AMatrix.RowCount - 1 do
    begin
      Temp := AMatrix.Cells[j, i]/AMatrix.Cells[i, i];
      AMatrix.AddLines(j, i, -Temp);
      BMatrix.AddLines(j, i, -Temp);
    end;
  end;

  for i := AMatrix.RowCount - 1 downto 0 do
    for j := i - 1 downto 0 do
    begin
      Temp := AMatrix.Cells[j, i]/AMatrix.Cells[i, i];
      AMatrix.AddLines(j, i, -Temp);
      BMatrix.AddLines(j, i, -Temp);
    end;

  for i := 0 to AMatrix.RowCount - 1 do
  begin
    Result := Result * AMatrix.Cells[i, i];
    Temp := 1/AMatrix.Cells[i, i];
    BMatrix.MultLine(i, Temp);
    AMatrix.MultLine(i, Temp);
  end;
end;

function M_Inverse(AMatrix: TMatrix): TMatrix;
var
  A: TMatrix;
begin
  if AMatrix.FRowCount <> AMatrix.FColCount
    then raise EMatrixException.Create('This operation can be applied only to' +
                                       ' square matrix');
  A := TMatrix.Create;
  A.Assign(AMatrix);

  Result := TMatrix.Create;
  Result.RowCount := AMatrix.RowCount;
  Result.ColCount := AMatrix.ColCount;
  Result.DiagMatrix(1);

  if SolveMatrix(A, Result) = 0 then
  begin
    raise EMatrixException.Create('Could not inverse this matrix');
    Result := nil;
  end;
end;

function M_SolveLinearSystem(A, B: TMatrix): TMatrix;
var
  C: TMatrix;
begin
  if (A.FRowCount <> A.FColCount) or (B.RowCount <> A.RowCount) or (B.ColCount <> 1)
    then raise EMatrixException.Create('It is not a linear system');

  C := TMatrix.Create;
  C.Assign(A);

  Result := TMatrix.Create;
  Result.Assign(B);

  if SolveMatrix(C, Result) = 0 then
  begin
    raise EMatrixException.Create('Could not solve this system');
    Result := nil;
  end;

end;

function M_MultMatrix(A, B: TMatrix): TMatrix;
begin
  Result := TMatrix.Create;
  Result.Assign(A);
  Result.MultMatrix(B);
end;

function M_AddMatrix(A, B: TMatrix): TMatrix;
begin
  Result := TMatrix.Create;
  Result.Assign(A);
  Result.AddMatrix(B);
end;

function M_MultValue(A: TMatrix; Value: Double): TMatrix;
begin
  Result := TMatrix.Create;
  Result.Assign(A);
  Result.MultValue(Value);
end;

{ -- TMatrix -- }
constructor TMatrix.Create(Arow:integer=3;Acol:integer=3);
var
  i: integer;
begin
  inherited Create;
  RowCount := Arow;
  ColCount := Acol;
end;

destructor TMatrix.Destroy;
var
  I: Integer;
begin
  for i := 0 to FRowCount - 1 do
    FreeMem(FRows[i], szDouble * FColCount);
  inherited Destroy;
end;

procedure TMatrix.Assign(Source: TPersistent);
var
  i, j: Byte;
begin
  RowCount := (Source as TMatrix).RowCount;
  ColCount := (Source as TMatrix).ColCount;
  for i := 0 to FRowCount - 1 do
    for j := 0 to FColCount - 1 do
      FRows[i]^[j] := (Source as TMatrix).Cells[i, j];
end;

procedure TMatrix.AssignValue(Value: Double);
var
  i, j: Integer;
begin
  for i := 0 to FRowCount - 1 do
    for j := 0 to FColCount - 1 do
      FRows[i]^[j] := Value;
end;

procedure TMatrix.AssignToLine(NLine: Byte; Values: Array of Double);
var
  j, Min: Integer;
begin
  if NLine >= FRowCount then exit;
  if FColCount > High(Values)
    then Min := High(Values)
    else Min := FColCount - 1;
  for j := 0 to Min do
    FRows[NLine]^[j] := Values[j];
end;

procedure TMatrix.AssignToCol(NCol: Byte; Values: Array of Double);
var
  j, Min: Integer;
begin
  if NCol >= FColCount then exit;
  if FRowCount > High(Values)
    then Min := High(Values)
    else Min := FRowCount - 1;
  for j := 0 to Min do
    FRows[j]^[NCol] := Values[j];
end;

procedure TMatrix.AddMatrix(AMatrix: TMatrix);
var
  i, j: Integer;
begin
  if (ColCount <> AMatrix.ColCount) or (RowCount <> AMatrix.RowCount)
    then raise EMatrixException.Create('Can not add these matrixes');
  for I := 0 to FRowCount - 1 do
    for j := 0 to FColCount - 1 do
      FRows[i]^[j] := FRows[i]^[j] + AMatrix.Cells[i, j];
end;

procedure TMatrix.MultMatrix(AMatrix: TMatrix);
var
  A: TMatrix;
  i, j, k: Integer;
begin
  if ColCount <> AMatrix.RowCount
    then raise EMatrixException.Create('Can not multiply these matrixes');
  A := TMatrix.Create;
  A.RowCount := RowCount;
  A.ColCount := AMatrix.ColCount;
  for k := 0 to RowCount - 1 do
    for i := 0 to AMatrix.ColCount - 1 do
      for j := 0 to ColCount - 1 do
        A.Cells[k, i] := A.Cells[k, i] + Cells[k, j] * AMatrix.Cells[j, i];
  Assign(A);
  A.Free;
end;

procedure TMatrix.MultValue(Value: Double);
var
  i, j: Byte;
begin
  for i := 0 to FRowCount - 1 do
    for j := 0 to FColCount - 1 do
      FRows[i]^[j] := FRows[i]^[j] * Value;
end;

procedure TMatrix.DiagMatrix(Value: Double);
var
  i: Integer;
begin
  if FRowCount <> FColCount
    then raise EMatrixException.Create('This operation can be applied only to' +
                                       ' square matrix');
  AssignValue(0);
  for i := 0 to FRowCount - 1 do
    FRows[i]^[i] := Value;
end;

function TMatrix.Determinant: Double;
var
  A, B: TMatrix;
begin
  if FRowCount <> FColCount
    then raise EMatrixException.Create('This operation can be applied only to' +
                                       ' square matrix');
  A := TMatrix.Create;
  A.Assign(Self);

  B := TMatrix.Create;
  B.RowCount := RowCount;
  B.ColCount := ColCount;
  B.DiagMatrix(1);

  Result := SolveMatrix(A, B);
end;

procedure TMatrix.Inverse;
var
  A: TMatrix;
begin
  if FRowCount <> FColCount
    then raise EMatrixException.Create('This operation can be applied only to' +
                                       ' square matrix');
  A := TMatrix.Create;
  A.RowCount := RowCount;
  A.ColCount := ColCount;
  A.DiagMatrix(1);

  if SolveMatrix(Self, A) = 0 then
  begin
    raise EMatrixException.Create('Could not inverse this matrix');
    Exit;
  end;
  Assign(A);
end;

procedure TMatrix.Transpose;
var
  T: Double;
  i, j: Integer;
begin
  if FRowCount <> FColCount
    then raise EMatrixException.Create('This operation can be applied only to' +
                                       ' square matrix');
  for i := 0 to FRowCount - 1 do
    for j := i + 1 to FColCount - 1 do
    begin
      T := FRows[i]^[j];
      FRows[i]^[j] := FRows[j]^[i];
      FRows[j]^[i] := T;
    end;
end;

function  TMatrix.GetCells(ARow, ACol: Integer): Double;
begin
  { if index is invalid then raise exception }
  if (ACol < 0) or (ACol > FColCount - 1) or
     (ARow < 0) or (ARow > FRowCount   - 1)
  then  raise EMatrixException.Create('Index out of bounds SKunit Cells');
  Result := FRows[ARow]^[ACol];
end;

procedure TMatrix.SetCells(ARow, ACol: Integer; Value: Double);
begin
  { if index is invalid then raise exception }
  if (ACol < 0) or (ACol > FColCount - 1) or
     (ARow < 0) or (ARow > FRowCount - 1)
  then  raise EMatrixException.Create('Index out of bounds SKunit ECell');
  FRows[ARow]^[ACol] := Value;
end;

procedure TMatrix.SetColCount(Value: Longint);
var
  OldColCount: Longint;
  i: Integer;
begin
  if FColCount <> Value then begin
    { ColCount >= 1 }
    if Value < 1 then Value := 1;
    OldColCount := FColCount;
    { reallocate memory }
    { if OldColCount < Value then new elements will be equal to 0 }
    { if OldColCount > Value then elements will be truncate }
    for i := 0 to FRowCount - 1 do
      ReAllocMem(FRows[i], szDouble * Value);
    { update FColCount }
    FColCount := Value;
  end
end;

procedure TMatrix.SetRowCount(Value: Longint);
var
  i: Integer;
begin
  if Value = FRowCount then Exit;
  { RowCount >= 1 }
  if Value < 1 then Value := 1;
  { if Value > RowCount, new RowCountw will be added }
  if Value > FRowCount then
    for i := FRowCount to Value - 1 do
      FRows[i] := AllocMem(szDouble * FColCount)
  { if Value < RowCount, unnessesary RowCount will be destroed }
  else if Value < FRowCount then
    for i := FRowCount - 1 downto Value do
      FreeMem(FRows[i], szDouble * FColCount);
  { update FRowCount }
  FRowCount := Value;
end;



procedure TMatrix.ChangeLines(i, j: Integer);
var
  k: Integer;
  Temp: Double;
begin
  for k := 0 to FColCount - 1 do
  begin
    Temp := FRows[i]^[k];
    FRows[i]^[k] := FRows[j]^[k];
    FRows[j]^[k] := Temp;
  end;
end;

procedure TMatrix.AddLines(i, j: integer; Factor: Double);
var
  k: Integer;
begin
  for k := 0 to FColCount - 1 do
    FRows[i]^[k] := FRows[i]^[k] + FRows[j]^[k] * Factor;
end;

procedure TMatrix.MultLine(i: Integer; Factor: Double);
var
  k: Integer;
begin
  for k := 0 to FColCount - 1 do
    FRows[i]^[k] := FRows[i]^[k] * Factor;
end;


end.
