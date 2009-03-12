unit USPCUtils;

interface

uses SysUtils, UVectors, classes, ansDataTypes, UVectorOp, UOutput, UCommands, Windows, UAnaToken,
     UVariables;

// ============================================================================
// Declaration of:
// Main methodes, functions and procedures.
// ============================================================================

type

  TArrayVariant = array of variant;
  TExcludeValueFunction = function(index: integer; df: TEpiDataframe): EpiFloat of object;
  TExcludeFunction = function(index: integer; df: TEpiDataframe): Boolean of object;

  TSPCUtils = class
  private
    //
    function GetCmd(): TCommand;
  protected
    // For internally available methods!
  public
    // For Externaly available methods.
    constructor Create();
    destructor Destroy; override;
    function LimitTest(LCLVec, UCLVec, CtrlVec: TEpiVector; var ResultVec: TEpiVector): Integer;
    function SigmaTest(SigmaVecLCL, SigmaVecUCL, CenterVec, CtrlVec: TEpiVector; Consecutive: integer; var ResultVec: TEpiVector): Integer;
    function RunsTest(CenterVec, CtrlVec: TEpiVector; ChartNo, BreakIndex: Integer;
      var Output: string; var OutsideLimits: integer): Integer;
    function ConsecutiveTest(CenterVec, CtrlVec: TEpiVector; var ResultVec: TEpiVector; Limit: Integer): Integer; overload;
    function TrendTest(CtrlVec: TEpiVector; var ResultVec: TEpiVector; Limit: Integer): Integer;
    function FindBreak(XVector: TEpiVector): TArrayVariant;
    function ExcludeInDataframe(CtrlVec: TEpiVector; ExcludeValueFunc: TExcludeValueFunction;
      ExcludeFunc: TExcludeFunction): boolean;
    property Cmd: TCommand read GetCmd;
  end;


var
  OSPCUtils: TSPCUtils;

implementation

uses UCmdProcessor, UDateUtils, Math, GeneralUtils, UGraph, Chart;

// ============================================================================
// Public methodes.
// ============================================================================
constructor TSPCUtils.Create();
begin
  //
end;

destructor TSPCUtils.Destroy();
begin
  //
end;

// LimitTest:
// Test for values in the Count vector to be outside the limits of
// to other vectors - here 'UCL' and 'LCL'
function TSPCUtils.LimitTest(LCLVec, UCLVec, CtrlVec: TEpiVector; var ResultVec: TEpiVector): Integer;
var
  i: Integer;
begin
  result := 0;
  if ResultVec = nil then
  begin
    ResultVec := TEpiFloatVector.Create('$TEST1', CtrlVec.Length);
    CtrlVec.Vectors.Add(ResultVec);
  end;
  for i:=1 to CtrlVec.Length do
  begin
    if ((not CtrlVec.IsMissing[i]) and
       (((CtrlVec.AsFloat[i] < LCLVec.AsFloat[i]) and (not LCLVec.IsMissing[i]))
       or
       ((CtrlVec.AsFloat[i] > UCLVec.AsFloat[i]) and (not UCLVec.IsMissing[i])))) then
    begin
      //situation;
      ResultVec.AsFloat[i] := CtrlVec.AsFloat[i];
      inc(Result);
    end;
  end;
end;

function TSPCUtils.SigmaTest(SigmaVecLCL, SigmaVecUCL, CenterVec, CtrlVec: TEpiVector; Consecutive: integer; var ResultVec: TEpiVector): Integer;
var
  i, j, Last, St, Count, Skipped: Integer;
  stop: boolean;
begin
  result := 0;
  if ResultVec = nil then
  begin
    ResultVec := TEpiFloatVector.Create('$TEST1', CtrlVec.Length);
    CtrlVec.Vectors.Add(ResultVec);
  end;
  // Inc consecutive count, to fit with the way function counts.
  Inc(Consecutive);


  // Below center.
  stop := false;
  count := 0;
  i := 1;
  last := 0;
  st := 0;
  Skipped := 0;
  while (i < CtrlVec.Length) do
  begin
    if CtrlVec.IsMissing[i] then
    begin
      inc(i);
      if St > 0 then
        Inc(Skipped);
      Continue;
    end;
    if SigmaVecLCL.IsMissing[i] then
    begin

    end;
    if CtrlVec.AsFloat[i] < SigmaVecLCL.AsFloat[i] then
    begin
      if St = 0 then St := i;
      Inc(count);
    end else begin
      if last = 0 then last := i
      else stop := ((I - Last) - Skipped) < Consecutive;
    end;
    if stop then
    begin
      last := 0;
      stop := false;
      if count >= (Consecutive - 1) then
      begin
        inc(result);
        for j := st to i-1 do
          ResultVec.AsFloat[j] := CtrlVec.AsFloat[j];
      end;
      count := 0;
      st := 0;
      Skipped := 0;
    end;
    Inc(i);
  end;

  // Above center
  stop := false;
  count := 0;
  i := 1;
  last := 0;
  Skipped := 0;
  while (i < CtrlVec.Length) do
  begin
    if CtrlVec.IsMissing[i] then
    begin
      inc(i);
      if St > 0 then
        Inc(Skipped);
      Continue;
    end;
    if CtrlVec.AsFloat[i] > SigmaVecUCL.AsFloat[i] then
    begin
      if St = 0 then St := i;
      Inc(count);
    end else begin
      if last = 0 then last := i
      else stop := (I - Last) < Consecutive;
    end;
    if stop then
    begin
      last := 0;
      stop := false;
      if count >= (Consecutive - 1) then
      begin
        inc(result);
        for j := st to i-1 do
          ResultVec.AsFloat[j] := CtrlVec.AsFloat[j];
      end;
      count := 0;
      st := 0;
      Skipped := 0;
    end;
    Inc(i);
  end;
end;


// RunsTest:
// Count the number of runs in the graph. A Run is defined as the
// number of series of points consecutive on the same side of the
// median. Values on the median are ignored
function TSPCUtils.RunsTest(CenterVec, CtrlVec: TEpiVector; ChartNo, BreakIndex: Integer;
  var Output: string; var OutsideLimits: integer): Integer;
const
  RunArLow: array[0..16] of integer = (4, 4, 5, 5, 6, 6, 6, 7, 7, 8, 8, 9, 9, 9,10,10,10);
  RunArHi: array[0..16] of integer = (11,12,12,13,13,14,15,15,16,16,17,17,18,19,19,20,21);
var
  i, j: integer;
  Last: EpiFloat;
  t, s: string;
begin
  result := 1;

  // avoid points starting on the median:
  j := 1;
  while CtrlVec.AsFloat[j] = CenterVec.AsFloat[1] do inc(j);
  last := CtrlVec.AsFloat[j] ;

  // now start counting runs from point j:
  for i:=j to CtrlVec.Length do
  begin
    if (CtrlVec.IsMissing[i]) then continue;
    if (CtrlVec.AsFloat[i] = CenterVec.AsFloat[i]) then continue;
    if (CtrlVec.AsFloat[i] < CenterVec.AsFloat[i]) and
       (Last < CenterVec.AsFloat[i]) then continue
    else
    if (CtrlVec.AsFloat[i] > CenterVec.AsFloat[i]) and
       (Last > CenterVec.AsFloat[i]) then continue;
    if (CtrlVec.AsFloat[i] = CenterVec.AsFloat[i]) then
    begin
      last := CenterVec.AsFloat[i];
      continue;
    end;
    Inc(Result);
    Last := CtrlVec.AsFloat[i];
  end;

  if (CtrlVec.Length<14) or (CtrlVec.Length>30) then
    s := 'na'
  else begin
    s := IntToStr(RunArLow[CtrlVec.Length-14]);
    t := IntToStr(RunArHi[CtrlVec.Length-14]);
    if (Result > RunArHi[CtrlVec.Length-14]) or (Result < RunArLow[CtrlVec.Length-14]) then
      OutsideLimits := 1
    else
      OutsideLimits := 0;
  end;
  if (s <> 'na') then
    Output := '('+ s + '-' + t + ')'
  else
    Output := '';

  dm.AddResult('$SPC' + IntToStr(ChartNo) + 'B' + IntToStr(BreakIndex) +
               'RUNS', EpiTyFloat, Result, 0, 0);
  dm.AddResult('$SPC' + IntToStr(ChartNo) + 'B' + IntToStr(BreakIndex) +
               'RUNEL', EpiTyFloat, S, 0, 0);
  dm.AddResult('$SPC' + IntToStr(ChartNo) + 'B' + IntToStr(BreakIndex) +
               'RUNEH', EpiTyFloat, T, 0, 0);
end;

// ConsecutiveTest:
// Test for consecusive values, from the Control vector, on the same side
// of the centerline.
function TSPCUtils.ConsecutiveTest(CenterVec, CtrlVec: TEpiVector; var ResultVec: TEpiVector; Limit: Integer): Integer;
var
  i, j, Count: Integer;
  Last: EpiFloat;
begin
  result := 0;
  if ResultVec = nil then
  begin
    ResultVec := TEpiFloatVector.Create('$TEST2', CtrlVec.Length);
    CtrlVec.Vectors.Add(ResultVec);
  end;
  Last := CenterVec.AsFloat[1];
  j := 1;
  Count := 1;
  for i:=1 to CtrlVec.Length do
  begin
    if (CtrlVec.IsMissing[i]) then continue;
    if (CtrlVec.AsFloat[i] < CenterVec.AsFloat[i]) and
       (Last < CenterVec.AsFloat[i]) then inc(Count)
    else if (CtrlVec.AsFloat[i] > CenterVec.AsFloat[i]) and
       (Last > CenterVec.AsFloat[i]) then inc(Count)
    else if (CtrlVec.AsFloat[i] = CenterVec.AsFloat[i]) then
      continue
    else begin
      // Now we are no longer on the same side of median/mean... maybe we have
      // a situation.
      if Count >= Limit then
      begin
        //Situation;
        while j <= i-1 do
        begin
          if not (CtrlVec.AsFloat[j] = CenterVec.AsFloat[i-1]) then
            ResultVec.AsFloat[j] := CtrlVec.AsFloat[j];
          inc(j);
        end;
        inc(Result);
      end;
      Count := 1;
      j := i;
    end;
    Last := CtrlVec.AsFloat[i];
  end;
  // It might have been that this series was the last in the dataset.
  if Count >= Limit then
  begin
    while j <= i-1 do
    begin
      if not (CtrlVec.AsFloat[j] = CenterVec.AsFloat[i-1]) then
        ResultVec.AsFloat[j] := CtrlVec.AsFloat[j];
      inc(j);
    end;
    inc(Result);
  end;
end;

// TrendTest:
// A trend test, to see if the Count Vector has a series of either rising
// or falling values.
function TSPCUtils.TrendTest(CtrlVec: TEpiVector; var ResultVec: TEpiVector; Limit: Integer): Integer;
type
  TTrendDirection = (tdUp, tdDown, tdSame);
var
  FollowTrend, CurTrend: TTrendDirection;
  i, j, Start, Count: Integer;
  Last: EpiFloat;

  function GetDirection(Val1, Val2: EpiFloat): TTrendDirection;
  begin
    if Val1 < Val2 then result := tdUp
    else if Val1 = Val2 then result := tdSame
    else result := tdDown;
  end;

begin
  if ResultVec = nil then
  begin
    ResultVec := TEpiFloatVector.Create('$TEST3', CtrlVec.Length);
    CtrlVec.Vectors.Add(ResultVec);
  end;
    
  result := 0;
  j := 1;
  While (j < CtrlVec.Length) and
    (GetDirection(CtrlVec.AsFloat[j], CtrlVec.AsFloat[j+1]) = tdSame) do inc(j);
  FollowTrend := GetDirection(CtrlVec.AsFloat[j], CtrlVec.AsFloat[j+1]);
  Start := j;
  inc(j);
  Last := CtrlVec.AsFloat[j];
  Count := 2;
  for i := j+1 to CtrlVec.Length do
  begin
    if (CtrlVec.IsMissing[i]) then continue;
    CurTrend := GetDirection(Last, CtrlVec.AsFloat[i]);
    if CurTrend = tdSame then continue;
    if CurTrend = FollowTrend then
      Inc(Count)
    else begin
      if Count >= Limit then
      begin
        //Situation;
        while (Start <= i-1) do
        begin
          ResultVec.AsFloat[Start] := CtrlVec.AsFloat[Start];
          inc(Start);
        end;
        inc(Result);
      end;
      Count := 2;
      Start := i-1;
    end;
    FollowTrend := CurTrend;
    Last := CtrlVec.AsFloat[i];
  end;
  // It might have been that this series was the last in the dataset.
  if Count >= Limit then
  begin
    //Situation;
    while (Start <= i-1) do
    begin
      ResultVec.AsFloat[Start] := CtrlVec.AsFloat[Start];
      inc(Start);
    end;
    inc(Result);
  end;
end;

function TSPCUtils.FindBreak(XVector: TEpiVector): TArrayVariant;
var
  Params: TArrayVariant;
  i,j, res : integer;
  dfFormat: TEpiDateFormat;

  procedure ArraySort(var List: array of variant; L, R: Integer);
  var
     I, J, P: Integer;
     Temp: Variant;
  begin
     I:=L;
     J:=R;
     P:=(L + R) shr 1;
     repeat
       while List[I] < List[P] do Inc(I);
       while List[J] > List[P] do Dec(J);
       if I <= J then
       begin
         // Swapping values
          Temp := List[I];
          List[I] := List[J];
          List[J] := Temp;
          if P=I then P:=J
          else if P=J then P:=I;
          Inc(I);
          Dec(J);
       end;
     until I>J;
     if L<J then ArraySort(List, L,J);
     if I < R then ArraySort(List, I, R);
  end;

begin
  i := 0;
  for j := 0 to Cmd.ParameterCount-1 do
  begin
    if not ((Cmd.Param[j].VarName = 'BREAK') or (Cmd.Param[j].VarName = 'B')) then
      continue;
    SetLength(Params, i+1);
    case XVector.DataType of
      EpiTyDate:
        begin
          dfFormat := DateFmtToEpiDateFmt(XVector.FieldDataFormat);
          EpiStrToDate(Cmd.Param[j].AsString, res, dfFormat);
          Params[i] := res;
        end;
      EpiTyInteger:
        Params[i] := StrToInt(Cmd.Param[j].AsString);
    else
      Params[i] := Cmd.Param[j].Value;
    end;
    inc(i);
  end;
  if i>0 then
    ArraySort(Params, 0, i-1);
  result := params;
end;

function TSPCUtils.ExcludeInDataframe(CtrlVec: TEpiVector; ExcludeValueFunc: TExcludeValueFunction;
  ExcludeFunc: TExcludeFunction): boolean;
var
  ExclVector: TEpiVector;
  ExclVar: TAnaVariableDescriptor;
  ExpParams: array of integer;
  ExvValue, ExcludeValue: EpiFloat;
  ExvZero : Boolean;
  i, j, Res: integer;
  Df: TEpiDataframe;


  function InExpList(no: integer; Params: array of integer): boolean;
  var
    i: integer;
  begin
    result := false;
    for i := 0 to high(params) do
      if params[i] = no then
        result := true;
  end;

begin
  result := false;
  df := CtrlVec.Vectors.DataFrame;
  // Checking for EXP option(s).
  SetLength(ExpParams, 0);
  i := 0;
  For j := 0 to Cmd.ParameterCount -1 do
  begin
    if Cmd.Param[j].VarName <> 'EXP' then continue;
    SetLength(ExpParams, i+1);
    ExpParams[i] := Cmd.Param[j].AsInteger;
    inc(i);
  end;

  // Checking for EXV option(s).
  ExvValue := MaxExtended;
  if (Cmd.ParamExists['EXV']) then
    ExvValue := Cmd.ParamByName['EXV'].AsFloat;

  if (Cmd.ParamExists['EXZ']) then
    ExvZero := True else ExvZero := False;

  ExclVector := TEpiFloatVector.Create('$EXCLUDED', df.RowCount);
  ExclVector.FieldDataSize := 12;
  ExclVector.FieldDataDecimals := 15;
  Df.Vectors.Add(ExclVector);

  for i := 1 to df.RowCount do
  begin
    ExcludeValue := ExcludeValueFunc(i, df);
    ExclVector.AsFloat[i] := ExcludeValue;
    if (ExcludeValue >= ExvValue) or
       (InExpList(i, ExpParams)) or
       (ExvZero and (ExcludeValue = 0.0 )) or
       (ExcludeFunc(i, df)) then
    begin
      CtrlVec.IsMissing[i] := true;
      Result := True;
    end;
  end;
end;

function TSPCUtils.GetCmd: TCommand;
begin
  result := OGraph.Cmd;
end;

end.
