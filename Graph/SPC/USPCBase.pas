unit USPCBase;

interface

uses
  Classes, UChartArray, UVectors, UOutput, AnsDatatypes, Series, Chart, UBaseChart;

type

  TCustomSPCChartClass = class of TCustomSPCChart;
  TSPCLine = (slNormal, slExcluded, slCenter, slSigma);

  TCustomSPCChart = class(TCustomChart)
  private
    fLVec: TEpiVector;
    fVarNames: TStrings;
    fExcluded: Boolean;
    fFrozen: Boolean;
    function SigmaFactor(SigmaLvl, Count: integer): EpiFloat;
    procedure AddToSigmaLine(SigmaLine: TLineSeries; SigmaVec: TEpiVector;
      Index: Integer; var ShowSigma: boolean; XPosOffset: Extended = 0.0); overload;
    procedure AddToSigmaLine(SigmaLine: TLineSeries; SigmaVal: EpiFloat;
      Index: Integer; var ShowSigma: boolean; XPosOffset: Extended = 0.0); overload;
    procedure AddToLine(Line: TCustomSeries; YVec: TEpiVector;
      Index: Integer; XPosOffset: Extended = 0.0); overload;
    procedure AddToLine(Line: TCustomSeries; YVal: EpiFloat;
      Index: Integer; XPosOffset: Extended = 0.0); overload;
    procedure AddNull(Line: TCustomSeries; YVec: TEpiVector; Index: Integer;
      XPosOffset: Extended = 0.0); overload;
    procedure AddNull(Line: TCustomSeries; YVal: EpiFloat; Index: Integer;
      XPosOffset: Extended = 0.0); overload;
    procedure AddTestResult(ChartNo, TestNo, BreakIndex: Integer; Value: EpiFloat);
    procedure SetVectorValue(Vec: TEpiVector; Index: Integer;
      FrzVec: TEpiVector; NormalVal: EpiFloat; SpcLine: TSPCLine);
    procedure NilVectorArrays(ArraySize: integer);
    procedure CommonSeriesCreate(Series: TCustomSeries;
      Title: string; LineCode: integer; ParentChart: TChart);
    function CreatePoint(Title: string; LineCode: integer;
      ParentChart: TChart): TPointSeries;
    function CreateLine(Title: string; LineCode: integer;
      ParentChart: TChart): TLineSeries;
    function CreateFreezeLine(OrgLine: TLineSeries;
      LineCode: integer): TLineSeries;
    procedure ReAssignToLengend(Series: TCustomSeries);
    function CreateVector(VectorName: string; Size: Integer): TEpiVector;
    function DoAllowFreeze(SpcLine: TSPCLine): boolean;
    function LimitValue(OptionName: string; TestNo: Integer; TestList: TStrings): Integer;
    function GetSpcTestList(): TStrings;
  protected
    df: TEpiDataFrame;
    CtrlVec, ExcludeVec,
    CenterVec,
    Sigma1LCLVec, Sigma1UCLVec,
    Sigma2LCLVec, Sigma2UCLVec,
    Sigma3LCLVec, Sigma3UCLVec: Array of TEpiVector;
    // Freeze Vectors:
    CenterFrzVec,
    Sigma1FrzLCLVec, Sigma1FrzUCLVec,
    Sigma2FrzLCLVec, Sigma2FrzUCLVec,
    Sigma3FrzLCLVec, Sigma3FrzUCLVec: Array of TEpiVector;
    // If pre aggregation needed override else leave as is.
    function PreAggregate(const Dataframe: TEpiDataframe): TEpiDataframe; virtual;
    // Vector to set missing on excluding. Default to Varnames[0] - override if needed;
    function GetExclusionVector(const Dataframe: TEpiDataframe): TEpiVector; virtual; 
    // Allow SPC to calculate freeze values for line - defaults is yes!
    function AllowFreeze(SpcLine: TSPCLine): Boolean; virtual;
    // Checks if number of recieved varnames is correct. Abort if not!
    procedure CheckVarnames(); virtual; abstract;
    // Create required charts for the graph, insert into ChartArray.
    procedure CreateCharts(ChartArray: TChartArray; const Dataframe: TEpiDataFrame); virtual; abstract;
    // Return a vector used for finding x-axis values for the whole chart.
    // Is used for preparing dataframe with breaks.
    function GetTimeVec(Dataframe: TEpiDataframe): TEpiVector; virtual; abstract;
    // Create the output table.
    function CreateOutputTable(): TStatTable; virtual; abstract;
    // Add varnames to the two lists used for including vectors during prepare dataframe
    // for each break period.
    procedure PrepareVarnames(IncludeVarnames, MissingVarnames: TStrings); virtual; abstract;
    // Exclude function used for excluding values.
    function ExcludeValueFunction(index: integer; df: TEpiDataframe): EpiFloat; virtual; abstract;
    function ExcludeFunction(index: integer; df: TEpiDataframe): Boolean; virtual;
    function GetCenterText(ChartNo: integer): string; virtual; abstract;
    function GetChartTypeText(ChartNo: integer): string; virtual; abstract;
    function GetCtrlText(const Dataframe: TEpiDataFrame; const ChartNo: integer): string; virtual; abstract;
    //
    procedure CleanupOutput(OutputTable: TStatTable); virtual; abstract;
    // =========================================================================
    // Functions called during a break period:
    // =========================================================================
    // Return the x-axis vector.  (Used in all SPC)
    function GetXVector(): TEpiVector; virtual; abstract;
    // Return the y-axis vector.  (Used in all SPC)
    function GetYVector(): TEpiVector; virtual; abstract;
    // Return the z-axis vector.  (Used in SPC with 2+ variables)
    function GetZVector(): TEpiVector; virtual; abstract;
    // Return the vector holding the original value before exclusion (ie. exclusion vector)
    // Default always return '$EXCLUDED' from Df. 
    function GetExcludedVector(ChartNo: Integer): TEpiVector; virtual;
    // Pre, during and post "mean" calculation functions
    procedure ResetMean(); virtual; abstract;
    procedure ExecuteMeanSuccess(LoopIdx, LastIdx: integer); virtual; abstract;
    procedure ExecuteMeanFail(LoopIdx, LastIdx: integer); virtual; abstract;
    procedure CalcMean(); virtual; abstract;
    // Calculate sigma value for control lines.
    function GetSigma(LoopIndex, ChartNo: integer): EpiFloat; virtual; abstract;
    // Calculate Center value.
    function GetCenter(LoopIndex, ChartNo: integer): EpiFloat; virtual; abstract;
    // Calculate Ctrl value.
    function GetCtrlVal(LoopIndex, ChartNo: integer): EpiFloat; virtual; abstract;
    // Calculate corresponding excluded value. Default is the default exlusion vector;
    function GetExclVal(LoopIndex, ChartNo: integer): EpiFloat; virtual;
    function MakeOutputLine(ChartNo: integer; OutputTable: TStatTable): EpiFloat; virtual; abstract;
    // Writes SigmaLCL, SigmaUCL and Center values to result variables.
    // Override with empty procedures to avoid results.
    procedure SigmaResults(ChartNo, SigmaNo, BreakIndex: Integer); virtual;
    procedure CenterResults(ChartNo, BreakIndex: Integer); virtual;
    // Text to be put as footnote on graphs
    function GetLCLInfoTxt(ChartNo: integer): string; virtual;
    function GetUCLInfoTxt(ChartNo: integer): string; virtual;
    function GetCenterInfoTxt(ChartNo: integer): string; virtual;
    // ===========================
    // End.
    // ===========================
    property ZVec: TEpiVector read GetZVector;
    property YVec: TEpiVector read GetYVector;
    property XVec: TEpiVector read GetXVector;
    property LVec: TEpiVector read fLVec;
    property Sigma[LoopIndex, ChartNo: integer]: EpiFloat read GetSigma;
    property Center[LoopIndex, ChartNo: integer]: EpiFloat read GetCenter;
    property CtrlVal[LoopIndex, ChartNo: integer]: EpiFloat read GetCtrlVal;
    property ExclVal[LoopIndex, ChartNo: integer]: EpiFloat read GetExclVal;
    property CenterText[ChartNo: integer]: string read GetCenterText;
    property ChartTypeText[ChartNo: integer]: string read GetChartTypeText;
    property VarNames: TStrings read fVarNames;
    property Excluded: Boolean read fExcluded;
    property Frozen: Boolean read fFrozen;
  public
    constructor Create; override;
    destructor Destroy; override;
    function DoSPCChart(aDataframe: TEpiDataframe; aVarnames: TStrings; var OutputTable: TStatTable): TChartArray;
  published

  end;


implementation

uses
  UCmdProcessor, UDebug, USPCUtils, Graphics, UGraph, TeEngine,
  GeneralUtils, SysUtils, UCommands, UDateUtils, TeCanvas, UGraphUtils,
  UAnaToken;

const
  UnitName = 'USPCBase';
  TestCount = 5;
  
{ TCustomSPCChart }

procedure TCustomSPCChart.AddNull(Line: TCustomSeries; YVec: TEpiVector;
  Index: Integer; XPosOffset: Extended = 0.0);
begin
  if YVec.IsMissing[Index] then exit;
  AddNull(Line, YVec.AsFloat[Index], Index, XPosOffset);
end;

procedure TCustomSPCChart.AddNull(Line: TCustomSeries; YVal: EpiFloat;
  Index: Integer; XPosOffset: Extended = 0.0);
begin
  if (YVal < 0) and (not Cmd.ParamExists['NEGLCL']) then exit;
  if XPosOffset = 0.0 then
    Line.AddNullXY(XVec.AsFloat[Index] + XPosOffset, YVal,
               LVec.GetValueLabel(LVec.AsString[Index], Cmd.ParameterList))
  else
    Line.AddNullXY(XVec.AsFloat[Index] + XPosOffset, YVal);
end;

procedure TCustomSPCChart.AddTestResult(ChartNo, TestNo, BreakIndex: Integer;
  Value: EpiFloat);
begin
  dm.AddResult('$SPC' + IntToStr(ChartNo) + 'B' + IntToStr(BreakIndex)  +
               'TEST' + IntToStr(TestNo), EpiTyFloat, Value, 0, 0);
end;

procedure TCustomSPCChart.AddToLine(Line: TCustomSeries; YVec: TEpiVector;
  Index: Integer; XPosOffset: Extended = 0.0);
begin
  AddToLine(Line, YVec.AsFloat[Index], Index, XPosOffset);
end;

procedure TCustomSPCChart.AddToLine(Line: TCustomSeries; YVal: EpiFloat;
  Index: Integer; XPosOffset: Extended = 0.0);
begin
  if XPosOffset = 0.0 then
    Line.AddXY(XVec.AsFloat[Index] + XPosOffset, YVal,
               LVec.GetValueLabel(LVec.AsString[Index], Cmd.ParameterList))
  else
    Line.AddXY(XVec.AsFloat[Index] + XPosOffset, YVal);
end;

procedure TCustomSPCChart.AddToSigmaLine(SigmaLine: TLineSeries;
  SigmaVec: TEpiVector; Index: Integer; var ShowSigma: boolean;
  XPosOffset: Extended = 0.0);
begin
  if SigmaVec.IsMissing[Index] then Exit;
  if (SigmaVec.AsFloat[Index] < 0) and (not Cmd.ParamExists['NEGLCL']) then
  begin
    if (Index > 1) and (not SigmaVec.IsMissing[Index-1]) then
      AddToLine(SigmaLine, SigmaVec.AsFloat[Index-1], Index, XPosOffset);
    AddNull(SigmaLine, -SigmaVec.AsFloat[Index], Index, XPosOffset);
    SigmaVec.IsMissing[Index] := True;
  end else begin
    AddToLine(SigmaLine, SigmaVec, Index, XPosOffset);
    ShowSigma := true;
  end;
end;

procedure TCustomSPCChart.AddToSigmaLine(SigmaLine: TLineSeries;
  SigmaVal: EpiFloat; Index: Integer; var ShowSigma: boolean;
  XPosOffset: Extended = 0.0);
begin
  if (SigmaVal < 0) and (not Cmd.ParamExists['NEGLCL']) then
//    AddNull(SigmaLine, -SigmaVal, Index)
  else begin
    AddToLine(SigmaLine, SigmaVal, Index, XPosOffset);
    ShowSigma := true;
  end;
end;


constructor TCustomSPCChart.Create;
const
  ProcName = 'Create';
  Version = '1.0.0.0';
begin
  ODebug.IncIndent();
  ODebug.Add(UnitName, ClassName, ProcName, Version, 3);
  try
    inherited Create();
    
  finally
    ODebug.DecIndent();
  end;
end;

destructor TCustomSPCChart.Destroy;
const
  ProcName = 'Destroy';
  Version = '1.0.0.0';
begin
  ODebug.IncIndent();
  ODebug.Add(UnitName, ClassName, ProcName, Version, 3);
  try
  
    inherited Destroy();
  finally
    ODebug.DecIndent();
  end;
end;

function TCustomSPCChart.DoSPCChart(aDataframe: TEpiDataframe;
  aVarnames: TStrings; var OutputTable: TStatTable): TChartArray;
var
  // Vectors:
  Dataframe: TEpiDataframe;
  TimeVec, TestVec, GlobExcludeVec: TEpiVector;
  // Graph series:
  CtrlLine, ExcludeLine: Array of TCustomSeries;
  CenterLine,
  Sigma1LCLLine, Sigma1UCLLine,
  Sigma2LCLLine, Sigma2UCLLine,
  Sigma3LCLLine, Sigma3UCLLine: Array of TLineSeries;
  CenterFrzLine,
  Sigma1FrzLCLLine, Sigma1FrzUCLLine,
  Sigma2FrzLCLLine, Sigma2FrzUCLLine,
  Sigma3FrzLCLLine, Sigma3FrzUCLLine: Array of TLineSeries;
  TestSeries: TCustomSeries;
  ChartText: Array of string;

  // Runchart only...
  str: string;
  v: integer;

  // Other:
  XDiff: EpiFloat;
  PrepNames, MissingNames: TStrings;
  Breaks: TArrayVariant;
  TestIdx, i, j, k, l, RowCount,
  Res, Offset: integer;
  LowVal, HighVal: Variant;
  dfFormat: TEpiDateFormat;
  ShowSigma1, ShowSigma2, ShowSigma3: Array of Boolean;
  Dummy: boolean;
  SpcTestList: TStrings;
const
  procname = 'DoSPCChart';
  procversion = '1.0.0.0';
begin
  ODebug.IncIndent();
  ODebug.Add(UnitName, ClassName, procname, procversion, 1);

  fVarNames := aVarNames;
  fFrozen := false;

  SpcTestList := GetSpcTestList();

  try
    CheckVarnames();

    // Create the graph and set customizable settings.
    result := TChartArray.Create();
    CreateCharts(result, aDataframe);


    Dataframe := PreAggregate(aDataFrame);

    // Breaks and excludes:
    TimeVec := GetTimeVec(Dataframe);
    Breaks := OSPCUtils.FindBreak(TimeVec);
    Dataframe.Sort(TimeVec.Name);

    fExcluded := OSPCUtils.ExcludeInDataframe(GetExclusionVector(Dataframe), ExcludeValueFunction, ExcludeFunction);

    dm.AddResult('$SPCBREAKS', EpiTyInteger, Length(Breaks), 0, 0);
    for i := 0 to Result.Count -1 do
    begin
      dm.AddResult('$SPC' + IntToStr(i+1) + 'TYPE', EpiTyString, ChartTypeText[i], 0, 0);
      dm.AddResult('$SPC' + IntToStr(i+1) + 'CENTERTYPE', EpiTyString, CenterText[i], 0, 0);
    end;

    // Output table:
    OutputTable := CreateOutputTable();
    TestIdx := OutputTable.ColCount + 1;
    for i := 1 to TestCount do
    begin
      str := Format('T%d', [i]);
      if Cmd.ParamExists[str] or (Cmd.ParamExists['T'] and (i <= 3))then
      begin
        OutputTable.AddColumn;
        OutputTable.Cell[OutputTable.ColCount, 1] := Format('Test %d:', [i]);
        if Cmd.ParamExists[str] and (Cmd.ParamByName[str].AsString <> '') then
          OutputTable.Footer := OutputTable.Footer + Format('Test %d = %s', [i, Cmd.ParamByName[str].AsString]);
      end;
      str := '';
    end;
    if TestIdx < OutputTable.ColCount + 1 then
      OutputTable.Footer := OutputTable.Footer + '<br>Tests list: ' + SpcTestList.CommaText;

    // Create Sigma lines - even if they should not be displayed.
    SetLength(Sigma1LCLLine, Result.Count);
    SetLength(Sigma1UCLLine, Result.Count);
    SetLength(Sigma2LCLLine, Result.Count);
    SetLength(Sigma2UCLLine, Result.Count);
    SetLength(Sigma3LCLLine, Result.Count);
    SetLength(Sigma3UCLLine, Result.Count);
    SetLength(ExcludeLine, Result.Count);
    SetLength(CenterLine, Result.Count);
    SetLength(CtrlLine, Result.Count);
    SetLength(ChartText, Result.Count);

    for i := 0 to Result.Count -1 do
    begin
      Sigma3UCLLine[i] := CreateLine('Sigma 3',     3, Result.Chart[i]);
      Sigma3LCLLine[i] := CreateLine('Sigma 3 LCL', 3, Result.Chart[i]);
      Sigma3LCLLine[i].ShowInLegend := false;
      Sigma2UCLLine[i] := CreateLine('Sigma 2',     4, Result.Chart[i]);
      Sigma2LCLLine[i] := CreateLine('Sigma 2 LCL', 4, Result.Chart[i]);
      Sigma2LCLLine[i].ShowInLegend := false;
      Sigma1UCLLine[i] := CreateLine('Sigma 1',     5, Result.Chart[i]);
      Sigma1LCLLine[i] := CreateLine('Sigma 1 LCL', 5, Result.Chart[i]);
      Sigma1LCLLine[i].ShowInLegend := false;

      CenterLine[i] := CreateLine(CenterText[i], 2, Result.Chart[i]);

      if Cmd.ParamExists['POINT'] then
        ExcludeLine[i] :=  CreatePoint('Excluded', 1, Result.Chart[i])
      else
        ExcludeLine[i] :=  CreateLine('Excluded', 1, Result.Chart[i]);
      ExcludeLine[i].LinePen.Style := psDot;
      ExcludeLine[i].Pointer.Visible := true;
      ExcludeLine[i].Pointer.Style := psCircle;
      ExcludeLine[i].Pointer.Color := clWhite;
      ExcludeLine[i].Pointer.Brush.Style := bsClear;
      ExcludeLine[i].Stairs := false;
      if not Excluded then
        ExcludeLine[i].ParentChart := nil;

      if Cmd.ParamExists['POINT'] then
        CtrlLine[i] := CreatePoint(GetCtrlText(Dataframe, i), 1, Result.Chart[i])
      else
        CtrlLine[i] := CreateLine(GetCtrlText(Dataframe, i), 1, Result.Chart[i]);
      CtrlLine[i].Pointer.Visible := true;
      CtrlLine[i].Pointer.Style := psCircle;
      CtrlLine[i].Pointer.Brush.Style := bsSolid;
      CtrlLine[i].Stairs := false;
      if (Cmd.ParamExists['YVALUE']) then
      with CtrlLine[i] do begin
        Marks.Visible := true;
        Marks.Style := smsValue;
        Marks.Color := clWhite;
      end;
    end;


    PrepNames := TStringList.Create();
    MissingNames := TStringList.Create();
    PrepareVarnames(PrepNames, MissingNames);

    // Set number of Vectors.
    SetLength(CtrlVec, Result.Count);
    SetLength(CenterVec, Result.Count);
    SetLength(ExcludeVec, Result.Count);
    SetLength(Sigma1LCLVec, Result.Count);
    SetLength(Sigma1UCLVec, Result.Count);
    SetLength(Sigma2LCLVec, Result.Count);
    SetLength(Sigma2UCLVec, Result.Count);
    SetLength(Sigma3LCLVec, Result.Count);
    SetLength(Sigma3UCLVec, Result.Count);
    // - number of freeze vectors
    SetLength(CenterFrzVec, Result.Count);
    SetLength(Sigma1FrzLCLVec, Result.Count);
    SetLength(Sigma1FrzUCLVec, Result.Count);
    SetLength(Sigma2FrzLCLVec, Result.Count);
    SetLength(Sigma2FrzUCLVec, Result.Count);
    SetLength(Sigma3FrzLCLVec, Result.Count);
    SetLength(Sigma3FrzUCLVec, Result.Count);
    NilVectorArrays(Result.Count);

    SetLength(ShowSigma1, Result.Count);
    SetLength(ShowSigma2, Result.Count);
    SetLength(ShowSigma3, Result.Count);
    for i := 0 to Result.Count - 1 do
    begin
      ShowSigma1[i] := false;
      ShowSigma2[i] := false;
      ShowSigma3[i] := false;
    end;

    // Calculate Sigma lvl's based on Freeze option.
    if DoAllowFreeze(slCenter) then
    begin
      // Documentation information.
      OutputTable.Footer := OutputTable.Footer +
        '<br>Estimation uses data from 1-' + Cmd.ParamByName['F'].AsString;

      LowVal := TimeVec.AsInteger[1] - 1;
      try
        case TimeVec.DataType of
          EpiTyDate:
            begin
              dfFormat := DateFmtToEpiDateFmt(TimeVec.FieldDataFormat);
              if EpiStrToDate(Cmd.ParamByName['F'].AsString, Res, dfFormat) then
                HighVal := Res
              else
                HighVal := TimeVec.AsInteger[Cmd.ParamByName['F'].AsInteger];
            end;
          EpiTyInteger, EpiTyFloat:
            HighVal := Cmd.ParamByName['F'].AsInteger;
        end;
      except
        Dm.Error('Invalid freeze point: %s', [Cmd.ParamByName['F'].AsString], 0);
      end;

      if (Length(Breaks) > 0) and (Breaks[0] < HighVal) then
        dm.Error('Freeze period cannot exceed first break.', []);

      SetLength(CenterFrzLine, Result.Count);
      SetLength(Sigma1FrzLCLLine, Result.Count);
      SetLength(Sigma1FrzUCLLine, Result.Count);
      SetLength(Sigma2FrzLCLLine, Result.Count);
      SetLength(Sigma2FrzUCLLine, Result.Count);
      SetLength(Sigma3FrzLCLLine, Result.Count);
      SetLength(Sigma3FrzUCLLine, Result.Count);

      for j := 1 to Dataframe.RowCount do
        if (TimeVec.AsInteger[j] > LowVal) and (TimeVec.AsInteger[j] <= HighVal) then
          Dataframe.Selected[j] := True
        else
          Dataframe.Selected[j] := False;
      Df := Dataframe.PrepareDataframe(PrepNames, MissingNames);

      RowCount := Df.SelectedRowCount;
      if RowCount < 2 then
        dm.Error('Freeze period too short', [], 0);

      // Find Mean or Median;
      offset := 1;
      ResetMean();
      while (Offset <= Df.RowCount) and YVec.IsMissing[Offset] do inc(Offset);
      if Offset > df.RowCount then
        dm.Error('Too few (aggregated) samples to calculate chart.', []);
      for i := Offset to df.RowCount do
      begin
        if (not YVec.IsMissing[i]) then
        begin
          ExecuteMeanSuccess(i, offset);
          Offset := i;
        end else
          ExecuteMeanFail(i, offset);
      end;
      CalcMean();

      if (Cmd.ParamExists['XLABEL']) then
        fLVec := df.VectorByName[Cmd.ParamByName['XLABEL'].AsString]
      else
        fLVec := XVec;

      for k := 0 to Result.Count - 1 do
      begin
        // Documentation information.
        ChartText[k] := ChartText[k] + ' Freeze: ' + Cmd.ParamByName['F'].AsString + ' |';

        // Create new sigma lines and convert old to dotted lines.
        Sigma3FrzUCLLine[k] := CreateFreezeLine(Sigma3UCLLine[k], 3);
        Sigma3FrzLCLLine[k] := CreateFreezeLine(Sigma3LCLLine[k], 3);
        Sigma3FrzLCLLine[k].ShowInLegend := false;
        Sigma2FrzUCLLine[k] := CreateFreezeLine(Sigma2UCLLine[k], 4);
        Sigma2FrzLCLLine[k] := CreateFreezeLine(Sigma2LCLLine[k], 4);
        Sigma2FrzLCLLine[k].ShowInLegend := false;
        Sigma1FrzUCLLine[k] := CreateFreezeLine(Sigma1UCLLine[k], 5);
        Sigma1FrzLCLLine[k] := CreateFreezeLine(Sigma1LCLLine[k], 5);
        Sigma1FrzLCLLine[k].ShowInLegend := false;
        CenterFrzLine[k] := CreateFreezeLine(CenterLine[k], 2);
        ReAssignToLengend(ExcludeLine[k]);
        ReAssignToLengend(CtrlLine[k]);

        // Create vectors:
        CenterFrzVec[k]    := CreateVector('$CENTERFRZ', df.RowCount);
        Sigma1FrzLCLVec[k] := CreateVector('$SIGMA1LCLFRZ', df.RowCount);
        Sigma1FrzUCLVec[k] := CreateVector('$SIGMA1UCLFRZ', df.RowCount);
        Sigma2FrzLCLVec[k] := CreateVector('$SIGMA2LCLFRZ', df.RowCount);
        Sigma2FrzUCLVec[k] := CreateVector('$SIGMA2UCLFRZ', df.RowCount);
        Sigma3FrzLCLVec[k] := CreateVector('$SIGMA3LCLFRZ', df.RowCount);
        Sigma3FrzUCLVec[k] := CreateVector('$SIGMA3UCLFRZ', df.RowCount);
{        df.Vectors.Add(CenterFrzVec[k]);
        df.Vectors.Add(Sigma1FrzLCLVec[k]);
        df.Vectors.Add(Sigma1FrzUCLVec[k]);
        df.Vectors.Add(Sigma2FrzLCLVec[k]);
        df.Vectors.Add(Sigma2FrzUCLVec[k]);
        df.Vectors.Add(Sigma3FrzLCLVec[k]);
        df.Vectors.Add(Sigma3FrzUCLVec[k]);    }

        for j := 1 to Df.RowCount do
        begin
          // Calculate values;
          CenterFrzVec[k].AsFloat[j] := Center[j,k];
          Sigma1FrzLCLVec[k].AsFloat[j] := CenterFrzVec[k].AsFloat[j] - SigmaFactor(1, Df.RowCount) * Sigma[j,k];
          Sigma1FrzUCLVec[k].AsFloat[j] := CenterFrzVec[k].AsFloat[j] + SigmaFactor(1, Df.RowCount) * Sigma[j,k];
          Sigma2FrzLCLVec[k].AsFloat[j] := CenterFrzVec[k].AsFloat[j] - SigmaFactor(2, Df.RowCount) * Sigma[j,k];
          Sigma2FrzUCLVec[k].AsFloat[j] := CenterFrzVec[k].AsFloat[j] + SigmaFactor(2, Df.RowCount) * Sigma[j,k];
          Sigma3FrzLCLVec[k].AsFloat[j] := CenterFrzVec[k].AsFloat[j] - SigmaFactor(3, Df.RowCount) * Sigma[j,k];
          Sigma3FrzUCLVec[k].AsFloat[j] := CenterFrzVec[k].AsFloat[j] + SigmaFactor(3, Df.RowCount) * Sigma[j,k];

        // Add value to fully drawn lines (within the freeze period)
          AddToLine(CenterFrzLine[k], CenterFrzVec[k], j, -0.5);
          AddToSigmaLine(Sigma1FrzLCLLine[k], Sigma1FrzLCLVec[k], j, ShowSigma1[k], -0.5);
          AddToSigmaLine(Sigma1FrzUCLLine[k], Sigma1FrzUCLVec[k], j, Dummy, -0.5);
          AddToSigmaLine(Sigma2FrzLCLLine[k], Sigma2FrzLCLVec[k], j, ShowSigma2[k], -0.5);
          AddToSigmaLine(Sigma2FrzUCLLine[k], Sigma2FrzUCLVec[k], j, Dummy, -0.5);
          AddToSigmaLine(Sigma3FrzLCLLine[k], Sigma3FrzLCLVec[k], j, ShowSigma3[k], -0.5);
          AddToSigmaLine(Sigma3FrzUCLLine[k], Sigma3FrzUCLVec[k], j, Dummy, -0.5);
        end;
        Dec(j);

        AddToLine(CenterFrzLine[k], CenterFrzVec[k], j, 0.5);
        if ShowSigma1[k] then
          AddToSigmaLine(Sigma1FrzLCLLine[k], Sigma1FrzLCLVec[k], j, ShowSigma1[k], 0.5);
        AddToSigmaLine(Sigma1FrzUCLLine[k], Sigma1FrzUCLVec[k], j, Dummy, 0.5);
        if ShowSigma2[k] then
          AddToSigmaLine(Sigma2FrzLCLLine[k], Sigma2FrzLCLVec[k], j, ShowSigma2[k], 0.5);
        AddToSigmaLine(Sigma2FrzUCLLine[k], Sigma2FrzUCLVec[k], j, Dummy, 0.5);
        if ShowSigma3[k] then
          AddToSigmaLine(Sigma3FrzLCLLine[k], Sigma3FrzLCLVec[k], j, ShowSigma3[k], 0.5);
        AddToSigmaLine(Sigma3FrzUCLLine[k], Sigma3FrzUCLVec[k], j, Dummy, 0.5);

        AddNull(CenterFrzLine[k], CenterFrzVec[k], j, 0.5);
        AddNull(Sigma1FrzLCLLine[k], Sigma1FrzLCLVec[k], j, 0.5);
        AddNull(Sigma1FrzUCLLine[k], Sigma1FrzUCLVec[k], j, 0.5);
        AddNull(Sigma2FrzLCLLine[k], Sigma2FrzLCLVec[k], j, 0.5);
        AddNull(Sigma2FrzUCLLine[k], Sigma2FrzUCLVec[k], j, 0.5);
        AddNull(Sigma3FrzLCLLine[k], Sigma3FrzLCLVec[k], j, 0.5);
        AddNull(Sigma3FrzUCLLine[k], Sigma3FrzUCLVec[k], j, 0.5);
      end;
      if Assigned(df) then FreeAndNil(df);
      fFrozen := true;
    end;   // end freeze...

    for i := 0 to Length(Breaks) do
    begin
      // Find current period.
      if i=0 then LowVal := TimeVec.AsInteger[1]-1 else LowVal := Breaks[i-1];
      if i=Length(breaks) then HighVal := TimeVec.AsInteger[TimeVec.Length]+1 else HighVal := Breaks[i];

      // now select all in current breakperiod:
      for j := 1 to Dataframe.RowCount do
        if (TimeVec.AsInteger[j] > LowVal) and (TimeVec.AsInteger[j] <= HighVal) then
          Dataframe.Selected[j] := True
        else
          Dataframe.Selected[j] := False;
      Df := Dataframe.PrepareDataframe(PrepNames, MissingNames);
      for j := 0 to Result.Count - 1 do
      begin
        // Create vectors:
        CtrlVec[j]      := CreateVector('$CTRL', df.RowCount);
        ExcludeVec[j]   := GetExcludedVector(j);
        CenterVec[j]    := CreateVector('$CENTER', df.RowCount);
        Sigma1LCLVec[j] := CreateVector('$SIGMA1LCL', df.RowCount);
        Sigma1UCLVec[j] := CreateVector('$SIGMA1UCL', df.RowCount);
        Sigma2LCLVec[j] := CreateVector('$SIGMA2LCL', df.RowCount);
        Sigma2UCLVec[j] := CreateVector('$SIGMA2UCL', df.RowCount);
        Sigma3LCLVec[j] := CreateVector('$SIGMA3LCL', df.RowCount);
        Sigma3UCLVec[j] := CreateVector('$SIGMA3UCL', df.RowCount);
        df.Vectors.Add(CtrlVec[j]);
        df.Vectors.Add(CenterVec[j]);
        df.Vectors.Add(Sigma1LCLVec[j]);
        df.Vectors.Add(Sigma1UCLVec[j]);
        df.Vectors.Add(Sigma2LCLVec[j]);
        df.Vectors.Add(Sigma2UCLVec[j]);
        df.Vectors.Add(Sigma3LCLVec[j]);
        df.Vectors.Add(Sigma3UCLVec[j]);
      end;

      if (Cmd.ParamExists['XLABEL']) then
        fLVec := df.VectorByName[Cmd.ParamByName['XLABEL'].AsString]
      else
        fLVec := XVec;

      // Not enough data for calculating LCL, UCL, etc.
      RowCount := df.SelectedRowCount;
      if RowCount < 2 then
        dm.Error('SPC Period too short (breaks ?)', [], 34002);

      // Find Mean or Median;
      offset := 1;
      ResetMean();
      while (Offset <= Df.RowCount) and YVec.IsMissing[Offset] do inc(Offset);
      if Offset > df.RowCount then
        dm.Error('Too few (aggregated) samples to calculate chart.', []);
      for j := Offset to df.RowCount do
      begin
        if (not YVec.IsMissing[j]) then
        begin
          ExecuteMeanSuccess(j, offset);
          Offset := j;
        end else
          ExecuteMeanFail(j, offset);
      end;
      CalcMean();

      for k := 0 to Result.Count - 1 do
      begin
        for j := 1 to df.RowCount do
        begin
          // Calculate values;
          CtrlVec[k].AsFloat[j] := CtrlVal[j,k];
          ExcludeVec[k].AsFloat[j] := ExclVal[j, k];
          SetVectorValue(CenterVec[k], j, CenterFrzVec[k], Center[j,k], slCenter);
          SetVectorValue(Sigma1LCLVec[k], j, Sigma1FrzLCLVec[k], CenterVec[k].AsFloat[j] - SigmaFactor(1, Df.RowCount) * Sigma[j,k], slSigma);
          SetVectorValue(Sigma1UCLVec[k], j, Sigma1FrzUCLVec[k], CenterVec[k].AsFloat[j] + SigmaFactor(1, Df.RowCount) * Sigma[j,k], slSigma);
          SetVectorValue(Sigma2LCLVec[k], j, Sigma2FrzLCLVec[k], CenterVec[k].AsFloat[j] - SigmaFactor(2, Df.RowCount) * Sigma[j,k], slSigma);
          SetVectorValue(Sigma2UCLVec[k], j, Sigma2FrzUCLVec[k], CenterVec[k].AsFloat[j] + SigmaFactor(2, Df.RowCount) * Sigma[j,k], slSigma);
          SetVectorValue(Sigma3LCLVec[k], j, Sigma3FrzLCLVec[k], CenterVec[k].AsFloat[j] - SigmaFactor(3, Df.RowCount) * Sigma[j,k], slSigma);
          SetVectorValue(Sigma3UCLVec[k], j, Sigma3FrzUCLVec[k], CenterVec[k].AsFloat[j] + SigmaFactor(3, Df.RowCount) * Sigma[j,k], slSigma);

          if not ExcludeVec[k].IsMissing[j] then
            AddToLine(ExcludeLine[k], ExcludeVec[k], j);

          // Dirtyhack for G-Chart!
          if (Cmd.CommandID = opGChart) and (j = 1) then
            AddNull(CtrlLine[k], CtrlVec[k].AsFloat[2], 1);

          if not CtrlVec[k].IsMissing[j] then
            AddToLine(CtrlLine[k], CtrlVec[k], j);

          if (j < Df.RowCount) then
            XDiff := (XVec.AsFloat[j+1] - XVec.AsFloat[j]) / 2
          else
            XDiff := (XVec.AsFloat[j] - XVec.AsFloat[j-1]) / 2;

          AddToLine(CenterLine[k], CenterVec[k], j, -0.5);
          AddToSigmaLine(Sigma1LCLLine[k], Sigma1LCLVec[k], j, ShowSigma1[k], -XDiff);
          AddToSigmaLine(Sigma1UCLLine[k], Sigma1UCLVec[k], j, Dummy, -XDiff);
          AddToSigmaLine(Sigma2LCLLine[k], Sigma2LCLVec[k], j, ShowSigma2[k], -XDiff);
          AddToSigmaLine(Sigma2UCLLine[k], Sigma2UCLVec[k], j, Dummy, -XDiff);
          AddToSigmaLine(Sigma3LCLLine[k], Sigma3LCLVec[k], j, ShowSigma3[k], -XDiff);
          AddToSigmaLine(Sigma3UCLLine[k], Sigma3UCLVec[k], j, Dummy, -XDiff);
        end;  // For j...

        Dec(j);

        if not Cmd.ParamExists['NOL'] then
        begin
          SigmaResults(k+1, 1, i+1);
          SigmaResults(k+1, 2, i+1);
          SigmaResults(k+1, 3, i+1);
        end;
        CenterResults(k+1, i+1);

        XDiff := (XVec.AsFloat[j] - XVec.AsFloat[j-1]) / 2;

        // Shift the final control limits by a half point.
        AddToLine(CenterLine[k], CenterVec[k], j, XDiff);
        if ShowSigma1[k] then
          AddToSigmaLine(Sigma1LCLLine[k], Sigma1LCLVec[k], j, ShowSigma1[k], XDiff);
        AddToSigmaLine(Sigma1UCLLine[k], Sigma1UCLVec[k], j, Dummy, XDiff);
        if ShowSigma2[k] then
          AddToSigmaLine(Sigma2LCLLine[k], Sigma2LCLVec[k], j, ShowSigma2[k], XDiff);
        AddToSigmaLine(Sigma2UCLLine[k], Sigma2UCLVec[k], j, Dummy, XDiff);
        if ShowSigma3[k] then
          AddToSigmaLine(Sigma3LCLLine[k], Sigma3LCLVec[k], j, ShowSigma3[k], XDiff);
        AddToSigmaLine(Sigma3UCLLine[k], Sigma3UCLVec[k], j, Dummy, XDiff);

        // Make a null point to break lines when using break option.
        AddNull(ExcludeLine[k], ExcludeVec[k], j);
        AddNull(CtrlLine[k], CtrlVec[k], j);
        AddNull(CenterLine[k], CenterVec[k], j, XDiff);
        AddNull(Sigma1LCLLine[k], Sigma1LCLVec[k], j, XDiff);
        AddNull(Sigma1UCLLine[k], Sigma1UCLVec[k], j, XDiff);
        AddNull(Sigma2LCLLine[k], Sigma2LCLVec[k], j, XDiff);
        AddNull(Sigma2UCLLine[k], Sigma2UCLVec[k], j, XDiff);
        AddNull(Sigma3LCLLine[k], Sigma3LCLVec[k], j, XDiff);
        AddNull(Sigma3UCLLine[k], Sigma3UCLVec[k], j, XDiff);

        OutputTable.AddRow;
        MakeOutputLine(k, OutputTable);
        if Cmd.ParamExists['TLIMIT'] then
          OutputTable.Footer := OutputTable.Footer +
            Format('<br>Sigma<sub>1</sub> = %.2f, Sigma<sub>2</sub> = %.2f, Sigma<sub>3</sub> = %.2f',
                   [SigmaFactor(1, df.RowCount), SigmaFactor(2, df.RowCount), SigmaFactor(3, df.RowCount)]);
        l := TestIdx;
        if (Cmd.ParamExists['T1'] or Cmd.ParamExists['T']) then
        begin
          // Dirty hack for runchart.
          if Cmd.CommandID = opRunChart then
          begin
            Res := OSPCUtils.RunsTest(CenterVec[k], CtrlVec[k], k+1, i+1, str, v);
            AddTestResult(k+1, 1, i+1, v);
            OutputTable.Cell[3, OutputTable.RowCount] := IntToStr(Res);
            OutputTable.Cell[4, OutputTable.RowCount] := str;
            OutputTable.Cell[PostInc(l), OutputTable.RowCount] := IntToStr(v);
          end else begin
            TestVec := nil;
            Res := OSPCUtils.LimitTest(Sigma3LCLVec[k], Sigma3UCLVec[k], CtrlVec[k], TestVec);
            AddTestResult(k+1, 1, i+1, Res);
            OutputTable.Cell[PostInc(l), OutputTable.RowCount] := IntToStr(Res);
            TestSeries := OGraph.PointSeries(XVec, TestVec, LVec);
            TestSeries.Pointer.Visible :=  true;
            TestSeries.Pointer.HorizSize := 7;
            TestSeries.Pointer.VertSize := 7;
            TestSeries.Pointer.Brush.Style := bsClear;
            TestSeries.Pointer.Style := psCircle;
            TestSeries.Color := clWhite;
            TestSeries.ShowInLegend := false;
            TestSeries.ParentChart := Result.Chart[k];
          end;
        end;
        if (Cmd.ParamExists['T2'] or Cmd.ParamExists['T']) then
        begin
          TestVec := nil;
          Res := OSPCUtils.ConsecutiveTest(CenterVec[k], CtrlVec[k], TestVec, LimitValue('T2', 2, SpcTestList));
          AddTestResult(k+1, 2, i+1, Res);
          OutputTable.Cell[PostInc(l), OutputTable.RowCount] := IntToStr(Res);
          TestSeries := OGraph.PointSeries(XVec, TestVec, LVec);
          TestSeries.Pointer.Visible :=  true;
          TestSeries.Pointer.HorizSize := 7;
          TestSeries.Pointer.VertSize := 7;
          TestSeries.Pointer.Brush.Style := bsClear;
          TestSeries.Pointer.Style := psRectangle;
          TestSeries.Color := clWhite;
          TestSeries.ShowInLegend := false;
          TestSeries.ParentChart := Result.Chart[k];
        end;
        if (Cmd.ParamExists['T3'] or Cmd.ParamExists['T']) then
        begin
          TestVec := nil;
          Res := OSPCUtils.TrendTest(CtrlVec[k], TestVec, LimitValue('T3', 3, SpcTestList));
          AddTestResult(k+1, 3, i+1, Res);
          OutputTable.Cell[PostInc(l), OutputTable.RowCount] := IntToStr(Res);
          TestSeries := OGraph.PointSeries(XVec, TestVec, LVec);
          TestSeries.Pointer.Visible :=  true;
          TestSeries.Pointer.HorizSize := 7;
          TestSeries.Pointer.VertSize := 7;
          TestSeries.Pointer.Brush.Style := bsClear;
          TestSeries.Pointer.Style := psDiamond;
          TestSeries.Color := clWhite;
          TestSeries.ShowInLegend := false;
          TestSeries.ParentChart := Result.Chart[k];
        end;
        if Cmd.ParamExists['T4'] then
        begin
          TestVec := nil;
          Res := OSPCUtils.SigmaTest(Sigma2LCLVec[k], Sigma2UCLVec[k], CenterVec[k], CtrlVec[k], LimitValue('T4', 4, SpcTestList), TestVec);
          AddTestResult(k+1, 4, i+1, Res);
          OutputTable.Cell[PostInc(l), OutputTable.RowCount] := IntToStr(Res);
          TestSeries := OGraph.LineSeries2(XVec, TestVec, LVec );
          TestSeries.LinePen.Width := 2;
          TestSeries.Color := TGraphUtils.GetSPCColour(6);
          TestSeries.Pointer.Visible := true;
          TestSeries.Pointer.Style := psCircle;
          TestSeries.Pointer.Brush.Style := bsSolid;
          TestSeries.ShowInLegend := false;
          TestSeries.ParentChart := Result.Chart[k];
        end;
        if Cmd.ParamExists['T5'] then
        begin
          TestVec := nil;
          Res := OSPCUtils.SigmaTest(Sigma1LCLVec[k], Sigma1UCLVec[k], CenterVec[k], CtrlVec[k], LimitValue('T5', 5, SpcTestList), TestVec);
          AddTestResult(k+1, 5, i+1, Res);
          OutputTable.Cell[PostInc(l), OutputTable.RowCount] := IntToStr(Res);
          TestSeries := OGraph.LineSeries2(XVec, TestVec, LVec );
          TestSeries.LinePen.Width := 2;
          TestSeries.Color := TGraphUtils.GetSPCColour(7);
          TestSeries.Pointer.Visible := true;
          TestSeries.Pointer.Style := psCircle;
          TestSeries.Pointer.Brush.Style := bsSolid;
          TestSeries.ShowInLegend := false;
          TestSeries.ParentChart := Result.Chart[k];
        end;
        if i > 0 then ChartText[k] := ChartText[k] + ' | ';
        ChartText[k] := ChartText[k] + ' ' +
           GetCenterInfoTxt(k) +
           GetLCLInfoTxt(k) +
           GetUCLInfoTxt(k);
      end; // For k ...
      if Assigned(df) then FreeAndNil(df);
    end;     // end breakpoint

    for i := 0 to Result.Count -1 do
    begin
      if not ShowSigma1[i] then
      begin
        Sigma1LCLLine[i].ParentChart := nil;
        if DoAllowFreeze(slSigma) then
          Sigma1FrzLCLLine[i].ParentChart := nil;
      end;
      if not ShowSigma2[i] then
      begin
        Sigma2LCLLine[i].ParentChart := nil;
        if DoAllowFreeze(slSigma) then
          Sigma2FrzLCLLine[i].ParentChart := nil;
      end;
      if not ShowSigma3[i] then
      begin
        Sigma3LCLLine[i].ParentChart := nil;
        if DoAllowFreeze(slSigma) then
          Sigma3FrzLCLLine[i].ParentChart := nil;
      end;
    end;
    CleanupOutput(OutputTable);

    if not (Cmd.ParamExists['NOINF']) then
    begin
      for i := 0 to Result.Count -1 do
      begin
        Result[i].SubFoot.Text.Add(ChartText[i]);
        Result[i].SubFoot.Visible := true;
      end;
    end;

  finally
    if Assigned(Dataframe) then FreeAndNil(Dataframe);
    ODebug.DecIndent();
  end;                        
end;

function TCustomSPCChart.SigmaFactor(SigmaLvl, Count: integer): EpiFloat;
begin
  result := 0;
  if Cmd.ParamExists['TLIMIT'] then
  begin
    if Count = 2 then
      result := 1.5
    else if (Count >= 3) and (Count <= 4) then
      result := 2.0
    else if (Count >= 5) and (Count <= 9) then
      result := 2.5
    else if (Count >= 10) and (Count <= 34) then
      result := 3.0
    else if (Count >= 35) and (Count <= 199) then
      result := 3.5
    else if (Count >= 200) then
      result := 4;
    result := (SigmaLvL / 3) * Result;
  end else
    result := SigmaLvl;
end;

procedure TCustomSPCChart.SigmaResults(ChartNo, SigmaNo, BreakIndex: Integer);
var
  LCLVec, UCLVec: TEpiVector;
begin
  case SigmaNo of
    1: Begin
         LCLVec := Sigma1LCLVec[ChartNo-1];
         UCLVec := Sigma1UCLVec[ChartNo-1];
       End;
    2: Begin
         LCLVec := Sigma2LCLVec[ChartNo-1];
         UCLVec := Sigma2UCLVec[ChartNo-1];
       End;
    3: Begin
         LCLVec := Sigma3LCLVec[ChartNo-1];
         UCLVec := Sigma3UCLVec[ChartNo-1];
       End;
  else
    exit;
  end;
  dm.AddResult('$SPC' + IntToStr(ChartNo) + 'B' + IntToStr(BreakIndex) +
               'SIGMA' + IntToStr(SigmaNo), EpiTyFloat, SigmaFactor(SigmaNo, UCLVec.Length), 0, 0);
  dm.AddResult('$SPC' + IntToStr(ChartNo) + 'B' + IntToStr(BreakIndex) +
               'SIGMAV' + IntToStr(SigmaNo) + 'U', EpiTyFloat, UCLVec.AsFloat[1], 0, 0);
  if not LCLVec.IsMissing[1] then
    dm.AddResult('$SPC' + IntToStr(ChartNo) + 'B' + IntToStr(BreakIndex) +
                 'SIGMAV' + IntToStr(SigmaNo) + 'L', EpiTyFloat, LCLVec.AsFloat[1], 0, 0);
end;

procedure TCustomSPCChart.CenterResults(ChartNo, BreakIndex: Integer);
begin
  dm.AddResult('$SPC' + IntToStr(ChartNo) + 'B' + IntToStr(BreakIndex) +
               'CENTER', EpiTyFloat, Center[1, ChartNo-1], 0, 0);
end;

procedure TCustomSPCChart.SetVectorValue(Vec: TEpiVector; Index: Integer;
  FrzVec: TEpiVector; NormalVal: EpiFloat; SpcLine: TSPCLine);
begin
  if DoAllowFreeze(SpcLine) and (Assigned(FrzVec)) and
     (Index <= FrzVec.Length) and (not FrzVec.IsMissing[Index]) then
    Vec.AsFloat[Index] := FrzVec.AsFloat[Index]
  else
    Vec.AsFloat[Index] := NormalVal;
end;

procedure TCustomSPCChart.NilVectorArrays(ArraySize: integer);
var
  i: integer;
begin
  for i := 0 to ArraySize - 1 do
  begin
    CtrlVec[i] := nil;
    CenterVec[i] := nil;
    ExcludeVec[i] := nil;
    Sigma1LCLVec[i] := nil;
    Sigma1UCLVec[i] := nil;
    Sigma2LCLVec[i] := nil;
    Sigma2UCLVec[i] := nil;
    Sigma3LCLVec[i] := nil;
    Sigma3UCLVec[i] := nil;

    CenterFrzVec[i] := nil;
    Sigma1FrzLCLVec[i] := nil;
    Sigma1FrzUCLVec[i] := nil;
    Sigma2FrzLCLVec[i] := nil;
    Sigma2FrzUCLVec[i] := nil;
    Sigma3FrzLCLVec[i] := nil;
    Sigma3FrzUCLVec[i] := nil;
  end;
end;

procedure TCustomSPCChart.CommonSeriesCreate(Series: TCustomSeries;
  Title: string; LineCode: integer; ParentChart: TChart);
begin
  // Logic LineCode numbering:
  // 1: Ctrl line (and excluded)
  // 2: Center line
  // 3-5: Sigma 3->1 lines.
  Series.Title := Title;
  Series.LinePen.Width := 2;
  Series.Color := TGraphUtils.GetSPCColour(LineCode);
  Series.Stairs := true;
  Series.ParentChart := ParentChart;
  if ((LineCode >= 4) and (not Cmd.ParamExists['SL'])) or
     ((LineCode >= 3) and (Cmd.ParamExists['NOL']))  then
    Series.ParentChart := nil;
end;

function TCustomSPCChart.CreatePoint(Title: string;
  LineCode: integer; ParentChart: TChart): TPointSeries;
begin
  Result := TPointSeries.Create(ParentChart);
  CommonSeriesCreate(Result, Title, LineCode, ParentChart);
end;

function TCustomSPCChart.CreateLine(Title: string;
  LineCode: integer; ParentChart: TChart): TLineSeries;
begin
  Result := TLineSeries.Create(ParentChart);
  CommonSeriesCreate(Result, Title, LineCode, ParentChart);
end;

function TCustomSPCChart.CreateFreezeLine(OrgLine: TLineSeries;
  LineCode: integer): TLineSeries;
begin
  Result := CreateLine(OrgLine.Title, LineCode, TChart(OrgLine.ParentChart));
  OrgLine.LinePen.Style := psDash;
  OrgLine.LinePen.EndStyle := esSquare;
  OrgLine.ShowInLegend := false;
end;

function TCustomSPCChart.GetCenterInfoTxt(ChartNo: integer): string;
begin
  result := '';
  Result := Format(' ' + CenterText[ChartNo] + ': %.2f', [CenterVec[ChartNo].AsFloat[1]]);
end;

function TCustomSPCChart.GetLCLInfoTxt(ChartNo: integer): string;
begin
  result := '';
  if Sigma3LCLVec[ChartNo].IsMissing[1] then exit;
  if Cmd.ParamExists['NOL'] then exit;
  Result := Format(' LCL: %.2f', [Sigma3LCLVec[ChartNo].AsFloat[1]]);
end;

function TCustomSPCChart.GetUCLInfoTxt(ChartNo: integer): string;
begin
  result := '';
  if Cmd.ParamExists['NOL'] then exit;
  Result := Format(' UCL: %.2f', [Sigma3UCLVec[ChartNo].AsFloat[1]]);
end;

function TCustomSPCChart.CreateVector(VectorName: string;
  Size: Integer): TEpiVector;
begin
  result := TEpiFloatVector.Create(VectorName, Size);
  result.FieldDataSize := 12;
  result.FieldDataDecimals := 14;
end;

function TCustomSPCChart.AllowFreeze(SpcLine: TSPCLine): Boolean;
begin
  result := true;
end;

function TCustomSPCChart.DoAllowFreeze(SpcLine: TSPCLine): boolean;
begin
  result := Cmd.ParamExists['F'] and AllowFreeze(SpcLine);
end;

procedure TCustomSPCChart.ReAssignToLengend(Series: TCustomSeries);
var
  TmpChart: TChart; 
begin
  TmpChart := TChart(Series.ParentChart);
  Series.ParentChart := nil;
  Series.ParentChart := TmpChart;
end;

function TCustomSPCChart.PreAggregate(
  const Dataframe: TEpiDataframe): TEpiDataframe;
begin
  result := DataFrame.PrepareDataframe('');
end;

function TCustomSPCChart.GetExclusionVector(
  const Dataframe: TEpiDataframe): TEpiVector;
begin
  result := Dataframe.FindVector(Varnames[0]);
end;

function TCustomSPCChart.GetExcludedVector(ChartNo: Integer): TEpiVector;
begin
  result := Df.FindVector('$EXCLUDED');
end;

function TCustomSPCChart.GetExclVal(LoopIndex, ChartNo: integer): EpiFloat;
begin
  result := ExcludeVec[ChartNo].AsFloat[LoopIndex];
end;

function TCustomSPCChart.LimitValue(OptionName: string;
  TestNo: Integer; TestList: TStrings): Integer;
var
  code: integer;
  res:  integer;
begin
  Val(TestList[TestNo-1], Result, Code);
  if Code > 0 then
    Dm.Error('Default value for test %d is not an integer.', [TestNo]);

  if not Cmd.ParamExists[OptionName] then exit;
  if Trim(Cmd.ParamByName[OptionName].AsString) = '' then exit;

  Val(Cmd.ParamByName[OptionName].AsString, Res, Code);
  if Code = 0 then
    Result := Res;
end;

function TCustomSPCChart.GetSpcTestList(): TStrings;
var
  Opt: TEpiOption;
  s: string;
begin
  if Dm.GetOptionValue('SPC TEST', Opt) then
    s := Opt.Value;
  Result := TStringList.Create;
  SplitString(s, result, [',']);
  if not Result.Count = 5 then
    Dm.Error('SPC TEST option has incorrect number of values: %d' + #13#10 + 'Must be exactly 5', [Result.Count]);
end;

function TCustomSPCChart.ExcludeFunction(index: integer;
  df: TEpiDataframe): Boolean;
begin
  result := false;
end;

end.
