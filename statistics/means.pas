unit means;

{$codepage UTF-8}
{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ast, epidatafiles, epidatafilestypes, epicustombase,
  executor, result_variables, interval_types,
  outputcreator;

type

  { TIntervalDescriptives }

  TIntervalDescriptives = class
  protected
    CategVar: TEpiField;
    CountVar: TEpiField;
    FExecutor: TExecutor;
    FOutputCreator: TOutputCreator;
    FResultDF: TIntervalDecriptivesDatafile;
    procedure FillDescriptor(SIdx, EIdx: Integer; ASum: EpiFloat; ST: TCustomVariableCommand);
    function  Anova: TAnovaRecord;
  public
    constructor Create(AExecutor: TExecutor; OutputCreator: TOutputCreator);
    destructor Destroy; override;
    function DoMeans(DataFile: TEpiDataFile; ST: TCustomVariableCommand): boolean;
    function DoIntervalDescriptives(DataFile: TEpiDataFile; ST: TCustomVariableCommand;
      out Descriptors: TIntervalDecriptivesDatafile): boolean;
    procedure OutMeans(MeanDataFile: TIntervalDecriptivesDatafile; ST: TCustomVariableCommand);
    procedure OutAnova(AnovaRec: TAnovaRecord);
    procedure OutTtest(AnovaRec: TAnovaRecord);
  end;

implementation

uses
  generalutils, Math, statfunctions, options_utils, epifields_helper;


{ TIntervalDescriptives }

procedure TIntervalDescriptives.FillDescriptor(SIdx, EIdx: Integer;
          ASum: EpiFloat; ST: TCustomVariableCommand);
var
  Idx, Obs, i: Integer;
  lMean, lAvgDev, lStdDev, lSSqDev, lStdErr, lSkew, lCurt, Val, lTvalue: EpiFloat;
  lCfiVal: Extended;

  function GetPercentile(min, max, d: integer):EpiFloat;
  var
    w : EpiFloat;
    ix: integer;
  begin
    // Try new method.
    ix := PercentileIndex(max-min+1, d/100, w);
    if w = 0 then
      result:= CountVar.AsFloat[ix + min-1]
    else
      result := CountVar.AsFloat[ix + min-1] + (CountVar.AsFloat[ix+ min]-CountVar.AsFloat[ix+ min-1])*w;
  end;

  function GetGvt(ST: TCustomVariableCommand): TEpiGetValueLabelType;
  begin
    result := ValueLabelTypeFromOptionList(ST.Options, FExecutor.SetOptions);
  end;

begin
  Idx := FResultDF.NewRecords();

  with FResultDF do
  begin
    // Category name
    Category.AsString[Idx] := CategVar.GetValueLabel(SIdx, GetGvt(ST));
    // Observation found in category
    Obs                    := EIdx - SIdx + 1;
    N.AsInteger[Idx]       := Obs;

    // Percentiles
    Min.AsFloat[Idx]       := CountVar.AsFloat[SIdx];
    P5.AsFloat[Idx]        := GetPercentile(SIdx, EIdx, 5);
    P10.AsFloat[Idx]       := GetPercentile(SIdx, EIdx, 10);
    P25.AsFloat[Idx]       := GetPercentile(SIdx, EIdx, 25);
    Median.AsFloat[Idx]    := GetPercentile(SIdx, EIdx, 50);
    P75.AsFloat[Idx]       := GetPercentile(SIdx, EIdx, 75);
    P90.AsFloat[Idx]       := GetPercentile(SIdx, EIdx, 90);
    P95.AsFloat[Idx]       := GetPercentile(SIdx, EIdx, 95);
    Max.AsFloat[Idx]       := CountVar.AsFloat[EIdx];

    // Statistics to be saved in FResultDF
    lMean                  := ASum / Obs;
    Mean.AsFloat[Idx]      := lMean;
    Sum.AsFloat[Idx]       := ASum;

    lAvgDev := 0;
    lSSqDev := 0;
    lStdErr := 0;
    lSkew   := 0;
    lCurt   := 0;
    FOR i := SIdx TO EIdx DO
    BEGIN
      Val     := CountVar.AsFloat[i] - lMean;
      lAvgDev := lAvgDev + abs(Val);
      lSSqDev := lSSqDev + (Val * Val);
    END;
    lAvgDev                := lAvgDev / Obs;
    AvgDev.AsFloat[Idx]    := lAvgDev;
    SumSS.AsFloat[Idx]     := lSSqDev;

    // Cannot calculate these statistics with 1 observation
    if (Obs > 1) then
      begin
        lSSqDev               := lSSqDev / (Obs - 1);
        StdVar.AsFloat[Idx]   := lSSqDev;
        StdDev.AsFloat[Idx]   := sqrt(lSSqDev);
        lStdErr               := sqrt(lSSqDev / Obs);
        StdErr.AsFloat[Idx]   := lStdErr;
        lCfiVal               := PTDISTRINV(Obs - 1, 0.025) * lStdErr;
        CfiL.AsFloat[Idx]     := lMean - lCfiVal;
        CfiH.AsFloat[Idx]     := lMean + lCfiVal;
      end;
  end;
end;

function TIntervalDescriptives.Anova: TAnovaRecord;
var
  i, lStrata: Integer;
  Obs: Int64;
  ASum, ASumSQ, ASSW, ASST, MPTmp, Tval: EpiFloat;
  m,s: EpiFloat;
begin
  if FResultDF.Size < 2 then
  // t-test of mean=0 if only one result
    begin
      with FResultDF do
      begin
        Tval := Mean.AsFloat[0] / (StdErr.AsFloat[0]);
        Obs  := N.AsInteger[0];
      end;
      with Result do
      begin
        F    := Tval;     // Making use of F for the t-value
        if (Obs < 2) or (isInfinite(Tval) or isNaN(Tval)) then
          PROB := 1
        else
          PROB := tdist(F,Obs-1);
      end;
      exit;
    end;

  // F-test if more than one stratum
  Obs    := 0;
  ASum   := 0;
  ASSW   := 0;

  with FResultDF do
  begin
    lStrata := Size - 2;  // Size includes the grand sums row
    for i := 0 to lStrata do
      ASSW += SumSS.AsFloat[i];
   // now get grand sums from last set of descriptives
    i    := lStrata + 1;
    ASST := SumSS.AsFloat[i];
    Obs  := N.AsInteger[i];
  end;

  with Result do
  begin
    SST := ASST;
    SSW := ASSW;
    SSB := SST - SSW;
    DFT := Obs - 1;
    DFB := lStrata;
    DFW := DFT - DFB;
    MSB := SSB / DFB;
    MSW := SSW / DFW;
    MST := SST / DFT;
    F   := MSB / MSW;
    if (isInfinite(F) or isNaN(F)) then
      PROB := 1
    else
      PROB := fdist(F, DFB, DFW);
  end;
end;

constructor TIntervalDescriptives.Create(AExecutor: TExecutor;
  OutputCreator: TOutputCreator);
begin
  FExecutor := AExecutor;
  FOutputCreator := OutputCreator;
end;

destructor TIntervalDescriptives.Destroy;
begin
  inherited Destroy;
end;

function TIntervalDescriptives.DoMeans(DataFile: TEpiDataFile;
  ST: TCustomVariableCommand): boolean;
var
  DummyDF: TIntervalDecriptivesDatafile;
begin
  DoIntervalDescriptives(DataFile, ST, DummyDF);
  DummyDF.Free;
end;

function TIntervalDescriptives.DoIntervalDescriptives(DataFile: TEpiDataFile; ST: TCustomVariableCommand; out
  Descriptors: TIntervalDecriptivesDatafile): boolean;
var
  opt: TOption;
  SortList: TEpiFields;
  i, StartIdx, EndIdx: Integer;
  Val, Sum, SumSq, Tsum: EpiFloat;
begin
  FResultDF := TIntervalDecriptivesDatafile.Create(nil, 0);

  CountVar := DataFile.Fields.FieldByName[ST.VariableList[0].Ident];

  if ST.Options.HasOption('by', opt) then
    begin
// stratified data
// Algorithm:
// * sort according to (Category, Values)
// * run through datafile
// * collect StartIdx, EndIndex, Value, Sum
// * whenever a change in category record values and change
      CategVar := DataFile.Fields.FieldByName[Opt.Expr.AsIdent];
      SortList := TEpiFields.Create(nil);
      SortList.AddItem(CategVar);
      SortList.AddItem(CountVar);
      DataFile.SortRecords(SortList);
      SortList.Free;

      StartIdx := 0;
      Val      := CountVar.AsFloat[0];
      Sum      := Val;
      Tsum     := 0;
      for i := 1 to DataFile.Size - 1 do
      begin
        EndIdx := i - 1;

        if CategVar.Compare(i-1, i) <> 0 then
          begin
            // New category, so fill descriptors
            FillDescriptor(StartIdx, EndIdx, Sum, ST);
            // keep running sum for full data set
            Tsum     += Sum;
            // Reset values
            StartIdx := i;
            Val      := CountVar.AsFloat[i];
            Sum      := Val;
          end
        else
          begin
            Val      := CountVar.AsFloat[i];
            Sum      += Val;
          end;
      end;
  // final stratum
    FillDescriptor(StartIdx, DataFile.Size -1, Sum, ST);
    Tsum += Sum;
  end

  else
// no 'by' option - just save sum of data
    begin
      CategVar := DataFile.NewField(ftInteger);
      Tsum := 0;
      for i := 0 to DataFile.Size -1 do
        Tsum += CountVar.AsFloat[i];
    end;
// Add in Descriptor for full data set (required for Total SS in Anova)
// In future, could decide to display all data results as well as stratum results
// However, will have to set appropriate result vars as well
// Considered adding a boolean parameter to the procedure call to indicate which
  FillDescriptor(0, DataFile.Size - 1, Tsum, ST);

  OutMeans(FResultDF, ST);

  if ST.Options.HasOption('t', opt) then
     if (FResultDF.Size >= 2)
    then
      OutAnova(Anova)
    else
      OutTtest(Anova);

  Descriptors := FResultDF;
  ST.ExecResult := csrSuccess;
end;

procedure TIntervalDescriptives.OutMeans(
  MeanDataFile: TIntervalDecriptivesDatafile; ST: TCustomVariableCommand);
var
  Offset, i, Idx, Sz: Integer;
  CatV, ObsV, SumV, MeanV, SvV, SdV, CfilV, CfihV: TCustomExecutorDataVariable;
  T: TOutputTable;
  GVT: TEpiGetVariableLabelType;
begin
  T := FOutputCreator.AddTable;

  GVT := VariableLabelTypeFromOptionList(ST.Options, FExecutor.SetOptions);
  T.Header.Text := CountVar.GetVariableLabel(GVT);
  Sz := MeanDataFile.Size;
  if Sz = 1 then
    begin
      Offset := 0;
      T.ColCount := 9;
      T.RowCount := 5;
    end
  else
    begin
      // final result set is for all data; don't display for now
      Sz         := Sz - 1;
      Offset     := 1;
      T.ColCount := 10;
      T.RowCount := 3 + (Sz * 2);
    end;

  // Column headers  (first section)
  if Offset > 0 then T.Cell[0,0].Text := CategVar.GetVariableLabel(Gvt);
  T.Cell[0 + Offset, 0].Text := 'Obs';
  T.Cell[1 + Offset, 0].Text := 'Sum';
  T.Cell[2 + Offset, 0].Text := 'Mean';
  T.Cell[3 + Offset, 0].Text := 'Variance';
  T.Cell[4 + Offset, 0].Text := 'Std. Dev.';
  T.Cell[5 + Offset, 0].Text := '( 95% CI';
  T.Cell[6 + Offset, 0].Text := 'mean )';
  T.Cell[7 + Offset, 0].Text := 'Std. Err.';
  T.SetRowAlignment(0, taRightJustify);
  T.SetRowBoxBorder(0);

  with FExecutor do
  begin

    if Sz = 1 then
      begin
        CatV  := AddResultConst('$means_category', ftString);
        ObsV  := AddResultConst('$means_obs',      ftInteger);
        SumV  := AddResultConst('$means_sum',      ftFloat);
        MeanV := AddResultConst('$means_mean',     ftFloat);
        SvV   := AddResultConst('$means_sv',       ftFloat);
        SdV   := AddResultConst('$means_sd',       ftFloat);
        CfilV := AddResultConst('$means_cfil',     ftFloat);
        CfihV := AddResultConst('$means_cfih',     ftFloat);
      end
    else
      begin
        CatV  := AddResultVector('$means_category', ftString, Sz);
        ObsV  := AddResultVector('$means_obs',      ftInteger, Sz);
        SumV  := AddResultVector('$means_sum',      ftFloat, Sz);
        MeanV := AddResultVector('$means_mean',     ftFloat, Sz);
        SvV   := AddResultVector('$means_sv',       ftFloat, Sz);
        SdV   := AddResultVector('$means_sd',       ftFloat, Sz);
        CfilV := AddResultVector('$means_cfil',     ftFloat, Sz);
        CfihV := AddResultVector('$means_cfih',     ftFloat, Sz);
      end;
    AddResultConst('$means_size',  ftInteger).AsIntegerVector[0] := Sz;
    AddResultConst('$means_var',  ftString).AsStringVector[0]    := CountVar.Name;
  end;


  with MeanDataFile do
  begin
    for i := 0 to Sz - 1 do
      begin
        if Offset > 0 then T.Cell[0, i + 1].Text := Category.AsString[i];
        T.Cell[0 + Offset, i + 1].Text := FormatFloat('0', N.AsFloat[i]);
        T.Cell[1 + Offset, i + 1].Text := FormatFloat('#.00', Sum.AsFloat[i]);
        T.Cell[2 + Offset, i + 1].Text := FormatFloat('#.00', Mean.AsFloat[i]);
// watch for statistics that could not be calculated
        if (N.AsInteger[i] < 2) or (isInfinite(Sum.AsFloat[i])) or (isNaN(Sum.AsFloat[i])) then
          begin
            T.Cell[3 + Offset, i + 1].Text := 'Cannot';
            T.Cell[4 + Offset, i + 1].Text := 'estimate';
            T.Cell[5 + Offset, i + 1].Text := '-';
            T.Cell[6 + Offset, i + 1].Text := '-';
            T.Cell[7 + Offset, i + 1].Text := '-';
          end
        else
          begin
            T.Cell[3 + Offset, i + 1].Text := FormatFloat('0.00', StdVar.AsFloat[i]);
            T.Cell[4 + Offset, i + 1].Text := FormatFloat('0.00', StdDev.AsFloat[i]);
            T.Cell[5 + Offset, i + 1].Text := FormatFloat('0.00', CfiL.AsFloat[i]);
            T.Cell[6 + Offset, i + 1].Text := FormatFloat('0.00', CfiH.AsFloat[i]);
            T.Cell[7 + Offset, i + 1].Text := FormatFloat('0.00', StdErr.AsFloat[i]);
          end;
        T.SetRowAlignment(i+1, taRightJustify);
        // Need to set this after the entire row has been set to right justify
        if Offset > 0 then T.Cell[0, i + 1].Alignment := taLeftJustify;

        CatV.AsStringVector[i]  := Category.AsString[i];
        ObsV.AsIntegerVector[i] := N.AsInteger[i];
        SumV.AsFloatVector[i]   := Sum.AsFloat[i];
        MeanV.AsFloatVector[i]  := Mean.AsFloat[i];
        if (N.AsInteger[i] < 2) then
          begin
            SvV.AsFloatVector[i]    := 0/0;
            SdV.AsFloatVector[i]    := 0/0;
            CfilV.AsFloatVector[i]  := 0/0;
            CfihV.AsFloatVector[i]  := 0/0;
          end
        else
        begin
          SvV.AsFloatVector[i]    := StdVar.AsFloat[i];
          SdV.AsFloatVector[i]    := StdDev.AsFloat[i];
          CfilV.AsFloatVector[i]  := CfiL.AsFloat[i];
          CfihV.AsFloatVector[i]  := CfiH.AsFloat[i];
        end;
      end;
  end;

  Idx := Sz + 1;

  if Offset > 0 then
    T.Cell[0, Idx].Text := CategVar.GetVariableLabel(Gvt);
  T.Cell[0 + Offset, Idx].Text := 'Min';
  T.Cell[1 + Offset, Idx].Text := 'p5';
  T.Cell[2 + Offset, Idx].Text := 'p10';
  T.Cell[3 + Offset, Idx].Text := 'p25';
  T.Cell[4 + Offset, Idx].Text := 'Median';
  T.Cell[5 + Offset, Idx].Text := 'p75';
  T.Cell[6 + Offset, Idx].Text := 'p90';
  T.Cell[7 + Offset, Idx].Text := 'p95';
  T.Cell[8 + Offset, Idx].Text := 'Max';
  T.SetRowAlignment(Idx, taRightJustify);
  T.SetRowBoxBorder(Idx);

  Inc(Idx);

  with MeanDataFile do
  begin
    for i := 0 to Sz - 1 do
      begin
        if Offset > 0 then T.Cell[0, Idx + i].Text := Category.AsString[i];
        T.Cell[0 + Offset, Idx + i].Text := FormatFloat('0.00', Min.AsFloat[i]);
        T.Cell[1 + Offset, Idx + i].Text := FormatFloat('0.00', P5.AsFloat[i]);
        T.Cell[2 + Offset, Idx + i].Text := FormatFloat('0.00', P10.AsFloat[i]);
        T.Cell[3 + Offset, Idx + i].Text := FormatFloat('0.00', P25.AsFloat[i]);
        T.Cell[4 + Offset, Idx + i].Text := FormatFloat('0.00', Median.AsFloat[i]);
        T.Cell[5 + Offset, Idx + i].Text := FormatFloat('0.00', P75.AsFloat[i]);
        T.Cell[6 + Offset, Idx + i].Text := FormatFloat('0.00', P90.AsFloat[i]);
        T.Cell[7 + Offset, Idx + i].Text := FormatFloat('0.00', P95.AsFloat[i]);
        T.Cell[8 + Offset, Idx + i].Text := FormatFloat('0.00', Max.AsFloat[i]);
        T.SetRowAlignment(Idx + i, taRightJustify);
        // Need to set this after the entire row has been set to right justify
        if Offset > 0 then T.Cell[0, Idx + i].Alignment := taLeftJustify;
      end;
  end;
end;

procedure TIntervalDescriptives.OutAnova(AnovaRec: TAnovaRecord);
var
  T: TOutputTable;
begin
  T := FOutputCreator.AddTable;
  if (isNaN(AnovaRec.F)) then
    begin
      T.ColCount := 1;
      T.RowCount := 1;
      T.Cell[0,0].Text := 'Cannot do ANOVA with these values';
      exit
    end;
  T.ColCount := 6;
  T.RowCount := 4;

  T.Cell[0,0].Text := 'Source';
  T.Cell[1,0].Text := 'DF';
  T.Cell[2,0].Text := 'SS';
  T.Cell[3,0].Text := 'MS';
  T.Cell[4,0].Text := 'F';
  T.Cell[5,0].Text := 'p Value';
  T.SetRowAlignment(0, taCenter);

  with AnovaRec do
  begin
    T.Cell[0,1].Text := 'Between';
    T.Cell[1,1].Text := IntToStr(DFB);
    T.Cell[2,1].Text := Format('%8.6f', [SSB]);
    T.Cell[3,1].Text := Format('%8.6f', [MSB]);
    T.Cell[4,1].Text := Format('%8.6f', [F]);
    T.Cell[5,1].Text := Format('%8.6f', [PROB]);
    if ((PROB < 0.0) or (PROB > 1.0)) then
      T.Cell[5,1].Text := ' (Invalid ' + T.Cell[5,1].Text + ' - Program error)';

    FExecutor.AddResultConst('$means_DFB', ftInteger).AsIntegerVector[0] := DFB;
    FExecutor.AddResultConst('$means_SSB', ftFloat).AsFloatVector[0]     := SSB;
    FExecutor.AddResultConst('$means_MSB', ftFloat).AsFloatVector[0]     := MSB;
    FExecutor.AddResultConst('$means_F',   ftFloat).AsFloatVector[0]     := F;
    FExecutor.AddResultConst('$means_PROB', ftFloat).AsFloatVector[0]    := PROB;

    T.Cell[0,2].Text := 'Within';
    T.Cell[1,2].Text := IntToStr(DFW);
    T.Cell[2,2].Text := Format('%8.6f', [SSW]);
    T.Cell[3,2].Text := Format('%8.6f', [MSW]);

    FExecutor.AddResultConst('$means_DFW', ftInteger).AsIntegerVector[0] := DFW;
    FExecutor.AddResultConst('$means_SSW', ftFloat).AsFloatVector[0]     := SSW;
    FExecutor.AddResultConst('$means_MSW', ftFloat).AsFloatVector[0]     := MSW;
  end;
end;

procedure TIntervalDescriptives.OutTtest(AnovaRec: TAnovaRecord);
var
  T: TOutputTable;
begin
  T := FOutputCreator.AddTable;
  if (isNaN(AnovaRec.F)) then
    begin
      T.ColCount := 1;
      T.RowCount := 1;
      T.Cell[0,0].Text := 'Cannot do a t-test with these values';
      exit
    end;
  T.ColCount       := 3;
  T.RowCount       := 2;
  T.Cell[0,0].Text := 'Test of Ho: mean=zero';
  T.Cell[1,0].Text := 't';
  T.Cell[2,0].Text := 'p Value';
  T.SetRowAlignment(0, taCenter);

  with AnovaRec do
  begin
    T.Cell[1,1].Text := Format('%8.6f', [F]);
    T.Cell[2,1].Text := Format('%8.6f', [PROB]);

    FExecutor.AddResultConst('$means_T', ftFloat).AsFloatVector[0]   := F;
    FExecutor.AddResultConst('$means_PROB', ftfloat).AsFloatVector[0] := PROB;
  end;
end;
end.
