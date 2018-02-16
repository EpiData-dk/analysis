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
    procedure FillDescriptor(SIdx, EIdx: Integer; ASum, ASumSq: EpiFloat; ST: TCustomVariableCommand);
    function  Anova: TAnovaRecord;
  public
    constructor Create(AExecutor: TExecutor; OutputCreator: TOutputCreator);
    destructor Destroy; override;
    function DoMeans(DataFile: TEpiDataFile; ST: TCustomVariableCommand): boolean;
    function DoIntervalDescriptives(DataFile: TEpiDataFile; ST: TCustomVariableCommand;
      out Descriptors: TIntervalDecriptivesDatafile): boolean;
    procedure OutMeans(MeanDataFile: TIntervalDecriptivesDatafile; ST: TCustomVariableCommand);
    procedure OutAnova(AnovaRec: TAnovaRecord);
  end;

implementation

uses
  generalutils, statfunctions, options_utils, epifields_helper;


{ TIntervalDescriptives }

procedure TIntervalDescriptives.FillDescriptor(SIdx, EIdx: Integer; ASum,
  ASumSq: EpiFloat; ST: TCustomVariableCommand);
var
  Idx, Obs, i: Integer;
  lMean, lAvgDev, lStdDev, lStdVar, lStdErr, lSkew, lCurt, Val: EpiFloat;
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

    // Statistics
    lMean                  := ASum / Obs;
    Mean.AsFloat[Idx]      := lMean;
    Sum.AsFloat[Idx]       := ASum;
    SumSq.AsFloat[Idx]     := ASumSq;

    lAvgDev := 0;
    lStdVar := 0;
    lStdErr := 0;
    lSkew   := 0;
    lCurt   := 0;
    FOR i := SIdx TO EIdx DO
    BEGIN
      Val := CountVar.AsFloat[i] - lMean;
      lAvgDev := lAvgDev + abs(Val);
      lStdVar := lStdVar + (Val * Val);
    END;
    lAvgDev := lAvgDev / Obs;

    AvgDev.AsFloat[Idx]    := lAvgDev;
    SumSS.AsFloat[Idx]     := lStdVar;

    if (Obs >= 2) then
      begin
        lStdVar := lStdVar / (Obs - 1);
        StdVar.AsFloat[Idx]   := lStdVar;
        StdDev.AsFloat[Idx]   := sqrt(lStdVar);
        lStdErr               := sqrt(lStdVar / Obs);
        StdErr.AsFloat[Idx]   := lStdErr;
        lCfiVal := PTDISTRINV(Obs - 1, 0.025) * lStdErr;
        CfiL.AsFloat[Idx]     := lMean - lCfiVal;
        CfiH.AsFloat[Idx]     := lMean + lCfiVal;
      end;
  end;
end;
// F distribution functions from OpenEpi
// *** NOT CORRECT ***
//The following F distribution functions are from John Pezzullo's page at http://members.aol.com/johnp71/pdfs.html


function FisherF (f : EpiFloat; n1, n2 : Integer) : EpiFloat;
    var
      x, th, a, c, sth, cth, piD2 : EpiFloat;
      k : Integer;
   function StatCom (q : EpiFloat; i, j, b : Integer ) : EpiFloat;
      var
          zz, z : EpiFloat;
          k : Integer;
      begin
          zz := 1;
          z := zz;
          k := i;
          while (k<=j) do
            begin
              zz := zz*q*k/(k-b);
              z := z+zz;
              k := k+2;
              end;
          StatCom := z;
      end;

   begin
         piD2 := Pi / 2;
         x := n2 / (n1 * f + n2);
         if ((n1 Mod 2) = 0) then
            Exit(StatCom(1-x,n2,n1+n2-4,n2-2) * exp(ln(x) * n2/2));
         if ((n2 Mod 2) = 0) then
            Exit(1-(StatCom(x,n1,n1+n2-4,n1-2) * exp(ln(1-x) * n1/2)));
         th := arctan(sqrt(n1*f/n2));
         a := th / piD2;
         sth := sin(th);
         cth := cos(th);
         if (n2 > 1) then
               a := a + (sth*cth*StatCom(cth*cth,2,n2-3,-1)/piD2);
         if (n1=1) then
            Exit(1-a);
         c := 4*StatCom(sth*sth,n2+1,n1+n2-4,n2-2)*sth*exp(ln(cth) * n2)/Pi;
       if (n2=1) then
            Exit(1 - (a+c/2));
       k := 2;
       while(k <= ((n2-1)/2)) do
         begin
            c := c*k/(k - 0.5);
            k := k+1;
            end;
       FisherF := 1-a+c;
   end;

function FfromP (p : EpiFloat; df1, df2 : Integer) : EpiFloat;
var
    v, dv, f : EpiFloat;
begin;
    v := 0.5;
    dv := 0.5;
    f := 0;
    while(dv>1e-10) do   //was -10
       begin
	   f := 1/v-1;
	   dv := dv/2;
	   if (FisherF(f,df1,df2) > p) then
	      v := v-dv
	   else
	      v := v+dv;
	end;
    FfromP := f;
end;

function PfromF(p : EpiFloat; df1,df2 : Integer) : EpiFloat;
begin;
  PfromF := FisherF(p,df1,df2);
end;

function TIntervalDescriptives.Anova: TAnovaRecord;
var
  i: Integer;
  Obs: Int64;
  ASum, ASumSQ, ASSW, ASSB, MPTmp: EpiFloat;
begin
  if FResultDF.Size < 2 then exit;

  Obs    := 0;
  ASum   := 0;
  ASumSQ := 0;
  ASSW   := 0;
  ASSB   := 0;

  with FResultDF do
  for i := 0 to Size -1 do
    begin
      Obs    += N.AsInteger[i];
      MPTmp  := Sum.AsFloat[i];

      ASum   += MPTmp;
      ASumSQ += SumSq.AsFloat[i];
      ASSW   += SumSS.AsFloat[i];
      ASSB   += (MPTmp * MPTmp) / N.AsInteger[i];
    end;

  with Result do
  begin
    SSW := ASSW;
    SSB := ASSB - ((ASum * ASum) / Obs);
    SST := SSB + SSW;
    DFB := FResultDF.Size - 1;
    DFW := Obs - FResultDF.Size;
    DFT := Obs - 1;
    MSB := SSB / DFB;
    MSW := SSW / DFW;
    MST := SST / DFT;
    F   := MSB / MSW;
    PROB := PfromF(F, DFB, DFW);
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
  Val, Sum, SumSq: EpiFloat;
begin
  FResultDF := TIntervalDecriptivesDatafile.Create(nil, 0);

  CountVar := DataFile.Fields.FieldByName[ST.VariableList[0].Ident];

  if ST.Options.HasOption('by', opt) then
    CategVar := DataFile.Fields.FieldByName[Opt.Expr.AsIdent]
  else
    CategVar := DataFile.NewField(ftInteger);

  SortList := TEpiFields.Create(nil);
  SortList.AddItem(CategVar);
  SortList.AddItem(CountVar);
  DataFile.SortRecords(SortList);
  SortList.Free;

  StartIdx := 0;
  Val := CountVar.AsFloat[0];
  Sum := Val;
  SumSq := Val * Val;

  // Algorithm:
  // * sort according to (Category, Values)
  // * run through datafile
  // * collect StartIdx, EndIndex, Value, Sum and SumSqaured
  // * whenever a change in category record values and change
  for i := 1 to DataFile.Size - 1 do
  begin
    EndIdx := i - 1;

    if CategVar.Compare(i-1, i) <> 0 then
      begin
        // Fill descriptors
        FillDescriptor(StartIdx, EndIdx, Sum, SumSq, ST);

        // Reset values
        StartIdx := i;
        Val := CountVar.AsFloat[i];
        Sum := Val;
        SumSq := Val * Val;
      end
    else
      begin
        Val := CountVar.AsFloat[i];
        Sum += Val;
        SumSq += Val * Val;
      end;
  end;

  FillDescriptor(StartIdx, DataFile.Size -1, Sum, SumSq, ST);
  OutMeans(FResultDF, ST);

  if ST.Options.HasOption('t', opt) and
     (FResultDF.Size >= 2)
  then
    OutAnova(Anova);

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
  if MeanDataFile.Size = 1 then
    begin
      Offset := 0;
      T.ColCount := 9;
      T.RowCount := 5;
    end
  else
    begin
      Offset := 1;
      T.ColCount := 10;
      T.RowCount := 3 + (MeanDataFile.Size * 2);
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
    Sz := MeanDataFile.Size;
    AddResultConst('$means_size',  ftInteger).AsIntegerVector[0] := Sz;
    AddResultConst('$means_var',  ftString).AsStringVector[0]    := CountVar.Name;

    if MeanDataFile.Size = 1 then
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
  end;


  with MeanDataFile do
  begin
    for i := 0 to Size - 1 do
      begin
        if Offset > 0 then T.Cell[0, i + 1].Text := Category.AsString[i];
        T.Cell[0 + Offset, i + 1].Text := FormatFloat('0.00', N.AsFloat[i]);
        T.Cell[1 + Offset, i + 1].Text := FormatFloat('0.00', Sum.AsFloat[i]);
        T.Cell[2 + Offset, i + 1].Text := FormatFloat('0.00', Mean.AsFloat[i]);
        T.Cell[3 + Offset, i + 1].Text := FormatFloat('0.00', StdVar.AsFloat[i]);
        T.Cell[4 + Offset, i + 1].Text := FormatFloat('0.00', StdDev.AsFloat[i]);
        T.Cell[5 + Offset, i + 1].Text := FormatFloat('0.00', CfiL.AsFloat[i]);
        T.Cell[6 + Offset, i + 1].Text := FormatFloat('0.00', CfiH.AsFloat[i]);
        T.Cell[7 + Offset, i + 1].Text := FormatFloat('0.00', StdErr.AsFloat[i]);
        T.SetRowAlignment(i+1, taRightJustify);
        // Need to set this after the entire row has been set to right justify
        if Offset > 0 then T.Cell[0, i + 1].Alignment := taLeftJustify;

        CatV.AsStringVector[i]  := Category.AsString[i];
        ObsV.AsIntegerVector[i] := N.AsInteger[i];
        SumV.AsFloatVector[i]   := Sum.AsFloat[i];
        MeanV.AsFloatVector[i]  := Mean.AsFloat[i];
        SvV.AsFloatVector[i]    := StdVar.AsFloat[i];
        SdV.AsFloatVector[i]    := StdDev.AsFloat[i];
        CfilV.AsFloatVector[i]  := CfiL.AsFloat[i];
        CfihV.AsFloatVector[i]  := CfiH.AsFloat[i];
      end;
  end;

  Idx := MeanDataFile.Size + 1 + 1;

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
    for i := 0 to Size - 1 do
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


//End of F distribution functions


{
// F-test functions from Buxbaum, but missing some inbuilt functions
function FDIST (F : EpiFloat; f1, f2 : Integer) : EpiFloat;

var N, M          : Integer;
    S, X, A, B, V : EpiFloat;


     function R0 (M, N : Integer; V : EpiFloat) : EpiFloat;

     var G, Summe : EpiFloat;
         k, Test  : Integer;

     begin
         k := 1;
         G := 1;
         Summe := G;
         Test := M div 2;
         while not (k = Test) do
             begin
                 k := succ(k);
                 G := G * V * (N + 2 * k - 4) / (2 * k - 2);
                 Summe := Summe + G;
             end;
         R0 := Summe;
     end;


     function R1 (M : Integer; V : EpiFloat) : EpiFloat;

     var G, Summe : EpiFloat;
         k, Test  : Integer;

     begin
         k := 1;
         G := sqrt(V);
         Summe := G;
         Test := (M - 1) div 2;
         while not (k = Test) do
             begin
                 k := succ(k);
                 G := G * V * (2 * k - 2) / (2 * k - 1);
                 Summe := Summe + G;
             end;
         R1 := Summe;
     end;


     function R2 (f1, f2 : Integer; V : EpiFloat) : EpiFloat;

     var G, Summe : EpiFloat;
         k, Test  : Integer;

     begin
         k := 1;
         G := 1;
         Summe := G;
         Test := (f1 - 1) div 2;
         while not (k = Test) do
             begin
                 k := succ(k);
                 G := G * V * (f2 + 2 * k - 3) / (2 * k - 1);
                 Summe := Summe + G;
             end;
         R2 := Summe;
     end;


     function Q (f2 : Integer) : EpiFloat;

     var k       : Integer;
         Produkt : EpiFloat;

     begin
         Produkt := 1;
         for k := 1 to ((f2 - 1) div 2) do
            Produkt := Produkt * (k / (k - 0.5));
         Q := (2 / pi) * Produkt;
     end;


begin
    if (f1 mod 2) = 0
      then
         begin
             X := f2 / (f2 + f1 * F);
             S := 1 - (R0(f1, f2, 1-x) * Pot(X, f2/2));
         end
      else
         if (f2 mod 2) = 0
           then
              begin
                  X := f2 / (f2 + f1 * F);
                  S := Pot(1-x, f1/2) * R0(f2, f1, X);
              end
           else
              if (f1 = 1) and (f2 = 1)
                then
                   S := (2 / pi) * arctan(sqrt(F))
                else
                   if (f1 = 1) and (f2 > 1)
                     then
                        begin
                            X := sqrt(F / f2);
                            V := 1 / (1 + sqr(X));
                            S := (2 / pi) * (arctan(X) + X * R1(f2, V) *
                                 sqrt(1 / (1 + sqr(X))));
                        end
                     else
                        if (f1 > 1) and (f2 = 1)
                          then
                             begin
                                 X := sqrt(1 / (f1 * F));
                                 V := 1 / (1 + sqr(X));
                                 S := 1 - (2 / pi) * (arctan(X) + X * R1(f1, V)
                                      * sqrt(1 / (1 + sqr(X))));
                             end
                          else           {f1 und f2 ungerade und > 1}
                             begin
                                 X := sqrt(F * f1 / f2);
                                 V := 1 / (1 + sqr(X));
                                 A := (2 / pi) * (arctan(X) + X * R1(f2, V) *
                                      sqrt(1 / (1 + sqr(X))));
                                 V := sqr(X) / (1 + sqr(X));
                                 B := Q(f2) * X * Pot(1/(1+sqr(X)), (f2+1)/2) *
                                      R2(f1, f2, V);
                                 S := A - B;
                             end;
    FDIST := 1 - S;
end;  }
end.
