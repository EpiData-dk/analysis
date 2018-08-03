unit tables_stat_rr;
// TODO: {jamie} add CI for attack rates, option to show on table?
{$mode objfpc}{$H+}

interface
uses
  Classes, SysUtils, tables_types, outputcreator, epidatafilestypes, epidatafiles,
  epifields_helper, options_utils, result_variables, executor, ast;

type

  { TTwoWayStatisticRR }

  TTwoWayStatisticRR = class(TTwoWayStatistic)
  private
    FConf: Integer;
    FRRLL, FRRUL: EpiFloat;
    FOrgTable: TTwoWayTable;
    FRelativeRisk: EpiFloat;
    FAR0, FAR1: EpiFloat;
    FA, FAB, FC, FCD: Integer;
    FMessage: UTF8String;
    FRowVar: TEpiField;  // need this to get value for Exposure = yes
  public
    property Message: UTF8String read FMessage;
    procedure CalcTable(Table: TTwoWayTable;Conf: Integer); override;
    procedure AddToOutput(OutputTable: TOutputTable; Options: TOptionList); override;
    procedure CreateResultVariables(Executor: TExecutor; const NamePrefix: UTF8String); override;
  end;

  { TTwoWayStatisticsRR }

  TTwoWayStatisticsRR = class(TTwoWayStatistics)
  private
    FConf: Integer;
    FMHRR: EpiFloat;
    FMHRRLL, FMHRRUL: EpiFloat;
  protected
    function GetStatistics(const Index: Integer): TTwoWayStatisticRR; override;
    function GetTwoWayStatisticClass: TTwoWayStatisticClass; override;
  public
    procedure AddToSummaryTable(OutputTable: TOutputTable; Options: TOptionList); override;
    procedure AddToCompactTable(Executor: TExecutor; T: TOutputTable; RowIdx, ColIdx: Integer; Options: TOptionList); override;
    procedure AddToCompactHeader(T: TOutputTable; Options: TOptionList); override;
    procedure CalcSummaryStatistics(Tables: TTwoWayTables;Conf: Integer); override;
    procedure CreateSummaryResultVariables(Executor: TExecutor; const NamePrefix: UTF8STring); override;
    procedure AddCompactResultVariables(Executor: TExecutor; Index: Integer; Results: TStatResult); override;
    function  CreateCompactResultVariables(Executor: TExecutor; Prefix: UTF8String; ResultRows: Integer): TStatResult; override;
    property Statistics[Const Index: Integer]: TTwoWayStatisticRR read GetStatistics;
  end;


implementation

uses
  tables, epimiscutils, generalutils, statfunctions, Math;

{ CalcTable}

procedure TTwoWayStatisticRR.CalcTable(Table: TTwoWayTable;Conf: Integer);
var
  a, ab, c, cd, n: Integer;
  Zconf, r, s, dgr, vgr: EpiFloat;
Begin
  FOrgTable      := Table;
  FRelativeRisk  := TEpiFloatField.DefaultMissing;
  FRowVar        := FOrgTable.RowVariable;

  if (FOrgTable.ColCount <> 2) or (FOrgTable.RowCount <> 2 ) then
  begin
    FMessage := 'Table is not 2x2.';
    exit;
  end;

  Zconf := PNormalInv((1 - (Conf / 100)) / 2);
  FConf := Conf;
  a     := FOrgTable.Cell[0,0].N;
  ab    := FOrgTable.RowTotal[0];
  c     := FOrgTable.Cell[0,1].N;
  cd    := FOrgTable.RowTotal[1];
  FAR0  := a / ab;
  FAR1  := c / cd;
  FA    := a;
  FAB   := ab;
  FC    :=c;
  FCD   :=cd;

  if (cd = 0) or (ab = 0) or (FOrgTable.ColTotal[0] = 0) or (FOrgTable.ColTotal[1] = 0) then
  begin
    FMessage := 'Table has a zero marginal.';
    exit;
  end
  else
    FRelativeRisk := (a * cd) / (c * ab);

  // Greenland/Robins confidence limits
  n := FOrgTable.Total;
  r := a * cd;
  s := c * ab;
  if ((r = 0) or (s = 0)) then
  begin
    FRRUL := TEpiFloatField.DefaultMissing;
    FRRLL := FRRUL;
  end
  else
  begin
    dgr := ((ab * cd * (a + c)) - (a * c * n));
    vgr := abs(dgr / (r * s));
    FRRLL := exp(ln(FRelativeRisk) - (Zconf  * sqrt(vgr)));
    FRRUL := exp(ln(FRelativeRisk) + (Zconf  * sqrt(vgr)));
  end;
end;

procedure TTwoWayStatisticRR.AddToOutput(OutputTable: TOutputTable; Options: TOptionList);
var
  S: String;
begin
  if (FRelativeRisk = TEpiFloatField.DefaultMissing) then
     S := 'Cannot estimate Risk Ratio. ' + FMessage
  else
    S := 'Risk Ratio: '+ FormatRatio(FRelativeRisk, Options);
  if (not IsInfinite(FRelativeRisk)) then
    S += ' ' + FormatCI(FRRLL, FRRUL, FConf, Options);
  OutputTable.Footer.Text := OutputTable.Footer.Text + LineEnding + S;
end;

procedure TTwoWayStatisticRR.CreateResultVariables(Executor: TExecutor;
  const NamePrefix: UTF8String);
begin
  if (FRelativeRisk = TEpiFloatField.DefaultMissing) then exit;
  inherited CreateResultVariables(Executor, NamePrefix);

  Executor.AddResultConst(NamePrefix + 'RR', ftFloat).AsFloatVector[0] := FRelativeRisk;
end;

function TTwoWayStatisticsRR.GetStatistics(const Index: Integer
  ): TTwoWayStatisticRR;
begin
  result := TTwoWayStatisticRR(inherited GetStatistics(Index));
end;

function TTwoWayStatisticsRR.GetTwoWayStatisticClass: TTwoWayStatisticClass;
begin
  result := TTwoWayStatisticRR;
end;

procedure TTwoWayStatisticsRR.AddToSummaryTable(OutputTable: TOutputTable; Options: TOptionList);
var
  ColIdx, i: Integer;
  Stat: TTwoWayStatisticRR;
begin
  Stat := Statistics[0];
  with Stat do
  begin
    if (FRelativeRisk = TEpiFloatField.DefaultMissing) then exit;

    ColIdx := OutputTable.ColCount;
    OutputTable.ColCount := OutputTable.ColCount + 2;
    OutputTable.Cell[ColIdx    , 0].Text := 'Risk Ratio';
    OutputTable.Cell[ColIdx + 1, 0].Text := IntToStr(FConf) + '% CI';

    OutputTable.Cell[ColIdx    , 1].Text := FormatRatio(FRelativeRisk, Options);
    OutputTable.Cell[ColIdx + 1, 1].Text := FormatCI(FRRLL, FRRUL, 0, Options);
    if (StatisticsCount = 1) then begin
       OutputTable.Cell[ColIdx    , 2].Text := '-';   // will be replaced by M-H OR
       exit;
    end;

    for i := 1 to StatisticsCount - 1 do   // skips unstratified table
    begin
      Stat := Statistics[i];
      if (FRelativeRisk = TEpiFloatField.DefaultMissing) then
      begin
        OutputTable.Cell[ColIdx    , i + 2].Text := '-';
        OutputTable.Cell[ColIdx + 1, i + 2].Text := '';
      end
      else
      begin
        OutputTable.Cell[ColIdx    , i + 2].Text := FormatRatio(FRelativeRisk, Options);
        if (not IsInfinite(FRelativeRisk)) then
          OutputTable.Cell[ColIdx + 1, i + 2].Text := FormatCI(FRRLL, FRRUL, 0, Options);
      end;
    end;
  end;
  if (FMHRR = TEpiFLoatField.DefaultMissing) then exit;
  OutputTable.Cell[ColIdx,     2].Text := FormatRatio(FMHRR, Options);
  if (IsInfinite(FMHRR)) then exit;
  OutputTable.Cell[ColIdx + 1, 2].Text := FormatCI(FMHRRLL, FMHRRUL, 0, Options);
end;

procedure TTwoWayStatisticsRR.AddToCompactHeader(T: TOutputTable; Options: TOptionList);
var
  ColIdx: Integer;
  Stat: TTwoWayStatisticRR;

begin
  Stat := Statistics[0];
  if (T.RowCount <> 2) then exit;
  ColIdx     := T.ColCount;
  T.ColCount := ColIdx + 1;
  T.SetColAlignment(ColIdx, taCenter);
  T.Cell[ColIdx, 0].Text := 'E+';
  T.Cell[ColIdx, 1].Text := 'value';
  if (Options.HasOption('ar')) then
    begin
      ColIdx                      := T.ColCount;
      T.ColCount                  := ColIdx + 6;
      T.Cell[ColIdx     , 0].Text := 'E+';
      T.Cell[ColIdx + 3 , 0].Text := 'E-';
      T.Cell[ColIdx     , 1].Text := 'O+';
      T.Cell[ColIdx + 1 , 1].Text := 'O-';
      T.Cell[ColIdx + 2 , 1].Text := 'AR';
      T.Cell[ColIdx + 3 , 1].Text := 'O+';
      T.Cell[ColIdx + 4 , 1].Text := 'O-';
      T.Cell[ColIdx + 5 , 1].Text := 'AR';
     end;
  if (Options.HasOption('rr') or Options.HasOption('ar')) then
    begin
      ColIdx                      := T.ColCount;
      T.ColCount                  := ColIdx + 2;
      T.Cell[ColIdx     , 1].Text := 'RR';
      T.Cell[ColIdx + 1 , 1].Text := IntToStr(Stat.FConf) + '% CI';
      T.SetColAlignment(ColIdx + 1, taCenter);  // just header alignment
    end;

end;

procedure TTwoWayStatisticsRR.AddToCompactTable(Executor: TExecutor;
         T: TOutputTable; RowIdx, ColIdx: Integer; Options: TOptionList);
var
  i: Integer;
  Stat: TTwoWayStatisticRR;
  ValueLabelType: TEpiGetValueLabelType;
begin
  Stat := Statistics[0];
  with Stat do
  begin
    if (Message <> '') then
      T.Footer.Text := T.Footer.Text + LineEnding +
                       T.Cell[0,RowIdx].Text + ': ' + Message;

    // save value of exposure = yes for CTable
    ValueLabelType := ValueLabelTypeFromOptionList(Options, Executor.SetOptions);
    T.Cell[ColIdx, RowIdx].Text := FRowVar.GetValueLabel(0, ValueLabelType);
    T.SetColAlignment(ColIdx,taCenter); // must repeat this as not simple to do at the end

    // if Attack rates requested, output them now, based on unstratified table
    if (Options.HasOption('ar')) then
      begin
        T.Cell[ColIdx + 1, RowIdx].Text := IntToStr(FA);
        T.Cell[ColIdx + 2, RowIdx].Text := IntToStr(FAB);
        T.Cell[ColIdx + 3, RowIdx].Text := FormatRatio(FAR0, Options);
        T.Cell[ColIdx + 4, RowIdx].Text := IntToStr(FC);
        T.Cell[ColIdx + 5, RowIdx].Text := IntToStr(FCD);
        T.Cell[ColIdx + 6, RowIdx].Text := FormatRatio(FAR1, Options);
        ColIdx += 6;
      end;
    if (StatisticsCount = 1) then
    // unstratified - crude result
    begin
      if (FRelativeRisk = TEpiFloatField.DefaultMissing) then
      begin
        T.Cell[ColIdx + 1, RowIdx].Text := '-';
        T.Cell[ColIdx + 2, RowIdx].Text := '';
      end
      else
      begin
        T.Cell[ColIdx + 1, RowIdx].Text := FormatRatio(FRelativeRisk, Options);
        if (IsInfinite(FRelativeRisk)) then exit;
        T.Cell[ColIdx + 2, RowIdx].Text := FormatCI(FRRLL, FRRUL, 0, Options);
      end;
      exit;
      end;
  end;
  // stratified - summary result
  if (FMHRR = TEpiFLoatField.DefaultMissing) then exit;
  T.Cell[ColIdx + 1, RowIdx].Text := FormatRatio(FMHRR, Options);
  if (IsInfinite(FMHRR)) then exit;
  T.Cell[ColIdx + 2, RowIdx].Text := FormatCI(FMHRRLL, FMHRRUL, 0, Options);

end;

procedure TTwoWayStatisticsRR.CreateSummaryResultVariables(Executor: TExecutor;
  const NamePrefix: UTF8String);

begin
  if (StatisticsCount = 1) or (isInfinite(FMHRR)) or (FMHRR = TEpiFLoatField.DefaultMissing) then exit;
//  inherited CreateResultVariables(Executor, NamePrefix);

  Executor.AddResultConst(NamePrefix + 'MHRR',   ftFloat).AsFloatVector[0] := FMHRR;
  Executor.AddResultConst(NamePrefix + 'MHRRLL', ftFloat).AsFloatVector[0] := FMHRRLL;
  Executor.AddResultConst(NamePrefix + 'MHRRUL', ftFloat).AsFloatVector[0] := FMHRRUL;

end;

procedure TTwoWayStatisticsRR.CalcSummaryStatistics(Tables: TTwoWayTables;Conf: Integer);
var
  a, ab, c, cd, n: Integer;
  Tab: TTwoWayTable;
  r, s, SumR, SumS, SumV: EpiFloat;
  Zconf, variance: EpiFloat;
begin
  if (StatisticsCount = 1) then exit;   // No stratified tables
  Zconf := PNormalInv((1 - (Conf / 100)) / 2);
  FConf := Conf;
  SumV  := 0;
  SumR  := 0;
  SumS  := 0;

  for Tab in Tables do   // skips unstratified table
    begin
      with Tab do begin
        a := Tab.Cell[0,0].N;
        ab := Tab.RowTotal[0];
        c := Tab.Cell[0,1].N;
        cd := Tab.RowTotal[1];
        n := Tab.Total;
        if ((a + c) > 0) then
        begin
          r    := (a * cd) / n;
          s    := (c * ab) / n;
          SumR += r;
          SumS += s;
          SumV += ((cd * ab * (a + c)) - (a * c * n)) / (n * n);
        end;
      end;
    end;

  if (SumR = 0) and (SumS > 0) then FMHRR := 0
  else if (SumS = 0) then FMHRR := TEpiFloatField.DefaultMissing
  else FMHRR := SumR / SumS;

  // Estimate upper and lower confidence limits
  if ((SumR * SumS) = 0) then
  begin
    FMHRRUL := TEpiFloatField.DefaultMissing;
    FMHRRLL := TEpiFloatField.DefaultMissing;
  end
  else begin
    variance := abs(SumV / (SumR * SumS));
    FMHRRLL := exp(ln(FMHRR) - (Zconf * sqrt(variance)));
    FMHRRUL := exp(ln(FMHRR) + (Zconf * sqrt(variance)));
  end;
end;

function TTwoWayStatisticsRR.CreateCompactResultVariables(Executor: TExecutor; Prefix: UTF8String;
         ResultRows: Integer): TStatResult;

begin
  setlength(Result,3);
  Result[0] := Executor.AddResultVector(Prefix + 'rr', ftFloat, ResultRows);
  Result[1] := Executor.AddResultVector(Prefix + 'rrll', ftFloat, ResultRows);
  Result[2] := Executor.AddResultVector(Prefix + 'rrul', ftFloat, ResultRows);
end;

procedure TTwoWayStatisticsRR.AddCompactResultVariables(Executor: TExecutor;
          Index: Integer; Results: TStatResult);
var
  Stat: TTwoWayStatisticRR;
begin
  if (StatisticsCount = 1) then
  begin
    Stat := Statistics[0];
    with Stat do
    begin
      Results[0].AsFloatVector[Index] := FRelativeRisk;
      Results[1].AsFloatVector[Index] := FRRLL;
      Results[0].AsFloatVector[Index] := FRRUL;
    end;
    exit;
  end;
// stratified
  Results[0].AsFloatVector[Index] := FMHRR;
  Results[1].AsFloatVector[Index] := FMHRRLL;
  Results[0].AsFloatVector[Index] := FMHRRUL;
end;

initialization
  RegisterTableStatistic(tsRR, TTwoWayStatisticsRR);


end.

