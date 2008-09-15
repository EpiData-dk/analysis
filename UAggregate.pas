unit UAggregate;

interface

uses UVectors, SysUtils, Classes, AnsDatatypes, UCommands;

type
  TAggrFuncType = (afCount, afSum, afMean, afMinMax, afPercentile,
                   afSD, afSV, afIQR, afISR, afIDR, afDES, afMV, afUNKNOWN);

  TAggrFunc = class(TObject)
  private
    fAggrVariable: string;
    fResVariable: string;
    fResultVector: TEpiVector;
    fAggregateVector: TEpiVector;
    fFuncType: TAggrFuncType;
  public
    constructor Create(ResultVar, AggregateVar: string);
    procedure Execute(Idx: integer); virtual; abstract;
    procedure SetOutput(Idx: integer); virtual; abstract;
    procedure Reset(); virtual; abstract;
    procedure CreateResultVector(Dataframe: TEpiDataframe); virtual; abstract;
    function ContainVarname(const Varname: string): boolean; virtual;
    function EFormat(Ints, DefDecimal: Integer): string;
    property ResultVariable: string read fResVariable;
    property AggregateVariable: string read fAggrVariable;
    property AggregateVector: TEpiVector read fAggregateVector write fAggregateVector;
    property FuncType: TAggrFuncType read fFuncType;

  end;

  TAggrSum = class(TAggrFunc)
  private
    Sum: EpiFloat;
  public
    constructor Create(ResultVar, AggregateVar: string);
    procedure Execute(Idx: integer); override;
    procedure SetOutput(Idx: integer); override;
    procedure Reset(); override;
    procedure CreateResultVector(Dataframe: TEpiDataframe);  override;
  end;

  TAggrCountType = (acMissing, acMissingValue, acNotMissing, acAll);

  TAggrCount = class(TAggrFunc)
  private
    Count: EpiInt;
    CountType: TAggrCountType;
  public
    constructor Create(ResultVar, AggregateVar: string; ACountType: TAggrCountType);
    procedure Execute(Idx: integer); override;
    procedure SetOutput(Idx: integer); override;
    procedure Reset(); override;
    procedure CreateResultVector(Dataframe: TEpiDataframe); override;
  end;

  TAggrMeanType = (amMean, amMCI, amStdVar, amStdDev);

  TAggrMean = class(TAggrFunc)
  private
    Sum, Stdvar: EpiFloat;
    Count: EpiInt;
    MeanType: TAggrMeanType;
    fLowerCI, fUpperCI: TEpiVector;
  public
    constructor Create(ResultVar, AggregateVar: string; AMeanType: TAggrMeanType);
    procedure Execute(Idx: integer); override;
    procedure SetOutput(Idx: integer); override;
    function ContainVarname(const Varname: string): boolean; override;
    procedure Reset(); override;
    procedure CreateResultVector(Dataframe: TEpiDataframe); override;
  end;

  TAggrMinMax = class(TAggrFunc)
  private
    Value: EpiFloat;
    Minimum: Boolean;
  public
    constructor Create(ResultVar, AggregateVar: string; FindMin: boolean);
    procedure Execute(Idx: integer); override;
    procedure SetOutput(Idx: integer); override;
    procedure Reset(); override;
    procedure CreateResultVector(Dataframe: TEpiDataframe); override;
  end;

  TAggrPercentileType = (ap1, ap5, ap10, ap25, ap50, ap75, ap90, ap95, ap99);

  TAggrPercentile = class(TAggrFunc)
  private
    Count, LastCount, MissingCount: EpiInt;
    PercentileType: TAggrPercentileType;
  public
    constructor Create(ResultVar, AggregateVar: string; APercentileType: TAggrPercentileType);
    procedure Execute(Idx: integer); override;
    procedure SetOutput(Idx: integer); override;
    procedure Reset(); override;
    procedure CreateResultVector(Dataframe: TEpiDataframe); override;
  end;

  TAggrList = class
  private
    fList: TList;
    function GetItem(const index: integer): TAggrFunc;
    function Unique(Func: TAggrFunc): boolean;
    function Extract(Index: Integer): TAggrFunc;
  public
    constructor Create();
    destructor Destroy(); override;
    procedure Add(Func: TAggrFunc);
    function ExtractPercentiles(): TAggrList;
    function IndexOf(const Name: string): Integer;
    procedure Insert(const Index: integer; Func: TAggrFunc);
    procedure SetOutputs(Idx: integer);
    procedure ResetAll();
    function Count(): integer;
    function GetVarList(): TStrings;
    property Items[const Index: integer]: TAggrFunc read GetItem;
  end;

  TAggregate = class(TObject)
  private
    fNameCounter: Integer;
    fAggrOptions: TStrings;
    function VarnameIsAggrFunc(const Varname: string): boolean;
    function VarnameToAggrFuncType(const Varname: string): TAggrFuncType;
    function CreateAggrFunc(const AggFuncName, VarName: string; Dest: TAggrList): TAggrFunc;
    function ResizeVarname(prefix, name: string): string;
  protected
    //
  public
    Cmd: TCommand;
    constructor Create();
    destructor Destroy();
    function DoAggregate(Dataframe: TEpiDataframe; ByVars: TStrings; Cmd: TCommand): TEpiDataframe;
    function DoStatTable(Dataframe: TEpiDataframe; Varnames, ByVars: TStrings; Cmd: TCommand): TEpiDataFrame;
    procedure OutAggregate(Df: TEpiDataframe);
    procedure OutStattables(Df, OrgDF: TEpiDataframe; AggList: TAggrList);
    function AggregateDataframe(Dataframe: TEpiDataframe; AggrByVars: TStringList; AggrList: TAggrList; aCmd: TCommand): TEpiDataframe;
  end;


var
  OAggregate: TAggregate;

implementation

uses Math, UFrames, UEpiDatatypes, UOutput, UCmdProcessor, UDebug, UAnaToken, GeneralUtils,
     UStatFunctions;

const
  UnitName = 'UAggregate';

constructor TAggregate.Create();
begin
  fAggrOptions := TStringList.Create();
  fNameCounter := 0;
  SplitString(AGGR_STAT_OPTIONS, fAggrOptions);
end;

destructor TAggregate.Destroy();
begin
  FreeAndNil(fAggrOptions);
end;

function TAggregate.DoAggregate(Dataframe: TEpiDataframe; ByVars: TStrings; Cmd: TCommand): TEpiDataframe;
var
  v: TEpiVector;
  Agl: TAggrList;
  i, j: integer;
  Varnames, Dropnames, AllVars: TStrings;
  dummy: boolean;
const
  procname = 'DoAggregate';
  procversion = '1.0.0.1';
begin
  ODebug.IncIndent;
  ODebug.Add(UnitName + ':' + procname + ' - ' + procversion, 1);
  Agl := nil;
  Dropnames := nil;
  AllVars := nil;
  dummy := false;
  self.Cmd := Cmd;

  try
    if ByVars = Nil then
    begin
      v := TEpiIntVector.Create('$AGGDummy', dataframe.RowCount);
      for i := 1 to dataframe.RowCount do
        v.AsInteger[i] := 0;
      dataframe.Vectors.Add(v);
      ByVars := TStringList.Create();
      ByVars.Add('$AGGDummy');
      Dummy := true;
      dropnames := TStringList.Create;
      dropnames.Add('$AGGDummy');
    end;

    AllVars := TStringList.Create;
    Varnames := TStringList.Create;
    Agl := TAggrList.Create();
    Agl.Add(TAggrCount.Create('N', '', acAll));

    for i := 0 to Cmd.ParameterList.Count -1 do
    begin
      if not VarnameIsAggrFunc(Cmd.Param[i].VarName) then continue;
      SplitString(Cmd.Param[i].AsString, Varnames, [' ', ',']);
      for j := 0 to Varnames.Count -1 do
      begin
        if AllVars.IndexOf(Varnames[j]) = -1 then
        begin
          AllVars.Add(Varnames[j]);
          Agl.Add(TAggrCount.Create(ResizeVarname('N', varnames[j]), varnames[j], acNotMissing));
        end;
        CreateAggrFunc(Cmd.Param[i].VarName, Varnames[j], Agl);
      end;
    end;

    result := AggregateDataframe(Dataframe, TStringList(ByVars), agl, Cmd);
    if dummy then result.DropVectors(dropnames);
    v := result.FindVector('N');
    if v <> nil then
      v.VariableLabel := '(N) Total observations used in aggregate';
    result.DataLabel := 'Aggregated: ' + dataframe.FileName + ' ' + dataframe.DataLabel;

    OutAggregate(result);
  finally
    if Assigned(DropNames) then FreeAndNil(DropNames);
    if Assigned(Agl) then FreeAndNil(Agl);
    self.Cmd := nil; 
    v := nil;
    ODebug.DecIndent;
  end;
end;

function TAggregate.DoStatTable(Dataframe: TEpiDataframe; Varnames, ByVars: TStrings; Cmd: TCommand): TEpiDataFrame;
var
  Agl: TAggrList;
  FuncNames: TStrings;
  i, j: integer;
  v: TEpiVector;
const
  procname = 'DoStatTable';
  procversion = '1.0.0.0';
begin
  ODebug.IncIndent;
  ODebug.Add(UnitName, Self.ClassName, procname, procversion, 1);

  Agl := nil;
  FuncNames := nil;
  self.Cmd := Cmd;

  try
    Agl := TAggrList.Create();

    if Cmd.ParamExists['N'] then
      Agl.Add(TAggrCount.Create('N', '', acAll));
    FuncNames := TStringList.Create;

    if Cmd.ParamExists['STAT'] then
      SplitString(Cmd.ParamByName['STAT'].AsString, FuncNames, [' ', ',']);

    for i := 0 to Varnames.Count -1 do
    begin
      if CMd.ParamExists['NV'] then
        Agl.Add(TAggrCount.Create(ResizeVarname('N', Varnames[i]), Varnames[i], acNotMissing));
      for j := 0 to FuncNames.Count -1 do
      begin
        CreateAggrFunc(AnsiUpperCase(FuncNames[j]), Varnames[i], Agl);
      end;
    end;

    result := AggregateDataframe(Dataframe, TStringList(ByVars), agl, Cmd);
    v := result.FindVector('N');
    if Assigned(V) then
      v.VariableLabel := 'N';
    result.DataLabel := 'Aggregated: ' + dataframe.FileName + ' ' + dataframe.DataLabel;

    OutStattables(result, dataframe, agl);
  finally
    if Assigned(Agl) then FreeAndNil(Agl);
    if Assigned(FuncNames) then FreeAndNil(FuncNames);
    self.Cmd := nil; 
    v := nil;
    ODebug.DecIndent;
  end;               
end;

procedure TAggregate.OutAggregate(Df: TEpiDataframe);
var
  i, j: integer;
  xtab: TStatTable;
  val: IValue;
  vns: TStrings;
  fmt: string;
const
  procname = 'OutAggregate';
  procversion = '1.0.0.1';
begin
  ODebug.IncIndent;
  ODebug.Add(UnitName + ':' + procname + ' - ' + procversion, 1);
  if df <> nil then
  begin
    if (cmd.ParamByName['Q'] = nil) then
    begin
      xtab:=dm.OutputList.NewTable(df.VectorCount, df.RowCount+1);
      xtab.TableType := sttNormal;

      fmt := '%8.2f';
      if Cmd.ParamByName['D0']<>nil then fmt := '%8.0f'
      else if Cmd.ParamByName['D1']<>nil then fmt := '%8.1f'
      else if Cmd.ParamByName['D2']<>nil then fmt := '%8.2f';

      for i:= 0 to df.VectorCount-1 do
        xtab.Cell[i+1,1] := Trim(df.Vectors[i].Name);

      for i:= 0 to df.VectorCount-1 do
        for j:= 1 to df.RowCount do
          if df.Vectors[i].DataType in [EpiTyFloat] then
            xtab.Cell[i+1,j+1] := Trim(Format(fmt, [df.Vectors[i].AsFloat[j]]))
          else
            xtab.Cell[i+1,j+1] := trim(df.Vectors[i].GetValueLabel(Trim(df.Vectors[i].AsString[j]), Cmd.ParameterList));

      dm.CodeMaker.OutputTable(xtab, '');
      dm.Sendoutput;
      FreeAndNil(xtab);
    end;

    val := cmd.ParamByName['SAVE'];
    if (val <> nil) then
    begin
      vns := TStringList.Create;
      df.Vectors.GetVectorNames(vns);
      dm.SaveDataFile(val.AsString, vns, df, Cmd);
      FreeAndNil(vns);
    end;
  end;
  ODebug.DecIndent;
end;

procedure TAggregate.OutStattables(Df, OrgDF: TEpiDataFrame; AggList: TAggrList);
var
  i, j, s: integer;
  xTab: TStatTable;
  HeaderList, ByVars: TStrings;
  SVec: TEpiVector;

  function AdjustHeader(Vec: TEpiVector): string;
  var
    idx: integer;
    af: TAggrFunc;
  begin
    idx := AggList.IndexOf(Vec.Name);
    if idx  > -1 then
    begin
      af := AggList.Items[idx];
      result := af.AggregateVector.GetVariableLabel(Cmd.ParameterList);
      case af.FuncType of
        afCount       : result := result + '<br>Count';
        afSum         : result := result + '<br>Sum';
        afMean        : case TAggrMean(af).MeanType of
                          amMean   : result := result + '<br>Mean';
                          amMCI    : begin
                                       if Vec = TAggrMean(af).fLowerCI then
                                         result := result + '<br>Lower 95% CI'
                                       else if Vec = TAggrMean(af).fUpperCI then
                                         result := result + '<br>Upper 95% CI'
                                       else
                                         result := result + '<br>Mean';
                                     end;
                          amStdVar : result := result + '<br>Std. Variance';
                          amStdDev : result := result + '<br>Std. Deviance';
                        end;
        afMinMax      : if TAggrMinMax(af).Minimum then
                          result := result + '<br>Min'
                        else
                          result := result + '<br>Max';
        afPercentile  : case TAggrPercentile(af).PercentileType of
                          ap1   : result := result + '<br>P1';
                          ap5   : result := result + '<br>P5';
                          ap10  : result := result + '<br>P10';
                          ap25  : result := result + '<br>P25';
                          ap50  : result := result + '<br>Median';
                          ap75  : result := result + '<br>P75';
                          ap90  : result := result + '<br>P90';
                          ap95  : result := result + '<br>P95';
                          ap99  : result := result + '<br>P99';
                        end;
      end;
    end else
      result := Vec.GetVariableLabel(Cmd.ParameterList);
  end;

const
  procname = 'OutStattables';
  procversion = '1.0.0.1';
begin
  ODebug.IncIndent;
  ODebug.Add(UnitName, self.ClassName, ProcName, ProcVersion, 1);

  HeaderList := nil;
  ByVars := nil;
  s := 0;
  try
    HeaderList := TStringList.Create();
    if Cmd.ParamExists['HEADER'] then
      SplitString(Cmd.ParamByName['HEADER'].AsString, HeaderList, [',']);

    ByVars := TStringList.Create();
    if Cmd.ParamExists['BY'] then
      SplitString(Cmd.ParamByName['BY'].AsString, ByVars, [' ', ',']);

    if Cmd.ParamExists['STRATA'] then
    begin
      Inc(s);
      SVec := Df.VectorByName[Cmd.ParamByName['STRATA'].AsString];
    end else
      SVec := TEpiIntVector.Create('$S', Df.RowCount);

    For i := 0 to Math.Min(HeaderList.Count, Df.VectorCount) - (s+1) do
      Df.Vectors[i+s].VariableLabel := HeaderList[i];

    xTab := dm.CodeMaker.Output.NewTable(Df.VectorCount-s, 1);
    if s = 1 then
      xTab.Caption := SVec.GetVariableLabel(Cmd.ParameterList) +
                      SVec.GetValueLabel(SVec.AsString[1], Cmd.ParameterList);
    xTab.TableType := sttNormal;

    // Output Headers
    For i := 1 to xTab.ColCount do
      xTab.Cell[i, 1] := AdjustHeader(Df.Vectors[i-1+s]); //Df.Vectors[i-1+s].GetVariableLabel(Cmd.ParameterList);

    for j := 1 to df.RowCount do
    begin
      xTab.AddRow;


      for i := 0 to (ByVars.Count -1) do
        if j = 1 then
          xtab.Cell[i+1, XTab.RowCount] := trim(df.Vectors[i].GetValueLabel(Trim(df.Vectors[i].AsString[j]), Cmd.ParameterList))
        else
          if (df.Vectors[i].compare(j-1, j) = 0) then
            xtab.Cell[i+1, XTab.RowCount] := ''
          else
           xtab.Cell[i+1, XTab.RowCount] := trim(df.Vectors[i].GetValueLabel(Trim(df.Vectors[i].AsString[j]), Cmd.ParameterList));


      // Output Content.
      for i:= (s + ByVars.Count) to (df.VectorCount-1) do
        xtab.Cell[i+1, XTab.RowCount] := trim(df.Vectors[i].GetValueLabel(Trim(df.Vectors[i].AsString[j]), Cmd.ParameterList));

      if (j<df.RowCount) and (SVec.compare(j, j+1) <> 0) then
      begin
        dm.CodeMaker.OutputTable(xTab);

        if Cmd.ParamExists['PAGE'] then
          dm.WriteDirect('<p style="page-break-after : always ; visibility : hidden ">&nbsp;</p>');

        xTab := dm.CodeMaker.Output.NewTable(Df.VectorCount-s, 1);
        xTab.Caption := SVec.GetVariableLabel(Cmd.ParameterList) + ': ' +
                        SVec.GetValueLabel(SVec.AsString[j+1], Cmd.ParameterList);
        xTab.TableType := sttNormal;
        For i := 1 to xTab.ColCount do
          xTab.Cell[i, 1] := Df.Vectors[i-1+s].GetVariableLabel(Cmd.ParameterList);
      end;
    end;
    dm.CodeMaker.OutputTable(xTab);
    dm.Sendoutput();
  finally
    if Assigned(xTab) then FreeAndNil(xTab);
    if Assigned(HeaderList) then FreeAndNil(HeaderList);
    ODebug.DecIndent;
  end;
end;

function TAggregate.AggregateDataframe(Dataframe: TEpiDataframe; AggrByVars: TStringList; AggrList: TAggrList; aCmd: TCommand): TEpiDataframe;
var
  sortlist: TEpiVectors;
  vector: TEpiVector;
  i, j, k, co, vco, levels, level: integer;
  AggrFunc: TAggrFunc;
  PercList: TAggrList;

  Function LevelChanged:boolean;
  var
    iv:  integer;
  begin
     result:=true;
     for iv:= 0 to vco-1 do
        if sortlist[iv].compare(i,i-1)<>0 then exit;
     result:=false;
  end;

  Procedure AssignByVarValues(FromVector, ToVector: TEpiVector; FromIdx, ToIdx: integer);
  begin
    case FromVector.DataType of
      EpiTyInteger: ToVector.AsInteger[ToIdx] := FromVector.AsInteger[FromIdx];
      EpiTyFloat:   ToVector.AsFloat[ToIdx]   := FromVector.AsFloat[FromIdx];
      EpiTyDate:    ToVector.AsDate[ToIdx]    := FromVector.AsDate[FromIdx];
      EpiTyString, EpiTyUppercase,
      EpiTyByte:    ToVector.AsString[ToIdx]  := FromVector.AsString[FromIdx];
      EpiTyBoolean: ToVector.AsByte[ToIdx]    := FromVector.AsByte[FromIdx];
    end; //case

  end;

const
  procname = 'AggregateDataframe';
  procversion = '1.0.0.0';
begin
  ODebug.IncIndent;
  ODebug.Add(UnitName + ':' + procname + ' - ' + procversion, 1);

  if aCmd = nil then
    exit;
  if aCmd <> Self.Cmd then
    Self.Cmd := aCmd;

  try
    result := nil;
    if Dataframe = nil then exit;
    if AggrByVars = nil then exit;
    if AggrList = nil then exit;

    sortlist := dataframe.GetVectorListByName(AggrByVars);
    vco := sortlist.Count;

    co := AggrByVars.Count;
    AggrByVars.Duplicates := dupIgnore;

    dataframe.Sort(AggrByVars.CommaText);
    for j := 0 to AggrByVars.Count-co-1 do
      AggrByVars.Delete(AggrByVars.Count-1);

    // Find the number of stratifications (levels)!
    co := dataframe.RowCount;
    levels := 1;
    i := 2;
    while i <= co do
    begin
      if LevelChanged then
        inc(levels);
      inc(i);
    end;

    result := TEpiDataframe.CreateTemp(levels);
    Dataframe.CheckProperties.Clone(result.CheckProperties);
    for i:=0 to AggrByVars.Count-1 do
    begin
      Vector := Dataframe.VectorByName[AggrByVars[i]].Clone(result, true);
      Vector.Length := levels;
      Result.Vectors.Add(Vector, i);
    end;
    Result.Sort(sortlist);


    for i:=0 to AggrList.Count -1 do
    begin
      AggrFunc := TAggrFunc(AggrList.Items[i]);
      if AggrFunc.AggregateVariable <> '' then
        AggrFunc.AggregateVector := Dataframe.VectorByName[aggrfunc.AggregateVariable];
      AggrFunc.CreateResultVector(result);
    end;

    // Percentile calculations not correct yet... :(
    PercList := AggrList.ExtractPercentiles();

    // AGGREGATE!!! Yeeeha.
    // Do "normal" aggregate operations (this means without any percentile calc.
    level := 1;
    for i := 1 to co do
    begin
      if (i>1) and (LevelChanged) then
      begin
        for j:=0 to AggrByVars.Count-1 do
          AssignByVarValues(dataframe.VectorByName[AggrByVars[j]], result.VectorByName[AggrByVars[j]],
                            i-1, level);
        AggrList.SetOutputs(level);
        AggrList.ResetAll();
        inc(level);
      end;
      for j := 0 to AggrList.Count -1 do
      begin
        AggrFunc := TAggrFunc(AggrList.Items[j]);
        AggrFunc.Execute(i);
      end;
    end;
    for j:=0 to AggrByVars.Count-1 do
      AssignByVarValues(dataframe.VectorByName[AggrByVars[j]], result.VectorByName[AggrByVars[j]],
                        i-1, level);
    AggrList.SetOutputs(level);
    AggrList.ResetAll();


    for k := 0 to PercList.Count -1 do
    begin
      AggrByVars.Add(TAggrFunc(PercList.Items[k]).AggregateVariable);
      dataframe.Sort(AggrByVars.CommaText);
      AggrByVars.Delete(AggrByVars.Count-1);
      level := 1;
      for i := 1 to co do
      begin
        if (i>1) and (LevelChanged) then
        begin
          for j:=0 to AggrByVars.Count-1 do
            AssignByVarValues(dataframe.VectorByName[AggrByVars[j]], result.VectorByName[AggrByVars[j]],
                              i-1, level);
          TAggrFunc(PercList.Items[k]).SetOutput(level);
          TAggrFunc(PercList.Items[k]).Reset();
          inc(level);
        end;
        TAggrFunc(PercList.Items[k]).Execute(i);
      end;
      for j:=0 to AggrByVars.Count-1 do
        AssignByVarValues(dataframe.VectorByName[AggrByVars[j]], result.VectorByName[AggrByVars[j]],
                          i-1, level);
      TAggrFunc(PercList.Items[k]).SetOutput(level);
      TAggrFunc(PercList.Items[k]).Reset();
      AggrList.Add(TAggrFunc(PercList.Items[k]));
    end;
  finally
    if Assigned(sortlist) then FreeAndNil(sortlist);
//    if Assigned(PercList) then FreeAndNil(PercList);
  end;
  ODebug.DecIndent;
end;

function TAggregate.VarnameIsAggrFunc(const Varname: string): boolean;
var
  i: integer;
const
  procname = 'VarnameIsAggrFunc';
  procversion = '1.0.0.0';
begin
  ODebug.Add(UnitName, self.ClassName, procname, procversion, 1);
  result := true;
  for i := 0 to fAggrOptions.Count -1 do
    if AnsiCompareText(Varname, fAggrOptions[i]) = 0 then
      exit;
  result := false;
end;

function TAggregate.VarnameToAggrFuncType(const Varname: string): TAggrFuncType;
var
  s: string;
const
  procname = 'VarnameToAggrFuncType';
  procversion = '1.0.0.0';
begin
  ODebug.Add(UnitName, self.ClassName, procname, procversion, 1);
  s := AnsiUppercase(varname);
  if s = 'MEAN' then result := afMean
  else if s = 'MCI' then result := afMean
  else if s = 'SD' then result := afSD
  else if s = 'SV' then result := afSV
  else if s = 'MEDIAN' then result := afPercentile
  else if s = 'SUM' then result := afSum
  else if s[1] = 'P' then result := afPercentile
  else if s = 'MIN' then result := afMinMax
  else if s = 'MAX' then result := afMinMax
  else if s = 'ISR' then result := afISR
  else if s = 'IQR' then result := afIQR
  else if s = 'IDR' then result := afIDR
  else if s = 'DES' then result := afDES
  else if s = 'MV' then result := afMV
  else result := afUNKNOWN;
end;

function TAggregate.CreateAggrFunc(const AggFuncName, VarName: string; Dest: TAggrList): TAggrFunc;
begin
  Case VarnameToAggrFuncType(AggFuncName) of
    afSum:           Dest.Add(TAggrSum.Create(ResizeVarname('SUM',Varname),Varname));
    afMean:          if AggFuncName = 'MCI' then
                       Dest.Add(TAggrMean.Create(ResizeVarname('MEA',VarName),VarName, amMCI))
                     else
                       Dest.Add(TAggrMean.Create(ResizeVarname('MEA',VarName),VarName, amMean));
    afMinMax:        if AggFuncName = 'MIN' then
                       Dest.Add(TAggrMinMax.Create(ResizeVarname('MIN', VarName), VarName, true))
                     else
                       Dest.Add(TAggrMinMax.Create(ResizeVarname('MAX', VarName), VarName, false));
    afPercentile:    begin
                       case AggFuncName[2] of
                         '1':  if length(AggFuncName) = 3 then
                                 Dest.Add(TAggrPercentile.Create(ResizeVarname('P10',VarName),VarName, ap10))
                               else
                                 Dest.Add(TAggrPercentile.Create(ResizeVarname('P1',VarName),VarName, ap1));
                         '2':  Dest.Add(TAggrPercentile.Create(ResizeVarname('P25',VarName),VarName, ap25));
                         'E':  Dest.Add(TAggrPercentile.Create(ResizeVarname('MED',VarName),VarName, ap50)); // mEdian... :)
                         '5':  if length(AggFuncName) = 3 then
                                 Dest.Add(TAggrPercentile.Create(ResizeVarname('MED',VarName),VarName, ap50))
                               else
                                 Dest.Add(TAggrPercentile.Create(ResizeVarname('P5',VarName),VarName, ap5));
                         '7':  Dest.Add(TAggrPercentile.Create(ResizeVarname('P75',VarName),VarName, ap75));
                         '9':  if AggFuncName[3] = '0' then
                                 Dest.Add(TAggrPercentile.Create(ResizeVarname('P90',VarName),VarName, ap90))
                               else if AggFuncName[3] = '5' then
                                 Dest.Add(TAggrPercentile.Create(ResizeVarname('P95',VarName),VarName, ap95))
                               else
                                 Dest.Add(TAggrPercentile.Create(ResizeVarname('P99',VarName),VarName, ap99));
                       end;
                     end;
    afSD:            Dest.Add(TAggrMean.Create(ResizeVarname('SD',VarName),VarName, amStdDev));
    afSV:            Dest.Add(TAggrMean.Create(ResizeVarname('SV',VarName),VarName, amStdVar));
    afIQR:           begin
                       Dest.Add(TAggrPercentile.Create(ResizeVarname('P25',VarName),VarName, ap25));
                       Dest.Add(TAggrPercentile.Create(ResizeVarname('P75',VarName),VarName, ap75));
                     end;
    afIDR:           begin
                       Dest.Add(TAggrPercentile.Create(ResizeVarname('P10',VarName),VarName, ap10));
                       Dest.Add(TAggrPercentile.Create(ResizeVarname('P90',VarName),VarName, ap90));
                     end;
    afISR:           begin
                       Dest.Add(TAggrPercentile.Create(ResizeVarname('P5',VarName),VarName, ap5));
                       Dest.Add(TAggrPercentile.Create(ResizeVarname('P95',VarName),VarName, ap95));
                     end;
    afDES:           begin
                       Dest.Add(TAggrMinMax.Create(ResizeVarname('MIN',VarName),VarName, true));
                       Dest.Add(TAggrPercentile.Create(ResizeVarname('MED',VarName),VarName, ap50));
                       Dest.Add(TAggrMinMax.Create(ResizeVarname('MAX',VarName),VarName, false));
                     end;
    afMV:            begin
                       Dest.Add(TAggrCount.Create(ResizeVarname('MIS',VarName),VarName, acMissing));
                       Dest.Add(TAggrCount.Create(ResizeVarname('MVD',VarName),VarName, acMissingValue));
                     end;
    afUNKNOWN:       dm.Error('Unknown aggregate function: %s', [AggFuncName], 100001);
  end;
end;

function TAggregate.ResizeVarname(prefix, name: string): string;
const
  procname = 'ResizeVarname';
  procversion = '1.0.0.0';
begin
  ODebug.IncIndent;
  ODebug.Add(UnitName, Self.ClassName, procname, procversion, 1);
  if length(prefix+name) < 10 then
    begin result := prefix+name; exit; end;
  if pos(prefix, name) = 1 then
    begin result := name; exit; end;
  if fNameCounter = 99 then
    dm.Error('Aggregate: Too many variables used.', [], 100002);
  inc(fNameCounter);
  result := prefix + copy(name, 1, 5) + IntToStr(fNameCounter);
  ODebug.DecIndent;
end;

{****************************
 * TAggrFunc implementation *
 ****************************}

constructor TAggrFunc.Create(ResultVar, AggregateVar: string);
const
  procname = 'Create';
  procversion = '1.0.0.0';
begin
  ODebug.Add(UnitName, self.ClassName, procname, procversion, 3);
  fAggrVariable := AggregateVar;
  fResVariable := ResultVar;
  Reset();
end;

function TAggrFunc.ContainVarname(const Varname: string): boolean;
begin
  result := AnsiCompareStr(Varname, ResultVariable) = 0;
end;

function TAggrFunc.EFormat(Ints, DefDecimal: Integer): string;
begin
  Result := '%' + Format('%d.', [Ints]);

  if OAggregate.Cmd.ParamExists['E0'] then
    result := result + '0'
  else if OAggregate.Cmd.ParamExists['E1'] then
    result := result + '1'
  else if OAggregate.Cmd.ParamExists['E2'] then
    result := result + '2'
  else if OAggregate.Cmd.ParamExists['E3'] then
    result := result + '3'
  else if OAggregate.Cmd.ParamExists['E4'] then
    result := result + '4'
  else
    result := result + Format('%d', [DefDecimal]);
  result := result + 'f';
end;

{****************************
 * TAggrList implementation *
 ****************************}

constructor TAggrList.Create();
const
  procname = 'Create';
  procversion = '1.0.0.0';
begin
  ODebug.Add(UnitName, self.ClassName, procname, procversion, 3);
  inherited create();
  fList := TList.Create();
end;

destructor TAggrList.Destroy();
begin
  if Assigned(fList) then FreeAndNil(fList);
  inherited;
end;

function TAggrList.GetItem(const index: integer): TaggrFunc;
const
  procname = 'GetItem';
  procversion = '1.0.0.0';
begin
  ODebug.IncIndent;
  ODebug.Add(UnitName, self.ClassName, procname, procversion, 3);
  result := TAggrFunc(fList.Items[index]);
  ODebug.DecIndent;
end;

function TAggrList.GetVarList(): TStrings;
var
  i: integer;
const
  procname = 'GetVarList';
  procversion = '1.0.0.0';
begin
  ODebug.IncIndent;
  ODebug.Add(UnitName, self.ClassName, procname, procversion, 1);
  Result := TStringList.Create;
  for i:=0 to Count-1 do
  begin
    if TAggrFunc(Items[i]).AggregateVariable = '' then continue;
    if Result.IndexOf(TAggrFunc(Items[i]).AggregateVariable) = -1 then
      Result.Add(TAggrFunc(Items[i]).AggregateVariable);
  end;
  ODebug.DecIndent;
end;

function TAggrList.Unique(Func: TAggrFunc): boolean;
var
  i: integer;
const
  procname = 'Add';
  procversion = '1.0.0.0';
begin
  ODebug.IncIndent;
  ODebug.Add(UnitName, self.ClassName, procname, procversion, 1);
  result := true;
  for i := 0 to Count -1 do
    if AnsiCompareStr(Items[i].ResultVariable, Func.ResultVariable) = 0 then
      begin result := false; break; end;
end;

function TAggrList.Extract(Index: Integer): TAggrFunc;
begin
  Result := TAggrFunc(fList.Extract(fList.Items[Index]));
end;

procedure TAggrList.Add(Func: TAggrFunc);
const
  procname = 'Add';
  procversion = '1.1.0.0';
begin
  ODebug.IncIndent;
  ODebug.Add(UnitName, self.ClassName, procname, procversion, 1);
  if Unique(Func) then
    fList.Add(Func)
  else
    dm.Error('Aggregate function already used for %s', [Func.AggregateVariable], 100003);
  ODebug.DecIndent;
end;

function TAggrList.ExtractPercentiles(): TAggrList;
var
  i: integer;
const
  procname = 'ExtractPercentiles';
  procversion = '1.0.0.0';
begin
  ODebug.IncIndent;
  ODebug.Add(UnitName, self.ClassName, procname, procversion, 1);
  Result := TAggrList.Create;
  for i := Count - 1 downto 0 do
    if Items[i].FuncType = afPercentile then
      result.Add(Extract(i));
  ODebug.DecIndent;
end;

function TAggrList.IndexOf(const Name: string): integer;
begin
  for result := 0 to fList.Count -1 do
    if Items[result].ContainVarname(Name) then exit;
  result := -1;
end;

procedure TAggrList.Insert(const Index: integer; Func: TAggrFunc);
const
  procname = 'Insert';
  procversion = '1.0.0.0';
begin
  ODebug.IncIndent;
  ODebug.Add(UnitName, self.ClassName, procname, procversion, 1);

  if Unique(Func) then
    fList.Insert(index, Func)
  else
    dm.Error('Aggregate function already used for %s', [Func.AggregateVariable], 100003);
  ODebug.DecIndent;
end;

procedure TAggrList.SetOutputs(Idx: integer);
var
  i: integer;
const
  procname = 'SetOutputs';
  procversion = '1.0.0.0';
begin
  ODebug.IncIndent;
  ODebug.Add(UnitName, self.ClassName, procname, procversion, 1);
  for i := 0 to Count-1 do
    TAggrFunc(Items[i]).SetOutput(Idx);
  ODebug.DecIndent;
end;

procedure TAggrList.ResetAll();
var
  i: integer;
const
  procname = 'ResetAll';
  procversion = '1.0.0.0';
begin
  ODebug.IncIndent;
  ODebug.Add(UnitName, self.ClassName, procname, procversion, 1);
  for i := 0 to Count-1 do
    TAggrFunc(Items[i]).Reset();
  ODebug.DecIndent;
end;

function TAggrList.Count(): integer;
const
  procname = 'Count';
  procversion = '1.0.0.0';
begin
  ODebug.IncIndent;
  ODebug.Add(UnitName, self.ClassName, procname, procversion, 1);
  result := fList.Count;
  ODebug.DecIndent;
end;

{****************************
 * TAggrSum implementation *
 ****************************}

constructor TAggrSum.Create(ResultVar, AggregateVar: string);
begin
  inherited create(ResultVar, AggregateVar);
  fFuncType := afSum;
end;

procedure TAggrSum.Execute(Idx: integer);
begin
  if fAggregateVector.IsMissing[idx] then exit;
  Sum := Sum + fAggregateVector.AsFloat[idx];
end;

procedure TAggrSum.SetOutput(Idx: integer);
begin
  fResultVector.AsFloat[idx] := Sum;
end;

procedure TAggrSum.Reset();
begin
  Sum := 0.0;
end;

procedure TAggrSum.CreateResultVector(Dataframe: TEpiDataframe);
var
  pVarDesc: TAnaVariableDescriptor;
begin
  pVarDesc := TAnaVariableDescriptor.Create(fResVariable, fAggregateVector.DataType,
                  fAggregateVector.FieldDataSize, fAggregateVector.FieldDataDecimals);
  Dataframe.NewVector(pVarDesc);
  fResultVector := Dataframe.VectorByName[fResVariable];
  fResultVector.VariableLabel := '(SUM) '+ fAggregateVector.GetVariableLabel;
end;

{****************************
 * TAggrCount implementation *
 ****************************}

constructor TAggrCount.Create(ResultVar, AggregateVar: string; ACountType: TAggrCountType);
begin
  inherited create(ResultVar, AggregateVar);
  CountType := ACountType;
  fFuncType := afCount;
end;

procedure TAggrCount.Execute(Idx: integer);
begin
  case CountType of
    acMissing: if fAggregateVector.IsMissing[Idx] then Inc(Count);
    acMissingValue: if fAggregateVector.IsMissingValue[Idx] then Inc(Count);
    acNotMissing: if not (fAggregateVector.IsMissingValue[Idx] or
                          fAggregateVector.IsMissing[Idx]) then Inc(Count);
    acAll: Inc(Count);
  end;
end;

procedure TAggrCount.SetOutput(Idx: integer);
begin
  fResultVector.AsFloat[idx] := Count;
end;

procedure TAggrCount.Reset();
begin
  Count := 0;
end;

procedure TAggrCount.CreateResultVector(Dataframe: TEpiDataframe);
var
  pVarDesc: TAnaVariableDescriptor;
begin
  pVarDesc := TAnaVariableDescriptor.Create(fResVariable, EpiTyInteger, 6, 0);
  Dataframe.NewVector(pVarDesc);
  fResultVector := Dataframe.VectorByName[fResVariable];
  if fAggregateVector <> nil then
    fResultVector.VariableLabel := '(N) '+ fAggregateVector.GetVariableLabel
end;

{****************************
 * TAggrMean implementation *
 ****************************}

constructor TAggrMean.Create(ResultVar, AggregateVar: string; AMeanType: TAggrMeanType);
begin
  inherited create(ResultVar, AggregateVar);
  fFuncType := afMean;
  MeanType := AMeanType;
end;

procedure TAggrMean.Execute(Idx: integer);
begin
  if fAggregateVector.IsMissing[idx] then exit;
  Sum := Sum + fAggregateVector.AsFloat[idx];
  Stdvar := Stdvar + IntPower(fAggregateVector.AsFloat[idx], 2);
  Inc(Count);
end;

procedure TAggrMean.SetOutput(Idx: integer);
var
  mean, stddev, lci, uci, f: EpiFloat;
begin
  if Count = 0 then
    fResultVector.AsFloat[idx] := NA_FLOAT
  else
  begin
    mean := Sum/Count;
    if count = 1 then
    begin
      stdvar := NA_FLOAT;
      stddev := NA_FLOAT;
      lci    := NA_FLOAT;
      uci    := NA_FLOAT;
    end else begin
      stdvar := (stdvar + (count * IntPower(Mean, 2) - 2 * Mean * Sum)) / (count-1);
      stddev := sqrt(stdvar);
      f:=   PTDISTRINV((Count-1), 0.025) * System.Sqrt(stdvar/count);
      lci    := Mean - f;
      uci    := Mean + f;
{      lci    := Mean - System.Sqrt(stdvar / count);
      uci    := Mean + System.Sqrt(stdvar / count);}
    end;
    case MeanType of
      amMCI,
      amMean: fResultVector.AsFloat[idx] := mean;
      amStdVar: fResultVector.AsFloat[idx] := stdvar;
      amStdDev: fResultVector.AsFloat[idx] := stddev;
    end;
    if MeanType = amMCI then
    begin
      fLowerCI.AsFloat[Idx] := lci;
      fUpperCI.AsFloat[Idx] := uci;
    end;
  end;
end;

procedure TAggrMean.Reset();
begin
  Sum := 0.0;
  Count := 0;
  StdVar := 0;
end;

function TAggrMean.ContainVarname(const Varname: string): boolean;
begin
  result := inherited ContainVarname(varname);

  if MeanType = amMCI then
  begin
    if (AnsiCompareStr(Varname, fLowerCI.Name) = 0) or
       (AnsiCompareStr(Varname, fUpperCI.Name) = 0) then
       result := true;
  end;
end;

procedure TAggrMean.CreateResultVector(Dataframe: TEpiDataframe);
var
  pVarDesc: TAnaVariableDescriptor;
begin
  pVarDesc := nil;
  try
    if (fAggregateVector is TEpiIntVector) then
      pVarDesc := TAnaVariableDescriptor.Create(fResVariable, EpiTyFloat, 6, 4, EFormat(6, 4))
    else
      pVarDesc := TAnaVariableDescriptor.Create(fResVariable, EpiTyFloat,
                      fAggregateVector.FieldDataSize, fAggregateVector.FieldDataDecimals,
                      EFormat(fAggregateVector.FieldDataSize, fAggregateVector.FieldDataDecimals));
    Dataframe.NewVector(pVarDesc);
    fResultVector := Dataframe.VectorByName[fResVariable];
    case MeanType of
      amMCI,
      amMean: fResultVector.VariableLabel := '(Mean) ' + fAggregateVector.GetVariableLabel;
      amStdVar: fResultVector.VariableLabel := '(Variance) ' + fAggregateVector.GetVariableLabel;
      amStdDev: fResultVector.VariableLabel := '(Deviance) ' + fAggregateVector.GetVariableLabel;
    end;
    if MeanType = amMCI then
    begin
      FreeAndNil(pVarDesc);
      pVarDesc := TAnaVariableDescriptor.Create(fResVariable+'LOCI', EpiTyFloat,
                      6, 4, EFormat(6, 4));
      Dataframe.NewVector(pVarDesc);
      fLowerCI := Dataframe.VectorByName[fResVariable+'LOCI'];
      fLowerCI.VariableLabel := '(Mean lower 95% CI) ' + fAggregateVector.GetVariableLabel;
      FreeAndNil(pVarDesc);
      pVarDesc := TAnaVariableDescriptor.Create(fResVariable+'HICI', EpiTyFloat,
                      6, 4, EFormat(6, 4));
      Dataframe.NewVector(pVarDesc);
      fUpperCI := Dataframe.VectorByName[fResVariable+'HICI'];
      fUpperCI.VariableLabel := '(Mean upper 95% CI) ' + fAggregateVector.GetVariableLabel;
    end;
  finally
    if Assigned(pVarDesc) then FreeAndNil(pVarDesc);
  end;
end;

{****************************
 * TAggrMinMax implementation *
 ****************************}

constructor TAggrMinMax.Create(ResultVar, AggregateVar: string; FindMin: boolean);
begin
  inherited Create(ResultVar, AggregateVar);
  Minimum := FindMin;
  fFuncType := afMinMax;
end;

procedure TAggrMinMax.Execute(Idx: integer);
begin
  if fAggregateVector.IsMissing[idx] then exit;
  if Value = NA_FLOAT then
    Value := fAggregateVector.AsFloat[Idx]
  else
    if Minimum then
      Value := Math.Min(fAggregateVector.AsFloat[Idx], Value)
    else
      Value := Math.Max(fAggregateVector.AsFloat[Idx], Value);
end;

procedure TAggrMinMax.SetOutput(Idx: integer);
begin
  fResultVector.AsFloat[idx] := Value;
end;

procedure TAggrMinMax.Reset();
begin
  Value := NA_FLOAT;
end;

procedure TAggrMinMax.CreateResultVector(Dataframe: TEpiDataframe);
var
  pVarDesc: TAnaVariableDescriptor;
begin
  pVarDesc := TAnaVariableDescriptor.Create(fResVariable, fAggregateVector.DataType,
                  fAggregateVector.FieldDataSize, fAggregateVector.FieldDataDecimals);
  Dataframe.NewVector(pVarDesc);
  fResultVector := Dataframe.VectorByName[fResVariable];
  if Minimum then
    fResultVector.VariableLabel := '(MIN) '+ fAggregateVector.GetVariableLabel
  else
    fResultVector.VariableLabel := '(MAX) '+ fAggregateVector.GetVariableLabel;
end;

{**********************************
 * TAggrPercentile implementation *
 **********************************}

constructor TAggrPercentile.Create(ResultVar, AggregateVar: string; APercentileType: TAggrPercentileType);
begin
  inherited create(ResultVar, AggregateVar);
  PercentileType := APercentileType;
  fFuncType := afPercentile;
  LastCount := 0;
  MissingCount := 1;
end;

procedure TAggrPercentile.Execute(Idx: integer);
begin
  Inc(Count);
  if not (fAggregateVector.IsMissing[Idx] or fAggregateVector.IsMissingValue[Idx]) then
    Inc(MissingCount);
end;

procedure TAggrPercentile.SetOutput(Idx: integer);
var
  d, ix, offset: integer;
  w: EpiFloat;
begin
  offset := LastCount;
  d := 0;
  case PercentileType of
    ap1 : d := 1;
    ap5 : d := 5;
    ap10: d := 10;
    ap25: d := 25;
    ap50: d := 50;
    ap75: d := 75;
    ap90: d := 90;
    ap95: d := 95;
    ap99: d := 99;
  end;
  w := Math.min(Math.max(MissingCount*(d/100), 1), MissingCount-1);
  ix := Math.max(trunc(MissingCount*(d/100)),1);
  if ((ix+offset+1)>fAggregateVector.Length) then
    ix := fAggregateVector.Length - offset -1;
  if ((ix-offset+1) > (MissingCount -1)) then
    ix := MissingCount -2;
  ix := Math.Max(Ix, 1);
  if w = ix then
    fResultVector.AsFloat[idx] := fAggregateVector.AsFloat[ix+offset]
  else
    fResultVector.AsFloat[idx] := fAggregateVector.AsFloat[ix+offset] +
        (fAggregateVector.AsFloat[ix+offset+1]-fAggregateVector.AsFloat[ix+offset])*(w-ix);
end;

procedure TAggrPercentile.Reset();
begin
  LastCount := LastCount + Count;
  MissingCount := 1;
  Count := 0;
end;

procedure TAggrPercentile.CreateResultVector(Dataframe: TEpiDataframe);
var
  pVarDesc: TAnaVariableDescriptor;
begin
  pVarDesc := TAnaVariableDescriptor.Create(fResVariable, EpiTyFloat,
                                            fAggregateVector.FieldDataSize, fAggregateVector.FieldDataDecimals,
                                            EFormat(fAggregateVector.FieldDataSize, fAggregateVector.FieldDataDecimals));
  Dataframe.NewVector(pVarDesc);
  fResultVector := Dataframe.VectorByName[fResVariable];
  case PercentileType of
    ap1 : fResultVector.VariableLabel := '(1 Percentile) '+ fAggregateVector.GetVariableLabel;
    ap5 : fResultVector.VariableLabel := '(5 Percentile) '+ fAggregateVector.GetVariableLabel;
    ap10: fResultVector.VariableLabel := '(10 Percentile) '+ fAggregateVector.GetVariableLabel;
    ap25: fResultVector.VariableLabel := '(25 Percentile) '+ fAggregateVector.GetVariableLabel;
    ap50: fResultVector.VariableLabel := '(Median) '+ fAggregateVector.GetVariableLabel;
    ap75: fResultVector.VariableLabel := '(75 Percentile) '+ fAggregateVector.GetVariableLabel;
    ap90: fResultVector.VariableLabel := '(90 Percentile) '+ fAggregateVector.GetVariableLabel;
    ap95: fResultVector.VariableLabel := '(95 Percentile) '+ fAggregateVector.GetVariableLabel;
    ap99: fResultVector.VariableLabel := '(99 Percentile) '+ fAggregateVector.GetVariableLabel;
  end;
end;

end.
