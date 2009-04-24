unit UTables;

interface

uses SysUtils, UVectors, classes, ansDataTypes, UOutput, UCommands, UAnaToken, Variants;

type

  TTableCell = class(TObject)
  private
    fN              : EpiUnInt;
    fNestL,fNestG    : EpiFloat ;        // Estimated NestL (local table), nestG (global table): EpiFloat;
    fRpct,fCpct,fTpct : EpiFloat ;        // row column total percent
  protected
    //
  public
    constructor Create();
    property N: EpiUnInt read fN;
    property NestL: EpiFloat read fNestL write fNestL;
    property NestG: EpiFloat read fNestG write fNestG;
    property Rpct:  EpiFloat read fRpct;
    property Cpct:  EpiFloat read fCpct;
    property Tpct:  EpiFloat read fTpct;
  end;

  TTwoWayTable = class;

  TTwoWaySortCompare = function(Table: TTwoWayTable; I1, I2: Integer; const index: integer): Integer;
  TTwoWaySortExchange = procedure(var Table: TTwoWayTable; Index1, Index2: Integer);

  TStringArray = class(TObject)
  private
    function GetCount(): integer;
  protected
    fStrings: array of string;
    fData: array of EpiVariant;
  public
    destructor Destroy(); override;
    procedure Add(s: string; value: EpiVariant); virtual;
    function Find(s: string; var index: integer): boolean;
    procedure MoveToBack(Index: Integer);
    procedure Exchange(Index1, Index2: integer);
    function Get(index: integer):string;
    function GetValue(index: integer):EpiVariant;
    property Count: integer read GetCount;
  end;

  TLabelArray = class(TStringArray)
  public
    destructor Destroy(); override;
    procedure Add(s: string; Value: EpiVariant); override;
  end;

  TTwoWayTable = class
  private
    fCaption,                                // Caption = Stratified caption
    fColHeader,                              // Column variable label
    fRowHeader       : string;               // Row variable label
    fColLabel,                               // Labels for Column headers
    fRowLabel        : TLabelArray;          // Label for Row headers
    fCells           : array of array of TTableCell;

    // For procedures which cannot be implemented as direct functions we need to
    // add variables to store them in external procedures.
    fDF                                   : EpiInt;        // Integer: Actual Degrees of Freedom, total N
    fChi, fpchi, fpexact, ffp2            : EpiFloat;      // Chi2, p for that and exact P for table
    fGamma, fpGamma2, fGammaUL, fGammaLL  : EpiFloat;      // Gamma coeff., p and CI
    foddsratio,fpOR, fORLL, fORUL         : EpiFloat;      // stratum OR of 2x2  - generalised Odds Ratio for nxk
    // summary variables for stratified analysis
    //fORChiNum,
    //fORChiDen         : EpiFloat;             // MantelHaenzel Weights - rr weights

    function GetRowCount(): cardinal;
    function GetColumnCount(): cardinal;
    function GetCell(const col, row: Cardinal): TTableCell;
    function GetColTotal(const index: integer): EpiUnInt;
    function GetRowTotal(const index: integer): EpiUnInt;
    function GetTotal(): EpiUnInt;
    function GetRowHeaderValue(const index: integer): EpiVariant;
    function GetRowHeaderLabel(const index: integer): String;
    function GetColHeaderValue(const index: integer): EpiVariant;
    function GetColHeaderLabel(const index: integer): String;
    // 2x2 table estimators as functions :
    function GetFishers2p() :EpiFloat;       // fishers exact 2 sided
    function GetCrudeOR(): EpiFloat;
    function GetCrudeRR(): EpiFloat;
    function GetRRUL: EpiFloat;
    function GetRRLL: EpiFloat;
    function Getgrci(WhichLim: Integer):EpiFloat;
    function MinCount: EpiInt;
    //sorting
    procedure InternalSortGeneric(SCompare: TTwoWaySortCompare; SExchange: TTwoWaySortExchange; L, R: integer; const index: integer; Desc: boolean);
  protected
    //
  public
    constructor Create(const Cols, Rows: integer);
    destructor Destroy(); override;
    procedure SortByColumn(const index: integer; Desc: boolean);
    procedure SortByRow(const index: integer; Desc: boolean);
    procedure SortByColumnLabel(Desc: boolean);
    procedure SortByColumnLabelText(Desc: boolean);
    procedure SortByRowLabel(Desc: boolean);
    procedure SortByRowLabelText(Desc: boolean);
    procedure SortByColumnTotal(Desc: boolean);
    procedure SortByRowTotal(Desc: boolean);
    property RowCount: cardinal read GetRowCount;
    property ColumnCount: cardinal read GetColumnCount;
    property Total: EpiUnInt read GetTotal;
    property Cell[const col, row: Cardinal]: TTableCell read GetCell;
    property ColTotal[const index: integer]: EpiUnInt read GetColTotal;
    property RowTotal[const index: integer]: EpiUnInt read GetRowTotal;
    property ColHeaderLabel[const index: integer]: string read GetColHeaderLabel;
    property ColHeaderValue[const index: integer]: EpiVariant read GetColHeaderValue;
    property RowHeaderLabel[const index: integer]: string read GetRowHeaderLabel;
    property RowHeaderValue[const index: integer]: EpiVariant read GetRowHeaderValue;

    property ColHeader: String read fColHeader;
    property RowHeader: String read fRowHeader;
    property Caption: String read fCaption;
    //add statistical estimators here - and the variable above in private section
    property DF: EpiInt read fDF write fDF;
    property CHI: EpiFloat read fChi write fChi;
    property pCHI: EpiFloat read fpChi write fpChi;
    property pexact: EpiFloat read fpexact write fpexact;
    property FishP2: EpiFloat read getFishers2p ;  // Fishers exact for 2x2

    property Gamma   : EpiFloat  read fGamma write fGamma ;
    property pGamma2 : EpiFloat   read fpGamma2 write fpGamma2 ;
    property GammaUL : EpiFloat read fGammaUL write fGammaUL ;
    property GammaLL : EpiFloat read fGammaLL write fGammaLL ;

    property OddsRatio: EpiFloat read {getCrudeOR ; //} foddsratio write foddsratio;
    property pOR: EpiFloat read fpOR write fpOR;
    property ORLL: EpiFloat read fORLL write fORLL;
    property ORUL: EpiFloat read fORUL write fORUL;

    property RR: EpiFloat read GetCrudeRR ;       // fRR write fRR;
    property   RRUL        : EpiFloat read Getrrul;
    property   RRLL        : EpiFloat read GetRRLL;

    property SmallE   : EpiInt  read MinCount ;
  end;

  TSumTable = class(TObject)
  private
    fMargTable             : TTwowaytable;   // marginal overall table
    fSubtables             : array of TTwowaytable ;  // Each stratum
    fSumtableCaption       : string;
    fTableType             : Byte;
    fWeighted              : Boolean;

    // the following are the estimated weighted summary measures.
    fChi, fpchi, fpMCChi,
    fMCCHI, fMCGam, fpMCGam             : EpiFloat;       // Chi2, p for that and exact P for table
    fNsim, fDf, fmcdf                   : EpiInt;         // Simulations
    fGamma, fpGamma2, fGammaUL,
    fGammaLL, fMCGammaUL, fMCGammaLL    : EpiFloat;       // Gamma coeff., p and CI
    fOddsratio, fpOR, fORLL, fORUL      : EpiFloat;       // generalised Odds Ratio for nxk
    fRR, fRRLL, fRRUL                   : EpiFloat;       // Summary RR of 2x2
    fWolfSum     : EpiFloat;       // Wolfsum for test of interaction

    function GetSubTable(const index: integer): TTwoWayTable;
    function GetTableCount(): integer;
    function GetTotal(): EpiUnInt;
    function GetValidTotal() : EpiUnInt;
    function GetDFZeroCount(): EpiUnInt;
    function GetWolfError(): Boolean;
    function GetWolfChiSum(): EpiFloat;

    procedure AddSubTable(aTable: TTwoWayTable);
  protected
    //
  public
    constructor Create();
    destructor Destroy(); override;
    property SubTable[const index: integer]: TTwoWayTable read GetSubTable ; default;
    property MargTable: TTwoWayTable read fMargTable write fMargTable;
    property TableCount: integer read GetTableCount;
    property Total: EpiUnInt read GetTotal;
    property TableType: Byte read fTableType;
    property DFZeroCount: EpiUnInt read GetDFZeroCount;
    property WolfError: Boolean read GetWolfError;

   //add statistical estimators here - and the variable above in private section
    property SumDF: EpiInt read fDF write fDF;
    property SumCHI: EpiFloat read fChi write fChi;
    property pSumCHI: EpiFloat read fpChi write fpChi;

    property NSIM: EpiInt read fNsim write fNsim;
    property MCCHI: EpiFloat read fMCChi write fMCChi;
    property pMCCHI: EpiFloat read fpMCChi write fpMCChi;

    property MCgamma: EpiFloat read fMCGam write fMCGam;
    property pMCpgamma: EpiFloat read fpMCGam write fpMCGam;
    property MCgammaUL: EpiFloat read fMCGammaUL write fMCGammaUL;
    property MCgammaLL: EpiFloat read fMCGammaLL write fMCGammaLL;

    property PartGamma   : EpiFloat  read fGamma write fGamma ;
    property PartGammaUL : EpiFloat read fGammaUL write fGammaUL ;
    property PartGammaLL : EpiFloat read fGammaLL write fGammaLL ;
    property pPartGamma2 : EpiFloat   read fpGamma2 write fpGamma2 ;

    property SumOR: EpiFloat read fOddsratio write fOddsratio;
    property SumpOR: EpiFloat read fpOR write fpOR;
    property SumORLL: EpiFloat read fORLL write fORLL;
    property SumORUL: EpiFloat read fORUL write fORUL;

    property SumRR: EpiFloat read fRR write fRR;
    property SumRRLL: EpiFloat read fRRLL write fRRLL;
    property SumRRUL: EpiFloat read fRRUL write fRRUL;

    property SumChiWolf: EpiFloat read getWolfChiSum;


    property ValidTotal : EpiUnInt read getValidTotal; // from informative subtables
  end;

  TORHeader = record
                Outcome,
                CCase,
                NCase,
                N,
                Exposed,
                NonExposed: String;
              end;
  TRRHeader = record
                Outcome,
                Exposed,
                NotExposed,
                N,
                SmallN,
                Ill,
                RR,
                AR: String;
              end;
  TLTHeader = record
                IntVal,
                Beg,
                Deaths,
                Lost,
                Survival,
                StdErr: string;
              end;  
  TTableFormats = record
                    EFmt, RFmt, CFmt, TFmt,
                    ColPctHdr, RowPCtHdr, PctHdr, TotPctHdr,
                    CIFmt, CIHdr: string;
                    ORHdr: TORHeader;
                    RRHdr: TRRHeader;
                    LTHdr: TLTHeader;
                  end;

  TTables = class
  private
    WeightName: string;
    Weighted:  boolean;
    OutputOptions: Cardinal;
    function SortOptions(sortname: string; var index: integer): boolean;
    procedure SortTable(aTable: TTwoWayTable);
    procedure GetTableOR(aTable: TTwoWayTable); // 2x2 table estimator Odds ratio, p value and limits:
    procedure OutS(SumTable: TSumTable);
    procedure OutEpi(SumTable: TSumTable);
    procedure SumtableToResultvar(SumTable: TSumTable);

    // functions to add columns with various estimates or counts:
    // - Outbreak Analysis functions.
    procedure AddOA(Tab: TstatTable; Sumtable: TSumTable; Col:integer; Fmts: TTableFormats);
    Procedure AddAR(Tab: TStatTable; Sumtable: Tsumtable; Col:integer; Fmts: TTableFormats; AddCI: Boolean);
    // - relative risk functions.
    Procedure AddRR(Tab: TStatTable; Sumtable: Tsumtable; Fmts: TTableFormats);
    // - Case Control functions
    procedure AddCC(var tab: TstatTable;Sumtable: TSumTable;col:integer; Fmts: TTableFormats);
    Procedure AddOR(Tab: TStatTable; Sumtable: Tsumtable; Cmd :TCommand; force: Boolean; Fmts: TTableFormats);
    // - Generel compact table functions.
    Procedure AddGam(Tab: TStatTable; Sumtable: Tsumtable; Cmd :TCommand; Fmts: TTableFormats);
    Procedure AddExact(Tab: TStatTable; Sumtable: Tsumtable; Cmd :TCommand);
    Procedure AddObs(Tab: TStatTable; Sumtable: Tsumtable; Cmd :TCommand);
    Procedure AddChi2Test(Tab: TStatTable; Sumtable: Tsumtable; Cmd :TCommand; var Small:Boolean);
    function AddTotals(const Tab: TStatTable; const TwoWayTable: TTwoWayTable): string;
    function AddPercents(const Tab: TStatTable; const TwoWayTable: TTwoWayTable; Cmd :TCommand): string;


    function  AdjustOutput(s: string): string;
    procedure GetORHeaders(Cmd: TCommand; var ORHeaders: TORHeader);
    procedure GetRRHeaders(Cmd: TCommand; var RRHeaders: TRRHeader);
    procedure GetLTHeaders(Cmd: TCommand; var LTHeaders: TLTHeader);
    procedure CollectEstimates(var allestimates : string);
    procedure DoFreqTable(Dataframe: TEpiDataframe; Varnames: TStrings);        // Common procedure for aggregating, statistics and outputting frequency tables
    procedure DoStratifiedTable(Dataframe: TEpiDataframe; Varnames: TStrings);  // Common procedure for aggregating, statistics and outputting stratified tables
    procedure DoFV(Dataframe: TEpiDataframe; Varnames: TStrings);               // Common procedure for aggregating, statistics and outputting FV tables
    function OutTwoWayTable(const TwoWayTable: TTwoWayTable): TStatTable;
    Function Get2x2statistics(Sumtable: TSumTable): Boolean;
  protected
    // For internally available methods!
  public
    Cmd: TCommand;
    // For Externaly available methods.
    procedure Getformats(cmd: TCommand; var TableFormats: TTableFormats);// overload;
    constructor Create();
    destructor Destroy(); override;
    procedure Outtable(SumTable: TSumTable);
    procedure OutFreqtable(SumTable: TSumTable);
    procedure OutFVTable(SumTable: TSumTable);
    function CreateFreqTable(df: TEpiDataframe; Varnames: TStrings): TSumTable;
    function CreateStratTable(df: TEpiDataframe; Varnames: TStrings): TSumTable;
    function CreateFVTable(df: TEpiDataframe; Varnames: TStrings): TSumTable;
    // Do Tables are commen entries from Dm (Datamodule).
    function DoTables(dataframe: TEpiDataframe; varnames: TStrings; cmd: TCommand): boolean;
  end;

var
  OTables: TTables;

implementation

uses UCmdProcessor, UAggregate, USKTables, {SMUtils}GeneralUtils, Math, UFormats, UEpiDatatypes,
  StrUtils, Forms, UTableStat, Ustatfunctions,EpiInfoStats, UDebug, EpiDataUtils, UCmdTypes,
  UDateUtils;

const
  UnitName = 'UTables';

// Output options.
  outTotal = $00000001;  // Added column and row totals
  outRPct =  $00000002;  // Added row percents.
  outCPct =  $00000004;  // Added col percents.
  outTPct =  $00000008;  // Added total percents.
  outMPct =  $00000010;  // Percents in seperate columns.

{*****************************************
  Extra routines
******************************************}

Function Ttables.Get2x2statistics(Sumtable: TSumTable): Boolean;
// this routine will take a filled sumtable and do the 2x2 summary statistics
// and estimate OR based on the Epi6 module for MLE OR calculation'
// In this organisation of source code all 2x2 special calculation
// are placed in the core Ttables (but should be defined as an object).
var
  i,s: integer;
  t2,t3 : TTableItem;
  Stratified : Boolean;
  FisherStat, FishLocal: TFisherStat;      //exact estimation for odds ratios.
  // summary stat evaluation
  MHChiSqr, StratPValue : epifloat;
  MantelNum : epifloat;
  MantelDiv, CHiNum, ChiDenom : epifloat;
  Asum, Bsum, Csum, Dsum : epifloat;
  Numstrata,PassNum : integer;
  (*Variables for Woolf's test for Heterogeneity of Odds Ratios*)
  (* from Schlesselman, p. 194*)
  WSum, ORSum, AORSum, WolfSum : epifloat;
  WolfError : Boolean (*If true, a zero cell was encountered*);
  (*Variables for Rothman's Evaluation of Effect Modification of Relative
    risk.  Pg222*)
   RRWSum, RRSum, RRPooled, RRChiSqr : epifloat;
  (*Variables for Robins, Greenland, Breslow Conf. limits on Odds Ratio
    and Greenland,Robins Relative Risk*)
  RRMHgr, SumDgr, SumRgr, SumSgr : epifloat;
  SumPRrgb, SumPSandQRrgb, SumQSrgb : epifloat;
  RRgr: epifloat;
  Grandtotal: EpiInt;

  // procedures moved here (Organise different ?)
  procedure initVar;
  begin
    PassNum := 0;
    MantelNum := 0.0;
    MantelDiv := 0.0;
    Asum := 0.0;
    Bsum := 0.0;
    Csum := 0.0;
    Dsum := 0.0;
    ChiNum := 0.0;
    ChiDenom := 0.0;
    Numstrata := 0;
    RRMHgr := 0.0;
    SumDgr := 0.0;
    SumRgr := 0.0;
    SumSgr := 0.0;
    SumPRrgb := 0.0;
    SumPSandQRrgb := 0.0;
    SumQSrgb := 0.0;
    RRgr := 0.0;
    GrandTotal :=0;
  end;

  function CalcRGBandGRSums (t2: TTableItem):boolean;
   {Produce sums for Greenland/Robins confidence limits for
   Relative Risk in single and stratified tables.
   Greenland, S, Robins, JM, Biometrics 1985;41:55-68}
   {Produce sums for Robins, Greenland, Breslow confidence limits        Rothman I. p 216 f 12-54
    on the odds ratio for single and stratified tables}
  Var
    R, S, Dgr, Rrgb, Srgb, Vgr, Prgb, Qrgb  : EpiFloat;
  begin
    // note t2 table has case-ref left and exposure at top !!
    R := t2.A * t2.N0 / t2.tot ;
    S := t2.C * t2.N1 / t2.tot;     // C: unexposed cases
    Dgr := ((t2.N1 * t2.N0 * t2.M1) - (t2.A * t2.C * t2.tot)) / Sqr(t2.tot);
    SumDgr := SumDgr + Dgr;
    SumRgr := SumRgr + R;
    SumSgr := SumSgr + S;
    {Done with GR sums}
    {Now do RGB sums}
    Prgb := (t2.A + t2.D) /t2.Tot;
    Qrgb := (t2.B + t2.C) /t2.Tot;
    SumPRrgb := SumPRrgb + (Prgb*(t2.A*t2.D)/t2.Tot);
    SumPSandQRrgb := SumPSandQRrgb + (Prgb*(t2.B*t2.C)/t2.Tot)+(Qrgb*(t2.A*t2.D)/t2.Tot);
    SumQSrgb := SumQSrgb + (Qrgb * (t2.B*t2.C)/t2.Tot);
  end;

  function AddTable(t2: TTableItem; FisherStat : TFisherStat):boolean;
  var
    TempW      : Epifloat;
    TempLnOR   : Epifloat;
    RRWi, RRi      :Epifloat;
  begin
    if not WolfError then
      inc(PassNum);
    FisherStat.addSet(t2);
    NumStrata :=NumStrata+1;
    CalcRGBandGRSums(t2);
    If Stratified Then
    Begin
      MantelNum := MantelNum + (t2.A * t2.D / t2.Tot);
      MantelDiv := MantelDiv  + (t2.B * t2.C / t2.Tot);
      WolfError := WolfError or (t2.A = 0) or (t2.B = 0) or (t2.C = 0) or (t2.D = 0);
      If (t2.a * t2.d > 0)  then
      begin
        Asum := Asum + t2.A;
        Bsum := Bsum + t2.B;
        Csum := Csum + t2.C;
        Dsum := Dsum + t2.D;
        ChiNum := ChiNum + (1.0*t2.M1*t2.N1/(t2.Tot));
        If t2.Tot > 1 then
          ChiDenom := ChiDenom +((1.0*t2.M1*t2.M0*t2.N1*t2.N0)/
        (1.0*(t2.Tot-1.0)*t2.Tot*t2.Tot));
      end;
      If Not WolfError then
      Begin
        TempLnOR := Ln ((t2.A*t2.D) / (t2.B*t2.C));        { Ln(ORi) }
        TempW := 1 / (1/t2.A + 1/t2.B + 1/t2.C + 1/t2.D);  { weight }
        RRWi := 1 / ( (t2.b/(t2.a*t2.N1)) + (t2.d/(t2.c*t2.N0)) );{ weight(i) Rothman p 188 12-10}
        RRi := (t2.a/t2.N1)/(t2.c/t2.N0);                  { RR(i) }
        RRWSum := RRWSum + RRWi;
        RRSum := RRSum + (RRWi * Ln(RRi));
        If PassNum = 2 then         // passnum 2 is when the final table is ready
        Begin
          AORSum := ln(exp(ORSum / WSum));
          WolfSum := WolfSum + Sqr(TempLnOR - AORSum)*TempW;
          RRPooled := exp(RRSum / RRWSum);
          RRChiSqr := RRChiSqr + ( Sqr(Ln(RRi) - Ln(RRPooled))*RRWi);
          exit;
        end;
        ORSum := ORSum + (TempW * TempLnOR);   { w1Ln(ORi) }
        WSum := WSum + TempW;                  { sum of the weights}
      End (*If wolferror*);
    End (*If Stratified *);
  end;

  // calculate summary rr plus ci
  Procedure CalculateRRMH(var sumtab: TSumTable);
  {Sept 87  RR calculations for Greenland/Robins method Biometrics, 1985; 41:55-68}
  Var
    V, RRgr, LowerRRgr, UpperRRgr : epifloat;
  begin
    If SumSgr <> 0 then
      RRgr := SumRgr/SumSgr
    else
      RRgr := -9999;
    If SumRgr * SumSgr > 0 then
      V := SumDgr / (SumRgr * SumSgr)
    else
      V := -9999;
    If V >= 0 then
    begin
      LowerRRgr := exp ( ln (RRgr) - (ConfScore * Sqrt (V)));
      UpperRRgr := exp ( ln (RRgr) + (ConfScore * Sqrt (V)));
    end
    else
    begin
      LowerRRgr := -9999;
      UpperRRgr := -9999;
    end;
    sumtab.sumrr := rrgr;
    sumtab.sumrrUL := UpperRRgr;
    sumtab.sumrrLL := LowerRRgr;
    sumtab.fDf := 1;  // set for Mantel Haenzel test summary Chi Sq
  end;
/////////////////////////////////////////////////////////////////////////////////////
//  no more helper routines                                                        //
/////////////////////////////////////////////////////////////////////////////////////

// get started:
begin   // do the actual work here:
// add crude odds ratios to marginal table and all subtables:
   GetTableOR(SUmtable.MargTable);
   for i := 1 to SumTable.TableCount do   //add all subtables for weighted estimates:
   GetTableOR(Sumtable.SubTable[i]);

   // now we have all crude odds ratios in place
// marginaltable handle marginal table first:

  //Now ready for the stratified analysis:
  if (SumTable.TableCount > 1) and  (sumtable.tabletype in [3,4]) then
  begin
    FisherStat := TFisherStat.Create(false);
    Stratified := True;
    initvar();
    try
      for i := 1 to SumTable.TableCount do   //add all subtables for weighted estimates:
      begin
        t2:= TtableItem.Create(SumTable[i].Cell[1,1].n,SumTable[i].Cell[2,1].n,
                               SumTable[i].Cell[1,2].n,SumTable[i].Cell[2,2].n,1);
        addtable(t2,FisherStat);   // for the stratfied analysis
        s := fisherstat.show(t2,i);      // for the stratfied analysis
      end;
     // after all strata we have the stratified values:
        Sumtable.SumOR := t2.sOR;
        Sumtable.SumORUL := t2.ORUL;
        Sumtable.SumORLL := t2.ORLL;
        Sumtable.SumpOR := t2.pOR;

        if (ChiDenom <> 0) then
          begin
            { Historical comment kept from EpiInfo source:
              dcs 5/6/94 Kevin says the corrected chi square is produced for summary
              and it should be uncorrected.  This looks like corrrected.
              Andy says put both in  (Andy Dean and Kevin Sullivan  -- added Jens Lauritsen}

            Sumtable.SumCHI := (Sqr(Abs(Asum-ChiNum))) / ChiDenom;
            Sumtable.pSumCHI := ChiPValue(Sumtable.SumCHI, 1);
          end
          else
          begin
          Sumtable.SumCHI := 0;
          Sumtable.pSumCHI := 0;
          end;
    except
      dm.error('2x2 table stats error %d', [i], 41001);
    end;
    // rr estimation:
    CalculateRRMH(Sumtable);
    if Assigned(t2) then FreeAndNil(t2); //t2.free;
    if Assigned(FisherStat) then FreeAndNil(FisherStat); //FisherStat.free;
  end; // stratified table analysis
end;



{*****************************************
  TTables:
******************************************}

// ============================================================================
// Public methodes.
// ============================================================================

constructor TTables.Create();
begin
  WeightName := '';
  Weighted := false;
  OSKTableAnalysis := TSKTableAnalysis.Create();
end;

destructor TTables.Destroy();
begin
  if Assigned(OSKTableAnalysis) then FreeAndNil(OSKTableAnalysis);
end;


function TTables.DoTables(dataframe: TEpiDataframe; varnames: TStrings; cmd: TCommand): boolean;
var
  agl: TAggrList;
  df: TEpiDataframe;
begin
  result := false;
  self.Cmd := cmd;
  self.OutputOptions := 0;
  df := nil;
  agl := nil;
  try
    // step 1a: test combination of options:
    if (cmd.ParamExists['CT']) then
    begin
      if (cmd.ParamExists['O']) and (cmd.ParamExists['AR']) then dm.error('Use either %s or %s', ['/O', '/AR'], 41002);
      if (cmd.ParamExists['O']) and (cmd.ParamExists['RR']) then dm.error('Use either %s or %s', ['/O', '/RR'], 41002);
    end;

    // step 1b: Weighted option
    if Cmd.ParamByName['W'] <> nil then
    begin
      WeightName := (Cmd.ParamByName['W'].AsString);
      Weighted := true;
      varnames.Delete(varnames.IndexOf(WeightName));
    end;
    dm.AddResult('$tblweight', EpiTyBoolean, Weighted, 0, 0);
    agl := TAggrList.Create();
    if not Weighted then
      agl.Insert(0, TAggrCount.Create('$S', '', acAll))
    else
      agl.Insert(0, TAggrSum.Create('$S', WeightName));

    // step 1c: Preparing dataframe.
    df := OAggregate.AggregateDataframe(dataframe, TStringList(varnames), agl, Cmd);
//    OAggregate.OutAggregate(df);

    // Step 1d: Which table type?
    if (varnames.Count = 1) or (cmd.ParamByName['F'] <> nil) or (cmd.CommandID = opFreq) then
      DoFreqTable(df, varnames)
    else if Cmd.ParamByName['FV'] <> nil then
      DoFV(df, varnames)
    else
      DoStratifiedTable(df, varnames);

    result := true;
  finally
    WeightName := '';
    Weighted := False;
    if Assigned(df) then FreeAndNil(df);
    if Assigned(agl) then FreeAndNil(agl);
    self.Cmd := nil;
    dm.Sendoutput();         // finally update viewer.
  end; //
end;

// ============================================================================
// Private/Protected methodes.
// ============================================================================

procedure TTables.DoFreqTable(Dataframe: TEpiDataframe; Varnames: TStrings);
var
  SumTab: TSumTable;
begin
  try
    // Step 2a: Create sumtable.
    sumtab := CreateFreqTable(Dataframe, varnames);  // frequency tables

    if not Assigned(sumtab) then
      exit;

    // Step 2b: Create resultvariables.
    SumtableToResultvar(SumTab);

    // Step 3: Do statistics.
    // None known so far.

    // Step 4: Output
    OutFreqTable(Sumtab);
  finally
    if Assigned(sumtab) then FreeAndNil(sumtab);
  end;
end;

procedure TTables.DoStratifiedTable(Dataframe: TEpiDataframe; Varnames: TStrings);
var
  sumtab: TSumTable;
  s: string;
  Statistics: boolean;
begin
  // Step 2a: Create sumtable.
  sumtab := nil;
  try
    sumtab := CreateStratTable(Dataframe, varnames);

    if not Assigned(sumtab) then
      exit;

    // Step 2b: Create resultvariables.
    //  SumtableToResultvar(SumTab);


    // Step 3: Statistics
    Statistics := false;
    collectestimates(s);
    if ((length(trim(s)) > 0) or  (cmd.ParamExists['CT'])) {or  (cmd.ParamByName['OA'] <> nil))} then
    begin
      Statistics := true;
      SKTableAnalysis(sumtab, cmd, s);
      if (sumtab.TableType in [3,4,7,8]) then      // add 2x2 estimates
        get2x2statistics(sumtab);
    end;

    // TODO -OTorsten : Seperate sumtable for statistics!
    SumtableToResultvar(SumTab);

    // Step 4: Output
    if (cmd.ParamExists['CT']) { or (cmd.ParamByName['OA'] <> nil)} then
      OutEpi(SumTab)
//    else if (cmd.ParamByName['FV'] <> nil) then
//      OutFV(SumTab)
    else
    begin
      Outtable(sumtab);
      if (Cmd.ParamByName['S'] <> nil) or ((statistics)  and  (SumTab.TableCount > 1)) then
      OutS(SumTab);
    end;

  finally
    if Assigned(sumtab) then FreeAndNil(sumtab);
  end;
end;

procedure TTables.DoFV(Dataframe: TEpiDataframe; Varnames: TStrings);
var
  SumTab: TSumTable;
  S, T: string;
  i: integer;
begin
  dm.Info('Option ''FV'' not fully implemented in tables command. e.g. <br> /t could give <font style="color: red">internal error</font>', [], 41007);
  try
    // Step 2a: Create sumtable.
    SumTab := CreateFVTable(Dataframe, varnames);  // frequency tables

    if not Assigned(SumTab) then
      exit;

    // Step 2b: Create resultvariables.
//    SumtableToResultvar(SumTab);

    // Step 3: Do statistics
    // None known so far.

    // TODO : Remove options check when implemented.
    t := '';
    for i := cmd.ParameterCount-1 downto 0 do
    begin
      if AnsiContainsText(' T GAM EX ADV ', ' '+cmd.Param[i].VarName+' ') then
      begin
        t := t + ' /' + cmd.Param[i].VarName;
        cmd.ParameterList.RemoveVar(cmd.Param[i]);
      end;
    end;
    if (t <> '') then
      dm.info('Option %s not implemented yet', [t], 203034);

//    CollectEstimates(s);
//    SKTableAnalysis(SumTab, cmd, s);

    // Step 4: Output
    OutFVTable(Sumtab);
  finally
    if Assigned(sumtab) then FreeAndNil(sumtab);
  end;
end;


function TTables.OutTwoWayTable(const TwoWayTable: TTwoWayTable): TStatTable;
var
  xtab: TStatTable;
  i, j: integer;
begin
  xtab := dm.OutputList.NewTable(TwoWayTable.ColumnCount+1, TwoWayTable.RowCount+1);
  xtab.TableType := sttNormal;

  if TwoWayTable.fcaption <> '' then
    xtab.Caption := xtab.Caption + '<br>';
  xtab.Caption := xtab.Caption + TwoWayTable.fColHeader;
  xtab.Cell[1,1] := AdjustOutput(TwoWayTable.fRowHeader);

  xtab.Cell[1,1] := TwoWayTable.fCaption;

  // Column headers:
  for i := 1 to TwoWayTable.ColumnCount do
    xtab.Cell[i+1,1] := TwoWayTable.fColLabel.Get(i-1);

  // Row Headers:
  for i := 1 to TwoWayTable.RowCount do
    xtab.Cell[1,i+1] := TwoWayTable.fRowLabel.Get(i-1);


  // Cell values:
  for i := 1 to TwoWayTable.ColumnCount do
    for j := 1 to TwoWayTable.RowCount do
      xtab.Cell[i+1,j+1] := IntToStr(TwoWayTable.Cell[i,j].N);

  result := xtab;
end;

function TTables.AddTotals(const Tab: TStatTable; const TwoWayTable: TTwoWayTable): string;
var
  i: integer;
begin
  Tab.AddRow;
  Tab.AddColumn;
  OutputOptions := OutputOptions or outTotal;

  // Column Totals:
  tab.Cell[1,tab.RowCount] := 'Total';
  for i := 1 to TwoWayTable.ColumnCount do
    tab.Cell[i+1,tab.RowCount] := IntToStr(TwoWayTable.ColTotal[i]);

  // Row Totals:
  tab.Cell[tab.ColCount,1] := 'Total';
  for i := 1 to TwoWayTable.RowCount do
    tab.Cell[tab.ColCount,i+1] := IntToStr(TwoWayTable.RowTotal[i]);

  // Grand total:
  tab.Cell[tab.ColCount, tab.RowCount] := IntToStr(TwoWayTable.Total);
end;


function TTables.AddPercents(const Tab: TStatTable; const TwoWayTable: TTwoWayTable; Cmd :TCommand): string;
var
  s : string;
  i, j: integer;
  TableCell: TTableCell;
  ShowRowPct, ShowColPct, ShowTotalPct: Boolean;
  Fmts: TTableFormats;
begin
  ShowRowPct := ((Cmd.ParamByName['RP'] <> nil) or (Cmd.ParamByName['R'] <> nil));
  ShowColPct := ((Cmd.ParamByName['CP'] <> nil) or (Cmd.ParamByName['C'] <> nil));
  ShowTotalPct := ((Cmd.ParamByName['TP'] <> nil));
  if not (ShowRowPct or ShowColPct or ShowTotalPct) then
    exit;

  if ShowRowPct then OutputOptions := OutputOptions or outRPct;
  if ShowColPct then OutputOptions := OutputOptions or outCPct;
  if ShowTotalPct then OutputOptions := OutputOptions or outTPct;

  GetFormats(Cmd, Fmts);

  result := '';
  if ShowRowPct then   result := result + Trim(EpiPercentFormat('Row', Fmts.rfmt)) + '&nbsp;';
  if ShowColPct then   result := result + Trim(EpiPercentFormat('Col', Fmts.cfmt)) + '&nbsp;';
  if ShowTotalPct then result := result + Trim(EpiPercentFormat('Total', Fmts.tfmt)) + '&nbsp;';

  // Insert new column for each selected persent type
  if Assigned(Cmd.ParamByName['PCT']) then
  begin
    dm.Info('Option %s not implemented yet', ['/PCT'], 203034);
    //OutputOptions := OutputOptions or outMPct;
    exit;
  end;

  // Cell percents.
  for i := TwoWayTable.ColumnCount downto 1 do
  begin
    Tab.InsertColumn(1+i);
    for j := 1 to TwoWayTable.RowCount do
    begin
      s := '';
      TableCell := TwoWayTable.Cell[i,j];
      if ShowRowPct then s := s + EpiPercentFormat(TableCell.rpct, Fmts.rfmt) + '&nbsp;';
      if ShowColPct then s := s + EpiPercentFormat(TableCell.cpct, Fmts.cfmt) + '&nbsp;';
      if ShowTotalPct then s := s + EpiPercentFormat(TableCell.tpct, Fmts.tfmt) + '&nbsp;';
      Tab.Cell[2+i, j+1] := trim(s);
    end;

    if (OutputOptions and outTotal) = outTotal then
    begin
      // Column total percents.
      s := '';
      if ShowRowPct  then  s := s + EpiPercentFormat(TwoWayTable.ColTotal[i]/TwoWayTable.Total*100, Fmts.rfmt) + '&nbsp;';
      if ShowColPct  then  s := s + EpiPercentFormat(100, Fmts.cfmt) + '&nbsp;';
      if ShowTotalPct then s := s + EpiPercentFormat(TwoWayTable.ColTotal[i]/TwoWayTable.Total*100, Fmts.tfmt)+ '&nbsp;';
      tab.Cell[2+i, Tab.RowCount] := trim(s);
    end;
  end;

  if (OutputOptions and outTotal) = outTotal then
  begin
    // Row total percents.
    Tab.InsertColumn(Tab.ColCount);
    Tab.Cell[Tab.ColCount, 1] := Fmts.TotPctHdr;
    for i := 1 to TwoWayTable.RowCount do
    begin
      s := '';
      if ShowRowPct  then  s := s + EpiPercentFormat(100, Fmts.rfmt) + '&nbsp;';
      if ShowColPct  then  s := s + EpiPercentFormat(TwoWayTable.RowTotal[i]/TwoWayTable.Total*100, Fmts.cfmt) + '&nbsp;';
      if ShowTotalPct then s := s + EpiPercentFormat(TwoWayTable.RowTotal[i]/TwoWayTable.Total*100, Fmts.tfmt)+ '&nbsp;';
      tab.Cell[Tab.ColCount, i+1] := trim(s);
    end;
  end;
end;

function TTables.CreateStratTable(df: TEpiDataFrame; Varnames: TStrings): TSumTable;
var
  i, j, k, l,                       // local for-loop vars
  subcount, cols, rows,             // count vars for tables, subtables etc.
  dfoffset, xtot,                   // Offset for dataframe rows.
  loopcount, N,
  tabcount:             integer;
  sumtable:             TSumTable;
  subtable, margtable:  TTwoWayTable;
  vlist:                TStrings;
  hasharray:            array of TStringArray;
  xvec, yvec:           TEpiVector;
  s1, s2,
  colheader, rowheader: string;
  opt:                  TEpiOption;
  dataframe:            TEpiDataFrame;
  Cont:                 Boolean;

  function LevelChange(Index: integer): boolean;
  var
    i: integer;
  begin
    result := true;
    if Index > Dataframe.RowCount then exit;
    for i := 0 to vlist.Count-3 do
      if Dataframe.VectorByName[vlist[i]].compare(Index, Index -1) <> 0 then exit;
    result := false;
  end;

begin
  try
    vlist := TStringList.Create();

    if (cmd.ParamExists['CT']) { or (cmd.ParamByName['OA'] <> nil)) }then
      loopcount := varnames.Count -1
    else
      loopcount := 1;

    sumtable := TSumTable.Create();

    tabcount := 0;
    for l := 1 to loopcount do
    begin
      if loopcount > 1 then
      begin
        vlist.Clear;
        vlist.Add(varnames[0]);
        vlist.Add(varnames[l]);
      end else
        vlist.AddStrings(varnames);
      vlist.Add('$S');

      try
        if dm.IsMissingOption(cmd) then
          dataframe := df.prepareDataframe(vlist, nil)
        else
          dataframe := df.prepareDataframe(vlist, vlist);

        if dataframe.SelectedRowCount = 0 then
        begin
          if loopcount > 1 then dm.Info('No Data using %s %s', [vlist[0], vlist[1]], 0);
          continue;
        end;
        vlist.Delete(vlist.IndexOf('$S'));

        // Move the two last variables to be sorted with lowest priority!
        vlist.Move(0, vlist.Count-1);
        vlist.Move(0, vlist.Count-1);

        // Ugly way to find stratified vars and levels.
        setlength(hasharray, vlist.Count);
        for i:=0 to vlist.Count -1 do
        begin
          dataframe.Sort(vlist[i]);
          hasharray[i] := TStringArray.create();
          for j:=1 to dataframe.RowCount do
            if dm.IsMissingOption(cmd) then
              hasharray[i].Add(dataframe.VectorByName[vlist[i]].AsString[j],
                               dataframe.VectorByName[vlist[i]].Value[j])
            else
              if (not dataframe.VectorByName[vlist[i]].IsMissingValue[j]) then
                hasharray[i].Add(dataframe.VectorByName[vlist[i]].AsString[j],
                                 dataframe.VectorByName[vlist[i]].Value[j]);
          if hasharray[i].Find('.', j) then
            hasharray[i].MoveToBack(j);
        end;

        subcount := 1;
        for i:=0 to high(hasharray)-2 do
          subcount := subcount * hasharray[i].Count;

        // Fill subtables.
        cols := hasharray[high(hasharray)-1].Count;
        rows := hasharray[high(hasharray)].Count;
        dfoffset := 1;

        if (cols = 2) and (rows = 2) then
          sumtable.fTableType := 3
        else
          sumtable.fTableType := 5;

        if (cmd.ParamExists['CT']) {or (cmd.ParamByName['OA'] <> nil)} then
          if (cols = 2) and (rows = 2) then
            sumtable.fTableType := 7
          else
            sumtable.fTableType := 9;

        // TODO: add tabletypes for "mrtable"
        if Weighted then inc(sumtable.fTableType);

        colheader := Dataframe.VectorByName[vlist[vlist.Count-2]].GetVariableLabel(Cmd.ParameterList);
        rowheader := Dataframe.VectorByName[vlist[vlist.Count-1]].GetVariableLabel(Cmd.ParameterList);

        sumtable.fSumtableCaption := Dataframe.VectorByName[vlist[vlist.Count-2]].GetVariableLabel(Cmd.ParameterList) +
                                     ' by '
                                     + Dataframe.VectorByName[vlist[vlist.Count-1]].GetVariableLabel(Cmd.ParameterList);
        if vlist.count > 2 then   sumtable.fSumtableCaption := sumtable.fSumtableCaption + ' adjusted for ';
        for i := 0 to vlist.Count-3 do
          sumtable.fSumtableCaption := sumtable.fSumtableCaption + ' ' +
                                       Dataframe.VectorByName[vlist[i]].GetVariableLabel(Cmd.ParameterList);

        if loopcount > 1 then
          sumtable.fSumtableCaption := Dataframe.VectorByName[varnames[0]].GetVariableLabel(Cmd.ParameterList);

        if l=1 then
        begin
          margtable := TTwoWayTable.Create(cols, rows);
          margtable.fcaption := '';
          margtable.fColHeader := colheader;
          margtable.fRowHeader := rowheader;
        end;

        dataframe.Sort(vlist.CommaText);

        while dfoffset<=dataframe.RowCount do
        begin
          subtable := TTwoWayTable.Create(cols,rows);
          cont := true;

          for j:=0 to vlist.Count-3 do
          begin
            subtable.fcaption := subtable.fcaption + '&nbsp;' +
                                 dataframe.VectorByName[vlist[j]].GetVariableLabel(Cmd.ParameterList) +
                                 ': ' +
                                 dataframe.VectorByName[vlist[j]].GetValueLabel(dataframe.VectorByName[vlist[j]].AsString[dfoffset], Cmd.ParameterList);

            if j < vlist.Count -3  then
              subtable.fcaption := subtable.fcaption + '<br>';
          end;
          subtable.fcaption := TrimRight(subtable.fcaption);
          if loopcount > 1 then
            subtable.fCaption := Dataframe.VectorByName[vlist[1]].GetVariableLabel(Cmd.ParameterList);
          subtable.fColHeader := colheader;
          subtable.fRowHeader := rowheader;

          for j:=0 to cols-1 do
          begin
            s1 := hasharray[high(hasharray)-1].Get(j);
            xvec := dataframe.VectorByName[vlist[high(hasharray)-1]];
            subtable.fColLabel.Add(xvec.GetValueLabel(s1, Cmd.ParameterList),
                                   hasharray[high(hasharray)-1].GetValue(j));
            if tabcount=0 then
              margtable.fColLabel.Add(xvec.GetValueLabel(s1, Cmd.ParameterList),
                                      hasharray[high(hasharray)-1].GetValue(j));
            for k:=0 to rows-1 do
            begin
              s2 := hasharray[high(hasharray)].Get(k);
              yvec := dataframe.VectorByName[vlist[high(hasharray)]];
              if j=0 then
                subtable.fRowLabel.Add(yvec.GetValueLabel(s2, Cmd.ParameterList),
                                       hasharray[high(hasharray)].GetValue(k));
              if (tabcount=0) and (j=0) then
                margtable.fRowLabel.Add(yvec.GetValueLabel(s2, Cmd.ParameterList),
                                        hasharray[high(hasharray)].GetValue(k));
              while (dfoffset<=dataframe.RowCount) and Cont and
                    (trim(xvec.AsString[dfoffset]) = s1) and (trim(yvec.AsString[dfoffset]) = s2) do
              begin
                N := dataframe.VectorByName['$S'].AsInteger[dfoffset];
                inc(subtable.fCells[j,k].fN, N);
                // This test because we wish to print the table if it is not 2x2 later on... 
                if (j < margtable.ColumnCount) and (k < margtable.RowCount) then
                  inc(margtable.fCells[j,k].fN, N);
                inc(dfoffset);
                Cont := not LevelChange(dfoffset);
              end;
            end;
          end;

          for j:=0 to cols-1 do
          begin
            xtot := subtable.ColTotal[j+1];
            for k:=0 to rows-1 do
            with subtable.fCells[j,k] do begin
              if subtable.RowTotal[k+1]>0 then
                fRpct := 100 * (fN / subtable.RowTotal[k+1]);
              if xtot > 0 then
                fCpct := 100 * (fN / xtot);
              fTpct := 100 * (fN / subtable.Total);
            end;
          end;

          subtable.fDF := (subtable.RowCount-1)*(subtable.ColumnCount-1);

          // test for conditions on all subtables here:
          if  (cmd.ParamExists['CT']) {or (cmd.ParamByName['OA'] <> nil)}  and
            (sumtable.fTableType in [1,2,3,4,5,6,9,10,11,12]) then
          begin
            dm.CodeMaker.OutputTable(OutTwoWayTable(subtable));
            dm.error('Table not 2x2 for: %s', [Dataframe.VectorByName[vlist[vlist.Count-1]].Name], 41003);
          end;

          // now consider sort order: - notice default for with /O /RR and /OA where /SD is default:
          if (Sumtable.TableType in [3,4]) and (cmd.ParamExists['O']) or (cmd.ParamExists['RR']) or (cmd.ParamExists['CT']) then
          begin
            if Dataframe.VectorByName[vlist[vlist.Count-2]].FieldDataType = EpiTyBoolean then
              Subtable.SortByRowLabelText(true)
            else
              Subtable.SortByRowLabel(true);
            if Dataframe.VectorByName[vlist[vlist.Count-1]].FieldDataType = EpiTyBoolean then
              SubTable.SortByColumnLabelText(true)
            else
              SubTable.SortByColumnLabel(true);
          end else begin
            // default sort on values for row labels and column labels, except with /OR /RR and /OA where /SA is default:
            subtable.SortByColumnLabel(false);
            subtable.SortByRowLabel(false);
          end;

          SortTable(subtable);

          SumTable.AddSubTable(SubTable);
          inc(tabcount);
        end; // END SubTable
      finally
        //TODO -oTorsten : if Assigned(dataframe) then FreeAndNil(dataframe);
        if System.Length(hasharray) > 0 then
          for j := 0 to System.Length(hasharray) -1 do
            if Assigned(hasharray[j]) then FreeAndNil(hasharray[j]);
      end;
    end; // LOOPCOUNT!

    // Marginal table

    for j:=0 to cols-1 do
    begin
      xtot := MargTable.ColTotal[j+1];
      for k:=0 to rows-1 do
      with MargTable.fCells[j,k] do begin
        if MargTable.RowTotal[k+1]>0 then
          fRpct := 100 * (fN / MargTable.RowTotal[k+1]);
        if xtot > 0 then
          fCpct := 100 * (fN / xtot);
        fTpct := 100 * (fN / MargTable.Total);
      end;
    end;

    // Primary sort options, may be chosen otherwise by user!
    // now consider other sort order:
    if (Sumtable.TableType in [3,4]) and (cmd.ParamExists['O']) or (cmd.ParamExists['RR']) or (cmd.ParamExists['CT']) then
    begin
      if Dataframe.VectorByName[vlist[vlist.Count-2]].FieldDataType = EpiTyBoolean
         then MargTable.SortByRowLabelText(true)
         else Margtable.SortByRowLabel(true);
      if Dataframe.VectorByName[vlist[vlist.Count-1]].FieldDataType = EpiTyBoolean
         then MargTable.SortByColumnLabelText(true)
         else MargTable.SortByColumnLabel(true);
    end else begin
      // default sort on values for row labels and column labels:
      MargTable.SortByRowLabel(false);
      MargTable.SortByColumnLabel(false);
    end;

    SortTable(MargTable);

    SumTable.fMargTable := margtable;
    result := sumtable;
  finally
    if Assigned(vlist) then FreeAndNil(vlist);
    if Assigned(dataframe) then FreeAndNil(dataframe);
  end;
end;

function TTables.CreateFVTable(df: TEpiDataframe; Varnames: TStrings): TSumTable;
var
  i, j, k, dfoffset: integer;
  VList: TStrings;
  SubTable: TTwoWayTable;
  TempDF: TEpiDataframe;
  XArray, YArray: TStringArray;
  XVec, YVec, NVec: TEpiVector;
  s1, s2: string;

begin
  vlist := TStringList.Create();

  result := TSumTable.Create();
//  setlength(result.fSubtables, varnames.Count - 1);

  for i := 1 to varnames.count -1 do
  begin
    vlist.Add(Varnames[0]);
    vlist.Add(varnames[i]);
    vlist.Add('$S');

    if dm.IsMissingOption(cmd) then
      TempDF := Df.PrepareDataframe(vlist, nil)
    else
      TempDF := Df.PrepareDataframe(vlist, vlist);

    if TempDF.SelectedRowCount = 0 then
    begin
      dm.Info('No Data using %s %s', [vlist[0], vlist[1]], 0);
      vlist.Clear;
      continue;
    end;
    vlist.Delete(vlist.Count-1); // Remove '$S'

    XArray := TStringArray.create();
    YArray := TStringArray.create();
    XVec := TempDF.VectorByName[vlist[0]];
    YVec := TempDF.VectorByName[vlist[1]];
    NVec := TempDF.VectorByName['$S'];

    TempDF.Sort(vlist[0]);
    for j:=1 to TempDF.RowCount do
      XArray.Add(XVec.AsString[j], XVec.Value[j]);
    TempDF.Sort(vlist[1]);
    for j:=1 to TempDF.RowCount do
      YArray.Add(YVec.AsString[j], YVec.Value[j]);

    SubTable := TTwoWayTable.Create(XArray.Count, YArray.Count);
    SubTable.fCaption := YVec.GetVariableLabel(Cmd.ParameterList);
    dfoffset := 1;

    SubTable.fColHeader := XVec.GetVariableLabel(Cmd.ParameterList);
    SubTable.fRowHeader := YVec.GetVariableLabel(Cmd.ParameterList);

    TempDF.Sort(VList.CommaText);
    for j:=0 to XArray.Count-1 do
    begin
      s1 := XArray.Get(j);
      subtable.fColLabel.Add(xvec.GetValueLabel(s1, Cmd.ParameterList),
                             XArray.GetValue(j));
      for k:=0 to YArray.Count-1 do
      begin
        s2 := YArray.Get(k);
        if j=0 then
          subtable.fRowLabel.Add(yvec.GetValueLabel(s2, Cmd.ParameterList),
                                 YArray.GetValue(k));
        while (dfoffset<=TempDF.RowCount) and (trim(xvec.AsString[dfoffset]) = s1) and
           (trim(yvec.AsString[dfoffset]) = s2) do 
        begin
          inc(subtable.fCells[j,k].fN, NVec.AsInteger[dfoffset]);
          inc(dfoffset);
        end;
      end;
    end;

    // Calculate percents.
    for j:=1 to subtable.ColumnCount do
    begin
      for k:=1 to subtable.RowCount do
      with subtable.Cell[j,k] do begin
        if subtable.RowTotal[k] > 0 then
          fRpct := 100 * (N / subtable.RowTotal[k]);
        if subtable.ColTotal[j] > 0 then
          fCpct := 100 * (N / subtable.ColTotal[j]);
        fTpct := 100 * (N / subtable.Total);
      end;
    end;

    SortTable(subtable);

    Result.AddSubTable(SubTable);
//    result.fSubtables[i-1] := SubTable;

    vlist.Clear;
    if Assigned(TempDF) then FreeAndNil(TempDF);
    if Assigned(XArray) then FreeAndNil(XArray);
    if Assigned(YArray) then FreeAndNil(YArray);
  end;
  if Assigned(VList) then FreeAndNil(VList);
end;

function TTables.AdjustOutput(s: string): string;
var i: integer;
begin
  result := s;
  if length(s) <= 20 then exit;
  i := 20;
  while (s[i] <> ' ') and (i < length(s)) do inc(i);
  if i = length(s) then exit;
  result := LeftStr(s, i) + '<br>' + RightStr(s, length(s)-i);
end;

function TTables.CreateFreqTable(df: TEpiDataFrame;  Varnames: TStrings): TSumTable;
var
  i, j, k, index, count: integer;
  total: EpiUnInt;
  vec: TEpiVector;
  cvec: TEpiVector;
  s1: string;
  StrArray: TStringArray;
  Sumtable: TSumTable;
  TwowayTable: TTwoWayTable;
  Dataframe: TEpiDataframe;
  newvars: TStrings;
const
  procname = 'CreateFreqTable';
  procversion = '1.0.0.0';
begin
  ODebug.IncIndent;
  ODebug.Add(UnitName + ':' + procname + ' - ' + procversion, 1);
  result := nil;
  Sumtable := TSumTable.Create();
  sumtable.fTableType := 1;
  if Weighted then inc(sumtable.fTableType);

  for k := 0 to varnames.Count-1 do
  begin
    StrArray := TStringArray.Create();
    NewVars := TStringList.Create();
    newvars.Add(varnames[k]);
    newvars.Add('$S');

    if dm.IsMissingOption(cmd) then
      dataframe := df.prepareDataframe(newvars, nil)
    else
      dataframe := df.prepareDataframe(newvars, newvars);


    if (dataframe.RowCount = 0) then
    begin
      if Assigned(NewVars) then FreeAndNil(NewVars);
      if Assigned(StrArray) then FreeAndNil(StrArray);
      if Assigned(Dataframe) then FreeAndNil(Dataframe);
      dm.error('Variable %s contains no data', [varnames[k]], 41004, -1);
      continue;
    end;

    dataframe.Sort(newvars[0]);

    for j:=1 to dataframe.RowCount do
      if (cmd.ParamByName['M'] <> nil) then
        StrArray.Add(dataframe.VectorByName[newvars[0]].AsString[j],
                     dataframe.VectorByName[newvars[0]].Value[j])
      else
        if (not dataframe.VectorByName[newvars[0]].IsMissingValue[j]) then
          StrArray.Add(dataframe.VectorByName[newvars[0]].AsString[j],
                       dataframe.VectorByName[newvars[0]].Value[j]);
    if StrArray.Find('.', j) then
      StrArray.MoveToBack(j);

    try
      total := 0;
      vec := dataframe.VectorByName[newvars[0]];
      cvec := dataframe.VectorByName['$S'];
      for i := 1 to dataframe.RowCount do
        inc(total, cvec.AsInteger[i]);

      //sumtable.fSumtableCaption := vec.VariableLabel;
      TwowayTable := TTwoWayTable.Create(1, StrArray.Count);
      TwowayTable.fColLabel.Add('N', 0);
      TwowayTable.fCaption := vec.GetVariableLabel(Cmd.ParameterList);
      if TwowayTable.fCaption = '' then
        TwowayTable.fCaption := vec.Name;

      index := 0;
      i := 1;
      while (i <= dataframe.RowCount) and (index <= StrArray.Count) do
      begin
        count := 0;
        s1 := StrArray.Get(index);
        TwowayTable.fRowLabel.Add(vec.GetValueLabel(s1, Cmd.ParameterList), StrArray.GetValue(index));
        while (i <= dataframe.RowCount) and (s1 = trim(vec.AsString[i])) do
        begin
          inc(count, cvec.AsInteger[i]);
          inc(i);
        end;
        TwowayTable.fCells[0,index].fN := count;
        TwowayTable.fCells[0,index].fTpct := (count/total)*100;
        inc(index);
      end;
      //sorting should be added here
      // Primary sort options, may be chosen otherwise by user!

      TwoWayTable.SortByRow(1,false);
      // options:              TODO was j - assumed k=variable . Sort incorrect
      if SortOptions('SLA', k)  then TwoWayTable.SortByRowLabelText(false);
      if SortOptions('SLD', k)  then TwoWayTable.SortByRowLabelText(true);
      if SortOptions('SA', k)   then TwoWayTable.SortByRow(1, false);
      if SortOptions('SD', k)   then  TwoWayTable.SortByRow(1, true);
      if SortOptions('SRAT', k) then TwoWayTable.SortByRowTotal(false);
      if SortOptions('SRDT', k) then TwoWayTable.SortByRowTotal(true);

      SetLength(Sumtable.fSubtables, k+1);
      sumtable.fSubtables[k] := TwowayTable;
    finally
      vec := nil;
      cvec := nil;
      if Assigned(NewVars) then FreeAndNil(NewVars);
      if Assigned(StrArray) then FreeAndNil(StrArray);
      if Assigned(Dataframe) then FreeAndNil(Dataframe);
    end;
  end;
  if (sumtable.GetTableCount = 0) then
    FreeAndNil(SumTable);
  result := sumtable;
  ODebug.DecIndent;
end;

function TTables.SortOptions(sortname: string; var index: integer): boolean;
var
  param: IValue;
begin
  result := false;
  param := cmd.ParamByName[sortname];
  if param = nil then exit;
  result := true;
  if param.AsString = '' then
    exit
  else
    index := cmd.ParamByName[sortname].AsInteger;
end;

procedure TTables.SortTable(aTable: TTwoWayTable);
var
  j: integer;
begin
  // Primary sort options, may be chosen otherwise by user!
  // should be fixed here to only sort on subtables for 2x2 tables. ?
  {  Indicate by /Sxxx where the x indicate:
          R:row  C:Column  A:Ascending  D:Descending  T:Total  L:label (else numerical)
       Accepted combinations:
          On value of category for Row and Column: /SA /SD
          On label (text sort) for row and column:  /SLA, /SLD
          Row and/or Column Totals: /SRAT, /SCAT, /SRDT, /SCDT
          Indicate specific column or row: /SRD=x, /SCD=x, /SRA=x, /SCA=x (e.g. /sca=2)
               For string variables use "label" sorting, e.g. /SLA /SLD
          Frequency tables: /SA /SD /SLA /SLD /SRAT /SRDT
   }

  // sort numerical:
  if SortOptions('SD', j) then       // sort descending
  begin
    aTable.SortByRowLabel(true);
    aTable.SortByColumnLabel(true);
  end;

  if SortOptions('SA', j) then       // sort ascending numerical
  begin
    aTable.SortByColumnLabel(false);
    aTable.SortByRowLabel(false);
  end;

  // sort label text
  if SortOptions('SLD', j) then       // sort descending
  begin
    aTable.SortByRowLabelText(true);
    aTable.SortByColumnLabelText(true);
  end;

  if SortOptions('SLA', j) then       // sort ascending
  begin
    aTable.SortByColumnLabelText(false);
    aTable.SortByRowLabelText(false);
  end;

  if SortOptions('SR', j) then aTable.SortByRowLabel(true);
  if SortOptions('SC', j) then aTable.SortByColumnLabel(true);

  // Secondary user sort options:
  if SortOptions('SRA', j) then aTable.SortByRow(j, false);
  if SortOptions('SRD', j) then aTable.SortByRow(j, true);
  if SortOptions('SCA', j) then aTable.SortByColumn(j, false);
  if SortOptions('SCD', j) then aTable.SortByColumn(j, true);

  // sort on totals
  if SortOptions('SRAT', j) then aTable.SortByRowTotal(false);
  if SortOptions('SRDT', j) then aTable.SortByRowTotal(true);
  if SortOptions('SCAT', j) then aTable.SortByColumnTotal(false);
  if SortOptions('SCDT', j) then aTable.SortByColumnTotal(true);
end;


procedure TTables.SumtableToResultvar(SumTable: TSumTable);
var
  i, j, k: integer;
  iname,s: string;
  Table: TTwoWayTable;
  values: array of array of EpiInt;
  labels: array of array of EpiString;

begin
  Dm.AddResult('$tables', EpiTyInteger, SumTable.TableCount, 0 ,0);
  Dm.AddResult('$tabletype', EpiTyInteger, SumTable.TableType, 0, 0);

  for k := 0 to SumTable.TableCount do
  begin
    // No marginal/crude table for frequency tables:
    if (k = 0) and (SumTable.TableType < 3) then continue;
    iname := IntToStr(k);
    if (k = 0) then
    begin
      Table := SumTable.Margtable;
      iname := '';
    end else
      Table := SumTable[k];

    if not Assigned(Table) then continue;
    SetLength(values, Table.ColumnCount+1, Table.RowCount+1);
    SetLength(labels, Table.ColumnCount+1, Table.RowCount+1);
    for i := 0 to Table.ColumnCount do
      for j := 0 to Table.RowCount do
      begin
        if (i or j) = 0 then
          values[i,j] := Table.Total
        else if (i=0) then
        begin
          values[i,j] := Table.RowTotal[j];
          labels[i,j] := Table.fRowLabel.Get(j-1);
        end else if (j=0) then
        begin
          values[i,j] := Table.ColTotal[i];
          labels[i,j] := Table.fColLabel.Get(i-1);
        end else
          values[i,j] := Table.Cell[i,j].N;
      end;
    Dm.AddResult('$table'+iname, EpiTyInteger, Values, 0, 0);
    Dm.AddResult('$labels'+iname, EpiTyString, Labels, 0, 0);
    Dm.AddResult('$rows'+iname, EpiTyInteger, Table.RowCount, 0, 0);
    Dm.AddResult('$cols'+iname, EpiTyInteger, Table.ColumnCount, 0, 0);
    Dm.AddResult('$total'+iname, EpiTyInteger, Table.Total, 0, 0);

    if (Sumtable.TableType < 3) then continue;       // No statistics for frequency tables fix for CI later
    collectestimates(s);

    // add statistics result variables:
    if (trim(s) <> '') then
      begin   // Some statistics, which ones:
      //odds ratios for 2x2 tables
      if (pos(' O ',s) > 0) and ( Sumtable.TableType in [3,4,7,8]) then
        begin
          Dm.AddResult('$OR'+iname, EpiTyFloat, Table.OddsRatio, 0, 0);
          Dm.AddResult('$pOR'+iname, EpiTyFloat, Table.pOR, 0, 0);
          Dm.AddResult('$ORUL'+iname, EpiTyFloat, Table.ORUL, 0, 0);
          Dm.AddResult('$ORLL'+iname, EpiTyFloat, Table.ORLL, 0, 0);
        end;

      if (pos(' RR ',s) > 0) and ( Sumtable.TableType in [3,4,7,8]) then
        begin
          Dm.AddResult('$RR'+iname, EpiTyFloat, Table.RR, 0, 0);
          Dm.AddResult('$RRUL'+iname, EpiTyFloat, Table.RRUL, 0, 0);
          Dm.AddResult('$RRLL'+iname, EpiTyFloat, Table.RRLL, 0, 0);
        end;

      // Chi2
      if (pos(' T ',s) > 0) then
        begin
           Dm.AddResult('$Chi'+iname, EpiTyFloat, Table.CHI, 0, 0);
           Dm.AddResult('$Df'+iname, EpiTyInteger, Table.Df, 0, 0);
           Dm.AddResult('$pChi'+iname, EpiTyFloat, Table.pCHI, 0, 0);
        end;

     // Exact
    if (pos(' EX ',s) > 0) and (table.DF > 0) then
          if (Sumtable.TableType in [3,4,7,8]) then
           Dm.AddResult('$pExact'+iname, EpiTyFloat, Table.FishP2, 0, 0);

      // Kruskal and Goodmann's gamma
    if (pos(' GAM ',s) > 0) then
        begin
          Dm.AddResult('$Gamma' +iname, EpiTyFloat, Table.Gamma, 0, 0);
          Dm.AddResult('$pGamma' +iname, EpiTyFloat, Table.pGamma2, 0, 0);
        end;

       //TODO : add other resultvariables here
     end;
     if sumtable.tablecount = 1 then exit;  // otherwise one subtable shown twice.
  end; // end subtable

  // summary measures 2x2 tables
  if (sumtable.TableCount > 1) then
    begin
    if (pos(' O ',s) > 0) and (SumTable.TableType in [3,4]) then
      begin
        Dm.AddResult('$SumOR', EpiTyFloat, SumTable.SumOR, 0, 0);
        Dm.AddResult('$SumORLL', EpiTyFloat, SumTable.SumORLL, 0, 0);
        Dm.AddResult('$SumORUL', EpiTyFloat, SumTable.SumORUL, 0, 0);
        Dm.AddResult('$SumpOR', EpiTyFloat, SumTable.SumpOR, 0, 0);
      end;

    if (pos(' GAM ',s) > 0) then
      begin
        Dm.AddResult('$PartGamma', EpiTyFloat, SumTable.PartGamma, 0, 0);
        Dm.AddResult('$pPartGamm', EpiTyFloat, SumTable.pPartGamma2, 0, 0);
      end;

    if (pos(' EX ',s) > 0) then
      begin
        Dm.AddResult('$McChi', EpiTyFloat, SumTable.MCCHI, 0, 0);
        Dm.AddResult('$SumDf', EpiTyFloat,SumTable.SumDF, 0, 0);
        Dm.AddResult('$pMcChi', EpiTyFloat, SumTable.pMCCHI, 0, 0);
      end;


    if (pos(' RR ',s) > 0) then
      begin
        Dm.AddResult('$SumRR', EpiTyFloat, SumTable.SumRR, 0, 0);
        Dm.AddResult('$SumRRLL', EpiTyFloat, SumTable.SumRRLL, 0, 0);
        Dm.AddResult('$SumRRUL', EpiTyFloat, SumTable.SumRRUL, 0, 0);
      end;

    if (pos(' T ',s) > 0) then
      begin
        Dm.AddResult('$SumChi', EpiTyFloat,SumTable.SumCHI, 0, 0);
        Dm.AddResult('$SumDf', EpiTyFloat,SumTable.SumDF, 0, 0);
        Dm.AddResult('$SumpChi', EpiTyFloat, SumTable.pSumCHI, 0, 0);
      end;
    end; // end summary measures.

end;

// output frequency tables:
procedure TTables.OutFreqtable(SumTable: TSumTable);
var
  i, j,k,
  cols, rows                 : integer;
  cumval,ul,ll                     : EpiFloat;
  tab                        : TStatTable;
  TwoWayTable                : TTwoWayTable;
  TableCell                  : TTableCell;
  Footer, s                  : string;
  opt                        : TEpiOption;
  allestimates               : string;
  Fmts                       : TTableFormats;

begin
  if (Cmd.ParamByName['Q'] <> nil) then exit;
  //GetFormats(Cmd,efmt, rfmt, cfmt, tfmt,colpcthead,rowpcthead, pctheader, totalpcthead,cifmt,ciheader);
  GetFormats(Cmd, Fmts);
  Fmts.rfmt := 'P' + Fmts.rfmt[2] + '  ';
  // Output all frequency tables:
  for k := 1 to SumTable.TableCount do
  begin
    TwoWayTable := SumTable.SubTable[k];
    if not Assigned(TwoWayTable) then continue;

    rows := TwoWayTable.RowCount;
    tab := dm.OutputList.NewTable(2,(rows+2));
    tab.TableType := sttFreq;

    // caption and headers:
    tab.Caption := TwoWayTable.fcaption ;
    tab.Cell[1,1] := '';
    tab.Cell[2,1] := 'N';
    tab.Cell[1,rows+2] := 'Total';

    // totals:
    tab.Cell[2,rows+2] := IntToStr(TwoWaytable.Total);

    // Counts:
    for i := 1 to rows do
      begin
      tab.Cell[1,i+1] := TwoWayTable.fRowLabel.Get(i-1);
      tab.Cell[2,i+1] := IntToStr(TwoWaytable.Cell[1,i].N);
      end;

    // Row percentages:
    if (cmd.ParamByName['C'] <> nil) or (cmd.ParamByName['R'] <> nil) then
    begin           // add percentages
      tab.AddColumn;
      tab.Cell[3,1] := Fmts.ColPctHdr;
      tab.Cell[3,rows+2] := EpiPercentFormat((100.000),Fmts.Rfmt);
     for i := 1 to rows do
      begin
      tab.Cell[3,i+1] := EpiPercentFormat(TwoWaytable.Cell[1,i].tpct,Fmts.rfmt);
      end;
    end;

    // Binomial Confidence Intervals
    if (cmd.ParamByName['CI'] <> nil) then
    begin           // calculate 95% CI
      tab.AddColumn;
      tab.Cell[(tab.ColCount), 1] := trim(Fmts.CIHdr);
      for i := 1 to rows do
      begin
        // calculate CI based on formula by Altman et al Statistics with Confidence
        EpiProportionCI(TwoWaytable.Cell[1,i].N,TwoWaytable.total,UL,LL);
        tab.Cell[tab.ColCount,i+1] :=
        EpiCIformat(1,100*LL, 100*UL, '%8.' + Fmts.rfmt[2] + 'f', Fmts.cifmt, Fmts.CIHdr,1);
      end;
    end;

    //Cumulative Percentage
    if (cmd.ParamByName['CUM'] <> nil) then
    begin           // add cumulative percent
      tab.AddColumn;
      tab.Cell[tab.ColCount,1] := 'Cum %';
      for i := 1 to rows do
      begin
        cumval := 0;
        for j := 1 to i do
          cumval := cumval + TwoWayTable.Cell[1, j].Tpct;
        tab.Cell[tab.colcount,i+1] := trim(EpiPercentFormat(cumval, copy(Fmts.rfmt,1,2)));
      end;
    end;

    Footer := '';
    if Weighted then
      Footer := footer + 'Weight: ' + WeightName + '<br>';

    // This table done - output to screen.
    dm.CodeMaker.OutputTable(tab, footer);
    // dm.Sendoutput();      // only once per command
    // Terminate output if user cancels process.
    Application.ProcessMessages();
    if dm.Cancelled then exit;
  end; // End of this frequency table, take next one or quit
end;


Procedure Ttables.AddChi2Test(Tab: TStatTable; Sumtable: Tsumtable; Cmd :TCommand; var Small: Boolean);
    // add optional parts to table:
var i:integer;
begin
  If (cmd.ParamByName['T'] = nil) then  exit;
  tab.AddColumn;
  tab.AddColumn;
  if SumTable.TableCount = 1 then
    begin
      Sumtable[1].chi := Sumtable.margtable.fChi;
      Sumtable[1].pchi := Sumtable.margtable.fpchi;
    end;
  tab.Cell[tab.ColCount-1,1] := 'Chi<sup>2</sup><small>(df=1)</small>';
  tab.Cell[tab.ColCount,1] := 'p';
  for i := 1 to SumTable.TableCount do
    begin
      tab.Cell[tab.ColCount-1, i+1] := format('%7.3f',[SumTable[i].CHI]);
      tab.Cell[tab.ColCount, i+1] := format('%7.4f',[SumTable[i].pChi]);
      if SumTable[i].SmallE > 0 then
        begin
          small := True;
          tab.Cell[tab.ColCount-1, i+1] := tab.Cell[tab.ColCount-1, i+1] + '<sup>*</sup>';
        end;
    end;
end;

Procedure Ttables.AddGam(Tab: TStatTable; Sumtable: Tsumtable; Cmd :TCommand; Fmts: TTableFormats);
    // add optional parts to table:
var i:integer;
begin
  If (cmd.ParamByName['GAM'] = nil) then  exit;
  tab.AddColumn;
  tab.AddColumn;
  tab.AddColumn;
  tab.Cell[tab.ColCount-2,1] := 'Gamma';
  tab.Cell[tab.ColCount,1] := 'p';
  for i := 1 to SumTable.TableCount do
    begin
      tab.Cell[tab.ColCount-2,i+1] := Epiformat(SumTable[i].Gamma, Fmts.efmt);
      //tab.Cell[tab.ColCount-1,i+1] := EpiCIformat(1,SumTable[i].gammaLL,SumTable[i].gammaUL,efmt, cifmt,' ',1);
      tab.Cell[tab.ColCount,i+1] := Epiformat(Sumtable.SubTable[i].pGamma2,'%7.3f');
    end;
end;

Procedure Ttables.AddRR(Tab: TStatTable; Sumtable: Tsumtable; Fmts: TTableFormats);
    // add optional parts to table:
var i:integer;
begin
  tab.AddColumn;
  tab.Cell[tab.ColCount,1] := 'RR';
  tab.AddColumn;
  tab.Cell[tab.ColCount,1] := Fmts.CIHdr; // '95% CI';
  for i := 1 to SumTable.TableCount do
    if  (SumTable[i].RowTotal[1] > 0)  and (SumTable[i].RowTotal[2] > 0) then
    begin
     tab.Cell[tab.ColCount-1, i+1] := Epiformat(SumTable[i].rr, Fmts.EFmt);
     tab.Cell[tab.ColCount, i+1] := EpiCIformat(SumTable[i].rr, SumTable[i].rrLL, SumTable[i].rRUL, Fmts.EFmt, Fmts.CIFmt, Fmts.CIHdr, 1);
    end;
end;

Procedure Ttables.AddAR(Tab: TStatTable; Sumtable: Tsumtable; Col: Integer; Fmts: TTableFormats; AddCI: Boolean);
    // add optional parts to table:
var
  i: integer;
  UL, LL: EpiFloat;
begin
  tab.TableType := sttCTStat2;
  tab.AddColumn;
  tab.Cell[tab.ColCount, 1] := Fmts.RRHdr.AR;
  for i := 1 to SumTable.TableCount do
  begin
    tab.Cell[tab.ColCount, i+1] := Trim(EpiPercentFormat(SumTable[i].Cell[1, col].rpct, Fmts.cfmt));
  end;

  if AddCI then
  begin
    tab.TableType := sttCTStat3;
    tab.AddColumn;
    tab.Cell[tab.ColCount,1] := Fmts.CIHdr;
    for i := 1 to SumTable.TableCount do
    begin
      EpiProportionCI(SumTable[i].Cell[1, col].N, SumTable[i].RowTotal[col], UL, LL);
      tab.Cell[tab.ColCount,i+1] := EpiCIformat(1,100*LL, 100*UL, '%8.' + Fmts.cfmt[2] + 'f', Fmts.CIFmt, Fmts.CIHdr,1);
    end;
  end; // end ci
end;

Procedure Ttables.AddOR(Tab: TStatTable; Sumtable: Tsumtable; Cmd :TCommand; force: Boolean; Fmts: TTableFormats);
    // add optional parts to table:
var i:integer;
begin
  If ((force) or (cmd.ParamByName['O'] <> nil)) and (SumTable.TableType in [3,4,7,8]) then
  begin
    tab.AddColumn;
    tab.Cell[tab.ColCount,1] := 'OR';
    tab.AddColumn;
    tab.Cell[tab.ColCount,1] := Fmts.CIHdr; // '95% CI';
    for i := 1 to SumTable.TableCount do
      if  (SumTable[i].RowTotal[1] > 0)  and (SumTable[i].RowTotal[2] > 0) then
        begin
         tab.Cell[tab.ColCount-1, i+1] := Epiformat(SumTable[i].OddsRatio, Fmts.EFmt);
         tab.Cell[tab.ColCount, i+1] := EpiCIformat(SumTable[i].OddsRatio,SumTable[i].ORLL, SumTable[i].ORUL, Fmts.EFmt, Fmts.CIFmt, Fmts.CIHdr,1);
        end; // end i
  end;
end;


Procedure Ttables.AddExact(Tab: TStatTable; Sumtable: Tsumtable; Cmd :TCommand);
    // add optional parts to table:
var i:integer;
begin
  If cmd.ParamByName['EX'] = nil then  exit;
     tab.AddColumn;
     tab.Cell[tab.ColCount,1] := 'p<sub><small>exact</small></sub>';
      for i := 1 to SumTable.TableCount do
          tab.Cell[tab.ColCount, i+1] := format('%7.4f',[SumTable[i].FishP2]);
end;

Procedure Ttables.AddObs(Tab: TStatTable; Sumtable: Tsumtable; Cmd :TCommand);
    // add optional parts to table:
var i:integer;
begin
  If cmd.ParamByName['OBS'] = nil then  exit;
  If (not Sumtable.tabletype in [3,4,7,8]) then exit;
  tab.AddColumn; tab.AddColumn;
  tab.Cell[tab.ColCount-1,1] := '<small>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;'
                       + SumTable.MargTable.fcolLabel.fStrings[0] +'<br>'
                     + SumTable.MargTable.fRowLabel.fStrings[0]+ '&nbsp;&nbsp;a<br>'
                     + SumTable.MargTable.fRowLabel.fStrings[1] + '&nbsp;&nbsp;c<br></small>';
  tab.Cell[tab.ColCount,1] := '<small>'+SumTable.MargTable.fcolLabel.fStrings[1] +'<br>&nbsp;&nbsp;b<br>&nbsp;&nbsp;d<br></small>';
  for i := 1 to SumTable.TableCount do
    begin
      tab.Cell[tab.ColCount-1, i+1] := format('%3d<br>%3d',[SumTable[i].Cell[1,1].N,SumTable[i].Cell[1,2].N]);
      tab.Cell[tab.ColCount, i+1] := format('%3d<br>%3d',[SumTable[i].Cell[2,1].N,SumTable[i].Cell[2,2].N]);
    end;
end;

 // add condensed table for attack rate
procedure Ttables.AddOA(Tab: TstatTable;Sumtable: TSumTable; Col:integer; Fmts: TTableFormats);
var
  i : integer;
  ul,ll : EpiFloat;
begin
  tab.TableType := sttCTStat1;
  tab.AddColumn;
  tab.Cell[tab.ColCount,1] := Fmts.RRHdr.SmallN;
  tab.AddColumn;
  tab.Cell[tab.ColCount,1] := Fmts.RRHdr.Ill;
  for i := 1 to SumTable.TableCount do
  begin
    tab.Cell[tab.ColCount-1, i+1] := Format('%d', [SumTable[i].RowTotal[col]]);
    tab.Cell[tab.ColCount, i+1] := Format('%d', [SumTable[i].Cell[1,col].N]);
  end;
  if col = 1 then tab.AddColumn;
end;

  // add condensed table for cc table
procedure Ttables.AddCC(var tab: TstatTable;Sumtable: TSumTable;col:integer; Fmts: TTableFormats);
var
  i: Integer;
begin
  tab.TableType := sttCTStat1;
  tab.AddColumn;
  tab.Cell[tab.ColCount,1] := '<small>' + Fmts.ORHdr.Exposed + '</small>';
  tab.AddColumn;
  tab.Cell[tab.ColCount,1] := '<small>' + Fmts.ORHdr.NonExposed + '</small>';
  for i := 1 to SumTable.TableCount do
  begin
    tab.Cell[tab.ColCount-1, i+1] := Format('%d', [SumTable[i].Cell[col,1].N]);
    tab.Cell[tab.ColCount, i+1] := Format('%d', [SumTable[i].Cell[col,2].N]);
  end;
  if col = 1 then tab.AddColumn;
end;

// output n*k and n*k*z tables:
procedure TTables.Outtable(SumTable: TSumTable);
var
  i, j, k, z,
  cols, rows, factor,
  tabcols, tabrows           : integer;
  cumval, ul,ll                     : EpiFloat;
  tab                        : TStatTable;
  TwoWayTable                : TTwoWayTable;
  TableCell                  : TTableCell;
  ShowRowPct, ShowColPct,
  SummaryTable, ShowTotalPct, MultiPct     : boolean;
  Footer, s                  : string;
  opt                        : TEpiOption;
  allestimates,cifmt,ciheader: string;
  Fmts:                       TTableFormats;


  function VarInc(var int: integer): integer;
  begin
    result := int;
    inc(int);
  end;

  function addmin(t: TTwowayTable): string;
  begin
     result := '';
     if T.SmallE > 0 then
     result :=
       '<br>&nbsp;&nbsp;Cells<sub>expected&lt;5:</sub>&nbsp;'
       + EpiFormat(TwoWayTable.smalle,'')
       + Format(' (%d pct.)',[round(100*(TwoWayTable.smalle/(TwoWayTable.RowCount*TwoWayTable.ColumnCount)))]);
  end;

begin
  if (Cmd.ParamByName['Q'] <> nil) then exit;
  ShowRowPct := ((Cmd.ParamByName['RP'] <> nil) or (Cmd.ParamByName['R'] <> nil)) and not (SumTable.TableType in [1,2]);
  ShowColPct := ((Cmd.ParamByName['CP'] <> nil) or (Cmd.ParamByName['C'] <> nil)) and  not (SumTable.TableType in [1,2]);
  ShowTotalPct := ((Cmd.ParamByName['TP'] <> nil)) or (SumTable.TableType in [1,2]);


  GetFormats(Cmd, Fmts);

  MultiPct := (Cmd.ParamByName['PCT'] <> nil) and not (SumTable.TableType in [1,2]);

  factor := 1;
  if multipct then
  begin
    if ShowRowPct then inc(factor);
    if ShowColPct then inc(factor);
    if ShowTotalPct then inc(factor);
  end else
    if ShowRowPct or ShowColPct or ShowTotalPct then inc(factor);

  // Output subtables:  - could be as well frequency or other type
  for k := 0 to SumTable.TableCount do
  begin
     // Skip strata tables if requested.
    if ((k>0) and (Cmd.ParamByName['NT'] <> nil)) then
      continue;

    if  {((k=0) and ((SumTable.TableCount = 1) or (SumTable.TableType in [1,2]))) or}
         ((k>0) and (SumTable.TableCount = 1)) or
         ((k=0) and (cmd.ParamByName['NC'] <> nil)) or
           (cmd.ParamByName['FV'] <> nil) or (cmd.ParamByName['CT'] <> nil) or (cmd.ParamByName['OA'] <> nil) then
      continue;


    if k = 0 then
      TwoWayTable := SumTable.MargTable
    else
      TwoWayTable := SumTable.SubTable[k];

    cols := TwoWayTable.ColumnCount;
    rows := TwoWayTable.RowCount;

    //          cols + cell %'s   label + total    total %
    tabcols := (factor*cols)      + 2              + (factor - 1);
    if (SumTable.TableType in [1,2]) then dec(tabcols);
    if (SumTable.TableType in [1,2]) and (Cmd.ParamByName['CUM'] = nil) then Dec(tabcols);
    tabrows := rows + 2;
    tab := dm.OutputList.NewTable(tabcols, tabrows);
    if factor > 1 then
      tab.TableType := sttPercents
    else
      tab.TableType := sttNormal;
    if (SumTable.TableType = 1) then
      tab.TableType := sttFreq;

    tab.Caption := TwoWayTable.fcaption ;

    //Table header
    if (cmd.ParamExists['OR']) then
      tab.Caption := Fmts.ORHdr.Outcome + TwoWayTable.fcaption ;
    if (cmd.ParamExists['RR']) then
      tab.Caption := Fmts.RRHdr.Outcome + TwoWayTable.fcaption ;

    if TwoWayTable.fcaption <> '' then
      tab.Caption := tab.Caption + '<br>';
    tab.Caption := tab.Caption + TwoWayTable.fColHeader; // AdjustOutput(tab.Caption + TwoWayTable.fColHeader);
    tab.Cell[1,1] := AdjustOutput(TwoWayTable.fRowHeader);

    // Row headers:
    for j := 2 to rows+1 do
      tab.Cell[1,j] := TwoWayTable.fRowLabel.Get(j-2);

    tab.Cell[1,rows+2] := 'Total';
    for i := 1 to cols do
    begin
      z := 1;
      tab.Cell[(i-1)*factor+2, 1] := TwoWayTable.fColLabel.Get(i-1);

      if multipct then
      begin
        if ShowRowPct then   tab.Cell[(i-1)*factor+2+VarInc(z), 1] := Fmts.RowPCtHdr;
        if ShowColPct then   tab.Cell[(i-1)*factor+2+VarInc(z), 1] := Fmts.ColPctHdr;
        if ShowTotalPct then tab.Cell[(i-1)*factor+2+VarInc(z), 1] := Fmts.TotPctHdr;
      end else begin
        s := Fmts.PctHdr;
        tab.Cell[(i-1)*factor+3, 1] := trim(s);
      end;

      for j := 1 to rows do
      begin
        z := 1;
        TableCell := TwoWayTable.Cell[i,j];
        tab.Cell[(i-1)*factor+2, j+1] := IntToStr(TableCell.N);

        if multipct then
        begin
          if ShowRowPct then   tab.Cell[(i-1)*factor+2+VarInc(z), j+1] := EpiPercentFormat(TableCell.rpct, Fmts.rfmt);
          if ShowColPct then   tab.Cell[(i-1)*factor+2+VarInc(z), j+1] := EpiPercentFormat(TableCell.cpct, Fmts.cfmt);
          if ShowTotalPct then tab.Cell[(i-1)*factor+2+VarInc(z), j+1] := EpiPercentFormat(TableCell.tpct, Fmts.tfmt);
        end else begin
          s := '';
          if ShowRowPct then   s := s + EpiPercentFormat(TableCell.rpct, Fmts.rfmt) + '&nbsp;';
          if ShowColPct then   s := s + EpiPercentFormat(TableCell.cpct, Fmts.cfmt) + '&nbsp;';
          if ShowTotalPct then s := s + EpiPercentFormat(TableCell.tpct, Fmts.tfmt) + '&nbsp;';
          tab.Cell[(i-1)*factor+3, j+1] := trim(s) + '';
        end;
      end; // End of Row

      // Col total:
      tab.Cell[(i-1)*factor+2, rows+2] := IntToStr(TwoWayTable.ColTotal[i]);
      // Col total pct:
      if factor > 1 then
      begin
      z := 1;
        if multipct then
        begin
          if ShowRowPct then   tab.Cell[(i-1)*factor + 2 + VarInc(z), rows+2] := EpiPercentFormat(TwoWayTable.ColTotal[i]/TwoWayTable.Total*100, Fmts.rfmt) + '&nbsp;';
          if ShowColPct then   tab.Cell[(i-1)*factor + 2 + VarInc(z), rows+2] := EpiPercentFormat(100, Fmts.cfmt) + '&nbsp;';
          if ShowTotalPct then tab.Cell[(i-1)*factor + 2 + VarInc(z), rows+2] := EpiPercentFormat(TwoWayTable.ColTotal[i]/TwoWayTable.Total*100, Fmts.tfmt)+ '&nbsp;';
        end else begin
          s := '';
          if ShowRowPct then   s := s + EpiPercentFormat(TwoWayTable.ColTotal[i]/TwoWayTable.Total*100, Fmts.rfmt) + '&nbsp;';
          if ShowColPct then   s := s + EpiPercentFormat(100, Fmts.cfmt) + '&nbsp;';
          if ShowTotalPct then s := s + EpiPercentFormat(TwoWayTable.ColTotal[i]/TwoWayTable.Total*100, Fmts.tfmt)+ '&nbsp;';
          tab.Cell[(i-1)*factor + 3, rows+2] := trim(s) + '';
        end;
      end;
    end; // End of Column

    // Row totals:
    tab.Cell[(factor*cols)+2,1] := 'Total';
    cumval := 0;
    for j := 2 to rows+1 do
        tab.Cell[(factor*cols)+2,j] := IntToStr(TwoWayTable.RowTotal[j-1]);

    // Row totals pct:
    if factor > 1 then
    begin
      z := 1;
      if multipct then
      begin
        if ShowRowPct then   tab.Cell[factor*cols + 2 + VarInc(z), 1] := Fmts.RowPCtHdr;
        if ShowColPct then   tab.Cell[factor*cols + 2 + VarInc(z), 1] := Fmts.ColPctHdr;
        if ShowTotalPct then tab.Cell[factor*cols + 2 + VarInc(z), 1] := Fmts.TotPctHdr;
      end else begin
        tab.Cell[(factor*cols)+3,1] := Fmts.PctHdr;
      end;
      for j := 2 to rows+1 do
      begin
        z := 1;
        if multipct then
        begin
          if ShowRowPct then   tab.Cell[factor*cols + 2 + VarInc(z), j] := EpiPercentFormat(100, Fmts.rfmt);
          if ShowColPct then   tab.Cell[factor*cols + 2 + VarInc(z), j] := EpiPercentFormat(TwoWayTable.RowTotal[j-1]/TwoWayTable.Total*100, Fmts.cfmt);
          if ShowTotalPct then tab.Cell[factor*cols + 2 + VarInc(z), j] := EpiPercentFormat(TwoWayTable.RowTotal[j-1]/TwoWayTable.Total*100, Fmts.tfmt);
        end else begin
          s := '';
          if ShowRowPct then   s := s + EpiPercentFormat(100, Fmts.rfmt) + '&nbsp;';
          if ShowColPct then   s :=  s + EpiPercentFormat(TwoWayTable.RowTotal[j-1]/TwoWayTable.Total*100, Fmts.cfmt) + '&nbsp;';
          if ShowTotalPct then s :=  s + EpiPercentFormat(TwoWayTable.RowTotal[j-1]/TwoWayTable.Total*100, Fmts.tfmt)+ '&nbsp;';
          tab.Cell[factor*cols+3, j] := trim(s) + '';
        end;
      end;
    end;


    // Grand total:
       tab.Cell[(factor*cols)+2,rows+2] := IntToStr(TwoWayTable.Total);

    // find statistics to add to footer:
   // find out formats and which estimates:
    CollectEstimates(allestimates);

    Footer := '';
      if (factor > 1) then Footer := 'Percents: ';
      if ShowRowPct then   Footer := Footer + Trim(EpiPercentFormat('Row', Fmts.rfmt)) + '&nbsp;';
      if ShowColPct then   Footer := Footer + Trim(EpiPercentFormat('Col', Fmts.cfmt)) + '&nbsp;';
      if ShowTotalPct then Footer := Footer + Trim(EpiPercentFormat('Total', Fmts.tfmt)) + '&nbsp;';
      if (factor > 1) then Footer := Footer + '<br>';

      if ((k=0) and (Sumtable.TableCount > 1)) then
        Footer := 'Unstratified table<br>';

   // add explanation for 2x2 tables:
   if ((cmd.ParamExists['RR']) or (cmd.ParamExists['O']))
         and (TwoWaytable.ColumnCount = 2) and (TwoWayTable.RowCount = 2) then
         Footer := footer + 'Exposure: ' + TwoWayTable .fRowHeader + ' = ' + TwoWayTable.fRowLabel.fStrings[0] +
               '<br>Outcome: ' + TwoWayTable.fColHeader + ' = '+TwoWayTable.fColLabel.fStrings[0] + '<br>';

   if Weighted then
      Footer := footer + 'Weight: ' + WeightName + '<br>';

    if TwoWayTable.DF > 0 then
     begin
       If (cmd.ParamByName['T'] <> nil ) then
         begin
           footer := footer + '<br>&nbsp;&nbsp;Chi<sup>2</sup>='+ EpiFormat(TwoWayTable.CHI,'%7.3f')
                          + ' df(' + trimleft(EpiFormat(TwoWayTable.Df,'%3d'))
                          + ') p= ' + EpiFormat(TwoWayTable.pCHI,'%7.4f ');
           if TwoWayTable.SmallE > 0 then
             footer := footer + addmin(TwoWayTable);   // smallE warning
         end;

       if (cmd.ParamByName['O'] <> nil) and ( Sumtable.TableType in [3,4]) then
           begin
           footer := footer + '<br>&nbsp;&nbsp;Odds Ratio = ' + Epiformat(TwoWayTable.OddsRatio,Fmts.efmt)
                            + EpiCIformat(TwoWayTable.OddsRatio,TwoWayTable.ORLL, TwoWayTable.ORUL, Fmts.EFmt, Fmts.CIFmt, Fmts.CIHdr,2);
           if (cmd.ParamByName['ADV'] <> nil ) then
              footer := footer + '<br>&nbsp;&nbsp;pOR = ' + EpiFormat(TwoWayTable.pOR,'%7.4f ');
           end;
       if (cmd.ParamByName['RR'] <> nil) and ( Sumtable.TableType in [3,4]) then
           footer := footer + '<br>&nbsp;&nbsp;RR = ' + Epiformat(TwoWayTable.RR, Fmts.efmt)
                            + EpiCIformat(TwoWayTable.RR,TwoWayTable.RRLL, TwoWayTable.RRUL, Fmts.efmt, Fmts.cifmt, Fmts.CIHdr,2);

       if (cmd.ParamByName['GAM'] <> nil) then
           footer := footer + format('<br>&nbsp;&nbsp;Gamma = ' + Fmts.efmt,[TwoWayTable.Gamma])
                            + EpiCIformat(1,TwoWayTable.gammaLL,TwoWayTable.gammaUL, Fmts.efmt, Fmts.cifmt, Fmts.CIHdr,2)
                            + format('. Two-sided p= %7.4f',[TwoWayTable.pGamma2]);
    end // informative strata
  else  if (Sumtable.TableType > 2) and (length(allestimates) > 1 ) then Footer := footer + 'Stratum non-informative<br>';


    If (cmd.ParamByName['EX'] <> nil) then
      if (TwoWaytable.ColumnCount = 2) and (TwoWayTable.RowCount = 2) and (TwoWayTable.DF > 0) then
         footer := footer + '<br>Fishers<small><sub>exact</sub><small> p= ' + format('%7.4f',[TwoWayTable.FishP2])
         else
         footer := footer + '<br>Exact p values available: single 2x2 and stratified nxk tables (when df>0)';

    // Strata table done - output to screen.
    dm.CodeMaker.OutputTable(tab, footer);
    //dm.Sendoutput();
    // Terminate output if user cancels process.
    Application.ProcessMessages();
    if dm.Cancelled then exit;
   end; // End of strata tables
end;



procedure TTables.OutS(SumTable: TSumTable);
// summary table output including 'CC' table
var
  tab:  TStatTable;
  cr,c,i: integer;
  footer, allestimates, est: string;
  opt: TEpiOption;
  Fmts : TTableFormats;
  small: Boolean;
  function newcol(var c: integer;n: integer): Boolean;    // add column
  var j : integer;
  begin
    for j := 1 to n do
     begin inc(c); tab.AddColumn; end;
     result := True;
  end;

begin
  if (Cmd.ParamByName['Q'] <> nil) then exit;
  GetFormats(Cmd, Fmts);
  // principle: add from left depending on options in Cmd. one column at a time
   tab := dm.OutputList.NewTable(3, SumTable.TableCount+4);
   tab.TableType := sttNormal;
   tab.Caption := SumTable.fSumtableCaption;
   tab.Cell[1,1] := format('N = %d', [SumTable.MargTable.Total]);

  // Find out which estimates to add:
    CollectEstimates(allestimates);
  c := 0;
  small := False;
  // label and count:
  newcol(c,2);
  tab.Cell[2,1] := 'N';
  tab.Cell[1, 2] := 'Crude';
  tab.Cell[2,2] := Format('%d', [SumTable.margtable.Total]);
  tab.Cell[1,3] := 'Adjusted';
  tab.Cell[2,3] := Format('%d', [SumTable.ValidTotal]);
  tab.cell[1,4]:='<p style="line-height:3px">&nbsp;</p>';

  for i := 1 to SumTable.TableCount do
    begin
      tab.Cell[1,i+4] := SumTable[i].fCaption;
      tab.Cell[c,i+4] := Format('%d', [SumTable[i].Total]);
    end;

  footer := '';
  // add columns to the summary table in order of preference of the user:
  While length(allestimates) > 0 do
  begin
    if (copy(allestimates,1,1) = ' ')  then allestimates := copy(allestimates,2,length(allestimates)-1);

   if pos(' ',allestimates)> 0 then
     est := ansiuppercase(trim(copy(allestimates,1,pos(' ',allestimates))))
     else est := allestimates;

      // Observations in 2x2 tables
  If (est = 'OBS') and (Sumtable.tabletype in [3,4]) then
    begin
      newcol(c,2);
      tab.Cell[c-1,1] := '<small>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;' + SumTable.MargTable.fcolLabel.fStrings[0] +'<br>'
                         + SumTable.MargTable.fRowLabel.fStrings[0] + '&nbsp;&nbsp;a<br>'
                         + SumTable.MargTable.fRowLabel.fStrings[1] + '&nbsp;&nbsp;c<br></small>';
      tab.Cell[c,1] := '<small>'+SumTable.MargTable.fcolLabel.fStrings[1] +'<br>&nbsp;&nbsp;b<br>&nbsp;&nbsp;d<br></small>';
      tab.Cell[c-1, 2] := format('%3d<br>%3d',[SumTable.Margtable.Cell[1,1].N,SumTable.Margtable.Cell[1,2].N]);
      tab.Cell[c, 2] := format('%3d<br>%3d',[SumTable.Margtable.Cell[2,1].N,SumTable.Margtable.Cell[2,2].N]);
      if SumTable.TableCount > 1 then
      begin
      for i := 1 to SumTable.TableCount do
        begin
          tab.Cell[c-1, i+4] := format('%3d<br>%3d',[SumTable[i].Cell[1,1].N,SumTable[i].Cell[1,2].N]);
          tab.Cell[c, i+4] := format('%3d<br>%3d',[SumTable[i].Cell[2,1].N,SumTable[i].Cell[2,2].N]);
        end;
      end;
    end;

   // Odds Ratio
   If  (est = 'O') and (SumTable.Tabletype in [3,4]) then
    begin
      newcol(c,2);
      tab.Cell[c-1,1] := 'OR';
      tab.Cell[c,1] := Fmts.CIHdr;
      tab.Cell[c-1, 2] := Epiformat(SumTable.margtable.OddsRatio,Fmts.EFmt);
      tab.Cell[c, 2] := EpiCIformat(SumTable.margtable.OddsRatio,SumTable.margtable.ORLL,SumTable.margtable.ORUL,Fmts.efmt, Fmts.cifmt,Fmts.CIHdr,1);
      if SumTable.TableCount > 1 then
        begin
          tab.Cell[c-1,3] := Epiformat(SumTable.SumOR,Fmts.efmt);
          tab.Cell[c, 3] :=EpiCIformat(SumTable.SumOR,SumTable.SumORLL,SumTable.SumORUL,Fmts.efmt, Fmts.cifmt,Fmts.CIHdr,1);
          for i := 1 to SumTable.TableCount do
            begin
              tab.Cell[c-1,i+4] := Epiformat(SumTable[i].OddsRatio,Fmts.efmt);
              tab.Cell[c, i+4] := EpiCIformat(SumTable[i].OddsRatio,SumTable[i].ORLL,SumTable[i].ORUL,Fmts.efmt, Fmts.cifmt,Fmts.CIHdr,1);
            end;
        end;
        if (cmd.ParamByName['ADV'] <> nil ) then
        begin
          newcol(c,1);
          tab.Cell[c,1] := 'pOR';
          tab.Cell[c, 2] := Epiformat(SumTable.margtable.pOR,'%7.3f');
          if SumTable.TableCount > 1 then
          begin
            tab.Cell[c,3] := Epiformat(SumTable.SumpOR,'%7.3f');
            for i := 1 to SumTable.TableCount do
              tab.Cell[c,i+4] := Epiformat(SumTable[i].pOR,'%7.3f');
          end;
        end;
     end;


   // Rate Ratio
   If  (est = 'RR') and (SumTable.Tabletype in [3,4]) then
    begin
      newcol(c,2);
      tab.Cell[c-1,1] := 'RR';
      tab.Cell[c,1] := Fmts.CIHdr;
      tab.Cell[c-1, 2] := Epiformat(SumTable.margtable.RR,Fmts.efmt);
      tab.Cell[c, 2] :=EpiCIformat(SumTable.margtable.RR,SumTable.margtable.RRLL,SumTable.margtable.RRUL,Fmts.efmt,Fmts.cifmt,Fmts.CIHdr,1);
      if SumTable.TableCount > 1 then
        begin
          tab.Cell[c-1,3] := Epiformat(SumTable.SumRR,Fmts.efmt);
          tab.Cell[c, 3] :=EpiCIformat(SumTable.SumRR,SumTable.SUMRRLL,SumTable.SumRRUL,Fmts.efmt, Fmts.cifmt,Fmts.CIHdr,1);
          for i := 1 to SumTable.TableCount do
            begin
              tab.Cell[c-1,i+4] := Epiformat(SumTable[i].RR,Fmts.efmt);
              tab.Cell[c, i+4] := EpiCIformat(SumTable[i].RR,SumTable[i].RRLL,SumTable[i].RRUL,Fmts.efmt, Fmts.cifmt,Fmts.CIHdr,1);
            end;
        end;
    end;

  // Chi square
  If (est = 'T') then
    begin
      newcol(c,3);
      tab.Cell[c-2,1] := 'Chi<sup>2</sup>';
      tab.Cell[c-1,1] := 'Df';
      tab.Cell[c,1] := 'p';
      tab.Cell[c-2, 2] := format('%7.3f',[SumTable.margtable.CHI]);
      tab.Cell[c-1, 2] := Epiformat(SumTable.MargTable.DF,'');
      tab.Cell[c, 2] :=  format('%7.3f',[SumTable.margtable.pChi]);

     if SumTable.TableCount > 1 then
     begin
     tab.Cell[c-2,3] := Epiformat(SumTable.SumChi,Fmts.efmt);
     tab.Cell[c-1,3] := Epiformat(SumTable.SumDF,'');
     tab.Cell[c, 3] :=format('%7.3f',[SumTable.pSumCHI]);
       for i := 1 to SumTable.TableCount do
        begin
          tab.Cell[c-2,i+4] := format('%7.3f',[SumTable[i].CHI]);
          tab.Cell[c-1,i+4] := Epiformat(SumTable[i].DF,'');
          tab.Cell[c,i+4] := format('%7.3f',[SumTable[i].pChi]);
          if SumTable[i].SmallE > 0 then
            begin
              small := True;
              tab.Cell[c-2, i+4] := tab.Cell[c-2, i+4] + '<sup>*</sup>';
            end;

        end;
      end;
    end;

   // Gamma
   If (est = 'GAM') then
    begin
      newcol(c,3);
      tab.Cell[c-2,1] := 'Gamma';
      tab.Cell[c-1,1] := Fmts.CIHdr;
      tab.cell[c,1] := 'p';
      tab.Cell[c-2, 2] := Epiformat(SumTable.margtable.Gamma,Fmts.efmt);
      //tab.cell[c-1,2] :='<font style="font-size: 0.5em">not ready</font>';
      tab.Cell[c-1, 2] :=EpiCIformat(1,SumTable.margtable.gammaLL,SumTable.margtable.gammaUL,Fmts.efmt, Fmts.cifmt,' ',1);
      tab.cell[c,2]   :=Epiformat(Sumtable.MargTable.pGamma2,'%7.3f');
     if SumTable.TableCount > 1 then
     begin
      //tab.Cell[c-2, 3] := Epiformat(SumTable.PartGamma,Fmts.efmt);
      tab.Cell[c-1, 3] :=EpiCIformat(SumTable.Partgamma,Sumtable.PartGammaLL,SumTable.margtable.gammaUL,Fmts.efmt, Fmts.cifmt,' ',1);
      tab.Cell[c, 3] :=Epiformat(SumTable.pPartGamma2,'%7.3f');

      for i := 1 to SumTable.TableCount do
        begin
          //tab.Cell[c-2,i+4] := Epiformat(SumTable[i].Gamma,Fmts.efmt);
          tab.Cell[c-1,i+4] := EpiCIformat(1,SumTable[i].gammaLL,SumTable[i].gammaUL,Fmts.efmt, Fmts.cifmt,' ',1);
          tab.Cell[c,i+4] := Epiformat(Sumtable.SubTable[i].pGamma2,'%7.3f');
        end;
      end;
    end;

    If (est = 'EX') and (Sumtable.tabletype in [3,4]) then
    begin
     newcol(c,1);
     tab.Cell[c,1] := 'p<small><sub>exact</sub></small>';
     tab.Cell[c, 2] := format('%7.4f',[SumTable.MargTable.FishP2]);
     if SumTable.TableCount > 1 then
     tab.Cell[c, 3] := format('%7.4f',[SumTable.pMCCHI]);
        for i := 1 to SumTable.TableCount do
          if SumTable[i].Df > 0 then tab.Cell[c,i+4] := format('%7.4f',[SumTable[i].FishP2]);
    end;
   // find next estimate to add to table:
   allestimates := trim(copy(allestimates,length(est)+1,length(allestimates)-length(est)+1));
  end; // add columns for estiamtes

   if ((Sumtable.TableCount-Sumtable.GetDFZeroCount) > 0) and (
       (cmd.ParambyName['S'] <> nil) or (cmd.ParamByName['O'] <> nil) or (cmd.ParamByName['T'] <> nil)
          or (cmd.ParamByName['GAM'] <> nil) or (cmd.ParamByName['RR'] <> nil) or (cmd.ParamByName['EX'] <> nil)
       )then
      begin
     // add summary estimates with stratified analysis to footer of table
        if (((SumTable.TableCount > 1) and not (SumTable.TableType in [1,2])) and (Sumtable.TableCount > SumTable.SumDF)) then
          footer := format('Summary Estimates <br>Total %3d strata. %3d informative & %3d non-informative.<br>',[Sumtable.TableCount,(Sumtable.TableCount-SumTable.dfzerocount),SumTable.dfzerocount])
        else footer :=format('Summary Estimates: %3d strata.<br>',[Sumtable.TableCount]);

      // explain exposure and outcome:
      if ((cmd.ParamByName['O'] <> nil) or (cmd.ParamByName['RR'] <> nil)) and (SumTable.TableType in [3,4]) then
      begin
        Footer := footer + '&nbsp;Exposure: ' + SumTable.MargTable.fRowHeader + ' = '
          + SumTable.MargTable.fRowLabel.fStrings[0]+'<br>&nbsp;Outcome: ' + SumTable.MargTable.fColHeader + ' = '
          + SumTable.MargTable.fColLabel.fStrings[0] ;
        if Sumtable.WolfError then
            dm.Sysinfo('<br><br>Zero cell, cannot test interaction')
          else dm.Sysinfo(format('<br><br>Interaction test: &nbsp;Chi<sup>2</sup>(%d) = %8.3f p=%6.4f ',
               [(Sumtable.TableCount-Sumtable.GetDFZeroCount-1),SumTable.SumChiWolf,
                 ChiPValue(SumTable.SumChiWolf,(Sumtable.TableCount-Sumtable.GetDFZeroCount-1))]));
      end;

       if ((cmd.ParamByName['ADV']  <> nil ) and not ( Sumtable.TableType in [3,4])) then
           footer := footer + format('<br>&nbsp;&nbsp;Generalised Pseudo Odds Ratio = ' + Fmts.efmt + ' ' + Fmts.CIHdr + ' %8.4f - %8.4f.',
                                                    [SumTable.SumOR,SumTable.SumORLL, SumTable.SumORUL]);

       if small then footer := footer + '<br>*: Small Expected Numbers, use P<sub>exact</sub> /ex';

       if (cmd.ParamByName['EX'] <> nil) then
           footer := footer
             + format('<br><br>Exact Monte Carlo tests: %3d simulations  <br>&nbsp;&nbsp;Chi<sup>2</sup> = %8.3f',
                      [Sumtable.NSIM, SumTable.MCCHI])
             + ' df(' + trim(format(' %3d ',[Sumtable.SumDF])) + ') '
             + format('p=%6.4f', [SumTable.pMCChi]);  // TODO change to exact
     end
      else
         if (SumTable.TableType > 2) and (Sumtable.TableCount > SumTable.SumDF) and ( Sumtable.TableCount > 1) then
          footer := format('Summary Estimates <br>Total %3d strata. %3d informative & %3d non-informative.<br>',[Sumtable.TableCount,SumTable.SumDF,(Sumtable.TableCount-SumTable.SumDF)])
        else footer :=' '; // All strata non-informative or no statistics requested';
//   end;  // end sum table

  dm.CodeMaker.OutputTable(tab,footer);
  dm.Sendoutput();
end;

procedure TTables.OutEpi(SumTable: TSumTable);
var
  tab:  TStatTable;
  i: integer;
  opt: TEpiOption;
  footer : string;
  ul,ll: EpiFloat;
  allestimates,s: string;
  small : Boolean;
  Fmts: TTableFormats;

begin
  // At this point we know that /CT has been used - check for /O, /RR or /AR options.
  if (Cmd.ParamByName['Q'] <> nil) then exit;
  GetFormats(Cmd, Fmts);

  CollectEstimates(allestimates);
  tab := dm.OutputList.NewTable(2, SumTable.TableCount+1);
  tab.TableType := sttNormal;
  ul := 2.00;
  ll:= 2.00;
  small := False;
  tab.Caption := SumTable.fSumtableCaption;
  tab.Cell[1,1] := SumTable.fSumtableCaption;
  if (Cmd.ParamExists['O']) then
    tab.Cell[2,1] := Fmts.ORHdr.N
  else
    tab.Cell[2,1] := Fmts.RRHdr.N;
  for i := 1 to SumTable.TableCount do
  begin
    tab.Cell[1, i+1] := Sumtable[i].fCaption;
    tab.Cell[2, i+1] := Format('%d', [SumTable[i].Total]);
  end;
  footer := ' ';

  if (Cmd.ParamExists['O']) then
    tab.Caption := Fmts.ORHdr.Outcome + ' ' + tab.caption
  else
    tab.Caption := Fmts.RRHdr.Outcome + ' ' + tab.caption ;

  if (Cmd.ParamExists['RR'] or Cmd.ParamExists['AR']) then
  BEGIN
    // add exposed
    AddOA(tab, sumtable, 1, Fmts);
    if Cmd.ParamExists['AR'] then AddAR(Tab, Sumtable, 1, Fmts, cmd.ParamExists['CI']);
      // Unexposed
    AddOA(tab, sumtable, 2, Fmts);
    if Cmd.ParamExists['AR'] then AddAR(Tab, Sumtable, 2, Fmts, cmd.ParamExists['CI']);
    small := False;
    // add columns for estimates:
    AddRR(Tab, Sumtable, Fmts);
  end else begin       // O table
    AddCC(tab, Sumtable, 1, Fmts);
    AddCC(tab, Sumtable, 2, Fmts);
    AddOR(Tab, Sumtable, Cmd, True, Fmts);
  end;

  AddChi2test(Tab,Sumtable,Cmd,Small);
  AddGam(Tab,Sumtable,Cmd,Fmts);
  AddExact(Tab,Sumtable,Cmd);
  AddOBS(Tab,Sumtable,Cmd);

  Tab.InsertRow(0);
  if (Cmd.ParamExists['RR']) or (Cmd.ParamExists['AR'])then
  begin
    tab.Cell[3,1] := Fmts.RRHdr.Exposed;
    tab.Cell[5,1] := Fmts.RRHdr.NotExposed;
  end else begin
    tab.Cell[3,1] := Fmts.ORHdr.CCase;
    tab.Cell[5,1] := Fmts.ORHdr.NCase;
  end;

  Footer := 'Exposure:';
  for i := 1 to SumTable.TableCount do
    if (i= 1) or (i < SumTable.TableCount) and ((i mod 5) = 0) then
      Footer := footer + ' (' + SumTable.SubTable[i].fRowHeader + '&nbsp;=&nbsp;'+ SumTable.SubTable[i].fRowLabel.fStrings[0] + ')<br>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;'
    else
      Footer := footer + ' (' + SumTable.SubTable[i].fRowHeader + '&nbsp;=&nbsp;'+ SumTable.SubTable[i].fRowLabel.fStrings[0] + ')';

  Footer := footer +  '<br>Outcome: ' + SumTable.MargTable.fColHeader + '&nbsp;=&nbsp;' +  SumTable.MargTable.fColLabel.fStrings[0] +'<br>';

  if small then footer := footer + '*: Small Expected Numbers, use P<sub>exact</sub> /ex<br>';
  dm.CodeMaker.OutputTable(tab, footer);
  //dm.Sendoutput();
end;


procedure TTables.OutFVTable(SumTable: TSumTable);
var
  tab, xtab:  TStatTable;
  i, j, k : integer;
  footer: string;
begin
  if (Cmd.ParamExists['Q']) then exit;

  tab := dm.OutputList.NewTable(2, SumTable.TableCount + 1);
  tab.TableType := sttStat;


  tab.Cell[1,1] := 'Variable:';
  tab.Cell[2,1] := 'N:';
  for i := 1 to SumTable.TableCount do
  begin
    footer := '';
    tab.Cell[1, i+1] := Sumtable[i].fCaption;
    tab.Cell[2, i+1] := Format('%d', [SumTable[i].Total]);
    xtab := OutTwoWayTable(Sumtable[i]);
    footer := footer + AddTotals(xtab, SumTable[i]);
    footer := footer + AddPercents(xtab, SumTable[i], Cmd);

    dm.CodeMaker.OutputTable(xtab, footer);
    FreeAndNil(xtab);
  end;

  dm.CodeMaker.OutputTable(tab, '');
  dm.Sendoutput();
end;

{*****************************************
  Formats for percentages and estimates:
******************************************}
procedure TTables.GetORHeaders(Cmd: TCommand; var ORHeaders: TORHeader);
var
  Opt: TEpiOption;
  s: string;
  List: TStrings;
begin
  if dm.GetOptionValue('TABLE CT OR HEADER', opt) then
    s := Opt.value else s := 'Outcome:,Case,Non Case,N,Exposed,Non exposed';
  List := TStringList.Create;
  SplitString(s, List, [',']);
  if List.Count <> 6 then
    dm.Error('Incorrect number of headers for %s header', ['CT OR'], 41006);

  With ORHeaders do
  begin
    Outcome := List[0];
    CCase   := List[1];
    NCase   := List[2];
    N       := List[3];
    Exposed := List[4];
    NonExposed := List[5];
  end;
  FreeAndNil(List);
end;

procedure TTables.GetRRHeaders(Cmd: TCommand; var RRHeaders: TRRHeader);
var
  Opt: TEpiOption;
  s: string;
  List: TStrings;
begin
  if dm.GetOptionValue('TABLE CT RR HEADER', opt) then
    s := Opt.value else s := 'Outcome:,Exposed,Not Exposed,N,n,Ill,RR,AR (%)';
  List := TStringList.Create;
  SplitString(s, List, [',']);
  if List.Count <> 8 then
    dm.Error('Incorrect number of headers for %s header', ['CT RR'], 41006);

  With RRHeaders do
  begin
    Outcome    := List[0];
    Exposed    := List[1];
    NotExposed := List[2];
    N          := List[3];
    SmallN     := List[4];
    Ill        := List[5];
    RR         := List[6];
    AR         := List[7];
  end;
  FreeAndNil(List);
end;

procedure TTables.GetLTHeaders(Cmd: TCommand; var LTHeaders: TLTHEader);
var
  Opt: TEpiOption;
  s: string;
  List: TStrings;
begin
  if dm.GetOptionValue('LIFETABLE HEADER', opt) then
    s := Opt.value else s := 'Interval,Beg. Total,Deaths,Lost,Survival,Std. Error';
  List := TStringList.Create;
  SplitString(s, List, [',']);
  if List.Count <> 6 then
    dm.Error('Incorrect number of headers for %s header', ['LifeTable'], 41006);

  With LTHeaders do
  begin
    IntVal   := List[0];
    Beg      := List[1];
    Deaths   := List[2];
    Lost     := List[3];
    Survival := List[4];
    StdErr   := List[5];
  end;
  FreeAndNil(List);
end;

procedure TTables.GetFormats(Cmd: TCommand; var TableFormats: TTableFormats);
var
  Opt: TEpiOption;
begin
  if dm.GetOptionValue('TABLE PERCENT FORMAT ROW', Opt) then
     TableFormats.RFmt := Opt.value else TableFormats.RFmt := 'P1()';
  if dm.GetOptionValue('TABLE PERCENT FORMAT COL', Opt) then
     TableFormats.CFmt := Opt.value else TableFormats.CFmt := 'P1()';
  if dm.GetOptionValue('TABLE PERCENT FORMAT TOTAL', Opt) then
     TableFormats.TFmt := Opt.value else TableFormats.TFmt := 'P1[]';
  if dm.GetOptionValue('TABLE PERCENT HEADER', opt) then
     TableFormats.PctHdr := Opt.value else TableFormats.PctHdr := '%';
  if dm.GetOptionValue('TABLE PERCENT HEADER ROW', opt) then
     TableFormats.RowPCtHdr := Opt.value else TableFormats.RowPCtHdr := '%';
  if dm.GetOptionValue('TABLE PERCENT HEADER COL', opt) then
     TableFormats.ColPctHdr := Opt.value else TableFormats.ColPctHdr := '%';
  if dm.GetOptionValue('TABLE PERCENT HEADER TOTAL', opt) then
     TableFormats.TotPctHdr := Opt.value else TableFormats.TotPctHdr := '%';

  if (cmd.ParamByName['D0'] <> nil) then
  begin
    TableFormats.RFmt[2] := '0';
    TableFormats.CFmt[2] := '0';
    TableFormats.TFmt[2] := '0';
  end;
  if (cmd.ParamByName['D1'] <> nil) then
  begin
    TableFormats.RFmt[2] := '1';
    TableFormats.CFmt[2] := '1';
    TableFormats.TFmt[2] := '1';
  end;
  if (cmd.ParamByName['D2'] <> nil) then
  begin
    TableFormats.rfmt[2] := '2';
    TableFormats.cfmt[2] := '2';
    TableFormats.tfmt[2] := '2';
  end;

  if (cmd.ParambyName['E1'] <> nil) then TableFormats.EFmt := '%8.1f'
    else if (cmd.ParambyName['E0']<> nil) then TableFormats.efmt := '%8.0f'
    else if (cmd.ParambyName['E3']<> nil) then TableFormats.efmt := '%8.3f'
    else if (cmd.ParambyName['E4']<> nil) then TableFormats.efmt := '%8.4f'
    else TableFormats.efmt := '%8.2f';

  // confidence interval formats:
  if dm.GetOptionValue('TABLE CI FORMAT', Opt) then
    TableFormats.CIFmt := Opt.value else TableFormats.CIFmt := '() - ';       // (lv - uv)
  if dm.GetOptionValue('TABLE CI HEADER', Opt) then
    TableFormats.CIHdr := Opt.value else TableFormats.CIHdr := '(95% CI)';
    TableFormats.CIHdr := '<font class=ci>' + TableFormats.CIHdr + '</font>';

  GetORHeaders(Cmd, TableFormats.ORHdr);
  GetRRHeaders(Cmd, TableFormats.RRHdr);
  GetLTHeaders(Cmd, TableFormats.LTHdr);
end;

{Procedure TTables.GetFormats(cmd : TCommand; var efmt, rfmt, cfmt, tfmt,
                colpcthead,rowpcthead, pctheader, totalpcthead
                ,cifmt,ciheader  : string);
var
 opt: TEpiOption;
 fmts: TTableFormats;
begin
  Getformats(cmd, fmts);
  efmt := fmts.EFmt;
  rfmt := fmts.RFmt;
  cfmt := fmts.CFmt;
  tfmt := fmts.TFmt;
  colpcthead := fmts.ColPctHdr;
  rowpcthead := fmts.RowPCtHdr;
  pctheader := fmts.PctHdr;
  totalpcthead := fmts.TotPctHdr;
  cifmt := fmts.CIFmt;
  ciheader := fmts.CIHdr;
end;
}
procedure TTables.CollectEstimates(var AllEstimates: string);
// get a list of statistics to calculate
var
  i: integer;
  opt: TEpiOption;
  s: string;
begin
  s:= ' ';
  for i:= 0 to cmd.ParameterCount-1 do
    if (pos(' '+cmd.param[i].VarName+' ',STAT_OPTIONS) > 0) then
      s := s + cmd.param[i].VarName + ' ';
  allestimates := s ;
end;


procedure TTables.GetTableOR(aTable: TTwoWayTable);
var
  s: integer;
  t2 : TTableItem;
  FisherStat: TFisherStat;      //exact estimation for odds ratios.
begin
  // Add all odds ratio related estimators for a single table
  FisherStat := TFisherStat.Create(false);
  t2:= TtableItem.Create(aTable.Cell[1,1].n,aTable.Cell[2,1].n,
        aTable.Cell[1,2].n,aTable.Cell[2,2].n,1);
  FisherStat.addSet(t2);
  //addtable(t2,FisherStat);
  s := fisherstat.show(t2,1);   // s is statuscode for result
  aTable.Oddsratio := t2.sOR;
  aTable.ORUL := t2.ORUL;
  aTable.ORLL := t2.ORLL;
  aTable.pOR := t2.pOR;
  FreeAndNil(t2); //t2.free;
  FreeAndNil(Fisherstat); //Fisherstat.Free;
  // done with this table
end;


{*****************************************
  TSumTable:
******************************************}

constructor TSumTable.Create();
begin
  inherited;
  fMargTable := nil;
  Setlength(fSubtables, 0);
end;

destructor TSumTable.Destroy();
var
  i: integer;
begin
  for i := 0 to TableCount-1 do
    FreeAndNil(fSubtables[i]);
  SetLength(fSubtables, 0);
  if Assigned(fMargTable) then
    FreeAndNil(fMargTable);
  inherited;
end;

function TSumTable.GetSubTable(const index: integer): TTwoWayTable;
begin
  result := fSubtables[index-1];
end;

function TSumTable.GetTableCount(): integer;
begin
  result := System.Length(fSubtables);
end;

function TSumTable.GetTotal(): EpiUnInt;
begin
  result := MargTable.Total;
end;

procedure TSumTable.AddSubTable(aTable: TTwoWayTable);
var
  cSize: integer;
begin
  cSize := TableCount;
  SetLength(fSubtables, cSize + 1);
  fSubtables[cSize] := aTable;
end;


function TSumTable.GetValidTotal(): EpiUnInt;
 var i : integer;
begin
  result := MargTable.Total;
  for i := 1 to TableCount do
    if self.getSubtable(i).df = 0 then result := result- getsubtable(i).total;
end;

function TSumTable.GetDFZeroCount(): EpiUnInt;
 var i : integer;
begin
  result := 0;
  for i := 1 to TableCount do
    if (GetSubTable(i).DF = 0) then inc(result);
end;

function TSumTable.GetWolfChiSum(): EpiFloat;
 var i : integer;
  function weight(Tab : TTwowaytable): EpiFloat;
   begin
     result :=
       (1/tab.Cell[1,1].N + 1/tab.Cell[1,2].N + 1/tab.Cell[2,1].N + 1/tab.Cell[2,2].N);
   end;
begin
  result := 0.0;
  for i := 1 to TableCount do
    result := result
      + sqr((ln(GetSubTable(i).Oddsratio)-ln(SumOR)))*weight(GetSubtable(i));
end;

function TSumTable.GetWolfError(): Boolean;
 var i : integer;
begin
  result := False;
  for i := 1 to TableCount do
       result := result or
       (GetSubTable(i).Cell[1,1].n = 0) or (GetSubTable(i).Cell[1,2].n = 0) or
       (GetSubTable(i).Cell[2,1].n = 0) or (GetSubTable(i).Cell[2,2].n = 0);
end;

{*****************************************
  TTwoWayTable:
******************************************}

constructor TTwoWayTable.Create(const Cols, Rows: integer);
var
  i, j: integer;
begin
  setlength(fCells,cols,rows);
  for i := 1 to Cols do
    for j := 1 to Rows do
      fCells[i-1,j-1] := TTableCell.Create();
  fcaption := '';
  fColLabel := TLabelArray.Create();
  fRowLabel := TLabelArray.Create();
  fDF := 0;
end;

destructor TTwoWayTable.Destroy();
var
  i, j: integer;
begin
  for i := 0 to Self.ColumnCount-1 do
    for j := 0 to Self.RowCount-1 do
      FreeAndNil(fCells[i,j]);
  FreeAndNil(fColLabel);
  FreeAndNil(fRowLabel);
  inherited;
end;


function TTwoWayTable.GetRowCount(): cardinal;
begin
  result := High(fCells[0]) + 1;
end;

function TTwoWayTable.GetColumnCount(): cardinal;
begin
  result := High(fCells) + 1;
end;


function TTwoWayTable.GetCell(const col, row: Cardinal): TTableCell;
begin
  result := nil;
  if (col<1) or (row<1) or (col>GetColumnCount) or (Row>GetRowCount) then exit;
  result := fCells[col-1][row-1];
end;

function TTwoWayTable.GetColTotal(const index: integer): EpiUnInt;
var
  i: integer;
begin
  result := 0;
  for i:=0 to GetRowCount-1 do
    inc(result, fCells[index-1][i].N);
end;


function TTwoWayTable.GetRowTotal(const index: integer): EpiUnInt;
var
  i: integer;
begin
  result := 0;
  for i:=0 to GetColumnCount-1 do
    inc(result, fCells[i][index-1].N);
end;

function TTwoWayTable.GetTotal: EpiUnInt;
var
  i: integer;
begin
  result := 0;
  for i := 1 to GetColumnCount do
    inc(result, ColTotal[i]);
end;

function TTwoWayTable.GetRowHeaderValue(const index: integer): EpiVariant;
begin
  result := fRowLabel.GetValue(index-1);
end;

function TTwoWayTable.GetRowHeaderLabel(const index: integer): String;
begin
  result := fRowLabel.Get(index-1);
end;

function TTwoWayTable.GetColHeaderValue(const index: integer): EpiVariant;
begin
  result := fColLabel.GetValue(index-1);
end;

function TTwoWayTable.GetColHeaderLabel(const index: integer): String;
begin
  result := fColLabel.Get(index-1);
end;


procedure ExchangeColumns(var Table: TTwoWayTable; Index1, Index2: integer);
var
  i: integer;
  tempcell: TTableCell;
begin
  for i := 1 to Table.RowCount do
  begin
    TempCell := Table.Cell[Index1, i];
    Table.fCells[Index1-1,i-1] := Table.Cell[Index2,i];
    Table.fCells[Index2-1,i-1] := TempCell;
  end;
  Table.fColLabel.Exchange(Index1-1, Index2-1);
end;

procedure ExchangeRows(var Table: TTwoWayTable; Index1, Index2: integer);
var
  i: integer;
  tempcell: TTableCell;
begin
  for i := 1 to Table.ColumnCount do
  begin
    TempCell := Table.Cell[i, Index1];
    Table.fCells[i-1, Index1-1] := Table.Cell[i, Index2];
    Table.fCells[i-1, Index2-1] := TempCell;
  end;
  Table.fRowLabel.Exchange(Index1-1, Index2-1);
end;

function CompareRows(Table: TTwoWayTable; I1, I2: integer; const index: integer): integer;
begin
   result := Table.Cell[I1, Index].N - Table.Cell[I2, Index].N;
end;

function CompareCols(Table: TTwoWayTable; I1, I2: integer; const index: integer): integer;
begin
   result := Table.Cell[Index, I1].N - Table.Cell[Index, I2].N;
end;

function CompareRowTotals(Table: TTwoWayTable; I1, I2: integer; const index: integer): integer;
begin
  result := Table.RowTotal[I1] - Table.RowTotal[I2];
end;

function CompareColTotals(Table: TTwoWayTable; I1, I2: integer; const index: integer): integer;
begin
  result := Table.ColTotal[I1] - Table.ColTotal[I2];
end;

function CompareRowLabels(Table: TTwoWayTable; I1, I2: integer; const index: integer): integer;
begin
  result := 0;
  case VarType(Table.fRowLabel.GetValue(I1-1)) of
    varSmallint,
    varInteger,
    varShortInt,
    varWord,
    varDate,
    varLongWord:
      begin
        if (Table.fRowLabel.GetValue(I1-1) = NA_INT) and
           (Table.fRowLabel.GetValue(I2-1) = NA_INT) then result := 0
        else if Table.fRowLabel.GetValue(I1-1) = NA_INT then result := 1
        else if Table.fRowLabel.GetValue(I2-1) = NA_INT then result := -1
        else result := Table.fRowLabel.GetValue(I1-1) - Table.fRowLabel.GetValue(I2-1);
      end;
    varSingle,
    varDouble,
    varInt64:
      begin
        if (Table.fRowLabel.GetValue(I1-1) = NA_FLOAT) and
           (Table.fRowLabel.GetValue(I2-1) = NA_FLOAT) then result := 0
        else if Table.fRowLabel.GetValue(I1-1) = NA_FLOAT then result := 1
        else if Table.fRowLabel.GetValue(I2-1) = NA_FLOAT then result := -1
        else result := Table.fRowLabel.GetValue(I1-1) - Table.fRowLabel.GetValue(I2-1);
      end;
    varString:
      begin
        if (Table.fRowLabel.GetValue(I1-1) = NA_STR) and
           (Table.fRowLabel.GetValue(I2-1) = NA_STR) then result := 0
        else if Table.fRowLabel.GetValue(I1-1) = NA_STR then result := 1
        else if Table.fRowLabel.GetValue(I2-1) = NA_STR then result := -1
        else result := AnsiCompareStr(Table.fRowLabel.GetValue(I1-1),
                                      Table.fRowLabel.GetValue(I2-1))
      end;
  else  // Else case for boolean entry with 0 and 1.
    begin
      if (Table.fRowLabel.GetValue(I1-1) = NA_INT) and
         (Table.fRowLabel.GetValue(I2-1) = NA_INT) then result := 0
      else if Table.fRowLabel.GetValue(I1-1) = NA_INT then result := 1
      else if Table.fRowLabel.GetValue(I2-1) = NA_INT then result := -1
      else result := Table.fRowLabel.GetValue(I1-1) - Table.fRowLabel.GetValue(I2-1);
    end;
  end;
end;

function CompareColLabels(Table: TTwoWayTable; I1, I2: integer; const index: integer): integer;
begin
  result := 0;
  case VarType(Table.fColLabel.GetValue(I1-1)) of
    varSmallint,
    varInteger,
    varShortInt,
    varWord,
    varDate,
    varLongWord:
      begin
        if (Table.fColLabel.GetValue(I1-1) = NA_INT) and
           (Table.fColLabel.GetValue(I2-1) = NA_INT) then result := 0
        else if Table.fColLabel.GetValue(I1-1) = NA_INT then result := 1
        else if Table.fColLabel.GetValue(I2-1) = NA_INT then result := -1
        else result := Table.fColLabel.GetValue(I1-1) - Table.fColLabel.GetValue(I2-1);
      end;
    varSingle,
    varDouble,
    varInt64:
      begin
        if (Table.fColLabel.GetValue(I1-1) = NA_FLOAT) and
           (Table.fColLabel.GetValue(I2-1) = NA_FLOAT) then result := 0
        else if Table.fColLabel.GetValue(I1-1) = NA_FLOAT then result := 1
        else if Table.fColLabel.GetValue(I2-1) = NA_FLOAT then result := -1
        else result := Table.fColLabel.GetValue(I1-1) - Table.fColLabel.GetValue(I2-1);
      end;
    varString:
      begin
        if (Table.fColLabel.GetValue(I1-1) = NA_STR) and
           (Table.fColLabel.GetValue(I2-1) = NA_STR) then result := 0
        else if Table.fColLabel.GetValue(I1-1) = NA_STR then result := 1
        else if Table.fColLabel.GetValue(I2-1) = NA_STR then result := -1
        else result := AnsiCompareStr(Table.fColLabel.GetValue(I1-1),
                                      Table.fColLabel.GetValue(I2-1))
      end;
  else  // Else case for boolean entry with 0 and 1.
    begin
      if (Table.fColLabel.GetValue(I1-1) = NA_INT) and
         (Table.fColLabel.GetValue(I2-1) = NA_INT) then result := 0
      else if Table.fColLabel.GetValue(I1-1) = NA_INT then result := 1
      else if Table.fColLabel.GetValue(I2-1) = NA_INT then result := -1
      else result := Table.fColLabel.GetValue(I1-1) - Table.fColLabel.GetValue(I2-1);
    end;
  end;
end;

function CompareColLabelsText(Table: TTwoWayTable; I1, I2: integer; const index: integer): integer;
begin
  result := AnsiCompareStr(Table.fColLabel.Get(I1-1),
                           Table.fColLabel.Get(I2-1))
end;

function CompareRowLabelsText(Table: TTwoWayTable; I1, I2: integer; const index: integer): integer;
begin
  result := AnsiCompareStr(Table.fRowLabel.Get(I1-1),
                           Table.fRowLabel.Get(I2-1))
end;

procedure TTwoWayTable.InternalSortGeneric(SCompare: TTwoWaySortCompare; SExchange: TTwoWaySortExchange; L, R: integer; const index: integer; Desc: boolean);
var
   I, J, P: Integer;
begin
  I:=L;
  J:=R;
  P:=(L + R) shr 1;
  repeat
    if Desc then
    begin
      while SCompare(self, I, P, index) > 0 do Inc(I);
      while SCompare(self, J, P, index) < 0 do Dec(J);
    end else begin
      while SCompare(self, I, P, index) < 0 do Inc(I);
      while SCompare(self, J, P, index) > 0 do Dec(J);
    end;
    if I <= J then
    begin
      SExchange(self,J,I);
      if p=i then p:=j
      else if p=j then p:=i;
      Inc(I);
      Dec(J);
    end;
  until I>J;
  if J>L then InternalSortGeneric(SCompare, SExchange, L, J, Index, Desc);
  if I<R then InternalSortGeneric(SCompare, SExchange, I, R, Index, Desc);
end;

procedure TTwoWayTable.SortByColumn(const index: integer; Desc: boolean);
begin
  if (index < 1) or (index > ColumnCount) then dm.Error('Sorting index out of bounds', [], 41005);
  InternalSortGeneric(CompareCols, ExchangeRows, 1, fRowLabel.Count, index, desc);
end;

procedure TTwoWayTable.SortByRow(const index: integer; Desc: boolean);
begin
  if (index < 1) or (index > ColumnCount) then dm.Error('Sorting index out of bounds', [], 41005);
  InternalSortGeneric(CompareRows, ExchangeColumns, 1, fColLabel.Count, index, desc);
end;

procedure TTwoWayTable.SortByColumnLabel(Desc: boolean);
begin
  InternalSortGeneric(CompareColLabels, ExchangeColumns, 1, fColLabel.Count, 0, desc);
end;

procedure TTwoWayTable.SortByColumnLabelText(Desc: boolean);
begin
  InternalSortGeneric(CompareColLabelsText, ExchangeColumns, 1, fColLabel.Count, 0, desc);
end;

procedure TTwoWayTable.SortByRowLabel(Desc: boolean);
begin
  InternalSortGeneric(CompareRowLabels, ExchangeRows, 1, fRowLabel.Count, 0, desc);
end;

procedure TTwoWayTable.SortByRowLabelText(Desc: boolean);
begin
  InternalSortGeneric(CompareRowLabelsText, ExchangeRows, 1, fRowLabel.Count, 0, desc);
end;

procedure TTwoWayTable.SortByColumnTotal(Desc: boolean);
begin
  InternalSortGeneric(CompareColTotals, ExchangeColumns, 1, fColLabel.Count, 0, desc);
end;

procedure TTwoWayTable.SortByRowTotal(Desc: boolean);
begin
  InternalSortGeneric(CompareRowTotals, ExchangeRows, 1, fRowLabel.Count, 0, desc);
end;


{*****************************************
  TTwoWayTable adapted 2x2 estimators:
******************************************}



Function TTwoWayTable.GetCrudeOR: EpiFloat;
begin
  if ((getcell(1,2).n*getcell(2,1).n) = 0) then
      result := -9999.0
    else if ((getcell(1,1).n*getcell(2,2).n) = 0) then
      result := 0.0
    else Result:=(getcell(1,1).n*getcell(2,2).n) / (getcell(2,1).n*getcell(2,1).n);
end;


function TTwoWayTable.GetCrudeRR: EpiFloat;
begin
  Result := -9999;
  if (getcell(1,2).n > 0) and (getrowTotal(1) > 0) and (GetrowTotal(2) > 0 ) then
          Result := ((getcell(1,1).n/GetRowTotal(1))/(getcell(1,2).n/GetRowTotal(2))) ;
  {        dm.info('r1: ' + inttostr(getcell(1,1).n)+ ' ' + inttostr(GetRowTotal(1))
                  + ' r2: ' + inttostr(getcell(1,2).n)+ ' ' + inttostr(GetRowTotal(2))
           + ' = ' + floattostr(result)          );
  }
end;

function TTwoWaytable.GetRRUL: EpiFloat;
begin
      Result := getgrci(1);
end;

function TTwoWaytable.GetRRLL: EpiFloat;
begin
      Result := getgrci(-1);
end;


function TTwoWaytable.GetGRCI(WhichLim: Integer):EpiFloat;
 {Produce sums for Greenland/Robins confidence limits for
 Relative Risk in single and stratified tables.
 Greenland, S, Robins, JM, Biometrics 1985;41:55-68}
Var R, S, Dgr, Rrgb, Srgb, Vgr, Prgb, Qrgb  : EpiFloat;
begin
    R := getcell(1,1).n * GetRowTotal(2) / gettotal ;
    S := getcell(1,2).n * GetRowTotal(1) / gettotal;
    Dgr := ((GetRowTotal(2) * GetRowTotal(1) * GetColTotal(1))
            - (getcell(1,1).n * getcell(1,2).n * gettotal)) / Sqr(gettotal);
{    R := A * N0 / Tot ;
    S := C * N1 / tot;
    Dgr := ((N1 * N0 * M1) - (A * C * tot)) / Sqr(tot);
}
    If R *S <> 0 then Vgr := Dgr / (R * S) else
         Vgr := -9999.0;
    If S <> 0 then RRgb := R / S else RRgb := -9999.0;
    If RRgb > 0 then
       begin
         if Vgr < 0 then Vgr:=Vgr*-1;
         if whichlim = -1 then
           result := Exp (ln (RRgb) - (ConfScore * Sqrt (Vgr)))
           else
           result := Exp (ln (RRgb) + (ConfScore * Sqrt (Vgr))); //upper
       end
       else
         result := -9999.0;
    if result > 9999.0 then result := 9999.0
end;

function TTwoWaytable.GetFishers2p() : EpiFloat;
Var
  AF, BF, CF, DF : EpiFloat;
  A2, B2, C2, D2, N1F, N0F, M1F, M0F, TF : EpiFloat;
  X, SN, ON, DEN : EpiFloat;
Begin
if (GetColumnCount <> 2) or (GetRowCount <> 2 ) then exit;
if ((getcell(1,1).n+ getcell(2,1).n= 0) or (getcell(1,2).n+getcell(2,2).n = 0)) then exit;
  If (getcell(1,1).n / (GetRowTotal(1))) < (getcell(1,2).n / (GetRowTotal(2))) then
    Begin
    AF := getcell(1,1).n; //a
    BF := getcell(2,1).n ; //B
    CF := getcell(1,2).n;   //c
    DF := getcell(2,2).n; //D
    End (*If*)
  Else
    Begin
    AF := getcell(1,2).n; //C;
    BF := getcell(2,2).n; //D;
    CF := getcell(1,1).n; //A;
    DF := getcell(2,1).n; //B
  End (*Else*);
  N1F := AF + BF;
  N0F := CF + DF;
  M1F := AF + CF;
  M0F := BF + DF;
  TF := N1F + N0F;
  SN := 1;
  ON := 0;
  DEN := 1;
  A2 := AF;
  D2 := DF;
  B2 := BF + 1;
  C2 := CF + 1;
  X := 1;
  While (X > 9.999999e-21) Do
      Begin
      X := X * A2 * D2 / (B2 * C2);
      SN := X + SN;
      DEN := X + DEN;
      A2 := A2 - 1;
      D2 := D2 - 1;
      B2 := B2 + 1;
      C2 := C2 + 1
      End (*While*);
  B2 := BF;
  C2 := CF;
  A2 := AF + 1;
  D2 := DF + 1;
  X := 1;
  While (X > 9.999999e-21) and (X < 1.0E+30) Do
      Begin
      X := X * B2 * C2 / (A2 * D2);
      DEN := X + DEN;
      If (X <= 1) then
         ON := X + ON;
      A2 := A2 + 1;
      D2 := D2 + 1;
      B2 := B2 - 1;
      C2 := C2 - 1
      End (*While*);
  // result := SN / DEN;  one sided fishers exact
  result := (SN + ON) / DEN;

End (*Fishers*);

{*****************************************
  Other tablestat functions
******************************************}

function TTwoWaytable.MinCount: EpiInt;
var i,j : integer;
 k: epifloat;
begin
  result := 0;
  for i:= 1 to columncount do
    for j:= 1 to rowcount do
    begin
      k := rowtotal[j]*(coltotal[i]/total);
      if k  < 5 then inc(result);
    end;
end;


{*****************************************
  TTableCell:
******************************************}

constructor TTableCell.Create();
begin
  inherited;
  fN := 0;
  fNestL := NA_FLOAT;
  fNestG := NA_FLOAT;
  fRpct := 0;
  fCpct := 0;
  fTpct := 0;
end;

{*****************************************
  TStringArray:
******************************************}

destructor TStringArray.Destroy();
begin
  Finalize(fStrings);
  Finalize(fdata);
  inherited;
end;

procedure TStringArray.Add(s: string; value: EpiVariant);
var
  i, index: integer;
begin
  index := high(fStrings)+1;
  // No duplicates.
  for i := 0 to index -1 do
    if fStrings[i] = trim(s) then exit;
  setlength(fStrings, index+1);
  setlength(fdata, index+1);
  fStrings[index] := trim(s);
  fData[index] := Value;
end;

function TStringArray.Get(index: integer): string;
begin
  result := fStrings[index];
end;

function TStringArray.Find(s: string; var index: integer): boolean;
var
  i: integer;
begin
  result := true;
  for i :=0 to high(fStrings) do
    if fStrings[i] = s then
    begin
      index := i;
      exit;
    end;
  index := -1;
  result := false;
end;

procedure TStringArray.MoveToBack(Index: Integer);
var
  tempstr: string;
  tempval: EpiVariant;
  i: integer;
begin
  tempstr := fstrings[index];
  tempval := fdata[index];
  for i := index to high(fdata)-1 do
  begin
    fstrings[i] := fstrings[i+1];
    fdata[i] := fdata[i+1];
  end;
  fdata[high(fdata)] := tempval;
  fstrings[high(fdata)] := tempstr;
end;

procedure TStringArray.Exchange(Index1, Index2: integer);
var
  tempstr: string;
  tempval: EpiVariant;
begin
  tempstr := fstrings[index1];
  tempval := fdata[index1];
  fstrings[index1] := fstrings[index2];
  fdata[index1] := fdata[index2];
  fstrings[index2] := tempstr;
  fdata[index2] := tempval;
end;

function TStringArray.GetValue(index: integer): EpiVariant;
begin
  result := fdata[index];
end;

function TStringArray.GetCount(): integer;
begin
  result := high(fStrings)+1;
end;

{ TLabelArray }

procedure TLabelArray.Add(s: string; Value: EpiVariant);
var
  index: integer;
begin
  index := high(fStrings)+1;
  setlength(fStrings, index+1);
  setlength(fdata, index+1);
  fStrings[index] := trim(s);
  fData[index] := Value;
end;

destructor TLabelArray.Destroy;
begin
  inherited;
end;

end.
