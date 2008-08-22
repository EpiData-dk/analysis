Unit RegComp;
{  Author: Fred J. Edberg
          +1-503-653-9835
          fedberg@teleport.com

  Original unit US Copyright [submission 12/08/1998],
  US Copyright (1999) © TXu-894-067

  The portions below released into the public sector as of 5/7/00 }


Interface


Uses SysUtils, {Dialogs,} Classes, WinTypes, Math, ansDataTypes;

{$Include RegComp.inc}

type

{$Include RegTypes.inc}


  TRegComp = class(TComponent)
    private
      FAlpha    : integer;
      FScanData : boolean;
      procedure SetAlpha (Value: integer);
      procedure SetScanData (Value: boolean);
//      function InitUnivariateRec: TUnivariateRec;
      function InitBivarStatRec: TBivarStatRec;
      function InitMultStatRec: TMultStatRec;
      function InitInterval: TIntervalRec;
      function NearZero                         (const dValue : extended): boolean;

      function ZTableValue                      (const Z      : extended): extended;

      function PadStr                           (const sIn    : string): string;

      function InitVector                       (const k      : byte;
                                                 const aValue : integer): XVector;

      procedure InitResidArray                   (var AnArray : array of extended;
                                                const aValue  : extended);

      function Cube                            (const InValue : extended): extended;

      function DeterminantMatrix              (const Matrix   : XMatrix;
                                               const k        : byte): extended;

      function MultiplyMatrices               (const Matrix1  : XMatrix;
                                               const Matrix2  : XMatrix;
                                               const k        : byte): XMatrix;

      function DivideMatrixScalar             (const Matrix   : XMatrix;
                                               const k        : byte;
                                               const aScalar  : extended): XMatrix;

      function MultiplyMatrixByVector          (const Matrix  : XMatrix;
                                                const Vector  : XVector;
                                                const k       : byte): XVector;

      function MultiplyVectorByVector          (const Vector1 : XVector;
                                                const Vector2 : XVector;
                                                const k       : byte): extended;

      function MultiplyVectorByMatrix          (const Matrix  : XMatrix;
                                                const Vector  : XVector;
                                                const k       : byte): XVector;

      function MultiplyVectorByTransposeVector (const Vector1 : XVector;
                                                const Vector2 : XVector;
                                                const k       : byte): extended;

      function TransposeVector                 (const Vector  : XVector;
                                                const k       : byte): XVector;

      function AddVector                       (const Vector1 : XVector;
                                                const Vector2 : XVector;
                                                const k       : byte): XVector;

      function SubtractVector                  (const Vector1 : XVector;
                                                const Vector2 : XVector;
                                                const k       : byte): XVector;

      function AddMatrices                    (const Matrix1  : XMatrix;
                                               const Matrix2  : XMatrix;
                                               const k        : byte): XMatrix;

      function SubtractMatrices               (const Matrix1  : XMatrix;
                                               const Matrix2  : XMatrix;
                                               const k        : byte): XMatrix;

      function MatrixTrace                    (const Matrix   : XMatrix;
                                               const k        : byte): extended;

      function MVRegressionBetaCI        (const aMultStatRec  : TMultStatRec;
                                          const XCrossProdInv : XMatrix;
                                          const i             : byte): TIntervalRec;

    protected
    public
      PRegressData      : PTRegressDataArray; // Holds X variables values [Dynamic]
      X, Y              : TResidualsArray;    // To hold Y values, and for X/Y
      Residuals         : TResidualsArray;    // Raw Residuals (e[i])
      Predicted         : TResidualsArray;    // Predicted Values (yCap[i])
      CIUpper           : TResidualsArray;    // Conf Interval + values
      CILower           : TResidualsArray;    // Conf Interval - values
      PIUpper           : TResidualsArray;    // Pred Interval + values
      PILower           : TResidualsArray;    // Pred Interval - values
      MultStatRec       : TMultStatRec;       // Return rec type of MultReg funcs
      BivarStatRec      : TBivarStatRec;      // Return rec type of BivarReg funcs
//      UnivarRec         : TUnivariateRec;     // Return rec type of Univar funcs
      Interval          : TIntervalRec;       // 2 extended values (upper,lower)
      CorMatrix         : XMatrix;            // Correlation matrix
      CovMatrix         : XMatrix;            // Covariance matrix
      Prepared          : boolean;

      constructor Create (aOwner: TComponent); override;

      destructor Destroy (aOwner: TComponent);

      procedure ZeroRowValues                      (var   PRegressData : PTRegressDataArray;
                                                    const k            : byte;
                                                    const RowNumber    : integer);

      procedure NewRowValues                       (var   PRegressData : PTRegressDataArray;
                                                    const Vector       : XVector;
                                                    const k            : byte;
                                                    const RowNumber    : integer);

      function Factorial                           (const IntIn        : byte): Int64;

      function Permutations                        (const TotalGroup   : byte;
                                                    const SubGroup     : byte): Int64;

      function Combinations                        (const TotalGroup   : byte;
                                                    const SubGroup     : byte): Int64;

      function CalcCp                               (const SSEp   : extended;
                                                     const MSEk   : extended;
                                                     const p      : byte;
                                                     const n      : integer): extended;

      function CalcFp                               (const R2p    : extended;
                                                     const R2k    : extended;
                                                     const k      : byte;
                                                     const p      : byte;
                                                     const n      : integer): extended;

      function CalcMSEp                             (const SSEp   : extended;
                                                     const p      : byte;
                                                     const n      : integer): extended;

      function BVCorrelationSampleTestStat (const PopCorrelation  : extended;
                                            const SmplCorrelation : extended;
                                            const n               : integer): extended;

      function LargeSampleCIforMean             (const YBar       : extended;
                                                 const Sigma      : extended;
                                                 const Percentage : string): TIntervalRec;

      function SmallSampleCIforMean             (const YBar       : extended;
                                                 const S          : extended;
                                                 const n          : integer): TIntervalRec;

      function CalcF                            (const MSR        : extended;
                                                 const MSE        : extended): extended;

      function CalcT                            (const XBar       : extended;
                                                 const Meu        : extended;
                                                 const S          : extended;
                                                 const n          : integer): extended;

{      function CalcChiSquare                    (const X          : array of extended;
                                                 const Y          : array of extended;
                                                 const n          : integer): extended;}

      function TTableValue2                     (const n          : integer): single;

      function CSqrTableValue                   (const n          : integer;
                                                 const dAlpha     : single): single;

      function ZTableValue2                (const PercentageLevel : string): extended;


      function ProbabilityZIsHigher             (const aSD        : extended): extended;

      function ProbabilityZIsLower              (const aSD        : extended): extended;

      function ProbabilityZBetweenPlusMinus     (const aSD        : extended): extended;

      function InitMatrix                       (const k          : byte;
                                                 const aValue     : extended): XMatrix;

      function InvertMatrix                     (const Matrix     : XMatrix;
                                                 const k          : byte;
                                                 var Det          : extended): XMatrix;

      function IdentityMatrix                   (const k          : byte): XMatrix;

      function CopyMatrix                       (const Matrix     : XMatrix;
                                                 const k          : byte): XMatrix;

      procedure SmoothData                      (var AnArray      : array of extended;
                                                 const n          : integer);


      function BVRegression                    ( const X          : array of extended;
                                                 const Y          : array of extended;
                                                 const n          : integer): TBivarStatRec;

      procedure BVPredictedY                   ( const X          : array of extended;
                                                 const Y          : array of extended;
                                                 var PredictedY   : array of extended;
                                                 const n          : integer);

      procedure BVResiduals                    ( const X          : array of extended;
                                                 const Y          : array of extended;
                                                 var Resids       : array of extended;
                                                 const n          : integer);

      function BVRegressionReverse           (const aBivarStatRec : TBivarStatRec;
                                              const aYValue       : extended): extended;

      function BVRegressionBeta1CI           (const aBivarStatRec : TBivarStatRec): TIntervalRec;

      function BVRegressionCI                (const aBivarStatRec : TBivarStatRec;
                                              const EstimatedY    : extended;
                                              const XValue        : extended): TIntervalRec;

      function BVRegressionPI                (const aBivarStatRec : TBivarStatRec;
                                              const EstimatedY    : extended;
                                              const XValue        : extended): TIntervalRec;

      function BVRegressionTestStatistic    ( const aBivarStatRec : TBivarStatRec): extended;

      function BVCalcYValue              (const BVRec         : TBivarStatRec;
                                          const XValue        : extended): extended;

      function BVQuadraticRegression     (const X             : array of extended;
                                          const Y             : array of extended;
                                          const n             : integer): TMultStatRec;

      function BVEquationString          (const aBVRec        : TBivarStatRec): string;

      function BVRegressionMemo          (const sStr          : string;
                                          const BVRec         : TBivarStatRec): string;

      function DummyXYRegression         (const X             : array of extended;
                                          const Y             : array of extended;
                                          const BooleanVar    : array of extended;
                                          const n             : integer;
                                          const TitleString   : string): string;

      procedure MVPrepare;

      procedure MVRemoveVariable          (var   PRegressData : PTRegressDataArray;
                                           var   k            : byte;
                                           const n            : integer;
                                           const VarNumber    : byte);

      function MVEquationString           (const MVRec        : TMultStatRec): string;

      function MVRegressionMemo           (const aMultStatRec : TMultStatRec): string;

      function MVCalcYValue               (const MVRec        : TMultStatRec;
                                           const XValues      : XVector): extended;

      function MVCheckXData               (const PRegressData : PTRegressDataArray;
                                           const k            : byte;
                                           const n            : integer): boolean;

      function MVRegression              ( const PRegressData : PTRegressDataArray;
                                           const Y            : array of extended;
                                           const k            : byte;
                                           const n            : integer): TMultStatRec;

      procedure MVPredictedY             ( const PRegressData : PTRegressDataArray;
                                           const Y            : array of extended;
                                           var   PredictedY   : array of extended;
                                           const k            : byte;
                                           const n            : integer);

      procedure MVResiduals              ( const PRegressData : PTRegressDataArray;
                                           const Y            : array of extended;
                                           var   Resids       : array of extended;
                                           const k            : byte;
                                           const n            : integer);

      procedure MVStdResiduals           ( const PRegressData : PTRegressDataArray;
                                           const Y            : array of extended;
                                           var   Resids       : array of extended;
                                           const k            : byte;
                                           const n            : integer);

      procedure MVStudentResiduals       ( const PRegressData : PTRegressDataArray;
                                           const Y            : array of extended;
                                           var   Resids       : array of extended;
                                           const k            : byte;
                                           const n            : integer);

      procedure MVCooksDistance          ( const PRegressData : PTRegressDataArray;
                                           const Y            : array of extended;
                                           var   CooksD       : array of extended;
                                           const k            : byte;
                                           const n            : integer);

      procedure MVLeverages               (const PRegressData : PTRegressDataArray;
                                             var LeverageVals : array of extended;
                                           const k            : byte;
                                           const n            : integer);

      procedure MVPredictedYCIUpper       (const PRegressData : PTRegressDataArray;
                                           const Y            : array of extended;
                                             var CIUpper      : array of extended;
                                           const k            : byte;
                                           const n            : integer);

      procedure MVPredictedYCILower       (const PRegressData : PTRegressDataArray;
                                           const Y            : array of extended;
                                             var CILower      : array of extended;
                                           const k            : byte;
                                           const n            : integer);

      procedure MVPredictedYPIUpper       (const PRegressData : PTRegressDataArray;
                                           const Y            : array of extended;
                                             var PIUpper      : array of extended;
                                           const k            : byte;
                                           const n            : integer);

      procedure MVPredictedYPILower       (const PRegressData : PTRegressDataArray;
                                           const Y            : array of extended;
                                             var PILower      : array of extended;
                                           const k            : byte;
                                           const n            : integer);

      function MVInteractionRegression    (const PRegressData : PTRegressDataArray;
                                           const Y            : array of extended;
                                           const k            : byte;
                                           const n            : integer;
                                           const Var1         : byte;
                                           const Var2         : byte): TMultStatRec;

      function MVPolynomialRegression     (const PRegressData : PTRegressDataArray;
                                           const Y            : array of extended;
                                           const k            : byte;
                                           const n            : integer;
                                           const Var1         : byte): TMultStatRec;

      function MVStepwiseForward         ( const PRegressData : PTRegressDataArray;
                                           const Y            : array of extended;
                                           const k            : byte;
                                           const n            : integer;
                                           const TitleString  : string): string;

      function MVStepwiseBackward        ( const PRegressData : PTRegressDataArray;
                                           const Y            : array of extended;
                                           const k            : byte;
                                           const n            : integer;
                                           const TitleString  : string): string;


      procedure LDistances                (const PRegressData : PTRegressDataArray;
                                           var   LDists       : TResidualsArray;
                                           const n            : integer;
                                           const k            : byte);

      procedure EuclideanDistances        (const PRegressData : PTRegressDataArray;
                                           var   EDists       : TResidualsArray;
                                           const n            : integer;
                                           const k            : byte);

      procedure NormEuclideanDistances    (const PRegressData : PTRegressDataArray;
                                           var   NEDists      : TResidualsArray;
                                           const n            : integer;
                                           const k            : byte);

      procedure MahalanobisDistances      (const PRegressData : PTRegressDataArray;
                                             var MDistances   : TResidualsArray;
                                           const k            : byte;
                                           const n            : integer);

      procedure MaximumLikelihoodDistances (const PRegressData : PTRegressDataArray;
                                              var MLDistances  : TResidualsArray;
                                            const k            : byte;
                                            const n            : integer;
                                            const APriori      : extended);


      function MeansDifferencesMatrix      (const MeansVector : XVector;
                                            const k           : byte): XMatrix;

      function CorrelationMatrix          (const PRegressData : PTRegressDataArray;
                                           const k            : byte;
                                           const n            : integer): XMatrix;

      function CovarianceMatrix           (const PRegressData : PTRegressDataArray;
                                           const k            : byte;
                                           const n            : integer): XMatrix;


      function MVPairedXYRegressions      ( const PRegressData : PTRegressDataArray;
                                            const Y            : array of extended;
                                            const k            : byte;
                                            const n            : integer): XVector;

      function MVAverageCorrelation       ( const PRegressData : PTRegressDataArray;
                                            const Y            : array of extended;
                                            const k            : byte;
                                            const n            : integer): extended;

      function CrossProducts             ( const PRegressData : PTRegressDataArray;
                                           const k            : byte;
                                           const n            : integer): XMatrix;

      function DurbinWatson              ( const PRegressData : PTRegressDataArray;
                                           const Y            : array of extended;
                                           const k            : byte;
                                           const n            : integer): extended;

      function FlagOutliers              ( const StdResids    : TResidualsArray;
                                           const dThreshold   : single;
                                           const n            : integer): TIntegerArray;

      function ComputeResults             (const PRegressData : PTRegressDataArray;
                                           const Y            : array of extended;
                                           const XTempInv     : XMatrix;
                                           const k            : byte;
                                           const n            : integer): TMultStatRec;

{      function YSummary                   (const Y            : array of extended;
                                           const n            : integer): TVarSummaryRec;

      function UnivariateSummary          (const PRegressData : PTRegressDataArray;
                                           const k            : byte;
                                           const n            : integer): TUnivariateRec;}


      function VarianceVector            (const PRegressData : PTRegressDataArray;
                                          const k            : byte;
                                          const n            : integer): XVector;

      function VarianceProportionsVector (const PRegressData : PTRegressDataArray;
                                          const k            : byte;
                                          const n            : integer): XVector;

      function MeanVector                 (const PRegressData : PTRegressDataArray;
                                           const k            : byte;
                                           const n            : integer): XVector;

      function SumVector                 (const PRegressData : PTRegressDataArray;
                                          const k            : byte;
                                          const n            : integer): XVector;

{      function MinVector                 (const PRegressData : PTRegressDataArray;
                                          const k            : byte;
                                          const n            : integer): XVector;

      function MaxVector                 (const PRegressData : PTRegressDataArray;
                                          const k            : byte;
                                          const n            : integer): XVector;}

      function GetZValue                 (const aValue       : extended;
                                          const aMean        : extended;
                                          const aStdDev      : extended): extended;

      function GetCenterValue            (const aValue       : extended;
                                          const aMean        : extended): extended;

{      function GetMedian                 (const AnArray      : array of extended;
                                          const n            : integer): extended;

      function GetSum                    (const AnArray      : array of extended;
                                          const n            : integer): extended;

      function GetMin                    (const AnArray      : array of extended;
                                          const n            : integer): extended;

      function GetMax                    (const AnArray      : array of extended;
                                          const n            : integer): extended;}

      function GetMean                   (const AnArray      : array of extended;
                                          const n            : integer): extended;

      function GetVariance               (const AnArray      : array of extended;
                                          const n            : integer): extended;

      function GetCoefficientOfVariation (const AnArray      : array of extended;
                                          const n            : integer): extended;

{      function GetSkew                   (const AnArray      : array of extended;
                                          const n            : integer): single;

      function GetKurtosis               (const AnArray      : array of extended;
                                          const n            : integer): single;}

      function GetTotalVariation         (const VarVector    : XVector;
                                          const k            : byte): extended;

      procedure TransformDataCenter       (var PRegressData  : PTRegressDataArray;
                                          const k            : byte;
                                          const n            : integer);

      procedure TransformDataStandardize  (var PRegressData  : PTRegressDataArray;
                                          const k            : byte;
                                          const n            : integer);

      procedure TransformDataNormalize    (var PRegressData  : PTRegressDataArray;
                                          const k            : byte;
                                          const n            : integer);

      procedure TransformArrCenter         (var X            : array of extended;
                                          const n            : integer);

      procedure TransformArrStandardize    (var X            : array of extended;
                                          const n            : integer);

      procedure TransformArrNormalize      (var X            : array of extended;
                                          const n            : integer);

      procedure TransformArrInverse        (var X            : array of extended;
                                          const n            : integer);

      procedure TransformArrSquare         (var X            : array of extended;
                                          const n            : integer);

      procedure TransformArrSquareRoot     (var X            : array of extended;
                                          const n            : integer);

      procedure TransformArrAddScalar      (var X            : array of extended;
                                          const aScalar      : extended;
                                          const n            : integer);

      procedure TransformArrMultiplyScalar (var X            : array of extended;
                                          const aScalar      : extended;
                                          const n            : integer);

      function MakePiped                     (const Mean     : extended;
                                              const StdDev   : extended;
                                              const Scalar   : extended): TIntervalRec;

      function GetLDistance                  (const Vector      : XVector;
                                              const MeanVector  : XVector;
                                              const k           : byte): extended;

      function GetEuclideanDistance          (const Vector      : XVector;
                                              const MeanVector  : XVector;
                                              const k           : byte): extended;

      function GetNormEuclideanDistance     (const Vector       : XVector;
                                             const MeanVector   : XVector;
                                             const CovMatrix    : XMatrix;
                                             const k            : byte): extended;

      function GetMahalanobisDistance       (const PRegressData : PTRegressDataArray;
                                             const Vector       : XVector;
                                             const k            : byte;
                                             const n            : integer): extended;

      function GetMaximumLikelihoodDistance (const PRegressData : PTRegressDataArray;
                                             var   Vector       : XVector;
                                             const k            : byte;
                                             const n            : integer;
                                             const APriori      : extended): extended;

      function GetAvgNormDistance           (const MeanVector1  : XVector;
                                             const MeanVector2  : XVector;
                                             const CovMatrix1   : XMatrix;
                                             const CovMatrix2   : XMatrix;
                                             const k            : byte): extended;

      function MVOptimalForwardModel        (const PRegressData : PTRegressDataArray;
                                             const Y            : array of extended;
                                             const k            : byte;
                                             const n            : integer): string;

    published
      property AlphaLevel : integer Read FAlpha Write SetAlpha;
      property ScanData   : boolean Read FScanData Write SetScanData default FALSE;
    end;



implementation

uses Ucmdprocessor;

constructor TRegComp.Create (AOwner: TComponent);
begin
  Inherited Create(AOwner);
  New(PRegressData);
  AlphaLevel  := 95;
  Prepared    := FALSE;
  ScanData    := TRUE;
end;


destructor TRegComp.Destroy;
begin
  Dispose (PRegressData);
  inherited Destroy;  { Call inherited destructor ! }
end;


procedure TRegComp.SetAlpha (Value: integer);
begin
  if (Value <> FAlpha) then begin
    if (Value in [90,95,99]) then begin
      FAlpha := Value;
    end;
  end;
end;


procedure TRegComp.SetScanData (Value: boolean);
begin
  if (Value = TRUE) then
     FScanData := TRUE
  else
     FScanData := FALSE;
end;


// ======================= Begin Code Functions, etc. ========================

function TRegComp.PadStr (const sIn: string): string;
var
  i: integer;
  c: integer;
  s: string;
begin
  s:= '';
  c:= 14 - length (sIn);
  for i:= 1 to c do
     s:= s + ' ';
  s:= s + sIn;

  Result := s;
end;


function TRegComp.NearZero(const dValue: extended): boolean;
begin
  result:= FALSE;
  if (dValue > -0.00001) and (dValue < 0.00001) then
     result:= TRUE;
end;


function TRegComp.Permutations (const TotalGroup: byte;
                                const SubGroup: byte): Int64;
var
  n1 : Int64;
  n2 : Int64;
begin
  Result:= 0;

  if (TotalGroup in [1..25]) and
     (SubGroup in [1..24]) and
     (TotalGroup > SubGroup) then

  begin
     n1:= Factorial(TotalGroup);
     n2:= Factorial(TotalGroup-SubGroup);
     if abs( n2 ) < 1 then begin
        Raise Exception.Create(PermutationError);
     end
     else
        Result:= n1 div n2;
  end;

end;


function TRegComp.Combinations (const TotalGroup: byte;
                                const SubGroup: byte): Int64;
var
  n1 : Int64;
  n2 : Int64;
  n3 : Int64;
begin
  Result:= 0;

  if ( TotalGroup in [1..25] ) and
     ( SubGroup in [1..24] ) and
     ( TotalGroup > SubGroup ) then
  begin
     n1:= Factorial(TotalGroup);
     n2:= Factorial(TotalGroup-SubGroup);
     n3:= Factorial(SubGroup);
     if ( abs ( n2 ) < 1 ) or ( abs ( n3 ) < 1 ) then begin
        Raise Exception.Create(CombinationError);
     end
     else
        Result:= n1 div (n2*n3);
  end;

end;


function TRegComp.Factorial (const IntIn : byte): int64;
type
  TFactorials = array [1..25] of Int64;
var
  Factorials  : TFactorials;
begin
  Result:= 0;

  if (IntIn in [1..20]) then begin
    Factorials[1]  :=                          1;
    Factorials[2]  :=                          2;
    Factorials[3]  :=                          6;
    Factorials[4]  :=                         24;
    Factorials[5]  :=                        120;
    Factorials[6]  :=                        720;
    Factorials[7]  :=                       5040;
    Factorials[8]  :=                      40320;
    Factorials[9]  :=                     362880;
    Factorials[10] :=                    3628800;
    Factorials[11] :=                   39916800;
    Factorials[12] :=                  479001600;
    Factorials[13] :=                 6227020800;
    Factorials[14] :=                87178291200;
    Factorials[15] :=              1307674368000;
    Factorials[16] :=             20922789888000;
    Factorials[17] :=            355687428096000;
    Factorials[18] :=           6402373705728000;
    Factorials[19] :=         121645100408800000;
    // Only 20! handled at present.
    Result:= Factorials[IntIn];
  end;
end;


// ===========================================================================

function TRegComp.TransposeVector (const Vector : XVector;
                                   const k      : byte): XVector;
var
  i: byte;
  j: byte;
  VectorOut: XVector;
begin

  VectorOut:= InitVector(MAXVARIABLES,0);

  j:= k;
  for i:= 1 to k do begin
    VectorOut[j]:= Vector[i];
    dec(j);
  end;

  Result:= VectorOut;

end;


function TRegComp.AddVector (const Vector1 : XVector;
                             const Vector2 : XVector;
                             const k       : byte): XVector;
var
  i: byte;
  VectorOut: XVector;
begin

  VectorOut:= InitVector(MAXVARIABLES,0);

  for i:= 1 to k do
    VectorOut[i]:= Vector1[i] + Vector2[i];

  Result:= VectorOut;

end;


function TRegComp.SubtractVector (const Vector1 : XVector;
                                  const Vector2 : XVector;
                                  const k       : byte): XVector;
var
  i: byte;
  VectorOut: XVector;
begin

  VectorOut:= InitVector(MAXVARIABLES,0);

  for i:= 1 to k do
    VectorOut[i]:= Vector1[i] - Vector2[i];

  Result:= VectorOut;

end;

function TRegComp.AddMatrices (const Matrix1 : XMatrix;
                               const Matrix2 : XMatrix;
                               const k       : byte): XMatrix;
var
  i: byte;
  j: byte;
  MatrixOut: XMatrix;
begin

  MatrixOut:= InitMatrix(MAXVARIABLES,0);

  for i:= 1 to k do
    for j:= 1 to k do
      MatrixOut[i,j] := Matrix1[i,j] + Matrix2[i,j];

  Result:= MatrixOut;

end;


function TRegComp.DivideMatrixScalar (const Matrix   : XMatrix;
                                      const k        : byte;
                                      const aScalar  : extended): XMatrix;
var
  i: byte;
  j: byte;
  MatrixOut: XMatrix;
begin

  MatrixOut:= InitMatrix(MAXVARIABLES,0);

  for i:= 1 to k do
    for j:= 1 to k do
      MatrixOut[i,j] := Matrix[i,j] / aScalar;

  Result:= MatrixOut;

end;


function TRegComp.SubtractMatrices (const Matrix1 : XMatrix;
                                    const Matrix2 : XMatrix;
                                    const k       : byte): XMatrix;
var
  i: byte;
  j: byte;
  MatrixOut: XMatrix;
begin

  MatrixOut:= InitMatrix(MAXVARIABLES,0);

  for i:= 1 to k do
    for j:= 1 to k do
      MatrixOut[i,j] := Matrix1[i,j] - Matrix2[i,j];

  Result:= MatrixOut;

end;


function TRegComp.MatrixTrace (const Matrix : XMatrix;
                               const k      : byte): extended;
var
  i: byte;
begin

  Result:= 0.0;
  for i := 1 to k do
    Result:= Result + Matrix[i,i];

end;


// ==========================================================================


function TRegComp.ProbabilityZIsHigher (const aSD: extended): extended;
var
  dZValue: extended;
begin

  Result:= 0.0;

  dZValue:= ZTableValue(aSD);

  if ( aSD < 0.0 ) then
    Result:= 0.5 + dZValue
  else if ( aSD > 0.0 ) then
    Result:= 0.5 - dZValue;

end;


function TRegComp.ProbabilityZIsLower (const aSD: extended): extended;
var
  dZValue: extended;
begin

  Result:= 0.0;

  dZValue:= ZTableValue(aSD);

  if ( aSD < 0.0 ) then
    Result:= 0.5 - dZValue
  else if ( aSD > 0.0 ) then
    Result:= 0.5 + dZValue;

end;


function TRegComp.ProbabilityZBetweenPlusMinus (const aSD: extended): extended;
var
  dZValue : extended;
begin

  dZValue  := ZTableValue(aSD); // the area under the z curve for upper half
  Result   := dZValue*2;        // double it for both sides of curve

end;


function TRegComp.ZTableValue (const Z: extended): extended;
type
  ZArray = array [0..310] of single;
var
  iTmpIndx: integer;
  dTmpVal: extended;
  ZArr: ZArray;
begin
{$Include z.inc}

  if ( Z < -3.1 ) then
     dTmpVal:= -3.1
  else if ( Z > 3.1 ) then
     dTmpVal:= 3.1
  else
     dTmpVal:= Z;

  iTmpIndx:= round( abs ( dTmpVal*100 ) );  //  2.42 becomes ... 242

  Result:= ZArr[iTmpIndx];

end;


function TRegComp.ZTableValue2 (const PercentageLevel : string): extended;
begin
  Result:= 0.0;

  if ( PercentageLevel = '.90' ) or ( PercentageLevel = '0.90' ) then
     Result:= 1.645;

  if ( PercentageLevel = '.95' ) or ( PercentageLevel = '0.95' ) then
     Result:= 1.960;

  if ( PercentageLevel = '.99' ) or ( PercentageLevel = '0.99' ) then
     Result:= 2.576;

end;


function TRegComp.CSqrTableValue (const n      : integer;
                                  const dAlpha : single): single;
type
  TChiSqrRec = record
                 df  : byte;
                 t05 : single;
                 t01 : single;
               end;

var
  ChiArr : array [0..30] of TChiSqrRec;
begin

  Result:= 0;

{$I cValues.inc}
  if ( round( dAlpha * 1000 ) = 10 ) then
     Result:= ChiArr[n].t01;

  if ( round( dAlpha * 1000 ) = 50 ) then
     Result:= ChiArr[n].t05;

end;


function TRegComp.TTableValue2 (const n : integer): single;
// Two-tailed t statistic values from tables
type
  tStatRec = record
  df   : byte;
  t10  : single;
  t05  : single;
  t025 : single;
  t010 : single;
  t005 : single;
  end;
var
  tStatArr: array [1..50] of tStatRec;
  df: integer;
begin
{$I tvalues.inc}

  Result   := 0.0;
  df       := n;

  if ( df > 30 ) then // t table values 'peak out' at 30
     df:= 30;

  if ( df in [1..30] ) then begin
    case AlphaLevel of
      90  : Result:= tStatArr[df].t10;   // 0.10
      95  : Result:= tStatArr[df].t05;   // 0.05
      99  : Result:= tStatArr[df].t010;  // 0.01
      995 : Result:= tStatArr[df].t005;  // 0.005
    end;
  end;

end;


// ========================= Some Test-Stats Calculated =======================

{
function TRegComp.CalcChiSquare (const X : array of extended;
                                 const Y : array of extended;
                                 const n : integer): extended;
var
  i: integer;
  dCSqr: extended;
  dTmp: extended;
begin

  Result:= 0.0;

  try
    dCSqr:= 0.0;

    if ( n > 2 ) and ( n < MAXOBSERVATIONS ) then begin
      for i:= 0 to n-1 do begin
        if ( Y[i] > 0.000001 ) then begin
          dTmp   := Sqr( X[i] - Y[i] );
          dCSqr  := dCSqr + ( dTmp / Y[i] );
        end;
      end;

      Result:= dCSqr;
    end;

  except
    Raise Exception.Create(ChiSquareError);
  end;

end;
}

function TRegComp.CalcT (const XBar : extended;
                         const Meu  : extended;
                         const S    : extended;
                         const n    : integer): extended;
begin

  Result:= 0;

  try
    if ( S > 0.0 ) and ( n > 0 ) then
      Result:= (XBar - Meu) / ( S * sqrt(n) )

  except
    Raise Exception.Create(TCalculatedError);
  end;

end;


function TRegComp.SmallSampleCIforMean (const YBar       : extended;
                                        const S          : extended;
                                        const n          : integer): TIntervalRec;
var
  CInterval: TIntervalRec;
begin

  CInterval := InitInterval;
  Result    := CInterval;

  try
    if ( n > 0 ) then begin
        with CInterval do begin // Remember, that the Alpha% is set by AlphaLevel property
           Upper:= YBar + TTableValue2(n) * ( s / sqrt ( n ) );
           Lower:= YBar - TTableValue2(n) * ( s / sqrt ( n ) );
        end;
    end;
    Result:= CInterval;

  except
    Raise Exception.Create(SmallSampleError);
  end;

end;


function TRegComp.LargeSampleCIforMean (const YBar       : extended;
                                        const Sigma      : extended;
                                        const Percentage : string): TIntervalRec;
var
  CInterval: TIntervalRec;
begin

  CInterval := InitInterval;
  Result    := CInterval;

  try
    with CInterval do begin
      Upper:= YBar + ZTableValue2(Percentage) * Sigma;
      Lower:= YBar - ZTableValue2(Percentage) * Sigma;
    end;
    Result:= CInterval;

  except
    Raise Exception.Create(LargeSampleError);
  end;

end;


// =============== Private Low-level matrix/vector utility funcs: =============


function TRegComp.MultiplyMatrices (const Matrix1 : XMatrix;
                                    const Matrix2 : XMatrix;
                                    const k       : byte): XMatrix;
var
  i: byte;
  j: byte;
  l: byte;
  cnt: byte;
  dSum: extended;
  MatrixOut: XMatrix;
begin

  MatrixOut:= InitMatrix(MAXVARIABLES,0);

  l:= k;
  for i:= 1 to k do begin
    for j:= 1 to k do begin
      dSum:= 0.0;
      for cnt:= 1 to l do
        dSum:= dSum + ( Matrix1[cnt,j] * Matrix2[i,cnt] );
      MatrixOut[i,j]:= dSum;
    end;
  end;

  Result:= MatrixOut;

end;


function TRegComp.InvertMatrix (const Matrix : XMatrix;
                                const k      : byte;
                                  var Det    : extended): XMatrix;
var
  i: byte;
  j: byte;
  l: byte;
  dDiver: extended;
  dRatio: extended;
  MatrixOut: XMatrix;
  MatrixIn: XMatrix;
begin

  MatrixOut   := IdentityMatrix(k);   // 'create' identity (k x k)
  MatrixIn    := CopyMatrix(Matrix,k);

  Det   := 1.0;                       // determinant

  for i := 1 to k do begin            // ok, do it
    dDiver := MatrixIn[i,i];
    Det    := Det * dDiver;

    for j := 1 to k do begin
      try
        MatrixIn[i,j]:= MatrixIn [i,j] / dDiver;
        MatrixOut[i,j]:= MatrixOut[i,j] / dDiver;
      except
        Raise Exception.Create('Error div by 0 inverting matrices.');
        dm.error('Error div by 0 inverting matrices.', [], 119001);
      end;
    end;

    for j := 1 to k do begin
      if ( i-j <> 0 ) then begin
        dRatio:= MatrixIn[j,i];
        for l := 1 to k do begin
          MatrixIn [j,l]:= MatrixIn[j,l] - ( dRatio * MatrixIn[i,l] );
          MatrixOut [j,l]:= MatrixOut[j,l] - ( dRatio * MatrixOut[i,l] );
        end;
      end;
    end;
  end;

  Result:= MatrixOut;             // return Result

end;


function TRegComp.DeterminantMatrix (const Matrix : XMatrix;
                                     const k      : byte): extended;
var
  i: byte;
  j: byte;
  l: byte;
  dDiver: extended;
  dDet: extended;
  dRatio: extended;
  MatrixOut: XMatrix;
  MatrixIn: XMatrix;
begin

  MatrixOut   := IdentityMatrix(k);   // 'create' identity (k x k)
  MatrixIn    := CopyMatrix(Matrix,k);

  dDet := 1.0;                        // determinant

  for i := 1 to k do begin            // ok, do it
    dDiver := MatrixIn[i,i];
    dDet   := dDet * dDiver;

    for j := 1 to k do begin
      try
        MatrixIn[i,j]:= MatrixIn[i,j] / dDiver;
        MatrixOut[i,j]:= MatrixOut[i,j] / dDiver;
      except
        Raise Exception.Create('Error div by 0 inverting matrices.');
        dm.error('Error div by 0 inverting matrices.', [], 119001);
      end;
    end;

    for j := 1 to k do begin
      if ( i-j <> 0 ) then begin
        dRatio:= MatrixIn[j,i];
        for l := 1 to k do begin
          MatrixIn[j,l]:= MatrixIn[j,l] - ( dRatio * MatrixIn[i,l] );
          MatrixOut[j,l]:= MatrixOut[j,l] - ( dRatio * MatrixOut[i,l] );
        end;
      end;
    end;
  end;

  Result:= dDet;     // return Result

end;


function TRegComp.IdentityMatrix (const k : byte): XMatrix;
var
  i,j: byte;
begin

  Result:= InitMatrix(MAXVARIABLES,0);

  for i:= 1 to k do
     for j:= 1 to k do
        Result[i,j]:= 0.0;

  for i:= 1 to k do
     Result[i,i]:= 1.0;

end;


function TRegComp.CopyMatrix (const Matrix : XMatrix;
                              const k      : byte): XMatrix;
var
  i,j: byte;
begin

  Result:= InitMatrix(MAXVARIABLES,0);

  for i:= 1 to k do
     for j:= 1 to k do
        Result[i,j]:= Matrix[i,j];

end;


function TRegComp.MultiplyVectorByMatrix (const Matrix : XMatrix;
                                          const Vector : XVector;
                                          const k      : byte): XVector;
var
  i: byte;
  j: byte;
  dSum: extended;
  VectorOut: XVector;
begin

  VectorOut:= InitVector(MAXVARIABLES,0);

  for i:= 1 to k do begin                         // rows
    dSum:= 0.0;
    for j:= 1 to k do
      dSum := dSum + ( Vector[j] * Matrix[j,i] ); // changed aMatrix[i,j] to
    VectorOut[i]:= dSum;                          // aMatrix[j,i] 11/30/98
  end;

  Result:= VectorOut;

end;


function TRegComp.MultiplyMatrixByVector (const Matrix : XMatrix;
                                          const Vector : XVector;
                                          const k      : byte): XVector;
var
  i: byte;
  j: byte;
  dSum: extended;
  VectorOut: XVector;
begin

  VectorOut:= InitVector(MAXVARIABLES,0);

  for i:= 1 to k do begin
    dSum:= 0.0;
    for j:= 1 to k do
      dSum := dSum + Vector[j] * Matrix[j,i];
    VectorOut[i]:= dSum;
  end;

  Result:= VectorOut;

end;


function TRegComp.MultiplyVectorByVector (const Vector1 : XVector;
                                          const Vector2 : XVector;
                                          const k       : byte): extended;
var
  i: byte;
  dSum: extended;
begin

  dSum:= 0.0;
  for i:= 1 to k do
    dSum:= dSum + Vector1[i] * Vector2[i];

  Result:= dSum;

end;


function TRegComp.MultiplyVectorByTransposeVector (const Vector1 : XVector;
                                                   const Vector2 : XVector;
                                                   const k       : byte): extended;
var
  i:byte;
  n: byte;
  dSum: extended;
begin

  dSum:= 0.0;
  n:= k;

  for i:= 1 to k do begin
    dSum:= dSum + Vector1[i] * Vector2[n];
    dec(n);
  end;

  Result:= dSum;

end;


// ===========================================================================


function TRegComp.GetZValue  (const aValue  : extended;
                              const aMean   : extended;
                              const aStdDev : extended): extended;
begin

  Result:= 0.0;

  if (aStdDev > 0.0) then
    Result:= ( aValue - aMean ) / aStdDev;

end;


function TRegComp.GetCenterValue (const aValue : extended;
                                  const aMean  : extended): extended;
begin
  Result:= aValue - aMean;
end;


function TRegComp.Cube (const InValue : extended): extended;
begin
  Result:= InValue * InValue * InValue;
end;


function TRegComp.CalcF (const MSR : extended;
                         const MSE : extended): extended;
begin

  Result:= 0.0;

  if ( MSE <> 0.0 ) then
    Result:= MSR / MSE;

end;


function TRegComp.CalcCp (const SSEp : extended;
                          const MSEk : extended;
                          const    p : byte;
                          const    n : integer): extended;

// p: subset of vars used in reduced model
// k: set of vars used in maximum model
begin

  try
    if ( MSEk > 0.0 ) or ( MSEk < 0.0 )  then
      Result:= ( SSEp / MSEk ) - ( n - ( 2 * ( p + 1 ) ) )
    else
      Result:= SSEp - ( n - ( 2 * ( p + 1 ) ) )

  except
    Raise Exception.Create(CalcCpError);
  end;

end;


function TRegComp.CalcFp (const R2p : extended;
                          const R2k : extended;
                          const   k : byte;
                          const   p : byte;
                          const   n : integer): extended;
var
  d1 : extended;
  d2 : extended;

begin

  Result:= 0.0;

  try
    if ( ( k - p ) <> 0 ) and ( ( n - k - 1 ) <> 0 ) then begin
      d1:= ( R2k - R2p ) / ( k - p );
      d2:= ( 1.0 - R2k ) / ( n - k - 1 );
      Result:= d1 / d2;
    end;

  except
    Raise Exception.Create(CalcFpError);
  end;

end;


function TRegComp.CalcMSEp (const SSEp : extended;
                            const    p : byte;
                            const    n : integer): extended;

begin

  Result:= 0.0;

  try
    if ( n - p - 1 ) <> 0 then
      Result:= SSEp / ( n - p - 1 );

  except
    Raise Exception.Create(CalcMSEpError);
  end;

end;


// ======================== Initialization Routines Below: ====================


function TRegComp.InitVector (const k      : byte;
                              const aValue : integer): XVector;
var
  i: byte;
  VectorOut: XVector;

begin
  setLength(VectorOut,MAXVARIABLES+2); //2 instead of 1 because dyn array start at 0

  for i:= 1 to MAXVARIABLES+1 do
    VectorOut[i]:= 0;

  for i:= 1 to k do
    VectorOut[i]:= aValue;

  Result:= VectorOut;

end;


function TRegComp.InitMatrix (const k      : byte;
                              const aValue : extended):XMatrix;

var
  i: byte;
  j: byte;
  MatrixOut: XMatrix;

begin

  for i:= 1 to MAXVARIABLES+1 do
    for j:= 1 to MAXVARIABLES+1 do
      MatrixOut[i,j]:= aValue;

  Result:= MatrixOut;

end;


function TRegComp.InitInterval: TIntervalRec;
var
  anInterval: TIntervalRec;
begin

  with anInterval do begin
    Upper:= 0.0;
    Lower:= 0.0;
  end;

  Result:= anInterval;

end;


procedure TRegComp.InitResidArray (var AnArray : array of extended;
                                 const aValue  : extended);
var
  i: integer;
begin

  for i:= 0 to MAXOBSERVATIONS-1 do
    AnArray[i]:= aValue;

end;

{
function TRegComp.InitUnivariateRec: TUnivariateRec;
var
  i: byte;
  urRec: TUnivariateRec;
begin

  with urRec do begin
    k:= 0;
    n:= 0;
    for i:= 1 to MAXVARIABLES+1 do begin
      Sums[i]      := 0;
      Mins[i]      := 0;
      Maxs[i]      := 0;
      Ranges[i]    := 0;
      Means[i]     := 0;
      StdDevs[i]   := 0;
      CofVs[i]     := 0;
      Skews[i]     := 0;
      Kurts[i]     := 0;
      Variances[i] := 0;
      Medians[i]   := 0;
    end;
  end;

  Result:= urRec;

end;

}
function TRegComp.InitBivarStatRec: TBivarStatRec;
var
  BVStatRec: TBivarStatRec;
begin

  with BVStatRec do begin
    N       := 0;
    r       := 0;
    XMean   := 0;
    YMean   := 0;
    Beta0   := 0;
    Beta1   := 0;
    SSXX    := 0;
    SSYY    := 0;
    SSXY    := 0;
    SumX    := 0;
    SumY    := 0;
    SumXY   := 0;
    SumX2   := 0;
    SumY2   := 0;
    z       := 0;
    t       := 0;
    df      := 0;
  end;

  Result:= BVStatRec;

end;


function TRegComp.InitMultStatRec: TMultStatRec;
var
  aMultStatRec: TMultStatRec;
  i: byte;
begin

  with aMultStatRec do begin
    N        := 0;
    k        := 0;
    R        := 0;
    R2       := 0;
    FStat    := 0;
    RMSE     := 0;
    MSE      := 0;
    MSR      := 0;
    SSE      := 0;
    SSR      := 0;
    SST      := 0;
    S2       := 0;
    S        := 0;
    DFModel  := 0;
    DFError  := 0;
    DFTotal  := 0;

    setLength(Beta,MAXVARIABLES+2); //2 instead of 1 because dyn array start at 0
    setLength(SeBeta,MAXVARIABLES+2);
    setLength(TValue,MAXVARIABLES+2);

    for i:= 1 to MAXVARIABLES+1 do begin
      Beta[i]   := 0;
      SeBeta[i] := 0;
      TValue[i] := 0;
    end;
  end;

  Result:= aMultStatRec;

end;


// ===========================================================================

procedure TRegComp.NewRowValues (var   PRegressData : PTRegressDataArray;
                                 const Vector       : XVector;
                                 const k            : byte;
                                 const RowNumber    : integer);
var
  i: integer;
begin

  for i:= 1 to k do
    PRegressData^[i,RowNumber]:= Vector[i];

  PRegressData^[0,RowNumber]:= 1;

  for i:= k+1 to MAXVARIABLES do
    PRegressData^[i,RowNumber]:= 0.0;

end;


procedure TRegComp.ZeroRowValues (var   PRegressData : PTRegressDataArray;
                                  const k            : byte;
                                  const RowNumber    : integer);
var
  i: integer;
begin

  for i:= 0 to MAXVARIABLES do
    PRegressData^[i,RowNumber]:= 0.0;

end;


// ================= 3-element mean filter for continuous data ================


procedure TRegComp.SmoothData (var AnArray : array of extended;
                               const n     : integer);
var
  i: integer;
  dSum: extended;
  TmpArray: TResidualsArray;
begin

  try
    if ( n > 2 ) and ( n < MAXOBSERVATIONS ) then begin

      for i:= 0 to n-1 do
        TmpArray[i] := AnArray[i];

      for i:= 1 to n-2 do begin  // don't use first nor last element of array
        dSum:= ( TmpArray[i-1] + TmpArray[i] + TmpArray[i+1] ) / 3.0;
        AnArray[i]:= dSum;
      end;

    end;

  except
    Raise Exception.Create(SmoothError);
  end;

end;


// ============== Utility func to check for identical values etc. =============

function TRegComp.MVCheckXData (const PRegressData : PTRegressDataArray;
                                const k            : byte;
                                const n            : integer): boolean;
const
  CLOSE_TO_ZERO = 0.01;  // somewhat arbitrary, but this is close to 0.0
var
  c: integer;
  a: byte;
  b: byte;
  ZeroSums: byte;
  ZeroDiffs: byte;
  dTmpNum: extended;
  SumVctr: XVector;
begin

  try
    ZeroSums  := 0;
    ZeroDiffs := 0;
    SumVctr   := InitVector(MAXVARIABLES,0);

    if  ( k > MAXOBSERVATIONS-1 ) or
        ( n < k ) or ( n > MAXOBSERVATIONS ) then
    begin
      Result:= FALSE;
      exit;
    end;

    // If the sum of a column = 0, then it is likely a problem for matrix inversion
    SumVctr:= SumVector(PRegressData,k,n);
    for a:= 1 to k do begin
      if ( abs ( SumVctr[a] ) < CLOSE_TO_ZERO ) then
        inc( ZeroSums );
    end;

    // Checks to see if any two sums of column values have a zero difference
    // if a 0 diff exists, it is likely that two columns have identical values
    // this also will likely be a problem for the matrix inversion
    for a:= 1 to k do begin
      for b:= 1 to k do begin
        if ( a > b ) then begin
          dTmpNum:= 0;

          for c:= 0 to n-1 do begin
            dTmpNum:= dTmpNum + ( PRegressData^[a,c] - PRegressData^[b,c] );
          end;

          if ( abs ( dTmpNum ) < 1 ) then
            inc(ZeroDiffs);

        end;
      end;
    end;

    // The below checks to see how many columns (variables) in the input data
    // array summed to very near 0.0.  If two or more did actually sum to 0.0,
    // then this could very likely make it impossible to invert the X matrix
    // which is required in the MultiVarRegression routine...
    if ( ZeroSums > 1 ) or ( ZeroDiffs > 0 ) then
      Result:= FALSE
    else
      Result:= TRUE;

  except
    Raise Exception.Create(CheckDataError);
  end;

end;


// ======== Univariate utility funcs [used mostly for Univ Summary] ===========
(*
function TRegComp.GetMedian (const AnArray : array of extended;
                             const n       : integer): extended;

  function RankSort ( AnArr : array of extended;
                      n     : integer): extended;

  var  I, J     : integer;
       T        : extended;
       IndexNum : integer;

  begin
    Result:= 0.0;

    for I := 1 to n do
      for J := n downto I+1 do
      if (AnArr[I] > AnArr[J]) then begin
        T        := AnArr[I];
        AnArr[I] := AnArr[J];
        AnArr[J] := T;
      end;

    if odd(n) then begin
     IndexNum:= (n-1) div 2;
     Result:= AnArr[IndexNum];
    end;

    if NOT Odd(n) then begin
      IndexNum:= n div 2;
      IndexNum:= IndexNum - 1;
      Result:= ( AnArr[IndexNum]+AnArr[IndexNum+1] ) / 2.0;
    end;
  end;


begin

  Result:= 0;

  if ( n > 0 ) and ( n < MAXOBSERVATIONS ) then
    Result:= RankSort(AnArray,n);

end;


function TRegComp.GetSum (const anArray : array of extended;
                          const n       : integer): extended;
var
  i: integer;
  dSum: extended;
begin

  Result := 0.0;
  dSum   := 0.0;

  try
    if ( n > 1 ) and ( n < MAXOBSERVATIONS ) then begin
      for i:= 0 to n-1 do
        dSum:= dSum + (AnArray[i]);

      Result:= dSum;
    end;

  except
    Raise Exception.Create(GetSumError);
  end;

end;


function TRegComp.GetMin (const anArray : array of extended;
                          const n       : integer): extended;
var
  i: integer;
  dMin: extended;
begin

  Result:= 0.0;

  dMin := VERYLARGENUMBER;
  try
    if ( n > 1 ) and ( n < MAXOBSERVATIONS ) then begin
      for i:= 0 to n-1 do begin
        if ( anArray[i] < dMin ) then
          dMin := anArray[i];
      end;

      Result:= dMin;
    end;

  except
    Raise Exception.Create(GetMinError);
  end;

end;


function TRegComp.GetMax (const anArray : array of extended;
                          const n       : integer): extended;
var
  i: integer;
  dMax: extended;
begin

  Result:= 0.0;
  dMax := VERYSMALLNUMBER;

  try
    if ( n > 1 ) and ( n < MAXOBSERVATIONS ) then begin
      for i:= 0 to n-1 do begin
        if ( anArray[i] > dMax ) then
          dMax := anArray[i];
      end;

      Result:= dMax;
    end;

  except
    Raise Exception.Create(GetMaxError);
  end;

end;

*)
function TRegComp.GetMean (const anArray : array of extended;
                           const n       : integer): extended;
var
  i: integer;
  dSum: extended;
begin

  Result:= 0.0;

  try
    if ( n > 1 ) and ( n < MAXOBSERVATIONS ) then begin
      dSum:= 0.0;
      for i:= 0 to n-1 do
        dSum:= dSum + anArray[i];

      Result:= dSum / n;
    end;

  except
    Raise Exception.Create(GetMeanError);
  end;

end;


function TRegComp.GetVariance (const anArray : array of extended;
                               const n       : integer): extended;
var
  i: integer;
  dSum: extended;
  dSumSqr: extended;
  dNumer: extended;
begin

  Result:= 0.0;

  try
    if ( n > 1 ) then begin
      dSum:= 0.0;
      dSumSqr:= 0.0;

      for i:= 0 to n-1 do begin
        dSum:= dSum + anArray[i];
        dSumSqr:= dSumSqr + sqr(anArray[i]);
      end;

      dNumer:= dSumSqr - ( sqr ( dSum ) / n );

      Result:= dNumer / ( n-1 );
    end;

  except
    Raise Exception.Create(GetVarianceError);
  end;

end;


function TRegComp.GetCoefficientOfVariation (const AnArray : array of extended;
                                             const n       : integer): extended;
var
  dMean     : extended;
  dVariance : extended;
begin

  Result:= 0;

  try
    if ( n > 2 ) then begin
      dMean:= GetMean(AnArray,n);
      dVariance:= GetVariance(AnArray,n);

      if ( dMean <> 0.0 ) then
        Result:= sqrt( dVariance ) / ( dMean );

    end;

  except
    Raise Exception.Create(GetCofVariationError);
  end;

end;

(*
function TRegComp.GetSkew (const AnArray   : array of extended;
                           const n         : integer): single;

// Adapted from the Skew function contained with the Delphi Source

var
  dSum: extended;
  dSumSquares: extended;
  dSumCubes: extended;
  dOverN: extended;
  dAccum: extended;
  dM1Sqr: extended;
  dS2N: extended;
  dS3N: extended;
  dM1: extended;
  dM2: extended;
  dM3: extended;
  dSkew: extended;
  I: Integer;
begin

  dOverN      := 1 / ( n + 1 );
  dSum        := 0;
  dSumSquares := 0;
  dSumCubes   := 0;

  for I := 0 to n-1 do begin
    dSum         := dSum + AnArray[I];
    dAccum       := Sqr(AnArray[I]);
    dSumSquares  := dSumSquares + dAccum;
    dAccum       := dAccum*AnArray[I];
    dSumCubes    := dSumCubes + dAccum;
  end;

  dM1    := dSum * dOverN;
  dM1Sqr := Sqr(dM1);
  dS2N   := dSumSquares * dOverN;
  dS3N   := dSumCubes * dOverN;
  dM2    := dS2N - dM1Sqr;
  dM3    := dS3N - ( dM1 * 3 * dS2N ) + 2 * dM1Sqr * dM1;
  dSkew  := dM3 * Power(dM2, -3/2);

  Result:= dSkew;

end;


function TRegComp.GetKurtosis (const AnArray   : array of extended;
                               const n         : integer): single;
var
  dM1: extended;
  dM2: extended;
  dM4: extended;
  dKurtosis: extended;
  dSum: extended;
  dSumSquares: extended;
  dSumCubes: extended;
  dSumQuads: extended;
  dOverN: extended;
  dAccum: extended;
  dM1Sqr: extended;
  dS2N: extended;
  dS3N: extended;
  I: Integer;
begin

  dOverN      := 1 / ( n + 1 );
  dSum        := 0;
  dSumSquares := 0;
  dSumCubes   := 0;
  dSumQuads   := 0;

  for I := 0 to n-1 do begin
    dSum         := dSum + AnArray[I];
    dAccum       := Sqr(AnArray[I]);
    dSumSquares  := dSumSquares + dAccum;
    dAccum       := dAccum * AnArray[I];
    dSumCubes    := dSumCubes + dAccum;
    dSumQuads    := dSumQuads + dAccum * AnArray[I];
  end;

  dM1         := dSum * dOverN;
  dM1Sqr      := Sqr(dM1);
  dS2N        := dSumSquares * dOverN;
  dS3N        := dSumCubes * dOverN;
  dM2         := dS2N - dM1Sqr;
  dM4         := ( dSumQuads * dOverN ) - ( dM1 * 4 * dS3N ) + ( dM1Sqr * 6 * dS2N - 3 * Sqr(dM1Sqr) );
  dKurtosis   := dM4 / Sqr(dM2);

  Result:= dKurtosis;

end;

*)
function TRegComp.GetTotalVariation (const VarVector : XVector;
                                     const k         : byte): extended;
var
  i: integer;
begin

  Result:= 0.0;

  for i:= 1 to k do
    Result:= Result + VarVector[i];

end;


// ======================== Univariate functions below: =======================
(*
function TRegComp.UnivariateSummary (const PRegressData : PTRegressDataArray;
                                     const k            : byte;
                                     const n            : integer): TUnivariateRec;
var
  i: byte;
  aUnivRec: TUnivariateRec;
begin

  aUnivRec:= InitUnivariateRec;

  if ( k in [1..MAXVARIABLES-1] ) and
     ( k < n ) and
     ( n > 0 ) and
     ( n < MAXOBSERVATIONS ) then

  begin
     try
        aUnivRec.k:= k;
        aUnivRec.n:= n;

        for i:= 1 to aUnivRec.k do begin
           aUnivRec.Sums[i]      := GetSum(PRegressData^[i],aUnivRec.n);
           aUnivRec.Mins[i]      := GetMin(PRegressData^[i],aUnivRec.n);
           aUnivRec.Maxs[i]      := GetMax(PRegressData^[i],aUnivRec.n);
           aUnivRec.Ranges[i]    := aUnivRec.Maxs[i]-aUnivRec.Mins[i];
           aUnivRec.Means[i]     := GetMean(PRegressData^[i],aUnivRec.n);
           aUnivRec.Variances[i] := GetVariance(PRegressData^[i],aUnivRec.n);
           aUnivRec.StdDevs[i]   := sqrt(aUnivRec.Variances[i]);
           aUnivRec.Skews[i]     := GetSkew(PRegressData^[i],aUnivRec.n);
           aUnivRec.Kurts[i]     := GetKurtosis(PRegressData^[i],aUnivRec.n);
           aUnivRec.CofVs[i]     := GetCoefficientofVariation(PRegressData^[i],aUnivRec.n);
           aUnivRec.Medians[i]   := GetMedian(PRegressData^[i],aUnivRec.n);
        end;

        Result:= aUnivRec;

     except
        Raise Exception.Create(UnivariateSummaryError);
     end;

  end;

end;


function TRegComp.YSummary (const Y : array of extended;
                            const n : integer): TVarSummaryRec;
var
  aYRec: TVarSummaryRec;
begin

  if ( n > 0 ) and ( n < MAXOBSERVATIONS ) then begin
     try
        aYRec.n:= n;

        begin
           aYRec.Sum      := GetSum(Y,n);
           aYRec.Min      := GetMin(Y,n);
           aYRec.Max      := GetMax(Y,n);
           aYRec.Range    := aYRec.Max-aYRec.Min;
           aYRec.Mean     := GetMean(Y,n);
           aYRec.Variance := GetVariance(Y,n);
           aYRec.StdDev   := sqrt(aYRec.Variance);
           aYRec.Skew     := GetSkew(Y,n);
           aYRec.Kurt     := GetKurtosis(Y,n);
           aYRec.CofV     := GetCoefficientofVariation(Y,n);
           aYRec.Median   := GetMedian(Y,n);
        end;
        Result:= aYRec;

     except
        Raise Exception.Create(UnivariateSummaryError);
     end;

  end;

end;
*)

// ================= Bivariate Regression utility routines below: ==============

function TRegComp.BVRegressionBeta1CI (const aBivarStatRec : TBivarStatRec): TIntervalRec;
var
  dSSE: extended;
  dS: extended;
  dTemp: extended;
  CInterval: TIntervalRec;
begin

  CInterval := InitInterval;
  Result    := CInterval;

  try
    with aBivarStatRec do begin
      dSSE := SSYY - Beta1 * SSXY;
      dS   := sqrt ( dSSE / ( n - 2 ) );
      dTemp:= TTableValue2 ( n - 2 ) * ( dS / sqrt ( SSXX ) );

      with CInterval do begin
        Upper:= Beta1 + dTemp;
        Lower:= Beta1 - dTemp;
      end;

    end;

    Result:= CInterval;

  except
    Raise Exception.Create(BVBeta1CIError);
  end;

end;


function TRegComp.BVRegressionCI (const aBivarStatRec : TBivarStatRec;
                                  const EstimatedY    : extended;
                                  const XValue        : extended): TIntervalRec;
var
  dSSE: extended;
  dS: extended;
  dTmp1: extended;
  dTmp2: extended;
  CInterval: TIntervalRec;
begin

  CInterval := InitInterval;
  Result    := CInterval;

  try
    with aBivarStatRec do begin
      dTmp1 := sqrt ( ( 1 / n ) + ( sqr ( XValue - XMean ) / SSXX ) );
      dSSE  := SSYY - Beta1 * SSXY;
      dS    := sqrt( dSSE / ( n-2 ) );
      dTmp2 := ( TTableValue2 ( n - 2 ) * dS * dTmp1 );

      with CInterval do begin
        Upper := EstimatedY + dTmp2;
        Lower := EstimatedY - dTmp2;
      end;
    end;

    Result:= CInterval;

  except
    Raise Exception.Create(BVRegCIError);
  end;

end;


function TRegcomp.BVRegressionPI (const aBivarStatRec : TBivarStatRec;
                                  const EstimatedY    : extended;
                                  const XValue        : extended): TIntervalRec;
var
  dSSE: extended;
  dS: extended;
  dTmp1: extended;
  dTmp2: extended;
  PInterval : TIntervalRec;
begin

  PInterval := InitInterval;
  Result    := PInterval;

  try
    with aBivarStatRec do begin
      dTmp1 := sqrt( 1 + ( 1 / n ) + ( sqr ( XValue - XMean ) / SSXX ) );
      dSSE  := SSYY - Beta1 * SSXY;
      dS    := sqrt( dSSE / ( n - 2 ) );
      dTmp2 := TTableValue2 ( n - 2 ) * dS * dTmp1;

      with PInterval do begin
        Upper:= EstimatedY + dTmp2;
        Lower:= EstimatedY - dTmp2;
      end;

    end;

    Result:= PInterval;

  except
    Raise Exception.Create(BVRegPIError);
  end;

end;


function TRegComp.BVRegressionTestStatistic (const aBivarStatRec: TBivarStatRec): extended;
var
  dSE : extended;
  dS  : extended;
begin

  Result := 0;

  try
    with aBivarStatRec do begin
      if ( SSXX > 0.0 ) and ( n > 2 ) and
        ( n < MAXOBSERVATIONS ) then begin
        dSE := SSYY - Beta1 * SSXY;
        dS  := sqrt ( dSE / ( n - 2 ) );
        Result:= Beta1 / ( dS / sqrt ( SSXX ) );
      end;
    end;

  except
    Raise Exception.Create(BVRegTestError);
  end;

end;


function TRegComp.BVCorrelationSampleTestStat (const PopCorrelation  : extended;
                                               const SmplCorrelation : extended;
                                               const n               : integer): extended;
var
  dPopTemp: extended;
  dSmplTemp: extended;
  dZStatVal: extended;
begin

  dZStatVal := 0.0;

  try

    if ( n > 3 ) then begin
      dPopTemp  := ONE_HALF * ( ln ( 1 + PopCorrelation ) / ( 1 - PopCorrelation ) );
      dSmplTemp := ONE_HALF * ( ln ( 1 + SmplCorrelation) / ( 1 - SmplCorrelation ) );
      dZStatVal := ( dSmplTemp - dPopTemp ) /  (1 / Sqrt ( n - 3 ) );
    end;

  except
    Raise Exception.Create(BVCorrelationSampleTestStatError);
  end;

  Result:= dZStatVal;

end;


function TRegComp.BVRegressionReverse (const aBivarStatRec : TBivarStatRec;
                                       const aYValue       : extended): extended;
begin

  Result := 0;

  try
    with aBivarStatRec do begin
      if (aBivarStatRec.n > 0) and (aBivarStatRec.SSXY > 0) then begin
        // Here, we are using a previously computed BivarStatRec to 'reconstruct'
        // the Slope and Intercept to calc the desired X from Y.
        Result:= Beta0 + ( Beta1 * aYValue );
      end;
    end;

  except
    Raise Exception.Create(BVRegReverseError);
  end;

end;


function TRegComp.BVRegression (const  X : array of extended;
                                const  Y : array of extended;
                                const  n : integer): TBivarStatRec;
const
  dTINY = 1e-20;
var
  Statistics: TBivarStatRec;
  i: integer;
begin
  Statistics   := InitBivarStatRec;      // Init stats
  Statistics.N := n;                     // N=size of input array
                                         // This part goes slow
  if ( Statistics.N > 2 ) then begin     // Compute sum's and sum's sqr's
    with Statistics do begin             // by looping through each obs
      df := n-2;
      for i:= 0 to N-1 do begin
        SumX  := SumX + X[i];
        SumY  := SumY + Y[i];
        SumXY := SumXY + ( X[i] * Y[i] );
        SumX2 := SumX2 + ( X[i] * X[i] );
        SumY2 := SumY2 + ( Y[i] * Y[i] );
      end;
    end;

    with Statistics do begin
      XMean := SumX / N;                       // Mean of X
      YMean := SumY / N;                       // Mean of Y
      SSXX  := SumX2 - ( SumX * SumX ) / N;
      SSYY  := SumY2 - ( SumY * SumY ) / N;
      SSXY  := SumXY - ( SumX * SumY ) / N;
      try
        r     := SSXY/sqrt ( SSXX * SSYY );    // 'r'
        Beta1 := SSXY/SSXX;                    // Slope(b1)
        Beta0 := YMean - ( Beta1 * XMean );    // Intercept(b0)
      except
//        Raise Exception.Create('Divide by zero calculating r, B1 or B0');
        DM.ERROR('Correlation undefined: Divide by zero calculating r, B1 or B0', [], 119002);

      end;
      z:= 0.5 * ln ( ( ( 1.0 + r ) + dTINY ) / ( ( 1.0 - r ) + dTINY ) );
      t:= r * sqrt( df / ( ( ( 1.0 - r ) + dTINY ) * ( ( 1.0 + r ) + dTINY ) ) );
    end;

  end;

  Result:= Statistics;                  // Return stats
                                        // If N > 2
end;


function TRegComp.BVCalcYValue (const BVRec  : TBivarStatRec;
                                const XValue : extended): extended;
begin
  Result:= BVRec.Beta0 + ( BVRec.Beta1 * XValue );
end;


procedure TRegComp.BVResiduals (const X      : array of extended;
                                const Y      : array of extended;
                                var   Resids : array of extended;
                                const n      : integer);
var
  Stats: TBivarStatRec;
  i: integer;
begin

  if ( n > 2 ) and ( n < MAXOBSERVATIONS ) then begin
    try
      Stats:= BVRegression(X,Y,n);

      with Stats do begin
        for i:= 0 to n-1 do
          Resids[i]:= Y[i] - ( Beta0 + ( X[i] * Beta1 ) );
      end;  // E[i] = YObs[i] - ( ExpectedY[i] )

    except
      Raise Exception.Create(BVResidualsError);
    end;

  end;

end;


procedure TRegComp.BVPredictedY (const X          : array of extended;
                                 const Y          : array of extended;
                                 var   PredictedY : array of extended;
                                 const n          : integer);
var
  Stats: TBivarStatRec;
  i: integer;
begin

  if ( n > 2 ) and ( n < MAXOBSERVATIONS ) then begin
    try
      Stats:= BVRegression(X,Y,n);

      with Stats do begin
        for i:= 0 to n-1 do
          PredictedY[i]:= Beta0 + ( X[i] * Beta1 );
      end;   // YCap[i] =  B0 + B1x[i]

    except
      Raise Exception.Create(BVPredictedError);
    end;

  end;

end;


function TRegComp.BVQuadraticRegression (const X : array of extended;
                                         const Y : array of extended;
                                         const n : integer): TMultStatRec;
var
  i: integer;
  TmpDataSet: PTRegressDataArray;
begin
  Result:= InitMultStatRec;

  New(TmpDataSet);

  try
    for i:= 0 to n-1 do begin
      TmpDataSet^[0,i]:= 1;
      TmpDataSet^[1,i]:= X[i];
      TmpDataSet^[2,i]:= X[i] * X[i]; // Create a new variable; in : 1 var;
    end;                              // but now, we have a 2 var (MV) case

    Result:= MVRegression(TmpDataSet,Y,2,n);

  finally
    Dispose(TmpDataSet);
  end;

end;


function TRegComp.MVRegressionBetaCI (const aMultStatRec  : TMultStatRec;
                                      const XCrossProdInv : XMatrix;
                                      const i             : byte): TIntervalRec;
var
  dTmp1: extended;
  dTmp2: extended;
  n: integer;
  k: integer;
  CInterval : TIntervalRec;
begin

  CInterval := InitInterval;
  Result    := CInterval;

  try
    if ( i > 0 ) and ( i < aMultStatRec.k ) then begin
      dTmp1 := aMultStatRec.Beta[i+1];
      n    := aMultStatRec.n;
      k    := aMultStatRec.k;
      dTmp2 := sqrt ( aMultStatRec.MSE ) * sqrt ( XCrossProdInv[i+1,i+1] );

      with CInterval do begin
        Upper:= dTmp1 + ( TTableValue2 ( n - ( k+1 ) ) * dTmp2 );
        Lower:= dTmp1 - ( TTableValue2 ( n - ( k+1 ) ) * dTmp2 );
      end;

    end;

    Result:= CInterval;

  except
    Raise Exception.Create(MVRegBetaCIError);
  end;

end;


// =============== Predictor Variable Matrix functions below: ================

function TRegComp.CorrelationMatrix (const PRegressData : PTRegressDataArray;
                                     const k            : byte;
                                     const n            : integer): XMatrix;
var
  i: integer;
  j: integer;
  CorMat: XMatrix;
  Stats: TBivarStatRec;
begin

  CorMat   := InitMatrix(MAXVARIABLES,0.0);
  Result   := CorMat;
  Stats    := InitBivarStatRec;

  if ( n > 2 ) and
     ( k < n ) and
     ( n < MAXOBSERVATIONS ) and
     ( k in [1..MAXVARIABLES-1] ) then begin
    // Note: 'k' is the number of dimensions (variables) of the input data.
    try
    for i:= 1 to k do
      for j:= 1 to k do begin
        if ( i > j ) then begin
          Stats:= BVRegression(PRegressData^[i],PRegressData^[j],n);
          CorMat[i,j]:= Stats.r;       // Grab 'r' value
          CorMat[j,i]:= Stats.r;       // Mirror value
        end;
      end;

      for i:= 1 to k do
        CorMat[i,i]:= 1.0;

    Result:= CorMat;                   // Return ACorrMatrix

    except
      Raise Exception.Create(CorrelationMatrixError);
    end;
  end;

end;


function TRegComp.CovarianceMatrix (const PRegressData : PTRegressDataArray;
                                    const k            : byte;
                                    const n            : integer): XMatrix;
var
  i: integer;
  j: integer;
  CovMat: XMatrix;
  CorMat: XMatrix;
  StdDevArr  : array [1..MAXVARIABLES+1] of extended;
begin

  CovMat:= InitMatrix(MAXVARIABLES,0.0);
  CorMat:= InitMatrix(MAXVARIABLES,0.0);
  Result:= CovMat;

  if ( n > 2 ) and
     ( k < n ) and
     ( n < MAXOBSERVATIONS ) and
     ( k in [1..MAXVARIABLES-1] ) then begin

    for i:= 1 to MAXVARIABLES+1 do
      StdDevArr[i]:= 0;

    CorMat:= CorrelationMatrix(PRegressData,k,n);

    for i:= 1 to k do
      StdDevArr[i]:= sqrt ( GetVariance ( PRegressData^[i],n ) );

    try
      for i:= 1 to k do
        for j:= 1 to k do begin
          if ( i >= j ) then begin          // do every unique pair of x/y calcs:
            CovMat[i,j]:= CorMat[i,j] * ( StdDevArr[i] * StdDevArr[j] );
            CovMat[j,i]:= CovMat[i,j];      // Mirror value
          end
        end;

    Result:= CovMat;                        // Return ACovMatrix

    except
      Raise Exception.Create(CovarianceMatrixError);
    end;
  end;

end;


function TRegComp.MeansDifferencesMatrix (const MeansVector : XVector;
                                          const k           : byte): XMatrix;
var
  i: integer;
  j: integer;
  MeansDMatrix: XMatrix;
begin

  MeansDMatrix := InitMatrix(MAXVARIABLES,0.0);
  Result       := MeansDMatrix;

  if ( k in [1..MAXVARIABLES-1] ) then begin
    try
      for i:= 1 to k do
        for j:= 1 to k do begin
          if ( i >= j ) then begin
            MeansDMatrix[i,j]:= MeansVector[i] - MeansVector[j];
            MeansDMatrix[j,i]:= MeansDMatrix[i,j];      // Mirror val.
          end
        end;

      Result:= MeansDMatrix;          // Return ACovMatrix

    except
      Raise Exception.Create(MeansDifferencesMatrixError);
    end;

  end;

end;


// ================== Variable (Array) Transformation Funcs below =============

procedure TRegComp.TransformDataCenter (var   PRegressData : PTRegressDataArray;
                                        const k            : byte;
                                        const n            : integer);
var
  i: integer;
begin

  for i:= 1 to k do
    TransformArrCenter(PRegressData^[i],n);

end;


procedure TRegComp.TransformDataStandardize (var   PRegressData : PTRegressDataArray;
                                             const k            : byte;
                                             const n            : integer);
var
  i: integer;
begin

  for i:= 1 to k do
    TransformArrStandardize (PRegressData^[i],n);

end;


procedure TRegComp.TransformDataNormalize (var   PRegressData : PTRegressDataArray;
                                           const k            : byte;
                                           const n            : integer);
var
  i: integer;
begin

  for i:= 1 to k do
    TransformArrNormalize(PRegressData^[i],n);

end;


procedure TRegComp.TransformArrStandardize (var X : array of extended;
                                          const n : integer);

var
  i: integer;
  dMean: extended;
  dSD: extended;
begin

  try
    if ( n > 2 ) and
       ( n < MAXOBSERVATIONS ) then begin
      dSD:= sqrt ( GetVariance ( X,n ) );
      dMean:= GetMean(X,n);

      if ( dSD > 0.0 ) then begin
        for i:= 0 to n-1 do
          X[i]:= ( X[i] - dMean ) / dSD;
      end
      else begin
        for i:= 0 to n-1 do          // loop through 0..number of obs
          X[i]:= 0.0;
      end;
    end;

  except
    Raise Exception.Create(StandardizeError);
  end;

end;


procedure TRegComp.TransformArrCenter (var X : array of extended;
                                     const n : integer);
var
  i: integer;
  dMean: extended;
begin

  try
    if ( n > 2 ) and
       ( n < MAXOBSERVATIONS ) then begin
      dMean:= GetMean(X,n);

      for i:= 0 to n-1 do          // loop through 0..number of obs
        X[i]:= ( X[i] - dMean );
    end;

  except
    Raise Exception.Create(StandardizeError);
  end;

end;


procedure TRegComp.TransformArrNormalize (var X : array of extended;
                                        const n : integer);
var
  dMax: extended;
  i: integer;
begin

  try
    dMax:= VERYSMALLNUMBER;

    if ( n > 0 ) and
       ( n < MAXOBSERVATIONS ) then begin

      for i:= 0 to n-1 do begin      // loop through 0..n-1
        if ( X[i] > dMax ) then
          dMax:= X[i];
      end;

      if ( dMax < -0.00001 ) or
         ( dMax >  0.00001 ) then begin // avoid div by 0
        for i:= 0 to n-1 do
          X[i]:= X[i] / dMax;
      end;
    end;

  except
    Raise Exception.Create(NormalizeError);
  end;

end;


procedure TRegComp.TransformArrInverse (var X  : array of extended;
                                      const n  : integer);

var
  i : integer;
begin

  try
    if ( n > 0 ) and
       ( n < MAXOBSERVATIONS ) then begin
      for i:= 0 to n-1 do begin
        if ( X[i] <> 0.000 ) then
          X[i]:= 1 / X[i];
      end
    end;

  except
    Raise Exception.Create(TransformInverseError);
  end;

end;


procedure TRegComp.TransformArrSquare (var X  : array of extended;
                                     const n  : integer);
var
  i: integer;
begin

  if ( n > 0 ) and
     ( n < MAXOBSERVATIONS ) then begin
    for i:= 0 to n-1 do
      X[i]:= sqr( X [i] );
  end;

end;


// Note: Negative and 0 values are set to 0.0
procedure TRegComp.TransformArrSquareRoot (var X  : array of extended;
                                         const n  : integer);
var
  i: integer;
begin

  try
    if ( n > 0 ) and
       ( n < MAXOBSERVATIONS ) then begin
      for i:= 0 to n-1 do begin
        if ( X[i] > 0.0 ) then
          X[i]:= sqrt ( X[i] )
        else
          X[i]:= 0;
      end;
    end;

  except
    Raise Exception.Create(TransformSquareRootError);
  end;

end;


procedure TRegComp.TransformArrAddScalar (var X           : array of extended;
                                        const aScalar     : extended;
                                        const n           : integer);
var
  i: integer;
begin

  for i:= 0 to n-1 do
    X[i]:= X[i] + aScalar;

end;


procedure TRegComp.TransformArrMultiplyScalar (var X           : array of extended;
                                             const aScalar     : extended;
                                             const n           : integer);
var
  i: integer;

begin

  for i:= 0 to n-1 do
    X[i]:= X[i] * aScalar;

end;


// ====================== Vector-utility routines below: =====================

function TRegComp.MeanVector (const PRegressData : PTRegressDataArray;
                              const k            : byte;
                              const n            : integer): XVector;
var
  i: integer;
  j: integer;
  dSum: extended;
  MeanVctr: XVector;
begin

  MeanVctr:= InitVector(MAXVARIABLES,0);

  if ( n > 2 ) and
     ( k < n ) and
     ( n < MAXOBSERVATIONS ) and
     ( k in [1..MAXVARIABLES-1] ) then begin

    for i:= 1 to k do begin
      dSum:= 0.0;

      for j:= 0 to n-1 do
        dSum:= dSum + PRegressData^[i,j];

      MeanVctr[i]:= dSum / n;
    end;

  end;

  Result:= MeanVctr;

end;


function TRegComp.VarianceVector (const PRegressData : PTRegressDataArray;
                                  const k            : byte;
                                  const n            : integer): XVector;
var
  i: byte;
  dVariance: extended;
  VarianceVctr: XVector;
begin

  VarianceVctr:= InitVector(MAXVARIABLES,0);

  try
    if ( n > 2 ) and
       ( k < n ) and
       ( n < MAXOBSERVATIONS ) and
       ( k in [1..MAXVARIABLES-1] ) then begin

      for i:= 1 to k do begin
        dVariance:= GetVariance(PRegressData^[i],n);
        VarianceVctr[i]:= dVariance;
      end;

    end;

  except
    Raise Exception.Create(VarianceVectorError);
  end;

  Result:= VarianceVctr;

end;


function TRegComp.VarianceProportionsVector (const PRegressData : PTRegressDataArray;
                                             const k            : byte;
                                             const n            : integer): XVector;
var
  i: byte;
  dSum: extended;
  VarianceVctr: XVector;
begin

  try
    VarianceVctr:= InitVector(MAXVARIABLES,0);
    if ( n > 2 ) and
       ( k < n ) and
       ( n < MAXOBSERVATIONS ) and
       ( k in [1..MAXVARIABLES-1] ) then begin
      VarianceVctr:= VarianceVector(PRegressData,k,n);
      dSum:= 0;

      for i:= 1 to k do
        dSum:= dSum + VarianceVctr[i];

      if (dSum > 0) then begin
        for i:= 1 to k do
          VarianceVctr[i]:= VarianceVctr[i] / dSum;
      end;

    end;

    Result:= VarianceVctr;

  except
    Raise Exception.Create(VarianceProportionsVectorError);
  end;

end;


function TRegComp.SumVector (const PRegressData : PTRegressDataArray;
                             const k            : byte;
                             const n            : integer): XVector;
var
  i: integer;
  j: integer;
  dSum: extended;
  SumVctr: XVector;
begin

  SumVctr:= InitVector(MAXVARIABLES,0);

  if ( n > 2 ) and
     ( k < n ) and
     ( n < MAXOBSERVATIONS ) and
     ( k in [1..MAXVARIABLES-1] ) then begin

    for i:= 1 to k do begin
      dSum:= 0.0;

      for j:= 0 to n-1 do
        dSum:= dSum + PRegressData^[i,j];

      SumVctr[i]:= dSum;
    end;

  end;

  Result:= SumVctr;

end;

(*
function TRegComp.MinVector (const PRegressData : PTRegressDataArray;
                             const k            : byte;
                             const n            : integer): XVector;
var
  i: integer;
  j: integer;
  dMin: extended;
  MinVctr: XVector;
begin

  MinVctr:= InitVector(MAXVARIABLES,0);

  if ( n > 2 ) and
     ( k < n ) and
     ( n < MAXOBSERVATIONS ) and
     ( k in [1..MAXVARIABLES-1] ) then begin

    for i:= 1 to k do begin
      dMin:= VERYLARGENUMBER;

      for j:= 0 to n-1 do begin
        if ( PRegressData^[i,j] < dMin ) then
          dMin := PRegressData^[i,j];
      end;

      MinVctr[i]:= dMin;

    end;

  end;

  Result:= MinVctr;

end;


function TRegComp.MaxVector (const PRegressData : PTRegressDataArray;
                             const k            : byte;
                             const n            : integer): XVector;
var
  i: integer;
  j: integer;
  dMax: extended;
  MaxVctr: XVector;
begin

  MaxVctr:= InitVector(MAXVARIABLES,0);

  if ( n > 2 ) and
     ( k < n ) and
     ( n < MAXOBSERVATIONS ) and
     ( k in [1..MAXVARIABLES-1] ) then begin

    for i:= 1 to k do begin
      dMax:= VERYSMALLNUMBER;

      for j:= 0 to n-1 do begin
        if (PRegressData^[i,j] > dMax) then
          dMax := PRegressData^[i,j];
      end;

      MaxVctr[i]:= dMax;

    end;

  end;

  Result:= MaxVctr;

end;
*)

function TRegComp.CrossProducts ( const PRegressData : PTRegressDataArray;
                                  const k            : byte;
                                  const n            : integer): XMatrix;
var
  i: integer;
  j: integer;
  cnt: integer;
  dSum: extended;
  XTemp: XMatrix;
begin
  // This function computes (X'X); where k= vars, n=obs;
  // XTemp is a k+1 x k+1 matrix

  // It is non-std in how it approaches the matrix
  // multiplication-so I'll describe it in detail below:

  { Goal: To compute X' * X: The Result is an k+1 by k+1
    matrix (so for example, if 2 variables are used in the
    regression (k=2) then, a 3x3 Results...(Note: the matrix
    subscript notation is defined as XTempInv[1..k+1,1..k+1]
    -the lowest subscript is based at 1, not 0.

        X'      *        X     =   (X'X) - a 3x3
    1  1  1  1        1  1  2       4  10  20
    1  2  3  4        1  2  4      10  30  60
    2  4  6  8        1  3  6      20  60 120
                      1  4  8

  I'll describe how I do the above by the each of 4
  steps below:

  Step 1. Compute the along each of the top left corner
  (since, the first column is the filler column if all 1's,
  it is possible just to sum the columns of [x,1] or [1,x]

                                   X11 X21 X31
           Compute these first:
           (then, mirror the value
           into X12, X13)

  Step 2. Compute the totals for the diagonal:

                                   X11
                                       X22
           Compute these next:             X33


  Step 3. Now, get all the others which lie elsewhere:
          These are the off-diagonal and not in the
          outer (top-left) edges...

           Compute these next:
           (mirror the value into          X32
           X23)

  Step 4. Finally, stick in 'n' the number of obs. for
          X11 := n.
  }

  XTemp:= InitMatrix(MAXVARIABLES,0);

  if ( n > 2 ) and ( k < n ) and
     ( n < MAXOBSERVATIONS ) and ( k in [1..MAXVARIABLES-1] ) then
  begin
    // compute the outer rows/cols of the matrix
    for i := 1 to k do begin             // Step 1.
      dSum := 0;
      for cnt := 0 to n-1 do
        dSum := dSum + PRegressData^[i,cnt];
      XTemp[1,i+1]:= dSum;
      XTemp[i+1,1]:= dSum;
    end;

    // Total up those elements on the diagonal
    // for j:= 0 to n-1 do
    //   XTemp[i+1,i+1] = RegressData[i,j] * RegressData[i,j]
    for i := 1 to k do begin              // Step 2.
      dSum := 0;
      for cnt := 0 to n-1 do
        dSum := dSum + (PRegressData^[i,cnt] * PRegressData^[i,cnt]);
      XTemp[i+1,i+1]:= dSum;
    end;

    // Now do those elements which are neither on the
    // diagonal nor along the edges of the matrix
    for i := 1 to k do begin                 // Step 3.
      for j := 1 to k do begin
        if ( j > i ) then begin
          dSum := 0;
          for cnt := 0 to n-1 do begin
            dSum := dSum + ( PRegressData^[i,cnt] * PRegressData^[j,cnt] );
          end;
          XTemp [i+1,j+1] := dSum;
          XTemp [j+1,i+1] := dSum;
        end;
      end;
    end;
                                      // Step 4.
    XTemp[1,1]:= n; // XTemp[1,1] (upper-left) = n
  end;

  Result:= XTemp;

end;


procedure TRegComp.MVLeverages ( const PRegressData  : PTRegressDataArray;
                                 var   LeverageVals  : array of extended;
                                 const k             : byte;
                                 const n             : integer);
type
  PRegressDataArray = ^TRegressDataArray;

// Computes X * (X'X)-1 * X' -the 'hat' matrix in the literature

var
  i: integer;
  j: integer;
  cnt: integer;
  dSum: extended;
  dDeterm: extended;
  XCrossProductsInv: XMatrix;
  TempData: PRegressDataArray;
begin

  InitResidArray(LeverageVals,0);

  if ( n > 2 ) and ( k < n ) and
     ( n < MAXOBSERVATIONS ) and
     ( k in [1..MAXVARIABLES-1]) then
  begin
    New(TempData);

    try
      // Find (X'X) from X ("RegressData") - an n by k+1 (rows by
      // columns) matrix of X's

      dDeterm := 0.0;

      // Find (X'X)-1 - a k+1 by k+1 square matrix Results

      XCrossProductsInv := InitMatrix(MAXVARIABLES,0);
      XCrossProductsInv := InvertMatrix (CrossProducts(PRegressData,k,n),k+1,dDeterm); // (X'X)

      // Remember, the k+1 is necessary to account for the column of 1's
      // in the column '0' spot

      // Index bases = 0   0
      // RegressData [Vars,Observations]

      // Compute: TempData = X * (X'X)-1

      // As The RegressData array 'X' is an n by k+1 matrix having many rows
      // and few columns and the (X'X)-1 is a square matrix of k+1 by k+1
      // ... the Result will be n by k+1. This fairly large array will be
      // needed to hold the 'intermediate' values which are used in the final
      // multiplication.

      for i:= 0 to n-1 do begin  // loop through rows 1..n
        for j:= 0 to k do begin  // check loop ranges in all this...
          dSum:= 0.0;
          for cnt:= 1 to k+1 do     // X    *   (X'X)-1
            dSum:= dSum + PRegressData^[cnt-1,i] * XCrossProductsInv[j+1,cnt];
          TempData^[j,i]:= dSum;    // Result is: X * (X'X)-1
        end;                        //            n x k+1
      end;

    //                                     TempData     *
    // Now, do the final multiplication, [X * (X'X)-1 ] * [X']
      for i:= 0 to n-1 do begin
        dSum:= 0;
        for j:= 0 to k do
          dSum:= dSum + TempData^[j,i] * PRegressData^[j,i];
        LeverageVals[i]:= dSum;
      end;

    finally
      Dispose(TempData);
    end;
  end;

end;


// ============ MVRegression and related functions are below: ================

function TRegComp.ComputeResults (const  PRegressData : PTRegressDataArray;
                                  const  Y            : array of extended;
                                  const  XTempInv     : XMatrix;
                                  const  k            : byte;
                                  const  n            : integer): TMultStatRec;
var
  i: integer;
  j: integer;
  dSum: extended;
  dSumY: extended;
  dMeanY: extended;
  dTmpYVal: extended;
  dTmpVal: extended;
  dYDiff: extended;
  TmpMat: XVector;
  MultStats: TMultStatRec;
begin

  MultStats    := InitMultStatRec;
  MultStats.n  := n;                        // Num of obs.
  MultStats.k  := k;
  dTmpYVal     := 0.0;
  dTmpVal      := 0.0;
  dSumY        := 0.0;
  TmpMat       := InitVector(MAXVARIABLES,0);

  for i:= 0 to n-1 do begin
    dTmpYVal := dTmpYVal + ( Y[i] * Y[i] ); // Y'Y
    dSumY    := dSumY + Y[i];               // Sum Y's
  end;

  dMeanY:= dSumY / n;                       // Y Mean

  with MultStats do begin
    for i := 0 to n-1 do begin              // compute SST
      dYDiff  := (Y[i] - dMeanY);
      SST     := SST + (dYDiff * dYDiff);
    end;
  end;

  for i := 0 to k do begin               // find X'Y, Result is a 1 x k+1
    dSum := 0;

    for j := 0 to n-1 do
      dSum:= dSum + ( PRegressData^[i,j] * Y[j] );

    TmpMat[i+1]:= dSum;

  end;

  with MultStats do begin
    for i := 1 to k+1 do begin  // find (X'X)-1 * (X'Y), Result is a 1 x k+1
      dSum := 0;

      for j := 1 to k+1 do
         dSum := dSum + ( XTempInv[i,j] * TmpMat[j] );

      Beta[i]:= dSum;                   // ß estimates; Result is a 1 x k+1
      dTmpVal:= dTmpVal + ( Beta[i] * TmpMat[i] );

    end;
  end;

  with MultStats do begin                           // do main calcs
    try
      DFModel := k;
      DFTotal := n-1;
      DFError := DFTotal - DFModel;
      SSE     := dTmpYVal - dTmpVal;                // Error Sum of Sqrs
      SSR     := SST - SSE;                         // Reg Sum of Sqrs
      R2      := SSR / SST;                         // Compute R-square
      R       := sqrt(R2);                          // Mult Regr Coef 'R'
      MSR     := SSR / DFModel;                     // Mean Sqr Regression
      MSE     := SSE / DFError;                     // MSE
      RMSE    := sqrt(MSE);                         // Root of MSE
      FStat   := (R2/k) / ((1 - R2)/(n - (k+1)));   // FStat.
      S2      := MSE;                               // MSE
      S       := sqrt(S2);                          // RMSE
    except
      Raise Exception.Create('Error computing regression coefficients');
    end;
  end;

  with MultStats do begin
    for i := 1 to k+1 do begin  // Compute std err, of beta param's
      SeBeta[i] := RMSE * (sqrt(XTempInv[i,i]));
      TValue[i] := Beta[i] / SeBeta[i];
    end;
  end;

  Result:= MultStats;           // Return Result

end;


procedure TRegComp.MVPrepare;
var
  i: integer;
begin

  for i:= 0 to MAXOBSERVATIONS-1 do
    PRegressData^[0,i]:= 1;

  Prepared:= TRUE;

end;


function TRegComp.MVRegression ( const PRegressData : PTRegressDataArray;
                                 const Y            : array of extended;
                                 const k            : byte;
                                 const n            : integer): TMultStatRec;
var
  dDeterm: extended;
  XCrossProdInv: XMatrix;
  bDataScanFlag: boolean;
begin

  Result:= InitMultStatRec;
  dDeterm:= 0.0;

  bDataScanFlag:= FALSE;

  if (ScanData) then
    bDataScanFlag:= MVCheckXData(PRegressData,k,n);

  if ( ( bDataScanFlag ) and ( ScanData ) ) OR
     NOT ( ScanData ) then begin
    XCrossProdInv := InitMatrix(MAXVARIABLES,0);

    if ( n > 2 ) and
       ( n > k ) and
       ( n < MAXOBSERVATIONS ) and
       ( k in [1..MAXVARIABLES-1] ) then begin
      XCrossProdInv := InvertMatrix (CrossProducts(PRegressData,k,n),k+1,dDeterm);
      Result        := ComputeResults (PRegressData,Y,XCrossProdInv,k,n);
    end

    else
      Raise Exception.Create('Number of vars must be > 0, and n > 2');
  end;

end;


function TRegComp.MVCalcYValue (const MVRec   : TMultStatRec;
                                const XValues : XVector): extended;
var
  i: integer;
  dTmp: extended;
begin

  dTmp:= 0;

  for i:= 2 to MVRec.k+1 do
    dTmp := dTmp + ( MVRec.Beta[i] * XValues[i-1] );

  dTmp:= dTmp + MVRec.Beta[1];           // Beta[1] is the intercept

  Result:= dTmp;

end;


procedure TRegComp.MVResiduals ( const PRegressData : PTRegressDataArray;
                                 const Y            : array of extended;
                                 var   Resids       : array of extended;
                                 const k            : byte;
                                 const n            : integer);
var
  i: integer;
  j: integer;
  dSum: extended;
  MultiStats: TMultStatRec;
begin

  InitResidArray(Resids,0.0);
  MultiStats:= InitMultStatRec;

  try
    if ( n > 2 ) and
       ( k < n ) and
       ( n < MAXOBSERVATIONS ) and
       ( k in [1..MAXVARIABLES-1] ) then
    begin
      MultiStats:= MVRegression(PRegressData,Y,k,n);

      with MultiStats do begin
        for i:= 0 to n-1 do begin
          dSum := 0;

          for j := 2 to k+1 do
            dSum := dSum + ( PRegressData^[j-1,i] * Beta[j] );

          Resids[i]:= Y[i] - ( dSum + Beta[1] );

        end;
      end;

    end;

  except
    Raise Exception.Create(MVResidualsError);
  end;

end;


procedure TRegComp.MVStdResiduals ( const PRegressData : PTRegressDataArray;
                                    const Y            : array of extended;
                                    var   Resids       : array of extended;
                                    const k            : byte;
                                    const n            : integer);
type
  PResidualsArray = ^TResidualsArray;
var
  i: integer;
  j: integer;
  dSum: extended;
  dTmpS: extended;
  iTmpN: integer;
  MultiStats: TMultStatRec;
  TmpArray: PResidualsArray;
begin

  InitResidArray (Resids,0.0);
  MultiStats:= InitMultStatRec;
  New(TmpArray);

  try
     try
        if ( n > 2 ) and
           ( k < n ) and
           ( n < MAXOBSERVATIONS ) and
           ( k in [1..MAXVARIABLES-1] ) then

        begin

           for i:= 0 to n-1 do
              TmpArray^[i]:= 0.0;

           MultiStats  := MVRegression(PRegressData,Y,k,n);
           iTmpN       := ( MultiStats.n - MultiStats.k - 1 );
           dTmpS       := sqrt ( MultiStats.SSE / iTmpN );

           with MultiStats do begin
              for i:= 0 to n-1 do begin
                 dSum := 0;

                 for j := 2 to k+1 do
                    dSum := dSum + ( PRegressData^[j-1,i] * Beta[j] );

                 TmpArray^[i]:= Y[i] - ( dSum + Beta[1] );
                 Resids[i]:= TmpArray^[i] / dTmpS;
              end;
           end;

        end;

     except
        Raise Exception.Create(MVStdResidualsError);
     end;
  finally
     Dispose(TmpArray);
  end;

end;


procedure TRegComp.MVStudentResiduals ( const PRegressData : PTRegressDataArray;
                                        const Y            : array of extended;
                                        var   Resids       : array of extended;
                                        const k            : byte;
                                        const n            : integer);
var
  i: integer;
  HatMatrix: TResidualsArray;
  TmpArray: TResidualsArray;
begin

  InitResidArray(TmpArray,0);
  InitResidArray(HatMatrix,0);
  InitResidArray(Resids,0);

  try
    if ( n > 2 ) and
       ( k < n ) and
       ( n < MAXOBSERVATIONS ) and
       ( k in [1..MAXVARIABLES-1] ) then

    begin

      MVLeverages(PRegressData,HatMatrix,k,n);
      MVStdResiduals(PRegressData,Y,TmpArray,k,n);

      for i:= 0 to n-1 do
        Resids[i]:= TmpArray[i] / ( sqrt(1 - HatMatrix[i] ) );

    end;

  except
    Raise Exception.Create(MVStudentResidualsError);
  end;

end;


procedure TRegComp.MVPredictedY (const PRegressData  : PTRegressDataArray;
                                 const Y             : array of extended;
                                 var   PredictedY    : array of extended;
                                 const k             : byte;
                                 const n             : integer);
var
  i: integer;
  j: integer;
  dSum: extended;
  MultiStats: TMultStatRec;
begin

  InitResidArray(PredictedY,0);
  MultiStats:= InitMultStatRec;

  try
    if ( n > 2 ) and
       ( k < n ) and
       ( n < MAXOBSERVATIONS ) and
       ( k in [1..MAXVARIABLES-1] ) then
    begin

      MultiStats:= MVRegression(PRegressData,Y,k,n);

      with MultiStats do begin
        for i:= 0 to n-1 do begin
          dSum := 0;

          for j := 2 to k+1 do
            dSum := dSum + ( PRegressData^[j-1,i] * Beta[j] );

          PredictedY[i]:= ( dSum + Beta[1] );  // Beta[1] is the intercept
        end;                                   // b0 in the nomenclature
      end;

    end;

  except
    Raise Exception.Create(MVPredictedError);
  end;

end;


procedure TRegComp.MVCooksDistance ( const PRegressData : PTRegressDataArray;
                                     const Y            : array of extended;
                                     var   CooksD       : array of extended;
                                     const k            : byte;
                                     const n            : integer);
type
  PResidualsArray = ^TResidualsArray;
var
  i: integer;
  j: integer;
  dSum: extended;
  dRMS: extended;
  HatValues: TResidualsArray;
  Resids: PResidualsArray;
  MultiStats: TMultStatRec;
begin

  InitResidArray(CooksD,0);
  InitResidArray(HatValues,0);
  MultiStats := InitMultStatRec;
  New(Resids);

  try
     try
     if ( n > 2 ) and
         ( k < n ) and
         ( n < MAXOBSERVATIONS ) and
         ( k in [1..MAXVARIABLES-1] ) then
        begin

           for i:= 0 to n-1 do
              Resids^[i]:= 0.0;

           MultiStats:= MVRegression(PRegressData,Y,k,n);
           MVLeverages(PRegressData,HatValues,k,n);    // Calc [X * (X'X)-1] * X'

           with MultiStats do begin
              dRMS:= SSE / ( n - k - 1 );

              for i:= 0 to n-1 do begin
                 dSum := 0;
                 for j := 2 to k+1 do
                    dSum := dSum + ( PRegressData^[j-1,i] * Beta[j] );
                 Resids^[i]:= Y[i] - ( dSum + Beta[1] );     // 'raw' resid e[i]
              end;

              for i:= 0 to n-1 do
                 CooksD[i]:= ( ( Resids^[i] * Resids^[i] ) * HatValues[i] ) /
                      ( ( k + 1 ) * dRMS * ( sqr ( 1 - HatValues[i ] ) ) );
           end;

        end;

     except
        Raise Exception.Create(CooksDError);
     end;
  finally
    Dispose(Resids);
  end;

end;


procedure TRegComp.MVPredictedYCIUpper (const PRegressData : PTRegressDataArray;
                                        const Y            : array of extended;
                                        var   CIUpper      : array of extended;
                                        const k            : byte;
                                        const n            : integer);
type
  PResidualsArray = ^TResidualsArray;
var
  i: integer;
  j: integer;
  dSum: extended;
  dDet: extended;
  dTmpnum: extended;
  dTTableValue: extended;
  dTmpResult: extended;
  XDataVector: XVector;
  TmpVector: XVector;
  XCrossProd: XMatrix;
  XCrossProdInv: XMatrix;
  MultStats: TMultStatRec;
  PredictedY: PResidualsArray;
begin

  InitResidArray(CIUpper,0.0);
  XCrossProd    := InitMatrix(MAXVARIABLES,0.0);
  XCrossProdInv := InitMatrix(MAXVARIABLES,0.0);
  TmpVector     := InitVector(MAXVARIABLES,0);
  XDataVector   := InitVector(MAXVARIABLES,0);
  dDet          := 0.0;
  MultStats     := InitMultStatRec;
  New(PredictedY);

  try
     try
     if ( k in [1..MAXVARIABLES-1] ) and
       ( n > 2 ) and
       ( k < n ) and
       ( n < MAXOBSERVATIONS ) then
        begin
           MultStats:= MVRegression(PRegressData,Y,k,n);
           with MultStats do begin
              for i:= 0 to n-1 do begin
                 dSum := 0;
                 for j := 2 to k+1 do
                    dSum := dSum + ( PRegressData^[j-1,i] * Beta[j] );
                 PredictedY^[i]:= ( dSum + Beta[1] );   // Beta[1] is the intercept
              end;                                      // b0 in the nomenclature
           end;

           XCrossProd    := CrossProducts(PRegressData,MultStats.k,MultStats.n);
           XCrossProdInv := InvertMatrix (XCrossProd,MultStats.k+1,dDet);
           dTTableValue  := TTableValue2 ( MultStats.n - ( MultStats.k+1 ) );

           with MultStats do begin
              for i:= 0 to n-1 do begin
                 for j:= 1 to k+1 do
                    XDataVector[j]:= PRegressData^[j-1,i];
                 TmpVector  := MultiplyVectorByMatrix(XCrossProdInv,XDataVector,k);
                 dTmpResult := MultiplyVectorByVector(TmpVector,XDataVector,k);
                 dTmpNum    := sqrt(MSE) * sqrt(dTmpResult);
                 CIUpper[i] := PredictedY^[i] + ( dTTableValue * dTmpNum );
              end;
           end;
        end;

     except
        Raise Exception.Create(MVRegCIError);
     end;
  finally
     Dispose(PredictedY);
  end;

end;


procedure TRegComp.MVPredictedYCILower (const PRegressData : PTRegressDataArray;
                                        const Y            : array of extended;
                                        var   CILower      : array of extended;
                                        const k            : byte;
                                        const n            : integer);
type
  PResidualsArray = ^TResidualsArray;
var
  i: integer;
  j: integer;
  dSum: extended;
  dDet: extended;
  dTmpnum: extended;
  dTTableValue: extended;
  dTmpResult: extended;
  XDataVector: XVector;
  TmpVector: XVector;
  XCrossProd: XMatrix;
  XCrossProdInv: XMatrix;
  MultStats: TMultStatRec;
  PredictedY: PResidualsArray;
begin

  InitResidArray(CILower,0.0);
  XCrossProd    := InitMatrix(MAXVARIABLES,0.0);
  XCrossProdInv := InitMatrix(MAXVARIABLES,0.0);
  TmpVector     := InitVector(MAXVARIABLES,0);
  XDataVector   := InitVector(MAXVARIABLES,0);
  dDet          := 0.0;
  MultStats     := InitMultStatRec;
  New(PredictedY);

  try
     try
     if ( k in [1..MAXVARIABLES-1] ) and
       ( n > 2 ) and
       ( k < n ) and
       ( n < MAXOBSERVATIONS ) then
        begin
           MultStats:= MVRegression(PRegressData,Y,k,n);
           with MultStats do begin
              for i:= 0 to n-1 do begin
                 dSum := 0;
                 for j := 2 to k+1 do
                    dSum := dSum + ( PRegressData^[j-1,i] * Beta[j] );
                 PredictedY^[i]:= ( dSum + Beta[1] ); // Beta[1] is the intercept
              end;                                   // b0 in the nomenclature
           end;

           XCrossProd    := CrossProducts(PRegressData,MultStats.k,MultStats.n);
           XCrossProdInv := InvertMatrix (XCrossProd,MultStats.k+1,dDet);
           dTTableValue  := TTableValue2 ( MultStats.n - ( MultStats.k+1 ) );

           with MultStats do begin
              for i:= 0 to n-1 do begin
                 for j:= 1 to k+1 do
                    XDataVector[j]:= PRegressData^[j-1,i];
                 TmpVector  := MultiplyVectorByMatrix(XCrossProdInv,XDataVector,k);
                 dTmpResult := MultiplyVectorByVector(TmpVector,XDataVector,k);
                 dTmpNum    := sqrt(MSE) * sqrt(dTmpResult);
                 CILower[i] := PredictedY^[i] - ( dTTableValue * dTmpNum );
              end;
           end;

        end;

     except
        Raise Exception.Create(MVRegCIError);
     end;
  finally
     Dispose(PredictedY);
  end;

end;


procedure TRegComp.MVPredictedYPIUpper (const PRegressData : PTRegressDataArray;
                                        const Y            : array of extended;
                                        var   PIUpper      : array of extended;
                                        const k            : byte;
                                        const n            : integer);
type
  PResidualsArray = ^TResidualsArray;
var
  i: integer;
  j: integer;
  dSum: extended;
  dDet: extended;
  dTmpnum: extended;
  dTTableValue: extended;
  dTmpResult: extended;
  XDataVector: XVector;
  TmpVector: XVector;
  XCrossProd: XMatrix;
  XCrossProdInv: XMatrix;
  MultStats: TMultStatRec;
  PredictedY: PResidualsArray;
begin

  InitResidArray(PIUpper,0.0);
  dDet          := 0.0;
  XCrossProd    := InitMatrix(MAXVARIABLES,0.0);
  XCrossProdInv := InitMatrix(MAXVARIABLES,0.0);
  XDataVector   := InitVector(MAXVARIABLES,0);
  TmpVector     := InitVector(MAXVARIABLES,0);
  MultStats     := InitMultStatRec;
  New(PredictedY);

  try
     try
     if ( k in [1..MAXVARIABLES-1] ) and
       ( n > 2 ) and
       ( k < n ) and
       ( n < MAXOBSERVATIONS ) then
        begin
           MultStats:= MVRegression(PRegressData,Y,k,n);
           with MultStats do begin
              for i:= 0 to n-1 do begin
                 dSum := 0;
                 for j := 2 to k+1 do
                    dSum := dSum + ( PRegressData^[j-1,i] * Beta[j] );
                 PredictedY^[i]:= ( dSum + Beta[1] ); // Beta[1] is the intercept
              end;                                   // b0 in the nomenclature
           end;

           XCrossProd    := CrossProducts(PRegressData,MultStats.k,MultStats.n);
           XCrossProdInv := InvertMatrix (XCrossProd,MultStats.k+1,dDet);
           dTTableValue  := TTableValue2 ( MultStats.n - ( MultStats.k+1 ) );

           with MultStats do begin
              for i:= 0 to n-1 do begin
                 for j:= 1 to k+1 do
                    XDataVector[j]:= PRegressData^[j-1,i];
                 TmpVector  := MultiplyVectorByMatrix(XCrossProdInv,XDataVector,k);
                 dTmpResult := MultiplyVectorByVector(TmpVector,XDataVector,k);
                 dTmpNum    := sqrt(MSE) * sqrt(dTmpResult+1);
                 PIUpper[i] := PredictedY^[i] + (dTTableValue * dTmpNum);
              end;
           end;

        end;

     except
        Raise Exception.Create(MVRegCIError);
     end;
  finally
     Dispose(PredictedY);
  end;

end;



procedure TRegComp.MVPredictedYPILower (const PRegressData : PTRegressDataArray;
                                        const Y            : array of extended;
                                        var   PILower      : array of extended;
                                        const k            : byte;
                                        const n            : integer);
type
  PResidualsArray = ^TResidualsArray;
var
  i: integer;
  j: integer;
  dSum: extended;
  dDet: extended;
  dTmpnum: extended;
  dTTableValue: extended;
  dTmpResult: extended;
  XDataVector: XVector;
  TmpVector: XVector;
  XCrossProd: XMatrix;
  XCrossProdInv: XMatrix;
  MultStats: TMultStatRec;
  PredictedY: PResidualsArray;
begin

  InitResidArray(PILower,0.0);
  dDet          := 0.0;
  XCrossProd    := InitMatrix(MAXVARIABLES,0.0);
  XCrossProdInv := InitMatrix(MAXVARIABLES,0.0);
  XDataVector   := InitVector(MAXVARIABLES,0);
  TmpVector     := InitVector(MAXVARIABLES,0);
  MultStats     := InitMultStatRec;
  New(PredictedY);

  try
     try
     if ( k in [1..MAXVARIABLES-1] ) and
       ( n > 2 ) and
       ( k < n ) and
       ( n < MAXOBSERVATIONS ) then
        begin
           MultStats:= MVRegression(PRegressData,Y,k,n);
           with MultStats do begin
              for i:= 0 to n-1 do begin
                 dSum := 0;
                 for j := 2 to k+1 do
                    dSum := dSum + ( PRegressData^[j-1,i] * Beta[j] );
                 PredictedY^[i]:= ( dSum + Beta[1] ); // Beta[1] is the intercept
              end;                                   // b0 in the nomenclature
           end;

           XCrossProd    := CrossProducts(PRegressData,MultStats.k,MultStats.n);
           XCrossProdInv := InvertMatrix (XCrossProd,MultStats.k+1,dDet);
           dTTableValue  := TTableValue2 ( MultStats.n - ( MultStats.k+1 ) );

           with MultStats do begin
              for i:= 0 to n-1 do begin
                 for j:= 1 to k+1 do
                    XDataVector[j]:= PRegressData^[j-1,i];
                 TmpVector  := MultiplyVectorByMatrix(XCrossProdInv,XDataVector,k);
                 dTmpResult := MultiplyVectorByVector(TmpVector,XDataVector,k);
                 dTmpNum    := sqrt(MSE) * sqrt(dTmpResult+1);
                 PILower[i] := PredictedY^[i] - (dTTableValue * dTmpNum);
              end;
           end;

        end;

     except
        Raise Exception.Create(MVRegCIError);
     end;
  finally
     Dispose(PredictedY);
  end;

end;


function TRegComp.DurbinWatson (const PRegressData : PTRegressDataArray;
                                const Y            : array of extended;
                                const k            : byte;
                                const n            : integer): extended;
var
  i: integer;
  dNumerator: extended;
  dDenominator: extended;
  Resids: TResidualsArray;
begin

  Result:= 0.0;
  dNumerator:= 0;
  dDenominator:= 0;

  try
    if (k in [1..MAXVARIABLES-1]) and (n > 2) and
       (k < n) and (n < MAXOBSERVATIONS) then begin
      MVResiduals (PRegressData,Y,Resids,k,n);

      for i:= 0 to n-1 do
        dDenominator := dDenominator + ( Resids[i] * Resids[i] );
      for i:= 1 to n-1 do
        dNumerator   := dNumerator + sqr( Resids[i] - Resids[i-1] );

      if (dDenominator <> 0.0) then
        Result:= dNumerator / dDenominator;
    end;

  except
    Raise Exception.Create(DurbinWatsonError);
  end;

end;


// ============================ Misc. routines ===============================

function TRegComp.MVPairedXYRegressions (const PRegressData : PTRegressDataArray;
                                         const Y            : array of extended;
                                         const k            : byte;
                                         const n            : integer): XVector;
var
  i: integer;
  BVRec: TBivarStatRec;
  CorrelationVector: XVector;
begin

  CorrelationVector := InitVector(MAXVARIABLES,0);
  Result            := CorrelationVector;
  BVRec             := InitBivarStatRec;

  try
    if ( k in [1..MAXVARIABLES-1] ) and
       ( n > 2 ) and
       ( k < n ) and
       ( n < MAXOBSERVATIONS ) then begin
      for i:= 1 to k do begin
        BVRec:= BVRegression(PRegressData^[i],Y,n);
        CorrelationVector[i]:= BVRec.r;
      end;
    end;

  except
    Raise Exception.Create('Error in MVPairedXYRegressios');
  end;

  Result:= CorrelationVector;

end;


function TRegComp.MVAverageCorrelation (const PRegressData : PTRegressDataArray;
                                        const Y            : array of extended;
                                        const k            : byte;
                                        const n            : integer): extended;
var
  i: integer;
  dSum: extended;
  BVRec: TBivarStatRec;
begin

  BVRec           := InitBivarStatRec;
  dSum            := 0.0;
  Result          := 0;

  if ( k in [1..MAXVARIABLES-1] ) and
     ( n > 2 ) and
     ( k < n ) and
     ( n < MAXOBSERVATIONS ) then begin
        for i:= 1 to k do begin
           BVRec:= BVRegression(PRegressData^[i],Y,n);
           dSum:= dSum + BVRec.r;
        end;

        Result:= dSum / k;
     end;

end;


// ============ Stepwise Regression functions are below: ======================


function TRegComp.MVStepwiseForward ( const PRegressData : PTRegressDataArray;
                                      const Y            : array of extended;
                                      const k            : byte;
                                      const n            : integer;
                                      const TitleString  : string): String;

{ Does a vars-added-one-at-a-time series of regression from 1..k:

  Calc Reg w/1 > RegMemo
  Calc Reg w/1 2 > RegMemo
  :
  1 2 3
  1 2 3 4
  1 2 3 4 5
  1 2 3 4 5 6
  1 2 3 4 5 6 7
  1 2 3 4 5 6 7 8 }
var
  i: integer;
  sTmp: string;
  sOut: string;
  dCp: extended;
  dMSEp: extended;
  dFp: extended;
  dFullModelR2: extended;
  dFullModelMSE: extended;
  MVRec: TMultStatRec;
begin

  sOut:= '';

  if ( k in [1..MAXVARIABLES-1] ) and
     ( n > k ) and
     ( n < MAXOBSERVATIONS ) then begin
    MVRec:= MVRegression(PRegressData,Y,k,n);
    dFullModelMSE   := MVRec.MSE;
    dFullModelR2    := MVRec.R2;
    sTmp            := TitleString + CRLF;

    for i:= 1 to k do begin
      MVRec := InitMultStatRec;
      sTmp  := '';
      MVRec := MVRegression(PRegressData,Y,i,n);
      dCp   := CalcCp(MVRec.SSE,dFullModelMSE,i,n);
      dFp   := CalcFp(MVRec.R2,dFullModelR2,k,i,n);
      dMSEp := CalcMSEp(MVRec.SSE,i,n);
      sTmp  := MVRegressionMemo(MVRec);
      sTmp  := sTmp + 'Mallow''s C: '  + Format(' %9.3f ',[dCp]) +
                       '  F Stat(p) : ' + Format(' %9.3f ',[dFp]) +
                       '  MSE(p) : '    + Format(' %9.3f ',[dMSEp]) +
                       CRLF + CRLF + CRLF;
      sOut:= sOut + Format('%s',[sTmp]);
    end;
    sOut:= sOut + CRLF;
  end;

  Result:= sOut;

end;


function TRegComp.MVStepwiseBackward ( const PRegressData : PTRegressDataArray;
                                       const Y            : array of extended;
                                       const k            : byte;
                                       const n            : integer;
                                       const TitleString  : string): String;

var
  i: integer;
  sTmp: string;
  sOut: string;
  dCp: extended;
  dMSEp: extended;
  dFp: extended;
  dFullModelR2: extended;
  dFullModelMSE: extended;
  MVRec: TMultStatRec;
begin

  sOut:= '';

  if (k in [1..MAXVARIABLES-1]) and
     (n > k) and
     (n < MAXOBSERVATIONS) then begin
    MVRec:= MVRegression(PRegressData,Y,k,n);
    dFullModelMSE   := MVRec.MSE;
    dFullModelR2    := MVRec.R2;
    sTmp            := TitleString + CRLF;

    for i:= k downto 1 do begin
      MVRec := InitMultStatRec;
      sTmp  := '';
      MVRec := MVRegression(PRegressData,Y,i,n);
      dCp   := CalcCp(MVRec.SSE,dFullModelMSE,i,n);
      dFp   := CalcFp(MVRec.R2,dFullModelR2,k,i,n);
      dMSEp := CalcMSEp(MVRec.SSE,i,n);
      sTmp  := MVRegressionMemo(MVRec);
      sTmp  := sTmp + 'Mallow''s C: '  + Format(' %9.3f ',[dCp]) +
                      '  F Stat(p) : ' + Format(' %9.3f ',[dFp]) +
                      '  MSE(p) : '    + Format(' %9.3f ',[dMSEp]) +
                      CRLF + CRLF + CRLF;
      sOut:= sOut + Format('%s',[sTmp]);
    end;
    sOut:= sOut + CRLF;
  end;

  Result:= sOut;
end;


// ================== Misc. Regression Functions are below: ====================

function TRegComp.MVInteractionRegression (const PRegressData : PTRegressDataArray;
                                           const Y            : array of extended;
                                           const k            : byte;
                                           const n            : integer;
                                           const Var1         : byte;
                                           const Var2         : byte): TMultStatRec;
var
  i: integer;
  TmpArr: TResidualsArray;
  MVStats: TMultStatRec;
begin

  MVStats:= InitMultStatRec;

  try
    if ( k in [1..MAXVARIABLES-2] ) and
       ( n > 2 ) and
       ( n < MAXOBSERVATIONS ) and
       ( k < n ) and
       ( Var1 in [1..k] ) and
       ( Var2 in [1..k] ) then

    begin
      // Multiplies PRegressData^[v1,i]*PRegressData^[v2,i] and puts the new
      // temp array of values in the array column k+1, then it does the
      // regression routine
      for i:= 0 to n-1 do
        TmpArr[i]:= PRegressData^[k+1,i];
      for i:= 0 to n-1 do
        PRegressData^[k+1,i]:= PRegressData^[Var1,i] * PRegressData^[Var2,i];
      MVStats:= MVRegression(PRegressData,Y,k+1,n);

      for i:= 0 to n-1 do
        PRegressData^[k+1,i]:= TmpArr[i];
    end;

    Result:= MVStats;

  except
    Raise Exception.Create(MVResidualsError);
  end;

end;


function TRegComp.MVPolynomialRegression (const PRegressData : PTRegressDataArray;
                                          const Y            : array of extended;
                                          const k            : byte;
                                          const n            : integer;
                                          const Var1         : byte): TMultStatRec;
var
  i: integer;
  MVStats: TMultStatRec;
  TmpArr: TResidualsArray;
begin

  MVStats:= InitMultStatRec;

  try
    if ( k in [1..MAXVARIABLES-2] ) and
       ( n < MAXOBSERVATIONS ) and
       ( n > 2 ) and
       ( k < n ) and
       ( Var1 in [1..k] ) then
    begin
      // Multiplies PRegressData^[v1,i]*PRegressData^[v1,i] and puts the new temp
      // array of values in the array column k+1, then it does the regression routine
      for i:= 0 to n-1 do
        TmpArr[i]:= PRegressData^[k+1,i];
      for i:= 0 to n-1 do
        PRegressData^[k+1,i]:= PRegressData^[Var1,i] * PRegressData^[Var1,i];
      MVStats:= MVRegression(PRegressData,Y,k+1,n);

      for i:= 0 to n-1 do
        PRegressData^[k+1,i]:= TmpArr[i];
    end;

    Result:= MVStats;

  except
    Raise Exception.Create(MVResidualsError);
  end;

end;


procedure TRegComp.MVRemoveVariable (var   PRegressData : PTRegressDataArray;
                                     var   k            : byte;
                                     const n            : integer;
                                     const VarNumber    : byte);
var
  i: integer;
  j: integer;
begin

  try
    if ( k in [1..MAXVARIABLES-1] ) and
       ( n < MAXOBSERVATIONS ) and
       ( n > 2 ) and
       ( k < n ) and
       ( VarNumber in [1..k] ) then

    begin
      if (VarNumber = k) then begin
        for i:= 0 to n-1 do
          PRegressData^[k,i]:= 0.0;
      end
      else begin
        for j:= VarNumber to k-1 do
          for i:= 0 to n-1 do
            PRegressData^[j,i]:= PRegressData^[j+1,i];
      end;
      k:= k - 1;
      MultStatRec.k:= MultStatRec.k-1;
    end;

  except
    Raise Exception.Create(RemoveVariableError);
  end;

end;


function TRegComp.DummyXYRegression (const X           : array of extended;
                                     const Y           : array of extended;
                                     const BooleanVar  : array of extended;
                                     const n           : integer;
                                     const TitleString : string): string;
var
  i: integer;
  sOutStr: string;
  iNonZeroCnt: integer;
  MVRec: TMultStatRec;
  TempDataSet: PTRegressDataArray;
begin

  MVRec       := InitMultStatRec;
  sOutStr     := '';
  Result      := '';
  iNonZeroCnt := 0;
  New(TempDataSet);  // Allocate & create a new "container" data array-2Mb !

  try
    for i:= 0 to n-1 do begin
      TempDataSet^[0,i]:= 1.0;                 // Place holder '1's
      TempDataSet^[1,i]:= X[i];                // the X values
      TempDataSet^[2,i]:= BooleanVar[i];       // The boolean var values
      TempDataSet^[3,i]:= BooleanVar[i]*X[i];  // The X values* boolean val [0 or 1]

      // It is possible to use a value other than 0.0 to stratify
      // the input 'flagging' array used to determine the two groups
      // of data points to compare
      if (BooleanVar[i] > 0.0) then
        inc(iNonZeroCnt);
    end;
    MVRec := MVRegression(TempDataSet,Y,3,n);

    sOutStr:= TitleString + CRLF + CRLF;
    sOutStr:= sOutStr + 'Section 1: Parameters for stratification value = 0' + CRLF;
    sOutStr:= sOutStr + Format(' ß0:%10.4f  ß1:%10.4f ',[ MVRec.Beta[1],MVRec.Beta[2] ] ) + CRLF;
    sOutStr:= sOutStr + 'Section 2: Parameters for stratification value > 0' + CRLF;
    sOutStr:= sOutStr + Format(' ß0:%10.4f  ß1:%10.4f ',[ MVRec.Beta[3],MVRec.Beta[4] ] ) + CRLF;

    sOutStr:= sOutStr + CRLF;
    sOutStr:= sOutStr + Format('Total Observations: %5d',[n]) + CRLF;
    sOutStr:= sOutStr + Format('Non-zero obs. (strat. var.): %5d',[iNonZeroCnt]) + CRLF;

  finally
    Dispose(TempDataSet);
  end;

  Result:= sOutStr;

end;


// ========================= Distance & related Functions ===================

function TRegComp.MakePiped (const Mean   : extended;
                             const StdDev : extended;
                             const Scalar : extended): TIntervalRec;
var
  Interval: TIntervalRec;
begin

  Interval.Lower := Mean - ( StdDev * Scalar );
  Interval.Upper := Mean + ( StdDev * Scalar );

  Result:= Interval;

end;


function TRegComp.GetLDistance (const Vector     : XVector;
                                const MeanVector : XVector;
                                const k          : byte): extended;

// Euclidean Dist: LD = (Sum of (X[i] - M(i)))
var
  i: byte;
  dSum: extended;
  dTemp: extended;
begin

  Result:= 0;

  if ( k in [1..MAXVARIABLES-1] ) then begin
    try
      dSum:= 0.0;
      for i:= 1 to k do begin
        dTemp:= Vector[i] - MeanVector[i];
        dSum:= dSum + dTemp;
      end;
      Result:= dSum;

    except
      Raise Exception.Create(LDistError);
    end;

  end;

end;


procedure TRegComp.LDistances (const PRegressData : PTRegressDataArray;
                               var   LDists       : TResidualsArray;
                               const n            : integer;
                               const k            : byte);

// LDist: LD = (Sum of (X[i] - M(i)))
var
  i: byte;
  iCnt: integer;
  dSum: extended;
  Vctr: XVector;
  MeanVctr: XVector;
begin

  try
    if ( k in [1..MAXVARIABLES-1] ) and
       ( k < n ) and
       ( n < MAXOBSERVATIONS ) then begin
      MeanVctr  := MeanVector(PRegressData,k,n);
      for iCnt := 0 to n-1 do begin
        for i:= 1 to k do
          Vctr[i]:= PRegressData^[i,iCnt];
        dSum:= 0.0;
        for i:= 1 to k do
          dSum:= dSum + ( Vctr[i] - MeanVctr[i] );
        LDists[iCnt]:= dSum;
      end;
    end;

  except
    Raise Exception.Create(LDistError);
  end;

end;


function TRegComp.GetEuclideanDistance (const Vector     : XVector;
                                        const MeanVector : XVector;
                                        const k          : byte): extended;

// Euclidean Dist: ED = sqrt(Sum of sqr(X[i] - M(i)))
var
  i: byte;
  dSum: extended;
  dTemp: extended;
begin

  Result:= 0;

  if ( k in [1..MAXVARIABLES-1] ) then begin
    try
      dSum:= 0.0;
      for i:= 1 to k do begin
        dTemp:= Vector[i] - MeanVector[i];
        dSum:= dSum + ( dTemp * dTemp );
      end;
      Result:= sqrt(dSum);

    except
      Raise Exception.Create(EuclidDistError);
    end;
  end;

end;


procedure TRegComp.EuclideanDistances (const PRegressData : PTRegressDataArray;
                                       var   EDists       : TResidualsArray;
                                       const n            : integer;
                                       const k            : byte);
var
  i: byte;
  iCnt: integer;
  dSum: extended;
  dTemp: extended;
  Vctr: XVector;
  MeanVctr: XVector;
begin

  try
    if ( k in [1..MAXVARIABLES-1] ) and
       ( k < n ) and
       ( n < MAXOBSERVATIONS ) then

    begin
      MeanVctr:= MeanVector(PRegressData,k,n);

      for iCnt := 0 to n-1 do begin

        for i:= 1 to k do
          Vctr[i]:= PRegressData^[i,iCnt];

        dSum:= 0.0;

        for i:= 1 to k do begin
          dTemp:= Vctr[i] - MeanVctr[i];
          dSum:= dSum + ( dTemp * dTemp );
        end;

        EDists[iCnt]:= dSum;

      end;

    end;

  except
    Raise Exception.Create(EuclidDistError);
  end;

end;


function TRegComp.GetNormEuclideanDistance (const Vector     : XVector;
                                            const MeanVector : XVector;
                                            const CovMatrix  : XMatrix;
                                            const k          : byte): extended;
var
  i: byte;
  dSum: extended;
  dTemp: extended;
begin

  Result:= 0;

  if ( k in [1..MAXVARIABLES-1] ) then begin
    try
      dSum:= 0.0;
      for i:= 1 to k do begin
        dTemp:= ( Vector[i] - MeanVector[i] ) / sqrt(CovMatrix[i,i]);
        dSum:= dSum + (dTemp * dTemp);
      end;
      Result:= sqrt(dSum);

    except
      Raise Exception.Create(NormEuclidDistError);
    end;
  end;

end;


procedure TRegComp.NormEuclideanDistances (const PRegressData : PTRegressDataArray;
                                           var   NEDists      : TResidualsArray;
                                           const n            : integer;
                                           const k            : byte);
var
  i: byte;
  iCnt: integer;
  dSum: extended;
  dTemp: extended;
  Vctr: XVector;
  MeanVctr: XVector;
  CovMat: XMatrix;
begin

  try
    if ( k in [1..MAXVARIABLES-1] ) and
       ( k < n ) and
       ( n < MAXOBSERVATIONS ) then
    begin
      MeanVctr := MeanVector(PRegressData,k,n);
      CovMat   := CovarianceMatrix(PRegressData,k,n);

      for iCnt := 0 to n-1 do begin

        for i:= 1 to k do
          Vctr[i]:= PRegressData^[i,iCnt];

        dSum:= 0.0;
        for i:= 1 to k do begin
          dTemp:= ( Vctr[i] - MeanVctr[i] ) / ( sqrt ( CovMat[i,i] ) ) ;
          dSum:= dSum + (dTemp * dTemp);
        end;

        NEDists[iCnt]:= dSum;
      end;

    end;

  except
    Raise Exception.Create(NormEuclidDistError);
  end;

end;


function TRegComp.GetMahalanobisDistance  (const PRegressData  : PTRegressDataArray;
                                           const Vector        : XVector;
                                           const k             : byte;
                                           const n             : integer): extended;

// Formula: MDistance = (X-M(i))t * InvC(i) * X-M(i)
//                      [   TempVector    ]
//
// Where: X is data vector,
//        M is mean sample/group values vector
//        InvC is the inverse of the sample/group
//          covariance matrix
var
  i: integer;
  j: byte;
  dSum: extended;
  dDet: extended;
  DataVctrMinusMeans: XVector;
  TempVctr: XVector;
  MeanVctr: XVector;
  InvCovMat: XMatrix;
  CovMat: XMatrix;
begin

  InvCovMat := InitMatrix(MAXVARIABLES,0);
  Result    := 0;

  try
    if ( k in [1..MAXVARIABLES-1] ) and
       ( k < n ) and
       ( n < MAXOBSERVATIONS ) then begin
      MeanVctr  := MeanVector(PRegressData,k,n);
      CovMat    := CovarianceMatrix(PRegressData,k,n);
      InvCovMat := InvertMatrix(CovMat,k,dDet);

      for i:= 1 to k do
        DataVctrMinusMeans[i]:= Vector[i] - MeanVctr[i];

      for i:= 1 to k do begin
        dSum:= 0.0;
          for j:= 1 to k do
            dSum := dSum + DataVctrMinusMeans[j] * InvCovMat[i,j];
        TempVctr[i]:= dSum;
      end;

      dSum:= 0.0;
      for i:= 1 to k do
        dSum:= dSum + TempVctr[i] * DataVctrMinusMeans[i];

      Result:= dSum;
    end;

  except
    Raise Exception.Create(MahalanobisDistanceError);
  end;

end;


procedure TRegComp.MahalanobisDistances (const PRegressData : PTRegressDataArray;
                                           var MDistances   : TResidualsArray;
                                         const k            : byte;
                                         const n            : integer);

// Formula: MDistance = (X-M(i))t * InvC(i) * X-M(i)
//                      [   TempVector    ]
//
// Where: X is data vector,
//        M is mean sample/group values vector
//        InvC is the inverse of the sample/group
//          covariance matrix
var
  i: byte;
  j: byte;
  iCnt: integer;
  dSum: extended;
  dDet: extended;
  DataVctrMinusMeans: XVector;
  TempVctr: XVector;
  MeanVctr: XVector;
  InvCovMat: XMatrix;
  CovMat: XMatrix;
begin

  InvCovMat := InitMatrix(MAXVARIABLES,0);
  CovMat    := InitMatrix(MAXVARIABLES,0);
  InitResidArray(MDistances,0);

  try
  if ( k in [1..MAXVARIABLES-1] ) and
     ( k < n ) and
     ( n < MAXOBSERVATIONS ) then

  begin
    MeanVctr  := MeanVector(PRegressData,k,n);
    CovMat    := CovarianceMatrix(PRegressData,k,n);
    InvCovMat := InvertMatrix(CovMat,k,dDet);

    for iCnt := 0 to n-1 do begin
      for i:= 1 to k do
        DataVctrMinusMeans[i]:= PRegressData^[i,iCnt] - MeanVctr[i];

      for i:= 1 to k do begin
        dSum:= 0.0;
        for j:= 1 to k do
          dSum := dSum + DataVctrMinusMeans[j] * InvCovMat[i,j];
        TempVctr[i]:= dSum;
      end;

      dSum:= 0.0;
      for i:= 1 to k do
        dSum:= dSum + TempVctr[i] * DataVctrMinusMeans[i];

      MDistances[iCnt]:= dSum;
    end;
  end;

  except
    Raise Exception.Create(MahalanobisDistancesError);
  end;

end;


function TRegComp.GetMaximumLikelihoodDistance (const PRegressData : PTRegressDataArray;
                                                var   Vector       : XVector;
                                                const k            : byte;
                                                const n            : integer;
                                                const APriori      : extended): extended;
var
  dMLDist: extended;
  dDeterminant: extended;
  CovMat: XMatrix;
  InvCovMat: XMatrix;
begin

  Result      := 0.0;
  CovMat      := InitMatrix(MAXVARIABLES,0);
  InvCovMat   := InitMatrix(MAXVARIABLES,0);

  if ( k < n ) and
     ( n < MAXOBSERVATIONS ) and
     ( k in [1..MAXVARIABLES-1] ) then

  begin
    try
      CovMat     := CovarianceMatrix(PRegressData,k,n);
      InvCovMat  := InvertMatrix(CovMat,k,dDeterminant);
      dMLDist    := GetMahalanobisDistance(PRegressData,Vector,k,n); // 1st, Mahalanobis Dists

      Result     := APriori - ( ONE_HALF * ln ( dDeterminant ) ) -
                           ( ONE_HALF * dMLDist );

    except
      Raise Exception.Create(MaximumLikelihoodDistancesError);
    end;

  end;

end;


procedure TRegComp.MaximumLikelihoodDistances (const PRegressData : PTRegressDataArray;
                                               var   MLDistances  : TResidualsArray;
                                               const k            : byte;
                                               const n            : integer;
                                               const APriori      : extended);
var
  i: integer;
  dDeterminant: extended;
  CovMat: XMatrix;
  InvCovMat: XMatrix;
  MHDists: TResidualsArray;
begin

  CovMat      := InitMatrix(MAXVARIABLES,0);
  InvCovMat   := InitMatrix(MAXVARIABLES,0);

  if ( n > 2 ) and
     ( k < n ) and
     ( n < MAXOBSERVATIONS ) and
     ( k in [1..MAXVARIABLES-1] ) then
  begin
    try
      CovMat     := CovarianceMatrix(PRegressData,k,n);
      InvCovMat  := InvertMatrix(CovMat,k,dDeterminant);
      MahalanobisDistances(PRegressData,MHDists,k,n); // 1st, calc Mahalanobis Dists
      for i:= 0 to n-1 do
      // Use [uncomment] the line below for the 'simple' ML calcs:
      //   -does not use term for APriori probabilities...
      // MLDistances[i]:= ln(aDeterminant) + MHDists[i];

        MLDistances[i]:= APriori - ( ONE_HALF * ln ( dDeterminant ) ) -
                          ( ONE_HALF * MHDists[i] );

    except
      Raise Exception.Create(MaximumLikelihoodDistancesError);
    end;

  end;

end;


// =================== Distribution-based functions are below: ===============

function TRegComp.GetAvgNormDistance (const MeanVector1 : XVector;
                                      const MeanVector2 : XVector;
                                      const CovMatrix1  : XMatrix;
                                      const CovMatrix2  : XMatrix;
                                      const k           : byte): extended;

// Computes the below quantity for each band - then
// computes the overall means for the set of values
//
// Sum of : for i = 1..vars
//
//    | Mean[1]   -   Mean[2] |
//      --------------------
//      StdDev[1] + StdDev[2]
//
// Note: StdDev1 and StdDev2 are computed from the input
// CovarianceMatrices respectively
var
  i: byte;
  dDiff: extended;
  dSum: extended;
  dSD1: extended;
  dSD2: extended;
begin

  Result  := 0;

  try
    dSum:= 0.0;
    if ( k in [1..MAXVARIABLES-1] ) then begin
      for i:= 1 to k do begin
        dDiff := abs ( MeanVector1[i] - MeanVector2[i] );
        dSD1  := sqrt(CovMatrix1[i,i]);
        dSD2  := sqrt(CovMatrix2[i,i]);
        dSum  := dSum + ( dDiff / ( dSD1 + dSD2 ) );
      end;
      Result:= dSum / k;
    end;

  except
    Raise Exception.Create(AvgNormDistanceError);
  end;

end;



// =====================  Interpretations Routines are below: =================

function TRegComp.FlagOutliers  (const StdResids  : TResidualsArray;
                                 const dThreshold : single;
                                 const n          : integer): TIntegerArray;
var
  i: integer;
  Outliers: TIntegerArray;
begin

  try
    if ( n > 0 ) and
       ( n < MAXOBSERVATIONS ) then
    begin
      for i:= 0 to n-1 do
        Outliers[i]:= 0;

      if ( dThreshold > 0.0 ) and ( dThreshold < 5.0 ) then begin
        for i:= 0 to n-1 do begin
          if ( abs ( StdResids[i] ) > dThreshold ) then
             Outliers[i]:= 1;
        end;
      end;
    end;

    Result:= Outliers;

  except
    Raise Exception.Create(FlagOutliersError);
  end;

end;


// ===========================================================================

function TRegComp.BVEquationString (const aBVRec: TBivarStatRec): string;
begin

  Result:= '';
  Result:= Format ('y = %10.4f + %10.4fx',[aBVRec.Beta0,aBVRec.Beta1]) + CRLF;

end;


function TRegComp.MVEquationString (const MVRec: TMultStatRec): string;
var
  i: byte;
  s1: string;
  s2: string;
  s3: string;
begin

  Result:= '';
  s1:= '';
  s2:= '';
  s3:= '';
  s1:= Format('Eq: y = %9.4f',[ MVRec.Beta[1] ] ) + ' ';

  for i:= 2 to MVRec.k+1 do begin
    s3:= '';
    s3:= Format(' %9.4f ',[ MVRec.Beta[i] ] );
    if ( i < ( MVRec.k+2 ) ) then
      s3:= ' + ' + s3 + 'x' + IntToStr(i-1);
    s2:= s2 + s3;
  end;

  s3:= '';
  s3:= s1 + s2;

  Result:= s3 + CRLF;

end;


function TRegComp.BVRegressionMemo (const sStr : string;
                                    const BVRec : TBivarStatRec): string;
var
  s: string;
  sBVEq: string;
  dB1TestStat: extended;
  B1CIStat: TIntervalRec;
begin

  dB1TestStat := BVRegressionTestStatistic(BVRec);
  B1CIStat    := BVRegressionBeta1CI(BVRec);
  sBVEq       := BVEquationString(BVRec);

  s:= '';
  s:= sStr + CRLF + CRLF;
  s:= sBVEq + CRLF;
  s:= s + Format('n                 : %5d',[BVRec.n]) + CRLF;
  s:= s + Format('Correlation (X|Y) : %10.4f', [BVRec.r]) + CRLF;
  s:= s + Format('Intercept (ß0)    : %10.4f', [BVRec.Beta0]) + CRLF;
  s:= s + Format('Slope (ß1)        : %10.4f', [BVRec.Beta1]) + CRLF;
  s:= s + Format('  Test Stat       : %10.4f', [dB1TestStat]) + CRLF;
  s:= s + Format('  C Interval (-)  : %10.4f', [B1CIStat.Lower]) + CRLF;
  s:= s + Format('  C Interval (+)  : %10.4f', [B1CIStat.Upper]) + CRLF;
  s:= s + Format('  Alpha (.%2d ',[AlphaLevel]) + ')' + CRLF;
  s:= s + Format('X Mean            : %10.4f', [BVRec.XMean]) + CRLF;
  s:= s + Format('Y Mean            : %10.4f', [BVRec.YMean]) + CRLF;

  Result:= s;

end;


function TRegComp.MVRegressionMemo (const aMultStatRec: TMultStatRec): string;
var
  i: byte;
  dTCrit: single;
  sTmp: string;
  sStr: string;
begin

  sStr:= '';
  if ( aMultStatRec.k > 0 ) and
     ( aMultStatRec.n > 2 ) then begin
    with aMultStatRec do begin
      dTCrit  := TTableValue2(n);
      sStr    := 'Regression Summary :  ' + CRLF + CRLF;
      sStr    := sStr + Format('n      = %5d',[n] ) + CRLF;
      sStr    := sStr + Format('k      = %5d',[k] ) + CRLF;
      sStr    := sStr + Format('DF Reg = %6d',[DFModel] ) + CRLF;
      sStr    := sStr + Format('DF Err = %6d',[DFError] ) + CRLF;
      sStr    := sStr + Format('DF Tot = %6d',[DFTotal] ) + CRLF + CRLF;
      sStr    := sStr + Format('R      = %10.3f',[R] ) + CRLF;
      sStr    := sStr + Format('R2     = %10.3f',[R2] ) + CRLF;
      sStr    := sStr + Format('F      = %10.3f',[FStat] ) + CRLF;
      sStr    := sStr + Format('RMSE   = %10.3f',[RMSE ] ) + CRLF;
      sStr    := sStr + Format('MSR    = %10.3f',[MSR] ) + CRLF;
      sStr    := sStr + Format('MSE    = %10.3f',[MSE] ) + CRLF;
      sStr    := sStr + Format('SSR    = %10.3f',[SSR] ) + CRLF;
      sStr    := sStr + Format('SSE    = %10.3f',[SSE] ) + CRLF;
      sStr    := sStr + Format('SST    = %10.3f',[SST] ) + CRLF + CRLF;
      sStr    := sStr + 'Parameter Estimates: ' + CRLF + CRLF;
      sStr    := sStr + 'Alpha : .' + IntToStr(AlphaLevel) + CRLF;

{      if ( n > 30 ) then
        sStr:= sStr + 'Warning: n is > 30.' + CRLF + CRLF;}

      for i:= 1 to k+1 do begin
        if ( abs ( TValue[i] ) > dTCrit ) then sTmp:= '*'
        else sTmp:= '';
        sStr:= sStr + 'Beta['+IntToStr(i-1)+'] ' +
                       PadStr(Format('%7.2f',[ Beta[i] ])) +
                       '    t calc ' +
                       PadStr(Format('%7.2f',[ TValue[i] ])) +
                       sTmp + CRLF + '  Std Err ' +
                       PadStr(Format('%7.2f',[ SeBeta[i] ])) +
                       '  t Crit ' +
                       PadStr(Format('%7.2f',[ dTCrit ])) +  CRLF;
      end;
      sStr:= sStr + CRLF;
    end;
  end;

  Result:= sStr;

end;


// ============================================================================


function TRegComp.MVOptimalForwardModel (const PRegressData : PTRegressDataArray;
                                         const Y            : array of extended;
                                         const k            : byte;
                                         const n            : integer): string;
type
  MallowsCArray = array [1..MAXVARIABLES+1] of extended;
var
  dCp            : extended;
  dMinCp         : extended;
  dFullModelMSE  : extended;
  iBestModel     : integer;
  i              : integer;
  MallowsCalc    : MallowsCArray;
  MallowsRef     : MallowsCArray;
  MDifferences   : MallowsCArray;
  MVRec          : TMultStatRec;
begin

  Result:= '';
  if (k in [1..MAXVARIABLES-1]) and
     (n > k) and
     (n < MAXOBSERVATIONS) then

  begin
    for i:= 1 to MAXVARIABLES do begin
      MallowsCalc[i]  := 0;
      MDifferences[i] := 0;
      MallowsRef[i]   := i + 1;
    end;

    MVRec           := MVRegression(PRegressData,Y,k,n);
    dFullModelMSE   := MVRec.MSE;

    for i:= 1 to k-1 do begin
      MVRec := InitMultStatRec;
      MVRec := MVRegression(PRegressData,Y,i,n);
      dCp   := CalcCp(MVRec.SSE,dFullModelMSE,i,n);
      MallowsCalc[i]:= dCp;
    end;

    for i:= 1 to k-1 do
      MDifferences[i]:= abs ( MallowsRef[i] - MallowsCalc[i] );

    iBestModel := -1;
    dMinCp     := 1e20;
    for i:= 1 to k-1 do begin
      if ( MDifferences[i] < dMinCp ) then begin
        dMinCp      := MDifferences[i];
        iBestModel  := i;
      end;
    end;

    Result:= Format('Best Stepwise Model (1-at-a-time BestModel) : %3d',[iBestModel]);
  end;

end;



end.
