Const {// MAXVARIABLES      =     20;
      // MAXOBSERVATIONS   =   5000;}
      VERYSMALLNUMBER   = -1e20;
      VERYLARGENUMBER   =  1e20;
      ONE_HALF          = 0.5;
      TWO               = 2;
      ERROR_PREFIX      = 'Error occurred in ';
      CRLF              = #13#10;

resourcestring
      VarianceProportionsVectorError  = ERROR_PREFIX + ' VarianceProportionsVector';
      CheckDataError                  = ERROR_PREFIX + ' CheckData';
      SpearmansRError                 = ERROR_PREFIX + ' SpearmansR';
      SpearmansRStdErrorError         = ERROR_PREFIX + ' SpearmansRStdError';
      ChiSquareError                  = ERROR_PREFIX + ' ChiSquare';
      SmoothError                     = ERROR_PREFIX + ' SmoothData';
      SkewError                       = ERROR_PREFIX + ' GetSkew';
      KurtosisError                   = ERROR_PREFIX + ' GetKurtosis';
      GetMinError                     = ERROR_PREFIX + ' GetMin';
      GetMaxError                     = ERROR_PREFIX + ' GetMax';
      GetMeanError                    = ERROR_PREFIX + ' GetMean';
      GetSumError                     = ERROR_PREFIX + ' GetSum';
      GetVarianceError                = ERROR_PREFIX + ' GetVariance';
      GetCofVariationError            = ERROR_PREFIX + ' GetCoefficientOfVariation';
      UnivariateSummaryError          = ERROR_PREFIX + ' UnivariateSummary';
      VarianceVectorError             = ERROR_PREFIX + ' VarianceVector';
      CalcCpError                     = ERROR_PREFIX + ' CalcCp';
      CalcFpError                     = ERROR_PREFIX + ' CalcFp';
      CalcMSEpError                   = ERROR_PREFIX + ' CalcMSEp';
      TCalculatedError                = ERROR_PREFIX + ' TCalculated';
      SmallSampleError                = ERROR_PREFIX + ' SmallSampleCIforMean';
      LargeSampleError                = ERROR_PREFIX + ' LargeSampleCIfor Mean';
      BVBeta1CIError                  = ERROR_PREFIX + ' BivarRegressionBeta1CI';
      BVRegCIError                    = ERROR_PREFIX + ' BivarRegressionCI';
      BVRegPIError                    = ERROR_PREFIX + ' BivarRegressionPI';
      BVRegTestError                  = ERROR_PREFIX + ' BivarRegressionTestStatistic';
      BVRegReverseError               = ERROR_PREFIX + ' BivarRegressionReverse';
      BVCorrelationSampleTestStatError= ERROR_PREFIX + ' BVCorrelationSampleTestStat';
      MVRegCIError                    = ERROR_PREFIX + ' MultiVarRegressionCI';
      MVRegPIError                    = ERROR_PREFIX + ' MultiVarRegressionPI';
      MVRegBetaTestStatError          = ERROR_PREFIX + ' MultiVarRegressionBetaTestStatistic';
      MVRegBetaCIError                = ERROR_PREFIX + ' MultiVarRegressionBetaCI';
      BVResidualsError                = ERROR_PREFIX + ' BivarResiduals';
      BVPredictedError                = ERROR_PREFIX + ' BivarPredictedY';
      CorrelationMatrixError          = ERROR_PREFIX + ' CorrelationMatrix';
      CovarianceMatrixError           = ERROR_PREFIX + ' CovarianceMatrix';
      MeansDifferencesMatrixError     = ERROR_PREFIX + ' MeansDifferencesMatrix';
      VectorDivideError               = ERROR_PREFIX + ' VectorDivideScalar';
      NormalizeVectorError            = ERROR_PREFIX + ' NormalizeVectorByMax';
      TransformInverseError           = ERROR_PREFIX + ' TransformInverse';
      TransformSquareError            = ERROR_PREFIX + ' TransformSquare';
      TransformSquareRootError        = ERROR_PREFIX + ' TransformSquareRoot';
      TransformLogError               = ERROR_PREFIX + ' TransformLog';
      StandardizeError                = ERROR_PREFIX + ' StandardizeData';
      NormalizeError                  = ERROR_PREFIX + ' NormalizeData';
      CreateInteractionVarError       = ERROR_PREFIX + ' CreateInteractionVar';
      NormalizeDataByMaxError         = ERROR_PREFIX + ' NormalizeDataByMax';
      MVResidualsError                = ERROR_PREFIX + ' MultiVarResiduals';
      MVStdResidualsError             = ERROR_PREFIX + ' MultiVarStdResiduals';
      MVStudentResidualsError         = ERROR_PREFIX + ' MultiVarStudentResiduals';
      MVPredictedError                = ERROR_PREFIX + ' MultiVarPredictedY';
      CooksDError                     = ERROR_PREFIX + ' CooksDistance';
      FlagOutliersError               = ERROR_PREFIX + ' FlagOutliers';
      DurbinWatsonError               = ERROR_PREFIX + ' DurbinWatson';
      MahalanobisDistanceError        = ERROR_PREFIX + ' MahalanobisDistance';
      MahalanobisDistancesError       = ERROR_PREFIX + ' MahalanobisDistances';
      MaximumLikelihoodDistancesError = ERROR_PREFIX + ' MaximumLikelihoodDistances';
      EuclidDistError                 = ERROR_PREFIX + ' EuclideanDistances';
      LDistError                      = ERROR_PREFIX + ' LDistances';
      NormEuclidDistError             = ERROR_PREFIX + ' NormEuclideanDistances';
      AvgNormDistanceError            = ERROR_PREFIX + ' AvgNormDistance';
      RemoveVariableError             = ERROR_PREFIX + ' RemoveVariable';
      CombinationError                = ERROR_PREFIX + ' Combinations';
      PermutationError                = ERROR_PREFIX + ' Permutations';
      MatrixInvertError               = ERROR_PREFIX + ' Error inverting matrix';


      RHighPos    =  'The R Statistic indicates strong positive overall correlation ' +
                     'between the independent variables and the dependent variable. ';

      RLowPos     =  'The R Statistic indicates moderate positive overall correlation ' +
                     'between the independent variables and the dependent variable. ';

      RNoCorr     =  'The R Statistic indicates little or no correlation ' +
                     'between the independent variables and the dependent variable. ';

      RHighNeg    =  'The R Statistic indicates strong negative overall correlation ' +
                     'between the independent variables and the dependent variable. ';

      RLowNeg     =  'The R Statistic indicates moderate negative overall correlation ' +
                     'between the independent variables and the dependent variable. ';

      Notes       =  'Note: Increasingly larger number of variables and/or larger number' +
                     ' of observations can often produce large values of R. ';

      CorrPosHigh =  'Strong positive correlation exists between the following pair of ' +
                     'independent variables: ';

      CorrNegHigh =  'Strong negative correlation exists between the following pair of ' +
                     'independent variables: ';

      DWHighPos   =  'The DW Statistic indicates strong positive serial correlation. ' +
                     'It is likely that the residuals sequentially affect one another. ' +
                     'The dependent and independent variables should be examined in detail. ';

      DWLowPos    =  'The DW Statistic indicates low to moderate positive serial correlation. ' +
                     'The residuals may sequentially affect one another and might need further ' +
                     'examination. ';

      DWOK        =  'The DW Statistic indicates the residuals not to be serially correlated. ' +
                     'No further examination is necessary. ';

      DWHighNeg   =  'The DW Statistic indicates strong negative serial correlation. ' +
                     'It is likely that the residuals sequentially affect one another. ' +
                     'The dependent and independent variables should be examined in detail. ';

      DWLowNeg    =  'The DW Statistic indicates low to moderate negative serial correlation. ' +
                     'The residuals may sequentially affect one another and might need further ' +
                     'examination. ';

      SkewStr     =  'Variables found to have atypical skewness: ';

      KurtStr     =  'Variables found to have atypical kurtosis: ';

      ZeroNote = 'A cond. index of 0 was found which corresponded to eigenvalue : ' ;
      CondZero = 'Note: Condition index values of 0 result from eigenvalues of 0';
      HiCINote = 'A high condition index (>5) was found for eigenvalue : ';

      BetasHigh  = 'The following ß parameters were => threshold (+): ';
      BetasNear0 = 'These ß values were < abs(threshold): ';
      BetasLow   = 'The following ß parameters were <= threshold (-): ';

