
// a [k x n] data array of single for input data
TRegressDataArray = array [0..MAXVARIABLES-1,0..MAXOBSERVATIONS-1] of extended;
PTRegressDataArray = ^TRegressDataArray;

// a [n] array to hold residuals or predicted Y's
TResidualsArray = array [0..MAXOBSERVATIONS-1] of extended;

// a [n] array to hold byte/int values for each obs
TIntegerArray = array [0..MAXOBSERVATIONS-1] of integer;

// a [k x k] matrix (used for covariance, correlation functions)
//TMatrix = array [0..MAXVARIABLES,0..MAXVARIABLES] of extended;
// some [0..n] and [1..n+1] vectors to hold means, mins, maxs...
//TVector        = array [0..MAXVARIABLES-1] of extended;

//TByteVector    = array [0..MAXVARIABLES-1] of byte;
//TByteArray     = array [1..MAXVARIABLES+1] of byte;

// Holds a pair of extended vars to represent upper & lower bounds
// for a confidence/prediction interval, etc.
TIntervalRec = record
    Upper : extended;
    Lower : extended;
end;

// Array types to hold interval records-used in ANOVARec
TIntervalArray = array [0..MAXVARIABLES-1] of TIntervalRec;
TIntervalMatrix = array [0..MAXVARIABLES-1,0..MAXVARIABLES-1] of TIntervalRec;


// A BivarStatRec used in BivarRegression function
TBivarStatRec = record
    N     : integer;   // Number of observations
    df    : integer;
    r,                 // Correlation coefficient
    XMean,             // Mean of X data values
    YMean,             // Mean of Y data values
    Beta0,             // B0 : Y intercept of LsLine
    Beta1,             // B1 : slope of LsLine
    SSXX,              // SumX2 - sqr(SumX's)/N
    SSYY,              // SumY2 - sqr(SumY's)/N
    SSXY,              // SumXY - (SumX*SumY)/N
    SumX,              // Sum of X[i]'s
    SumY,              // Sum of Y[i]'s
    SumXY,             // Sum of X[i]'s * Y[i]'s
    SumX2,             // Sum of sqr(X[i]'s)
    SumY2: extended;   // Sum of sqr(Y[i]'s)
    z    : extended;   // z value
    t    : extended    // t value
end;



