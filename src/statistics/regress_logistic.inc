{ ******************************************************************
  This unit fits the logistic function :

                                     1
                        y = ----------------------
                            1 + exp(-∑(b[i]*x[i]))

  ****************************************************************** 
  Modern iteration solution
  See https://en.wikipedia.org/wiki/Logistic_regression
  Binary logistic regression (y=0 or y=1) can, for example, be calculated using 
  iteratively reweighted least squares (IRLS), which is equivalent to maximizing 
  the log-likelihood of a Bernoulli distributed process using Newton's method. 
  If the problem is written in vector matrix form, with 
  X the matrix of independent variables, with 1's in the first column) 
  y is the vector of responses (0,1)
  parameters wT = [β0,β1,β2,…]  // T indicates transpose
  explanatory variables x(i) = [1, x1(i), x2(i), …]T
  and expected value of the Bernoulli distribution μ(i) = 1 / (1+e(−wT.x(i)))
  
  The parameters can be found using the following iterative algorithm:

  w[k+1] = (XT.Sk.X)^(-1). XT.(S[k].X.w[k] + y - μ[k])

  where S is diag(μ(i)(1-μ(i))) is a diagnonal weighting matrix, and
  μ is the vector of expected values.

  }

{ Unweighted fit of logistic function
  Input parameters:  X, Y     = point coordinates
                     Lb, Ub   = array bounds
                     MaxIter  = max. number of iterations
                     Tol      = tolerance on parameters
  Output parameters: B        = regression parameters
                     V        = inverse matrix }
procedure epiLogiFit(X, Y     : TVector;
                     Lb, Ub   : Integer;
                     MaxIter  : Integer;
                     Tol      : Float;
                     B        : TVector;
                     V        : TMatrix);

function epiLogiFit_Func(X: TMatrix; ix: Integer; B : TVector) : Float;
function epiLogiIterate(X: TMatrix; Y: TVector; M: TVector; V:TMatrix): TVector;
implementation

function epiLogiFit_Func(X: TMatrix; ix, UB: Integer; B : TVector) : Float;
{ ------------------------------------------------------------------
  Computes the regression function at point X.
  B is the vector of parameters
  ------------------------------------------------------------------ }
var
  D : Float;
  i: Integer;
begin
  X[0, ix] := 1;
  D := 0;
  for i := 0 to UB do
    D -= X[i, ix] * B[i];
  result := 1 / (1 + Expo(D));
end;

function epiLogiIterate(X: TMatrix; Y: TVector; M: TVector; V:TMatrix): TVector;
var
  i, j, n, l: Integer;
  Det: Float;
  S: TMatrix;
  XT: TMatrix;
  SX: TMatrix;
  SXW: TVector;
  MU: TVector;
begin
  // create diagonal S
  l := length(Y)-1;
  n := length(M)-1;
  DimVector(MU,l);
  DimMatrix(S, l, l);
  DimMatrix(V, n, n)
  DimVector(result, n);
  for i := 0 to l do
    begin
      for j :0 = to l do
        S[i,j] := 0;
    end;
    MU[i] := epiLogiFit_Func(X, i, l, M);
    S[i,i] := MU[i] * (1 - MU[i]);
  end;
  // Transpose X
  XT := MatTranspose(X,0); // should move this out; it never changes
  // one iteration
  SX := MatMul(S,X);
  T := MatMul(XT,SX);
  // MatInv(T); // use linear equation solver for this; also get determinant
  LinEq(T,M,0,l,Det);
  if (MathErr <> MathOK) then
    begin
      Executor.Error('Error inverting matrix: ' + MathErr);
      exit;
    end;
  T := MatMul(T,XT);
  SXW := MatVecMul(SX, M);
  SXW := VecAdd(SXW, Y);
  SXW := VecSub(SXW, MU);
  result := MatVecMul(T, SXW);
end;

procedure epiLogiFit(X, Y,    : TVector;
                     Lb, Ub   : Integer;
                     MaxIter  : Integer;
                     Tol      : Float;
                     B        : TVector;
                     V        : TMatrix);
var
  l, iter: Integer;
  M: TVector;
  D: TVector;
  T: Float;
begin
  if MaxIter = 0 then Exit;
  l := Length(B)-1;
  DimVector(M,l);
  DimVector(D,l);
  for i := 0 to l do
    M[i] := 1;
  for i := 0 to MaxIter do
    begin
      B := epiLogiIterate(X, Y, M, V);
      D := abs(VecSub(M,B));
      T := max(D);
      if (T < Tol) then 
        exit;
    end;
end;

end.
