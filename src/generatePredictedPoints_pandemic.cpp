#include <Rcpp.h>
using namespace Rcpp;
using std::log;
using std::exp;

double muFun(R_xlen_t t, double a, double b, double c, double f, bool logScale);
inline double dealNA(double y, double NA_replacement);
inline bool checkDFnames(DataFrame df, std::string check);

class predictor
{
public:
  void operator()(R_xlen_t pos, R_xlen_t t, double N){ predict(pos, t, N); }
  NumericMatrix mu, yL, yS;

  void initializeMatrices(R_xlen_t M, R_xlen_t h)
  {
    mu = NumericMatrix(M,h);
    yL = NumericMatrix(M,h);
    yS = NumericMatrix(M,h);
    ch_size = M;
  }
  void buildShortPrediction()
  {
    for (R_xlen_t i = 0; i < ch_size; i++)
      yS.row(i) = cumsum(yL.row(i)).get();
  }

protected:
  R_xlen_t ch_size;

  virtual void predict(R_xlen_t pos, R_xlen_t t, double N) = 0;
};

class seasonal
{
public:
  seasonal(DataFrame pars, std::vector<unsigned int> s,
           std::vector<unsigned int> ss,
           std::vector<unsigned int> sss) : s1(s), s2(ss), s3(sss)
  {
    int nW = s.size();
    R_xlen_t M = pars.nrows();
    NumericVector temp;

    d1 = NumericMatrix(M,nW);
    std::fill(d1.begin(), d1.end(), 1.);
    d2 = NumericMatrix(M,nW);
    std::fill(d2.begin(), d2.end(), 1.);
    d3 = NumericMatrix(M,nW);
    std::fill(d3.begin(), d3.end(), 1.);

    if (nW > 1)
      for (int j=0;j<nW;j++)
      {
        if (checkDFnames(pars, "d_1[" + std::to_string(j + 1) + "]"))
        {
          temp = pars["d_1[" + std::to_string(j + 1) + "]"];
          d1(_,j) = temp;
        }
        else
          d1(_,j) = NumericVector(M);
        if (checkDFnames(pars, "d_2[" + std::to_string(j + 1) + "]"))
        {
          temp = pars["d_2[" + std::to_string(j + 1) + "]"];
          d2(_,j) = temp;
        }
        else
          d2(_,j) = NumericVector(M);
        if (pars.containsElementNamed("d_3"))
        {
          temp = pars["d_3[" + std::to_string(j + 1) + "]"];
          d3(_,j) = temp;
        }
        else
          d3(_,j) = NumericVector(M);
      }
    else
    {
      if (checkDFnames(pars, "d_1"))
      {
        temp = pars["d_1"];
        d1(_, 0) = temp;
      }
      if (checkDFnames(pars, "d_2"))
      {
        temp = pars["d_2"];
        d2(_, 0) = temp;
      }
      if (checkDFnames(pars, "d_3"))
      {
        temp = pars["d_3"];
        d3(_, 0) = temp;
      }
    }
  }
protected:
  std::vector<unsigned int> s1, s2, s3;
  NumericMatrix d1, d2, d3;
};

class singleWave
{
public:
  singleWave(DataFrame pars)
  {
    a = pars["a"];
    b = pars["b"];
    c = pars["c"];
    f = pars["f"];
  }
protected:
  NumericVector a, b, c, f;
};

class multiWave
{
public:
  unsigned int nWaves;

  multiWave(DataFrame pars, unsigned int n) : nWaves(n)
  {
    NumericVector temp;
    a = NumericMatrix(pars.nrows(),n);
    b = NumericMatrix(pars.nrows(),n);
    c = NumericMatrix(pars.nrows(),n);
    alpha = NumericMatrix(pars.nrows(),n);
    delta = NumericMatrix(pars.nrows(),n);
    for (int j = 0; j < nWaves; j++)
    {
      temp = pars["a[" + std::to_string(j + 1) + "]"];
      a(_,j) = temp;
      temp = pars["b[" + std::to_string(j + 1) + "]"];
      b(_,j) = temp;
      temp = pars["c[" + std::to_string(j + 1) + "]"];
      c(_,j) = temp;
      temp = pars["alpha[" + std::to_string(j + 1) + "]"];
      alpha(_,j) = temp;
      temp = pars["delta[" + std::to_string(j + 1) + "]"];
      delta(_,j) = temp;
    }
  }
protected:
  NumericMatrix a, b, c, alpha, delta;
};

class negbin
{
public:
  negbin(DataFrame pars) {phi = pars["phi"];}
protected:
  NumericVector phi;
};

class poisson_singleWave : public singleWave, public predictor
{
public:
  poisson_singleWave(DataFrame pars) : singleWave(pars){}

private:
  void predict(R_xlen_t pos, R_xlen_t t, double N)
  {
    double y;
    R_xlen_t i;

    for (i = 0; i < ch_size; i++)
    {
      mu(i, pos) = muFun(t, a[i], b[i], c[i], f[i],false);
      y = R::rpois(mu(i, pos));
      yL(i,pos) = dealNA(y,N);
    }
  }
};

class poisson_singleWave_season : public singleWave, public seasonal,
                                  public predictor
{
public:
  poisson_singleWave_season(DataFrame pars, std::vector<unsigned int> s,
                            std::vector<unsigned int> ss,
                            std::vector<unsigned int> sss) : singleWave(pars),
                              seasonal(pars,s,ss,sss) {}
private:
  void predict(R_xlen_t pos, R_xlen_t t, double N)
  {
    double y;

    for (R_xlen_t i = 0; i < ch_size; i++)
    {
      mu(i, pos) = muFun(t, a[i], b[i], c[i], f[i], false) *
        pow(d1(i, 0), !((t - s1[0]) % 7) * (s1[0] > 0)) *
        pow(d2(i, 0), !((t - s2[0]) % 7) * (s2[0] > 0)) *
        pow(d3(i, 0), !((t - s3[0]) % 7) * (s3[0] > 0));
      y = R::rpois(mu(i, pos));
      yL(i, pos) = dealNA(y, N);
    }
  }
};

class poisson_multiWave : public seasonal, public multiWave, public predictor
{
public:
  poisson_multiWave(DataFrame pars, unsigned int nW,
                    std::vector<unsigned int> s,
                    std::vector<unsigned int> ss,
                    std::vector<unsigned int> sss) : seasonal(pars, s, ss, sss),
                      multiWave(pars, nW) {}
private:
  void predict(R_xlen_t pos, R_xlen_t t, double N)
  {
    double y;
    R_xlen_t i, j;
    for (i=0; i < ch_size; i++)
    {
      mu(i, pos) = 0;
      for (j = 0; j < nWaves; j++)
        {mu(i,pos) += exp(muFun(t, a(i, j), b(i, j), c(i, j), 1, true) +
          R::pnorm(alpha(i, j) * (t - delta(i, j)), 0, 1, true, true)) *
          pow(d1(i, j), !((t - s1[j]) % 7) * (s1[j] > 0)) *
          pow(d2(i, j), !((t - s2[j]) % 7) * (s2[j] > 0)) *
          pow(d3(i, j), !((t - s3[j]) % 7) * (s3[j] > 0));}
      y = R::rpois(mu(i, pos));
      yL(i,pos) = dealNA(y, N);
    }
  }
};

class negbin_singleWave : public singleWave, public seasonal,
                          public predictor, public negbin
{
public:
  negbin_singleWave(DataFrame pars, std::vector<unsigned int> s,
                            std::vector<unsigned int> ss,
                            std::vector<unsigned int> sss) : singleWave(pars),
                              seasonal(pars,s,ss,sss), negbin(pars) {}
private:
  void predict(R_xlen_t pos, R_xlen_t t, double N)
  {
    double y;
    for (R_xlen_t i = 0; i < ch_size; i++)
    {
      mu(i, pos) = muFun(t, a[i], b[i], c[i], f[i], false) *
        pow(d1(i, 0), !((t - s1[0]) % 7) * (s1[0] > 0)) *
        pow(d2(i, 0), !((t - s2[0]) % 7) * (s2[0] > 0)) *
        pow(d3(i, 0), !((t - s3[0]) % 7) * (s3[0] > 0));
      y = R::rpois(R::rgamma(phi[i] * mu(i, pos), 1 / phi[i]));
      yL(i, pos) = dealNA(y, N);
    }
  }
};

class negbin_multiWave : public negbin, public seasonal, public multiWave,
                         public predictor
{
public:
  negbin_multiWave(DataFrame pars, unsigned int nW,
                   std::vector<unsigned int> s,
                   std::vector<unsigned int> ss,
                   std::vector<unsigned int> sss) : negbin(pars),
                   seasonal(pars, s, ss, sss), multiWave(pars, nW) {}
private:
  void predict(R_xlen_t pos, R_xlen_t t, double N)
  {
    double y;
    R_xlen_t i, j;
    for (i = 0; i < ch_size; i++)
    {
      mu(i, pos) = 0;
      for (j = 0; j < nWaves; j++)
      {mu(i, pos) += exp(muFun(t, a(i, j), b(i, j), c(i, j), 1, true) +
        R::pnorm(alpha(i, j) * (t - delta(i, j)), 0, 1, true, true)) *
        pow(d1(i, j), !((t - s1[j]) % 7) * (s1[j] > 0)) *
        pow(d2(i, j), !((t - s2[j]) % 7) * (s2[j] > 0)) *
        pow(d3(i, j), !((t - s3[j]) % 7) * (s3[j] > 0));}
      y = R::rpois(R::rgamma(phi[i] * mu(i, pos), 1 / phi[i]));
      yL(i, pos) = dealNA(y, N);
    }
  }
};

// [[Rcpp::export]]
List generatePredictedPoints_pandemicC(R_xlen_t M, DataFrame chains,
                                       R_xlen_t horizon, double NA_rep,
                                       String model_name, R_xlen_t final_time,
                                       R_xlen_t nWaves, List s_code)
{
  predictor* predict;
  std::string cname = model_name.get_cstring();
  std::vector<unsigned int> s1 = as<std::vector<unsigned int> >(s_code["s1"]),
    s2 = as<std::vector<unsigned int> >(s_code["s2"]),
    s3 = as<std::vector<unsigned int> >(s_code["s3"]);

  if (cname == "poisson: static generalized logistic")
    predict = new poisson_singleWave(chains);
  else if (cname == "poisson: static seasonal generalized logistic")
    predict = new poisson_singleWave_season(chains, s1, s2, s3);
  else if (cname == "poisson: multi_waves(" + std::to_string(nWaves) + ")")
    predict = new poisson_multiWave(chains, nWaves, s1, s2, s3);
  else if (cname == "negbin: static generalized logistic")
    predict = new negbin_singleWave(chains, s1, s2, s3);
  else if (cname == "negbin: multi_waves(" + std::to_string(nWaves) + ")")
    predict = new negbin_multiWave(chains, nWaves, s1, s2, s3);
  else stop("Model name not recognized.");

  predict->initializeMatrices(M,horizon);
  for (R_xlen_t t = 0; t < horizon; t++)
    (*predict)(t, final_time + t + 1, NA_rep);

  predict->buildShortPrediction();

  if (is_true(any(predict->yL == NA_rep))) Rcout <<
    "Prediction had NA values. Replaced with large value for identification.";
  List output = List::create(Named("yL") = predict->yL,
                             Named("yS") = predict->yS,
                             Named("mu") = predict->mu);

  return output;
}

double muFun(R_xlen_t t, double a, double b, double c, double f, bool logScale)
{
  double out = log(f) + log(a) + log(c) - (c * t) - (f + 1) * log( b + exp(-c * t) );
  if (logScale)
  {
    return out;
  }
  else
  {
    return exp(out);
  }
}

inline double dealNA(double y, double NA_replacement){
  return (R_IsNA(y) ? NA_replacement : y);
}

inline bool checkDFnames(DataFrame df, std::string check)
{
  CharacterVector nn = df.names();
  int s = check.size();
  String name;
  std::string ss;
  bool verify = false;

  for(int i = 0; i < df.size(); i++)
  {
    name = nn[i];
    ss = name.get_cstring();
    for(int j = 0; j < s; j++)
    {
      if (ss[j] != check[j]) break;
      if (j == (s-1)) verify = true;
    }
    if (verify) return verify;
  }

  return verify;
}
