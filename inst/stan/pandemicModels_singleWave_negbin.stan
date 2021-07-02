

//
// Stan model to evaluated the cases of Covid-19 - Negative Binomial
// model: static generalized logistic

functions{
    real genLog(int t, real a, real b, real c, real f, int logScale){
    real logV = log(f)+log(a)+log(c)-(c*t)-(f+1) .* log( b+exp(-c*t) );
    if (logScale){
      return logV;
    } else {
      return exp(logV);
    }
  }
}

data {

  //-----------------------------
  // observed data
  int<lower=1> n; // number of observations
  int<lower=0> y[n]; // counts of new cases
  real pop;
  real<lower=0, upper=1> p;
  int<lower=0, upper=7> w1;
  int<lower=0, upper=7> w2;
  int<lower=0, upper=7> w3;
  real<lower=0> fTrunc;
  real<lower=0> phiTrunc;
  //-----------------------------
}


parameters {
  real b1;
  real<lower=fTrunc> f;
  real<lower=0, upper=p*pop*exp(f * b1)> a;
  real<lower=0> c;

  // seasonal effects
  real<lower=0> d_1;
  real<lower=0> d_2;
  real<lower=0> d_3;

 // Negative Binomial variables
  real<lower=phiTrunc> phi;
  vector<lower=0>[n] lambda;
}

transformed parameters{

  real<lower=0> b = exp(b1);
  vector<lower=0, upper=pop>[n] mu;

  for(t in 1:n){
    mu[t] = genLog(t,a,b,c,f,0)*d_1^( ((t - w1) % 7 == 0)*(w1 > 0))*d_2^( ((t - w2) % 7 == 0)*(w2 > 0))*d_3^( ((t - w3) % 7 == 0)*(w3 > 0));
  }
}


model {
  //----------------------------
  // likelihood function
  y ~ poisson(lambda);
  lambda ~ gamma(phi * mu,phi);
  //----------------------
  // prior distributions
  a ~ gamma(0.1,0.1);
  c ~ gamma(2,9);
  f ~ gamma(0.01,0.01);
  d_1 ~ gamma(2,1);
  d_2 ~ gamma(2,1);
  d_3 ~ gamma(2,1);
  phi ~ gamma(0.1,0.1);
  b1 ~ normal(0, sqrt(20));
}
