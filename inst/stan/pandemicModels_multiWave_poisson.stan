

//
// Stan model to evaluated the cases of Covid-19 - Poisson multiwave

functions{
  real genLog(int t, real a, real b, real c, int logScale){
    real logV = log(a)+log(c)-(c*t)-2 * log( b+exp(-c*t) );
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
  int<lower=1> nCurves;
  int<lower=0, upper=7> w1[nCurves];
  int<lower=0, upper=7> w2[nCurves];
  int<lower=0, upper=7> w3[nCurves];
  //-----------------------------
}

parameters {
  vector[nCurves] b1;
  real<lower = 0, upper = min(p * pop * exp(b1))> a[nCurves];
  //real<lower=0> a[nCurves];
  real<lower=0> c[nCurves];

  // seasonal effects
  real<lower=0> d_1[nCurves];
  real<lower=0> d_2[nCurves];
  real<lower=0> d_3[nCurves];

  // multiple curves
  vector<lower=0>[nCurves] alpha;
  vector[nCurves] delta;

}

transformed parameters{

  vector<lower=0>[nCurves] b = exp(b1);
  vector<lower=0, upper=pop>[n] mu;

  for(t in 1:n){
    mu[t] = 0;
    for (curve in 1:nCurves){
      mu[t] += exp(
        normal_lcdf(alpha[curve] * (t - delta[curve]) | 0,1) +
        genLog(t, a[curve], b[curve], c[curve], 1) +
        ( ((t - w1[curve]) % 7 == 0) .* (w1[curve] > 0)) .* log(d_1[curve]) +
        ( ((t - w2[curve]) % 7 == 0) .* (w2[curve] > 0)) .* log(d_2[curve]) +
        ( ((t - w3[curve]) % 7 == 0) .* (w3[curve] > 0)) .* log(d_3[curve])
        );
    }
  }
}


model {
  //----------------------------
  // likelihood function
  y ~ poisson(mu);
  //----------------------
  // prior distributions
  a ~ gamma(0.1,0.1);
  delta ~ normal(0,100);
  c ~ gamma(2,9);
  alpha ~ gamma(0.01,0.01);
  d_1 ~ gamma(2,1);
  d_2 ~ gamma(2,1);
  d_3 ~ gamma(2,1);
  b1 ~ normal(0, sqrt(20));
}
