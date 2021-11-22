

//
// Stan model to evaluated the cases of Covid-19 - Negative Binomial multiwave

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
  real<lower=0> phiTrunc;
  //-----------------------------

  // Prior Parameters
   real<lower=0> a_alpha;
   real<lower=0> a_beta;
   real mu_delta;
   real<lower=0> sigma2_delta;
    real<lower=0> c_alpha;
   real<lower=0> c_beta;
    real<lower=0> alpha_alpha;
   real<lower=0>   alpha_beta;
   real<lower=0> d_1_alpha;
   real<lower=0> d_1_beta;
    real<lower=0> d_2_alpha;
   real<lower=0> d_2_beta;
   real<lower=0> d_3_alpha;
   real<lower=0> d_3_beta;
   real  mu_b_1;
   real<lower=0> sigma2_b_1;
    real<lower=0> phi_alpha;
   real<lower=0> phi_beta;
   real<lower=0> f_alpha;
   real<lower=0> f_beta;


}


parameters {
  vector[nCurves] b1;
  real<lower=0, upper=min(p*pop*exp(b1))> a[nCurves];
  //real<lower=0> a[nCurves];
  real<lower=0> c[nCurves];

  // seasonal effects
  real<lower=0> d_1[nCurves];
  real<lower=0> d_2[nCurves];
  real<lower=0> d_3[nCurves];

  // multiple curves
  vector<lower=0>[nCurves] alpha;
  vector[nCurves] delta;

  // Negative binomial variables
  real<lower=phiTrunc> phi;
  vector<lower=0>[n] lambda;

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
  y ~ poisson(lambda);
  lambda ~ gamma(phi * mu,phi);
  //----------------------
  // prior distributions
  a ~ gamma(a_alpha,a_beta); // 0.1,0.1
  delta ~ normal(mu_delta,sigma2_delta); // 0,100

  alpha ~ gamma(alpha_alpha,alpha_beta); // 0.01,0.01
  d_1 ~ gamma(d_1_alpha,d_1_beta); // 2,1
  d_2 ~ gamma(d_2_alpha,d_2_beta); // 2,1
  d_3 ~ gamma(d_3_alpha,d_3_beta); // 2,1
  phi ~ gamma(phi_alpha,phi_beta); // 0.1,0.1
  b1 ~ normal(mu_b_1,sigma2_b_1); // 0, sqrt(20)
}

