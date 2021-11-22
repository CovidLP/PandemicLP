

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
   a ~ gamma(a_alpha,a_beta); // 0.1,0.1
  c ~ gamma(c_alpha,c_beta); // 2,9
  f ~ gamma(f_alpha,f_beta); // 0.01,0.01
  d_1 ~ gamma(d_1_alpha,d_1_beta); // 2,1
  d_2 ~ gamma(d_2_alpha,d_2_beta); // 2,1
  d_3 ~ gamma(d_3_alpha,d_3_beta); // 2,1
  phi ~ gamma(phi_alpha,phi_beta); // 0.1,0.1
  b1 ~ normal(mu_b_1,sigma2_b_1); // 0, sqrt(20)
}
