//
// Stan model to evaluated the cases of Covid-19 - Poisson model
// model: static generalized logistic


data {

  //-----------------------------
  // observed data
  int<lower=1> n; // number of observations
  int<lower=0> y[n]; // counts of new case
  real pop;
  real<lower=0,upper=1> p;
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

  real<lower=1> f;
  real<lower=-30> b1;
  real<lower=0, upper=p*pop*exp(f*b1)> a;
  real<lower=0> c;

}

transformed parameters{

  real<lower=0> b;
  real<lower=0, upper=pop> mu[n];

  b = exp(b1);

  for(t in 1:n){
    mu[t] = exp(log(f)+log(a)+log(c)-(c*t)-(f+1)*log( b+exp(-c*t) ) );
  }

}


model {
  //----------------------------
  // likelihood function
    y ~ poisson(mu); // observed model
  //----------------------
   // prior distributions

  a ~ gamma(a_alpha,a_beta); // 0.1,0.1
  b1 ~ normal(mu_b_1,sigma2_b_1); // 0, sqrt(20)
  c ~ gamma(c_alpha,c_beta); // 2,9
  f ~ gamma(f_alpha,f_beta); // 0.01,0.01


}
