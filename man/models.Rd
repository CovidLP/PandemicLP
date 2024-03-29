% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/models.R
\name{models}
\alias{models}
\title{Models used in the PandemicLP package}
\description{
This document explains the models used in the PandemicLP package in some
detail.
}
\section{Growth curve for the single wave mean cases}{

The count data for number of cases or deaths is modeled according to an
epidemiological model of growth. In particular, the average counts are
\eqn{\mu(t)} modeled with a generalized logistic curve:
\deqn{\mu(t) = a c f \frac{e^{-c t}}{(b+e^{-c t})^{f+1}}.}
All parameters, that is \eqn{a, b, c} and \eqn{f} are positive.

Parameter \eqn{c} is interpreted as the infection rate. Parameter \eqn{f}
controls the asymmetry, so if it is equal to 1, then the curve is symmetric.
If it is lesser than 1, then the cases grow slower before the peak than they
decrease after. The behavior is inverted when \eqn{f} is greater than 1.

The counts for the Covid-19 pandemic typically had a behavior with positive
asymmetry, and so the default for the package functions is to use a greater
than 1 truncation for \eqn{f}.

It was common in the early stages of the Covid-19 pandemic that the
predictions would result in very high and absurd values for the total number
of cases (TNC). It is straightforward to show that
\deqn{TNC = \frac{a}{b^f}.}
Since all locations displayed a total number of cases that never exceeded
5\% of that location's population, another truncation is applied, so that
\eqn{a\le b^f 0.08 Pop}, where \eqn{Pop} is the location's population. This
is the reason why the model requires the region's population in order to
run the model estimation.
}

\section{Probabilistic model}{

The simplest probabilistic model for the counts is the Poisson model.
If \eqn{y_t} is the count at time \eqn{t}, then
\deqn{y_t | \theta \sim Poisson(\mu(t)),} where \eqn{\theta} represents the
model parameters.
}

\section{Advanced modeling}{

Here we present some other forms for the growth curve in the mean. The
respective parameters can be adjusted in the \code{\link{pandemic_model}}
function.
\subsection{Seasonal effects}{
A weekly seasonal effect can be added. This is done by multiplying
\eqn{\mu(t)} by a positive effect \eqn{d} when \eqn{t} is the desired
weekday. If \eqn{d < 1} then that weekday represents under-reporting. It is
over-reporting if \eqn{d > 1}. Currently, only weekdays are accepted as
seasonal effects.}
\subsection{Multiple curves}{
Additionally, two or more curves can be fitted, as happened in the Covid-19
pandemic in many locations. In this case the model is slightly different. In
this case,
\deqn{\mu(t) = \mu_1(t)+...+\mu_K(t)}
\deqn{\mu_j(t) = a_j c_j \frac{e^{-c_j t}}{(b_j+e^{-c_j t})^2}\Phi(\alpha_j (t-\delta_j)), j = 1, ..., K,}
where \eqn{\Phi(.)} is the probit function. The probit function induces
asymmetry in the curve, similarly to parameter \eqn{f}, which is thus
excluded in this case.}
\subsection{Negative Binomial}{
In addition to the Poisson family, it is possible to fit a Negative Binomial
model. The model is parameterized so that the overdispersion does not depend
on the mean. This particular parameterization has shown best results when
combined with the multiple waves and seasonal effects described above. The
model is
\deqn{y_t | \lambda_t \sim Poisson(\lambda_t)}
\deqn{\lambda_t | \theta \sim Gamma(\phi \mu(t), \phi)}.
}
}

\section{Prior distribution}{

Apart from the truncation mentioned above, the prior is defined as
independent priors, detailed below. The format is as follows.
\eqn{p\sim D(h1, h2): def1, def2}, where \eqn{p} is the parameter, \eqn{D}
is the distribution family, \eqn{h1} and \eqn{h2} are the hyperparameter
encoding such that they can be changed in the \code{prior_parameters}
argument of the \code{\link{pandemic_model}} function. Finally, \eqn{def1}
and \eqn{def2} are the default values if they are not changed by the user.
Note that every available model used in the \code{\link{pandemic_model}}
function uses only a subset of these parameters. The parameterization of the
distributions is such that the values are passed directly to the \code{stan}
code.
\deqn{a_j\sim Gamma(a_alpha, a_beta), j = 1, ..., K: 0.1, 0.1}
\deqn{b_j\sim LogNormal(mu_{b_1}, sigma2_{b_1}), j = 1, ..., K: 0, 20}
\deqn{c_j\sim Gamma(c_alpha, c_beta), j = 1, ..., K: 2, 9}
\deqn{f\sim Gamma(f_alpha, f_beta): 0.01, 0.01}
\deqn{d_j\sim Gamma(d_{j_alpha}, d_{j_beta}), j = 1, 2, 3: 2, 1}
\deqn{\delta_j\sim Normal(mu_delta, sigma2_delta), j = 1, ..., K: 0, 100}
\deqn{\alpha_j\sim Gamma(alpha_alpha, alpha_beta), j = 1, ..., K: 0.01, 0.01}
\deqn{\phi\sim Gamma(phi_alpha, phi_beta): 0.1, 0.1}
Note that the prior for waves parameters are the same for all waves. However,
it is possible to use a specific prior for each seasonal effect. For example,
if the user wants to change the \code{mu_b_1} and \code{d_2_beta} for a model
with at least two seasonal effects, they would include the argument
\code{prior_parameters = list(mu_b_1 = 1, d_2_beta = 0.001)} in the
\code{\link{pandemic_model}} function.
}

\section{Options for the \code{\link{pandemic_model}} function}{

Four arguments in the function change the fitted model, as described below:
\itemize{
  \item 'seasonal_effect': By leaving this argument \code{NULL}, the standard
  model is fitted. By supplying it with a vector of up to three weekdays, the
  desired seasonal effects are added to the model.
  \item 'n_waves': By leaving this argument equal to 1, the standard model is
  fitted. By changing it to 2 or more implies a multiple waves model.
  \item 'family': The standard model is fitted with the default value of
  "poison". When changed to "negbin", the negative binomialm model is used.
  \item 'prior_parameters': If left as \code{NULL}, the default prior values
  are used. By setting a \code{list} with any objects as described above,
  the provided values will be used.}
}

\references{
Dani Gamerman, Marcos O. Prates, Thais Paiva and Vinicius D. Mayrink (2021).
Building a Platform for Data-Driven Pandemic Prediction: From Data Modelling
to Visualisation - The CovidLP Project. CRC Press

URL: \url{http://est.ufmg.br/covidlp/home/en/}
}
\seealso{
\code{\link{pandemic_model}} and
\code{\link{posterior_predict.pandemicEstimated}}.
}
