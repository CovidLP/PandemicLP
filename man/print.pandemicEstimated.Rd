% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/print.pandemicEstimated.R
\name{print.pandemicEstimated}
\alias{print.pandemicEstimated}
\title{Print method for \code{pandemicEstimated} objects}
\usage{
\method{print}{pandemicEstimated}(x, digits = 3, probs = c(0.025, 0.5, 0.975), info = TRUE, ...)
}
\arguments{
\item{x}{an object of S3 class \code{\link{pandemicEstimated-objects}}.}

\item{digits}{Number of digits to use for formatting numbers.}

\item{probs}{a numeric vector of quantiles of interest. The default is
\code{c(0.025,0.5,0.975)}.}

\item{info}{TRUE or FALSE: more details for output interpretation. The Default is TRUE.}

\item{...}{currently unused.}
}
\value{
Returns \code{x}, invisibly.
}
\description{
The print method for \code{pandemicEstimated} object of class S3 displays a compact summary of the
fitted model. See the \strong{Details} section below for descriptions of the different components of the printed
output.  For additional summary statistics and diagnostics use \code{\link{summary.pandemicEstimated}}.
}
\details{
\subsection{Point estimates}{
Regardless of the estimation algorithm, point estimates are mean and (or) quantiles computed from simulations.
For models fit using MCMC ("sampling", this is default algorithim of \code{pandemic_model} function),  the posterior sample
is used. For others estimation algorithm see \code{\link[rstan]{sampling}}  (\pkg{rstan} package).

}

\subsection{Convergence and efficiency diagnostics for Markov Chains}{

Included in the print are: split effective sample sizes (n_eff) and split Rhats.


The R-hat convergence diagnostic compares the
between- and within-chain estimates for model parameters and other univariate
quantities of interest. If chains have not mixed well (ie, the between- and
within-chain estimates don't agree), R-hat is larger than 1.
We recommend running at least four chains by default and only using the
sample if R-hat is less than 1.05.

}

\subsection{Priors}{

A list with information about the prior distributions used and model restrictions (if there are any).
For more information go to \code{\link{models}}.


}
}
\seealso{
\code{\link{summary.pandemicEstimated}}.
}
