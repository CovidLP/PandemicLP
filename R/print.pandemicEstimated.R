#' Print method for \code{pandemicEstimated} objects
#'
#' The print method for \code{pandemicEstimated} object of class S3 displays a compact summary of the
#' fitted model. See the \strong{Details} section below for descriptions of the different components of the printed
#' output. \cr
#' \cr
#' The \code{fit} component of the \code{\link{pandemicEstimated-objects}} object generated
#' by the \code{\link{pandemic_model}} function can be summarised to provide more information.
#' Check \code{?'summary,stanfit-method'} for additional summary statistics and diagnostics.
#'
#' @method print pandemicEstimated
#' @templateVar pandemicEstimatedArg x
#' @param x An object of S3 class \code{\link{pandemicEstimated-objects}}.
#' @param digits Number of digits to use for formatting numbers.
#' @param ... Currently unused.
#' @return Returns \code{x}, invisibly.
#' @details
#' \subsection{Convergence and efficiency diagnostics for Markov Chains}{
#'
#' Included in the summary are: split effective sample sizes (n_eff), Monte Carlo standard errors
#' (se_mean) and split Rhats.\cr
#' \cr
#' The Monte Carlo standard error provides relevant information for a posterior sample with more than one chain.\cr
#' \cr
#' The R-hat convergence diagnostic compares the
#' between- and within-chain estimates for model parameters and other univariate
#' quantities of interest. If chains have not mixed well (ie, the between- and
#' within-chain estimates don't agree), R-hat is larger than 1.
#' We recommend running at least four chains by default and only using the
#' sample if R-hat is less than 1.05.
#'
#' }
#' \subsection{covidLPconfig}{
#' This subsection shows the main input settings used by the fitted model, and indicates whether default settings
#' of the CovidLP app (\url{http://est.ufmg.br/covidlp/home/en/})
#' were used (\code{covidLPconfig = TRUE} or \code{FALSE}).
#' Check the default settings of the CovidLP app in \code{\link{pandemic_model}}.
#' }
#' \subsection{Priors}{
#'
#' A list with information about the prior distributions used and model restrictions (if there are any).
#' In future versions of the package \pkg{PandemicLP}, the user will be allowed to modify the priors.
#' For more information, see the \strong{Details} section of \code{\link{pandemic_model}}.
#'
#'
#' }
#'
#' @seealso \code{\link{'summary,stanfit-method`}}
#'
#' @importMethodsFrom rstan summary
#'
#' @export

print.pandemicEstimated=function(x,digits=3,...){

  cat("pandemic_model")
  cat("\n Distribution: ", "poisson")
  cat("\n Model:        ", "static generalized logistic (d=0)")
  cat("\n Type of Case: ", x$cases.type)
  cat("\n Location:     ", x$Y$name)
  cat("\n 0bservations: ", nrow(x$Y$data),"\n")

  cat("\n------\n")
  cat("Parameters:\n")
  tab=rstan::summary(x$fit,pars=c("a","b","c","f"),probs=c(0.025,0.5,0.975))$summary
  tab=round(tab,digits)
  print(tab)
  #cat("\n")
  tab1=rstan::summary(x$fit,pars="mu",probs=c(0.025,0.5,0.975))$summary
  takeout=which(colnames(tab1)=="n_eff" | colnames(tab1)=="Rhat" | colnames(tab1)=="se_mean") #convergence diagnosis does not make sense for mu
  tab1=tab1[,-takeout] #takeout convergence statistics for mu
  tab1=round(tab1,digits)
  cat("\nFitted values:\n")
  rownames(tab1)[1:3]=c("mu[1] ","mu[2] ","mu[3] ")  #gambiarra para print dos mu[t] ficar alinhado
  print(head(tab1,n=3))
  colnames(tab1)=rep("...",length(colnames(tab1)))
  print(tail(tab1,n=3))                              #gambiarra para printar só alguns mu[t]

  cat("\n------\n")
  cat("covidLPconfig = ", x$config.inputs$covidLPconfig,":\n")
  cat("\n warmup:                           ", x$config.inputs$use_inputs$warmup)
  cat("\n thin:                             ", x$config.inputs$use_inputs$thin)
  cat("\n sample_size:                      ", x$config.inputs$use_inputs$sample_size)
  cat("\n chains:                           ", x$config.inputs$use_inputs$number_chains)
  cat("\n maximum total number of cases:    ", x$config.inputs$use_inputs$p,"*population")
  #cat("\n init:                           ", x$config.inputs$use_inputs$init,"\n")
  #cat("\n init:                           ", x[["fit"]]@stan_args[[1]][[6]],"\n")
  init = data.frame(x$config.inputs$use_inputs$init[[1]][1],x$config.inputs$use_inputs$init[[1]][2],
              x$config.inputs$use_inputs$init[[1]][3],x$config.inputs$use_inputs$init[[1]][4])
  init = as.numeric(init[,order(colnames(init))])
  cat("\n init (chain_id = 1):               a = ",init[1],", b = ",init[2],", c = ",init[3],", f = ",init[4],"\n")


  cat("\n------\n")
  cat("Priors:\n")
  cat("\n a ","~ ","Gamma(0.1, 0.1)")
  cat("\n b ","~ ","LogNormal(0, 20)")
  cat("\n c ","~ ","Gamma(2, 9)")
  cat("\n f ","~ ","Gamma(0.01, 0.01)\n")

  cat("\nRestrictions:")
  cat("\n 1: ", "a/b^f","<",x$config.inputs$use_inputs$p,"*population")
  cat("\n 2: ", "f > 1\n")


  cat("\n------\n")
  cat("*For help interpreting the printed output see ?print.pandemicEstimated\n")
  cat("*For more information see ?'summary,stanfit-method'\n")
  cat("*For details on the model, priors and restrictions, see the Details section in ?pandemic_model\n")
  #cat("**a/b^f represents the assymptote of the cumulative cases curve")

  invisible(x)
}
