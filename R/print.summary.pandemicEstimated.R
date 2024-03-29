#' Summary method for \code{pandemicEstimated} objects
#' @rdname summary.pandemicEstimated
#' @method print summary.pandemicEstimated
#'
#' @param x an object of class \code{"summary.pandemicEstimated"}.
#' @param ... currently unused.
#' @importFrom utils head
#' @importFrom utils tail
#' @export

print.summary.pandemicEstimated=function(x,...){


  if(x$n_waves==1){
    name="static generalized logistic"   # name="static generalized logistic"
  } else {
    name=paste0("multi_waves(",x$n_waves,")")      #multiwaves
  }

  cat("pandemic_model")
  cat("\n Family:             ", x$family)
  cat("\n Mean function form: ", name)
  cat("\n Type of Case:       ", x$cases.type)
  cat("\n Location:           ", x$location)
  if(!is.null(x$seasonal_effect)){
  cat("\n Seasonal effect:    ", paste0(x$seasonal_effect,"(","d_",1:length(x$seasonal_effect),")" ) )
  }
  cat("\n 0bservations:       ", x$n,"\n")

  cat("\n------\n")
  cat("Parameters:\n")
  tab=round(x$tab.parameters,x$print.digits)
  print(tab)
  #cat("\n")

  cat("\nFitted values:\n")
  tab1=round(x$tab.fitted_values,x$print.digits)
  rownames(tab1)[1:3]=c("mu[1] ","mu[2] ","mu[3] ")  #gambiarra para print dos mu[t] ficar alinhado
  print(utils::head(tab1,n=3))
  colnames(tab1)=rep("...",length(colnames(tab1)))
  print(utils::tail(tab1,n=3))                              #gambiarra para printar só alguns mu[t]

  cat("\n------\n")
  cat("covidLPconfig = ", x$covidLPconfig,":\n")
  cat("\n warmup:                           ", x$warmup)
  cat("\n thin:                             ", x$thin)
  cat("\n sample_size:                      ", x$sample_size)
  cat("\n chains:                           ", x$number_chains)
  cat("\n maximum total number of cases:    ", x$p,"*population")
  cat("\n init (chain_id = 1):\n")

  print(x$init1)   #show only chain_id=1


  if(is.null(x$seasonal_effect)){

    if(x$n_waves==1) {   #gen logistic
      cat("\n------\n")
      cat("Priors:\n")
      if(x$family=="negbin"){
        cat("\n phi","~ ","Gamma(0.1, 0.1)")
      }
      cat("\n a  ","~ ","Gamma(0.1, 0.1)")
      cat("\n b  ","~ ","LogNormal(0, 20)")
      cat("\n c  ","~ ","Gamma(2, 9)")
      cat("\n f  ","~ ","Gamma(0.01, 0.01)\n")
    } else {

      cat("\n------\n")
      cat("Priors:\n")
      if(x$family=="negbin"){
        cat("\n phi     ","~ ","Gamma(0.1, 0.1)")
      }
      cat("\n a_i     ","~ ","Gamma(0.1, 0.1)")
      cat("\n alpha_i ","~ ","Gamma(0.01, 0.01)")
      cat("\n b_i     ","~ ","LogNormal(0, 20)")
      cat("\n c_i     ","~ ","Gamma(2, 9)")
      cat("\n delta_i ","~ ","Normal(0, 100)\n")
    }


  } else {

    if(x$n_waves==1){
      cat("\n------\n")
      cat("Priors:\n")
      if(x$family=="negbin"){
        cat("\n phi ","~ ","Gamma(0.1, 0.1)")
      }
      cat("\n a   ","~ ","Gamma(0.1, 0.1)")
      cat("\n b   ","~ ","LogNormal(0, 20)")
      cat("\n c   ","~ ","Gamma(2, 9)")
      cat("\n d_i ","~ ","Gamma(2,1)")
      cat("\n f   ","~ ","Gamma(0.01, 0.01)\n")
    } else {

      cat("\n------\n")
      cat("Priors:\n")
      if(x$family=="negbin"){
        cat("\n phi     ","~ ","Gamma(0.1, 0.1)")
      }
      cat("\n a_i     ","~ ","Gamma(0.1, 0.1)")
      cat("\n alpha_i ","~ ","Gamma(0.01, 0.01)")
      cat("\n b_i     ","~ ","LogNormal(0, 20)")
      cat("\n c_i     ","~ ","Gamma(2, 9)")
      cat("\n d_i     ","~ ","Gamma(2,1)")
      cat("\n delta_i ","~ ","Normal(0, 100)\n")

    }

  }


  if(x$n_waves==1){              #gen logistic ou gen logistic
    cat("\nRestrictions:")
    cat("\n 1: ", "a/b^f","<",x$p,"*population")
    if(x$family=="negbin"){
      cat("\n 2: ", "f >",x$fTrunc)
      cat("\n 3: ", "phi >",x$phiTrunc)
    } else {
      cat("\n 2: ", "f > 1\n")
    }

  } else {            #multiwaves
    cat("\nRestrictions:")
    cat("\n 1: ", "a_i/b_i","<",x$p,"*population")
    cat("\n 2: ", "alpha_i > 0")
    if(x$family=="negbin"){
    cat("\n 3: ","phi",">",x$phiTrunc)
    }
  }


  if(x$info){
  cat("\n------\n")
  cat("*For help interpreting the printed output see ?summary.pandemicEstimated\n")
  cat("*For more information see ?'summary,stanfit-method'\n")
  cat("*For details on the model, priors and restrictions, see ?models\n")
  #cat("**a/b^f represents the assymptote of the cumulative cases curve")
  }

  invisible(x)

}


