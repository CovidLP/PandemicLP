// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <RcppEigen.h>
#include <Rcpp.h>

using namespace Rcpp;

// generatePredictedPoints_pandemicC
List generatePredictedPoints_pandemicC(R_xlen_t M, DataFrame chains, R_xlen_t horizon, double NA_rep, String model_name, R_xlen_t final_time, R_xlen_t nWaves, List s_code);
RcppExport SEXP _PandemicLP_generatePredictedPoints_pandemicC(SEXP MSEXP, SEXP chainsSEXP, SEXP horizonSEXP, SEXP NA_repSEXP, SEXP model_nameSEXP, SEXP final_timeSEXP, SEXP nWavesSEXP, SEXP s_codeSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< R_xlen_t >::type M(MSEXP);
    Rcpp::traits::input_parameter< DataFrame >::type chains(chainsSEXP);
    Rcpp::traits::input_parameter< R_xlen_t >::type horizon(horizonSEXP);
    Rcpp::traits::input_parameter< double >::type NA_rep(NA_repSEXP);
    Rcpp::traits::input_parameter< String >::type model_name(model_nameSEXP);
    Rcpp::traits::input_parameter< R_xlen_t >::type final_time(final_timeSEXP);
    Rcpp::traits::input_parameter< R_xlen_t >::type nWaves(nWavesSEXP);
    Rcpp::traits::input_parameter< List >::type s_code(s_codeSEXP);
    rcpp_result_gen = Rcpp::wrap(generatePredictedPoints_pandemicC(M, chains, horizon, NA_rep, model_name, final_time, nWaves, s_code));
    return rcpp_result_gen;
END_RCPP
}

RcppExport SEXP _rcpp_module_boot_stan_fit4multiwaves_mod();
RcppExport SEXP _rcpp_module_boot_stan_fit4pandemicModels_SeasonalsingleWave_poisson_mod();
RcppExport SEXP _rcpp_module_boot_stan_fit4pandemicModels_multiWave_negbin_mod();
RcppExport SEXP _rcpp_module_boot_stan_fit4pandemicModels_multiWave_poisson_mod();
RcppExport SEXP _rcpp_module_boot_stan_fit4pandemicModels_singleWave_negbin_mod();
RcppExport SEXP _rcpp_module_boot_stan_fit4pandemicModels_singleWave_poisson_mod();
RcppExport SEXP _rcpp_module_boot_stan_fit4poisson_static_generalized_logistic_mod();
RcppExport SEXP _rcpp_module_boot_stan_fit4seasonal_mod();

static const R_CallMethodDef CallEntries[] = {
    {"_PandemicLP_generatePredictedPoints_pandemicC", (DL_FUNC) &_PandemicLP_generatePredictedPoints_pandemicC, 8},
    {"_rcpp_module_boot_stan_fit4multiwaves_mod", (DL_FUNC) &_rcpp_module_boot_stan_fit4multiwaves_mod, 0},
    {"_rcpp_module_boot_stan_fit4pandemicModels_SeasonalsingleWave_poisson_mod", (DL_FUNC) &_rcpp_module_boot_stan_fit4pandemicModels_SeasonalsingleWave_poisson_mod, 0},
    {"_rcpp_module_boot_stan_fit4pandemicModels_multiWave_negbin_mod", (DL_FUNC) &_rcpp_module_boot_stan_fit4pandemicModels_multiWave_negbin_mod, 0},
    {"_rcpp_module_boot_stan_fit4pandemicModels_multiWave_poisson_mod", (DL_FUNC) &_rcpp_module_boot_stan_fit4pandemicModels_multiWave_poisson_mod, 0},
    {"_rcpp_module_boot_stan_fit4pandemicModels_singleWave_negbin_mod", (DL_FUNC) &_rcpp_module_boot_stan_fit4pandemicModels_singleWave_negbin_mod, 0},
    {"_rcpp_module_boot_stan_fit4pandemicModels_singleWave_poisson_mod", (DL_FUNC) &_rcpp_module_boot_stan_fit4pandemicModels_singleWave_poisson_mod, 0},
    {"_rcpp_module_boot_stan_fit4poisson_static_generalized_logistic_mod", (DL_FUNC) &_rcpp_module_boot_stan_fit4poisson_static_generalized_logistic_mod, 0},
    {"_rcpp_module_boot_stan_fit4seasonal_mod", (DL_FUNC) &_rcpp_module_boot_stan_fit4seasonal_mod, 0},
    {NULL, NULL, 0}
};

RcppExport void R_init_PandemicLP(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
