// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <Rcpp.h>

using namespace Rcpp;

// EM_loop
NumericVector EM_loop(double beta_hat, double Z_hat, double sigma_sq_hat, double p_hat, NumericVector port_exc_ret, NumericVector mkt_exc_ret, NumericVector mkt_sigma, double tol, int MaxIter, int criterion, double start_timestamp, double end_timestamp);
RcppExport SEXP _ZCAPM_EM_loop(SEXP beta_hatSEXP, SEXP Z_hatSEXP, SEXP sigma_sq_hatSEXP, SEXP p_hatSEXP, SEXP port_exc_retSEXP, SEXP mkt_exc_retSEXP, SEXP mkt_sigmaSEXP, SEXP tolSEXP, SEXP MaxIterSEXP, SEXP criterionSEXP, SEXP start_timestampSEXP, SEXP end_timestampSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< double >::type beta_hat(beta_hatSEXP);
    Rcpp::traits::input_parameter< double >::type Z_hat(Z_hatSEXP);
    Rcpp::traits::input_parameter< double >::type sigma_sq_hat(sigma_sq_hatSEXP);
    Rcpp::traits::input_parameter< double >::type p_hat(p_hatSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type port_exc_ret(port_exc_retSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type mkt_exc_ret(mkt_exc_retSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type mkt_sigma(mkt_sigmaSEXP);
    Rcpp::traits::input_parameter< double >::type tol(tolSEXP);
    Rcpp::traits::input_parameter< int >::type MaxIter(MaxIterSEXP);
    Rcpp::traits::input_parameter< int >::type criterion(criterionSEXP);
    Rcpp::traits::input_parameter< double >::type start_timestamp(start_timestampSEXP);
    Rcpp::traits::input_parameter< double >::type end_timestamp(end_timestampSEXP);
    rcpp_result_gen = Rcpp::wrap(EM_loop(beta_hat, Z_hat, sigma_sq_hat, p_hat, port_exc_ret, mkt_exc_ret, mkt_sigma, tol, MaxIter, criterion, start_timestamp, end_timestamp));
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_ZCAPM_EM_loop", (DL_FUNC) &_ZCAPM_EM_loop, 12},
    {NULL, NULL, 0}
};

RcppExport void R_init_ZCAPM(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
