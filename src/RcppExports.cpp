// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <Rcpp.h>

using namespace Rcpp;

#ifdef RCPP_USE_GLOBAL_ROSTREAM
Rcpp::Rostream<true>&  Rcpp::Rcout = Rcpp::Rcpp_cout_get();
Rcpp::Rostream<false>& Rcpp::Rcerr = Rcpp::Rcpp_cerr_get();
#endif

// proximalGraph
void proximalGraph(Rcpp::NumericVector& U, int& p, std::string& regul, Rcpp::NumericMatrix& grp, Rcpp::NumericMatrix& grpV, Rcpp::NumericVector& etaG, double lam);
RcppExport SEXP _netcox_proximalGraph(SEXP USEXP, SEXP pSEXP, SEXP regulSEXP, SEXP grpSEXP, SEXP grpVSEXP, SEXP etaGSEXP, SEXP lamSEXP) {
BEGIN_RCPP
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::NumericVector& >::type U(USEXP);
    Rcpp::traits::input_parameter< int& >::type p(pSEXP);
    Rcpp::traits::input_parameter< std::string& >::type regul(regulSEXP);
    Rcpp::traits::input_parameter< Rcpp::NumericMatrix& >::type grp(grpSEXP);
    Rcpp::traits::input_parameter< Rcpp::NumericMatrix& >::type grpV(grpVSEXP);
    Rcpp::traits::input_parameter< Rcpp::NumericVector& >::type etaG(etaGSEXP);
    Rcpp::traits::input_parameter< double >::type lam(lamSEXP);
    proximalGraph(U, p, regul, grp, grpV, etaG, lam);
    return R_NilValue;
END_RCPP
}
// netcox_cpp
Rcpp::List netcox_cpp(Rcpp::NumericMatrix& x, Rcpp::NumericVector& start, Rcpp::NumericVector& stop, Rcpp::NumericVector& event, int& n_unique, std::string& regul, Rcpp::NumericVector& lam, Rcpp::NumericMatrix& grp, Rcpp::NumericMatrix& grpV, Rcpp::NumericVector& etaG, Rcpp::NumericVector& init, Rcpp::Function& l_ld, double& init_stepsize, double& ls_shrink, double& partol, int& maxit, bool& verbose);
RcppExport SEXP _netcox_netcox_cpp(SEXP xSEXP, SEXP startSEXP, SEXP stopSEXP, SEXP eventSEXP, SEXP n_uniqueSEXP, SEXP regulSEXP, SEXP lamSEXP, SEXP grpSEXP, SEXP grpVSEXP, SEXP etaGSEXP, SEXP initSEXP, SEXP l_ldSEXP, SEXP init_stepsizeSEXP, SEXP ls_shrinkSEXP, SEXP partolSEXP, SEXP maxitSEXP, SEXP verboseSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::NumericMatrix& >::type x(xSEXP);
    Rcpp::traits::input_parameter< Rcpp::NumericVector& >::type start(startSEXP);
    Rcpp::traits::input_parameter< Rcpp::NumericVector& >::type stop(stopSEXP);
    Rcpp::traits::input_parameter< Rcpp::NumericVector& >::type event(eventSEXP);
    Rcpp::traits::input_parameter< int& >::type n_unique(n_uniqueSEXP);
    Rcpp::traits::input_parameter< std::string& >::type regul(regulSEXP);
    Rcpp::traits::input_parameter< Rcpp::NumericVector& >::type lam(lamSEXP);
    Rcpp::traits::input_parameter< Rcpp::NumericMatrix& >::type grp(grpSEXP);
    Rcpp::traits::input_parameter< Rcpp::NumericMatrix& >::type grpV(grpVSEXP);
    Rcpp::traits::input_parameter< Rcpp::NumericVector& >::type etaG(etaGSEXP);
    Rcpp::traits::input_parameter< Rcpp::NumericVector& >::type init(initSEXP);
    Rcpp::traits::input_parameter< Rcpp::Function& >::type l_ld(l_ldSEXP);
    Rcpp::traits::input_parameter< double& >::type init_stepsize(init_stepsizeSEXP);
    Rcpp::traits::input_parameter< double& >::type ls_shrink(ls_shrinkSEXP);
    Rcpp::traits::input_parameter< double& >::type partol(partolSEXP);
    Rcpp::traits::input_parameter< int& >::type maxit(maxitSEXP);
    Rcpp::traits::input_parameter< bool& >::type verbose(verboseSEXP);
    rcpp_result_gen = Rcpp::wrap(netcox_cpp(x, start, stop, event, n_unique, regul, lam, grp, grpV, etaG, init, l_ld, init_stepsize, ls_shrink, partol, maxit, verbose));
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_netcox_proximalGraph", (DL_FUNC) &_netcox_proximalGraph, 7},
    {"_netcox_netcox_cpp", (DL_FUNC) &_netcox_netcox_cpp, 17},
    {NULL, NULL, 0}
};

RcppExport void R_init_netcox(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
