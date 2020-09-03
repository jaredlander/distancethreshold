// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include "../inst/include/distancethreshold.h"
#include <RcppArmadillo.h>
#include <Rcpp.h>
#include <string>
#include <set>

using namespace Rcpp;

// threshold_distance
List threshold_distance(DataFrame obj, double threshold, String x_col, String y_col, String id_col);
static SEXP _distancethreshold_threshold_distance_try(SEXP objSEXP, SEXP thresholdSEXP, SEXP x_colSEXP, SEXP y_colSEXP, SEXP id_colSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::traits::input_parameter< DataFrame >::type obj(objSEXP);
    Rcpp::traits::input_parameter< double >::type threshold(thresholdSEXP);
    Rcpp::traits::input_parameter< String >::type x_col(x_colSEXP);
    Rcpp::traits::input_parameter< String >::type y_col(y_colSEXP);
    Rcpp::traits::input_parameter< String >::type id_col(id_colSEXP);
    rcpp_result_gen = Rcpp::wrap(threshold_distance(obj, threshold, x_col, y_col, id_col));
    return rcpp_result_gen;
END_RCPP_RETURN_ERROR
}
RcppExport SEXP _distancethreshold_threshold_distance(SEXP objSEXP, SEXP thresholdSEXP, SEXP x_colSEXP, SEXP y_colSEXP, SEXP id_colSEXP) {
    SEXP rcpp_result_gen;
    {
        Rcpp::RNGScope rcpp_rngScope_gen;
        rcpp_result_gen = PROTECT(_distancethreshold_threshold_distance_try(objSEXP, thresholdSEXP, x_colSEXP, y_colSEXP, id_colSEXP));
    }
    Rboolean rcpp_isInterrupt_gen = Rf_inherits(rcpp_result_gen, "interrupted-error");
    if (rcpp_isInterrupt_gen) {
        UNPROTECT(1);
        Rf_onintr();
    }
    bool rcpp_isLongjump_gen = Rcpp::internal::isLongjumpSentinel(rcpp_result_gen);
    if (rcpp_isLongjump_gen) {
        Rcpp::internal::resumeJump(rcpp_result_gen);
    }
    Rboolean rcpp_isError_gen = Rf_inherits(rcpp_result_gen, "try-error");
    if (rcpp_isError_gen) {
        SEXP rcpp_msgSEXP_gen = Rf_asChar(rcpp_result_gen);
        UNPROTECT(1);
        Rf_error(CHAR(rcpp_msgSEXP_gen));
    }
    UNPROTECT(1);
    return rcpp_result_gen;
}

// validate (ensure exported C++ functions exist before calling them)
static int _distancethreshold_RcppExport_validate(const char* sig) { 
    static std::set<std::string> signatures;
    if (signatures.empty()) {
        signatures.insert("List(*threshold_distance)(DataFrame,double,String,String,String)");
    }
    return signatures.find(sig) != signatures.end();
}

// registerCCallable (register entry points for exported C++ functions)
RcppExport SEXP _distancethreshold_RcppExport_registerCCallable() { 
    R_RegisterCCallable("distancethreshold", "_distancethreshold_threshold_distance", (DL_FUNC)_distancethreshold_threshold_distance_try);
    R_RegisterCCallable("distancethreshold", "_distancethreshold_RcppExport_validate", (DL_FUNC)_distancethreshold_RcppExport_validate);
    return R_NilValue;
}

static const R_CallMethodDef CallEntries[] = {
    {"_distancethreshold_threshold_distance", (DL_FUNC) &_distancethreshold_threshold_distance, 5},
    {"_distancethreshold_RcppExport_registerCCallable", (DL_FUNC) &_distancethreshold_RcppExport_registerCCallable, 0},
    {NULL, NULL, 0}
};

RcppExport void R_init_distancethreshold(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
