// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <RcppArmadillo.h>
#include <Rcpp.h>

using namespace Rcpp;

// distributeCpp
List distributeCpp(double flow, NumericVector soc, NumericVector vol, NumericVector share, NumericVector level, LogicalVector active, NumericVector eff, NumericVector cap);
RcppExport SEXP _eflows_distributeCpp(SEXP flowSEXP, SEXP socSEXP, SEXP volSEXP, SEXP shareSEXP, SEXP levelSEXP, SEXP activeSEXP, SEXP effSEXP, SEXP capSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< double >::type flow(flowSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type soc(socSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type vol(volSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type share(shareSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type level(levelSEXP);
    Rcpp::traits::input_parameter< LogicalVector >::type active(activeSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type eff(effSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type cap(capSEXP);
    rcpp_result_gen = Rcpp::wrap(distributeCpp(flow, soc, vol, share, level, active, eff, cap));
    return rcpp_result_gen;
END_RCPP
}
// formatFlexSteps
NumericMatrix formatFlexSteps(NumericMatrix matrix, IntegerVector flex_step, int max_step);
RcppExport SEXP _eflows_formatFlexSteps(SEXP matrixSEXP, SEXP flex_stepSEXP, SEXP max_stepSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericMatrix >::type matrix(matrixSEXP);
    Rcpp::traits::input_parameter< IntegerVector >::type flex_step(flex_stepSEXP);
    Rcpp::traits::input_parameter< int >::type max_step(max_stepSEXP);
    rcpp_result_gen = Rcpp::wrap(formatFlexSteps(matrix, flex_step, max_step));
    return rcpp_result_gen;
END_RCPP
}
// divideInChunks
NumericVector divideInChunks(float x, float precision);
RcppExport SEXP _eflows_divideInChunks(SEXP xSEXP, SEXP precisionSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< float >::type x(xSEXP);
    Rcpp::traits::input_parameter< float >::type precision(precisionSEXP);
    rcpp_result_gen = Rcpp::wrap(divideInChunks(x, precision));
    return rcpp_result_gen;
END_RCPP
}
// whichMin
int whichMin(NumericVector x);
RcppExport SEXP _eflows_whichMin(SEXP xSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericVector >::type x(xSEXP);
    rcpp_result_gen = Rcpp::wrap(whichMin(x));
    return rcpp_result_gen;
END_RCPP
}
// sliceCurrent
NumericVector sliceCurrent(NumericVector vec, int start, int end);
RcppExport SEXP _eflows_sliceCurrent(SEXP vecSEXP, SEXP startSEXP, SEXP endSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericVector >::type vec(vecSEXP);
    Rcpp::traits::input_parameter< int >::type start(startSEXP);
    Rcpp::traits::input_parameter< int >::type end(endSEXP);
    rcpp_result_gen = Rcpp::wrap(sliceCurrent(vec, start, end));
    return rcpp_result_gen;
END_RCPP
}
// envCurrent
Environment envCurrent(Environment input, Environment out, int start, int span);
RcppExport SEXP _eflows_envCurrent(SEXP inputSEXP, SEXP outSEXP, SEXP startSEXP, SEXP spanSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Environment >::type input(inputSEXP);
    Rcpp::traits::input_parameter< Environment >::type out(outSEXP);
    Rcpp::traits::input_parameter< int >::type start(startSEXP);
    Rcpp::traits::input_parameter< int >::type span(spanSEXP);
    rcpp_result_gen = Rcpp::wrap(envCurrent(input, out, start, span));
    return rcpp_result_gen;
END_RCPP
}
// listToCube
arma::cube listToCube(List mtx_list);
RcppExport SEXP _eflows_listToCube(SEXP mtx_listSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< List >::type mtx_list(mtx_listSEXP);
    rcpp_result_gen = Rcpp::wrap(listToCube(mtx_list));
    return rcpp_result_gen;
END_RCPP
}
// cubeToList
List cubeToList(arma::cube xcube);
RcppExport SEXP _eflows_cubeToList(SEXP xcubeSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< arma::cube >::type xcube(xcubeSEXP);
    rcpp_result_gen = Rcpp::wrap(cubeToList(xcube));
    return rcpp_result_gen;
END_RCPP
}
// foreShiftCpp
List foreShiftCpp(List mtx_list, NumericVector cap_charge, Environment env_fit, Language call_fit, Environment env_aux, Language call_aux);
RcppExport SEXP _eflows_foreShiftCpp(SEXP mtx_listSEXP, SEXP cap_chargeSEXP, SEXP env_fitSEXP, SEXP call_fitSEXP, SEXP env_auxSEXP, SEXP call_auxSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< List >::type mtx_list(mtx_listSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type cap_charge(cap_chargeSEXP);
    Rcpp::traits::input_parameter< Environment >::type env_fit(env_fitSEXP);
    Rcpp::traits::input_parameter< Language >::type call_fit(call_fitSEXP);
    Rcpp::traits::input_parameter< Environment >::type env_aux(env_auxSEXP);
    Rcpp::traits::input_parameter< Language >::type call_aux(call_auxSEXP);
    rcpp_result_gen = Rcpp::wrap(foreShiftCpp(mtx_list, cap_charge, env_fit, call_fit, env_aux, call_aux));
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_eflows_distributeCpp", (DL_FUNC) &_eflows_distributeCpp, 8},
    {"_eflows_formatFlexSteps", (DL_FUNC) &_eflows_formatFlexSteps, 3},
    {"_eflows_divideInChunks", (DL_FUNC) &_eflows_divideInChunks, 2},
    {"_eflows_whichMin", (DL_FUNC) &_eflows_whichMin, 1},
    {"_eflows_sliceCurrent", (DL_FUNC) &_eflows_sliceCurrent, 3},
    {"_eflows_envCurrent", (DL_FUNC) &_eflows_envCurrent, 4},
    {"_eflows_listToCube", (DL_FUNC) &_eflows_listToCube, 1},
    {"_eflows_cubeToList", (DL_FUNC) &_eflows_cubeToList, 1},
    {"_eflows_foreShiftCpp", (DL_FUNC) &_eflows_foreShiftCpp, 6},
    {NULL, NULL, 0}
};

RcppExport void R_init_eflows(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
