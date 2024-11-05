#pragma once

#include <unordered_map>
#include <unordered_set>

#include <Rcpp.h>

Rcpp::NumericVector rcpp_bm25 (const Rcpp::DataFrame &idfs, const Rcpp::List &tokensList, Rcpp::DataFrame &these_tokens, const double ntoks_avg);
