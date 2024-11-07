#pragma once

#include <unordered_map>
#include <unordered_set>

#include <Rcpp.h>

void make_idf_map (
    const Rcpp::DataFrame &idfs,
    std::unordered_map <std::string, double> &idf_map);
void make_these_tokens_map (
    const Rcpp::DataFrame &these_tokens,
    std::unordered_map <std::string, double> &these_tokens_map);

Rcpp::NumericVector rcpp_bm25 (const Rcpp::DataFrame &idfs, const Rcpp::List &tokensList, Rcpp::DataFrame &these_tokens, const double ntoks_avg);
