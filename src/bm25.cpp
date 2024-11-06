#include "bm25.h"

void make_idf_map (
    const Rcpp::DataFrame &idfs,
    std::unordered_map <std::string, double> &idf_map) {

    const Rcpp::CharacterVector idf_tokens = idfs ["token"];
    const Rcpp::NumericVector idf_idf = idfs ["idf"];
    for (int i = 0; i < idfs.nrow (); i++) {
        std::string this_tok = static_cast<std::string> (idf_tokens [i]);
        idf_map.emplace (this_tok, idf_idf [i]);
    }
}

void make_these_tokens_map (
    const Rcpp::DataFrame &these_tokens,
    std::unordered_map <std::string, int> &these_tokens_map) {

    const Rcpp::CharacterVector these_tokens_str = these_tokens ["token"];
    const Rcpp::IntegerVector these_tokens_n = these_tokens ["np"];

    for (int i = 0; i < these_tokens.nrow (); i++) {
        const std::string this_string = static_cast <std::string> (these_tokens_str [i]);
        these_tokens_map.emplace (this_string, these_tokens_n [i]);
    }
}

// [[Rcpp::export]]
Rcpp::NumericVector rcpp_bm25 (const Rcpp::DataFrame &idfs, const Rcpp::List &tokensList, Rcpp::DataFrame &these_tokens, const double ntoks_avg) {

    // Fixed parameters used in the BM25 function. See wikipedia reference for
    // these values.
    const double k = 1.2;
    const double b = 0.75;

    // Set up primary 'idf_map' to map all tokens to their IDFs over whole corpus:
    std::unordered_map <std::string, double> idf_map;
    make_idf_map (idfs, idf_map);

    std::unordered_map <std::string, int> these_tokens_map;
    make_these_tokens_map (these_tokens, these_tokens_map);

    const int ndocs = tokensList.size();
    Rcpp::NumericVector bm25 (ndocs, 0.0);

    for (int i = 0; i < ndocs; i++) {

        const Rcpp::DataFrame tokens_i = Rcpp::as <Rcpp::DataFrame> (tokensList [i]);
        if (tokens_i.nrow () == 0L) {
            continue;
        }
        const Rcpp::IntegerVector tokens_n = tokens_i ["n"];
        const Rcpp::CharacterVector tokens_tok = tokens_i ["token"];

        int doc_len_i = 0;
        for (auto n: tokens_n) doc_len_i += n;

        // convert tokensList [[i]]  data.frame to map from token to count, but
        // only for tokens that are in "these_tokens_map"::
        std::unordered_map <std::string, int> doc_tokens_map;
        for (int j = 0; j < tokens_i.nrow (); j++) {
            const std::string tok_str = static_cast <std::string> (tokens_tok [j]);
            if (idf_map.find (tok_str) != idf_map.end() && these_tokens_map.find (tok_str) != these_tokens_map.end ()) {
                doc_tokens_map.emplace (tok_str, tokens_n [j]);
            }
        }

        // Find out where any of 'these_tokens' are in doc_tokens_set
        int n_toks_in_these = 0;
        for (auto tok_iter: these_tokens_map) {
            if (doc_tokens_map.find (tok_iter.first) != doc_tokens_map.end ()) n_toks_in_these++;
        }

        double bm25_i = 0.0;
        if (n_toks_in_these > 0) {
            for (auto tok_iter: doc_tokens_map) {
                const int freq_i = tok_iter.second;
                const double num_i = freq_i * (k + 1);
                const double denom_i = freq_i + k * (1 - b + b * doc_len_i / ntoks_avg);
                const double idf_i = idf_map.find (tok_iter.first) -> second;
                bm25_i += idf_i * num_i / denom_i;
            }
        }
        
        bm25 (i) = bm25_i;
    }

    return bm25;
}
