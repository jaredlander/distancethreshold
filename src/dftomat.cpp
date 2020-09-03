#include <RcppArmadillo.h>
using namespace Rcpp;
// [[Rcpp::plugins(cpp11)]]
// [[Rcpp::depends(RcppArmadillo)]]

//' @title dftomat
//' @description Converts selected columns of `data.frame` to a `matrix`
//' @md
//' @param obj `data.frame`
//' @param cols `character` `vector` of columns to convert to matrix
//' @export
//' @return A `matrix`
//' thedf <- data.frame(ID=rep(c('A', 'B'), length.out=10), A=sample(10), B=rnorm(10), C=sample(10))
//' dftomat(thedf, cols=c('A'))
//' dftomat(thedf, cols=c('A', 'B'))
//' dftomat(thedf, cols=c('A', 'B', 'C'))
// [[Rcpp::export]]
arma::mat dftomat(DataFrame obj, CharacterVector cols)
{
    // this will be our new matrix
    arma::mat M;

    // add each member of cols to the matrix
    for(String thiscol : cols)
    {
        // need to temporarily store the column as a vector before putting it into the matrix
        arma::vec tempvec = obj[thiscol];
        M = arma::join_rows(M, tempvec);
    }

    return M;
}
