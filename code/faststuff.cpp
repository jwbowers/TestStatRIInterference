#include <RcppArmadillo.h>

// [[Rcpp::depends(RcppArmadillo)]]

using namespace Rcpp;
using namespace arma;

// from https://stackoverflow.com/questions/29851637/efficiently-perform-row-wise-distribution-test
double KS(arma::colvec x, arma::colvec y) {
	int n = x.n_rows;
	arma::colvec w = join_cols(x, y);
	arma::uvec z = arma::sort_index(w);
	w.fill(-1); w.elem( find(z <= n-1) ).ones();
	return max(abs(cumsum(w)))/n;
}
// [[Rcpp::export]]
Rcpp::NumericVector K_S(arma::mat mt) {
	int n = mt.n_cols;
	Rcpp::NumericVector results(n);
	for (int i=1; i<n;i++) {
		arma::colvec x=mt.col(i-1);
		arma::colvec y=mt.col(i);
		results[i] = KS(x, y);
	}
	return results;
}

// [[Rcpp::export]]
arma::rowvec fastcolMeans(const arma::mat & X){
	arma::rowvec out = arma::mean(X,0); //Armadillo mean(X,dim)
	return(out);
}


// [[Rcpp::export]]
mat fastcolSums(const mat & X){
	arma::mat out = arma::sum(X,0); //Armadillo mean(X,dim)
	return(out);
}

// [[Rcpp::export]]
mat fastcolVars(const mat & X){
	arma::mat out = arma::var(X,0); //Armadillo var(X,dim),0=col, 1=row
	return(out);
}

// [[Rcpp::export]]
vec fastrowMeans(const arma::mat & X){
	arma::vec out = arma::mean(X,1); //Armadillo mean(X,dim)
	return(out);
}


// [[Rcpp::export]]
vec fastrowSums(const mat & X){
	arma::vec out = arma::sum(X,1); //Armadillo mean(X,dim)
	return(out);
}

// [[Rcpp::export]]
vec fastrowSds(const arma::mat & X){
	arma::vec out = arma::stddev(X,0,1); //Armadillo stddev(X,norm_type=0,dim)
	return(out);
}

// [[Rcpp::export]]
mat fastcolSds(const arma::mat & X){
	arma::mat out = arma::stddev(X,0,0); //Armadillo stddev(X,norm_type=0,dim)
	return(out);
}
