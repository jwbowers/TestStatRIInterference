#include <RcppArmadillo.h>

// [[Rcpp::depends(RcppArmadillo)]]

using namespace Rcpp;
using namespace arma;

// [[Rcpp::export]]
double ksFastTestStatistic(arma::colvec y, arma::colvec z){
	uvec trt = find( z == 1 );
	uvec ctrl = find( z == 0 );
	vec yT = y.elem(trt);
	vec	yC = y.elem(ctrl);
	// assuming no missing vals	yT = yT[!is.na(yT)];
	double nX = yT.n_rows; //length(yT);
	double nY = yC.n_rows;
	double n = nX * nY/(nX + nY);
	colvec w = join_cols(yT, yC); // c(yT,yC)
	uvec orderedw = sort_index(w); //order(w)
	int totalN = nX + nY;
	colvec d(totalN);
	d.fill(1/nX);
	d.elem( find( orderedw > nX ) ).fill(-1/nY);
	// d = cumsum(Rcpp::ifelse(orderedw <= nX, 1/nX, -1/nY));
	colvec F = cumsum(d);
	// check for ties
	vec uniqF = unique(w);
	vec out = F;
	if ( uniqF.n_elem < totalN ){
		//uvec nonzerodiffs = find( diff(sort(F)) != 0 );
		vec Fnonzerodiffs = F.elem( find( diff(sort(F)) != 0 ) );
		vec out = Fnonzerodiffs; // I dont know why I cant use join_cols (Fnonzerodiffs,F(totalN-1))
		out.insert_rows(1,F(totalN-1)); // z <- z[c(which(diff(sort(w)) != 0), n.x + n.y)]
		}
	// return(max(abs(out)));
	return(max(abs(out)));
}

