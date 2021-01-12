#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
NumericVector EM_loop(double beta_hat, double Z_hat, double sigma_sq_hat, double p_hat,
                      NumericVector port_exc_ret, NumericVector mkt_exc_ret, NumericVector mkt_sigma,
                      double tol, int MaxIter, int criterion, double start_timestamp, double end_timestamp){
  double delta = 1.0;
  int cnt = 0;
  bool flag = TRUE;
  NumericVector eta_pos, eta_neg, p_hat_t, D_hat_t;
  while(flag){
    cnt += 1;

    // compute p_hat_it
    eta_pos = exp( - pow((port_exc_ret - beta_hat * mkt_exc_ret - Z_hat * mkt_sigma), 2.0) / (2 * sigma_sq_hat));
    eta_neg = exp( - pow((port_exc_ret - beta_hat * mkt_exc_ret + Z_hat * mkt_sigma), 2.0) / (2 * sigma_sq_hat));
    p_hat_t = eta_pos * p_hat / (eta_pos * p_hat + eta_neg * (1 - p_hat));

    // solve equation system to update beta_hat and Z_hat
    D_hat_t = 2 * p_hat_t - 1;

    // get elements of LHS
    double LHS11 = sum(pow(mkt_exc_ret, 2.0));
    double LHS21 = sum(D_hat_t * mkt_exc_ret * mkt_sigma);
    double LHS22 = sum(pow(mkt_sigma, 2.0));
    double RHS1 = sum(port_exc_ret * mkt_exc_ret);
    double RHS2 = sum(D_hat_t * port_exc_ret * mkt_sigma);
    double beta_hat_new = (LHS22 * RHS1 - LHS21 * RHS2) / (LHS11 * LHS22 - pow(LHS21, 2));
    double Z_hat_new = (LHS11 * RHS2 - LHS21 * RHS1) / (LHS11 * LHS22 - pow(LHS21, 2));
    if(Z_hat_new < 0){
      Z_hat_new = - Z_hat_new;
    };

    // update sigma_sq_hat
    double sigma_sq_hat_new = mean(pow(port_exc_ret - beta_hat * mkt_exc_ret - Z_hat_new * D_hat_t * mkt_sigma, 2.0) + pow(Z_hat_new, 2.0) * (1-pow(D_hat_t, 2.0)) * pow(mkt_sigma, 2.0));

    // update p_hat
    double p_hat_new = mean(p_hat_t);

    //testing the difference
    NumericVector diff(4);
    if(criterion == 1){
      diff[0] = (beta_hat_new - beta_hat)/abs(beta_hat);
      diff[1] = (Z_hat_new - Z_hat)/abs(Z_hat);
      diff[2] = (sigma_sq_hat_new - sigma_sq_hat)/abs(sigma_sq_hat);
      diff[3] = (p_hat_new - p_hat)/abs(p_hat);
    }
    if(criterion == 2){
      diff[0] = (beta_hat_new - beta_hat)/(abs(beta_hat) + 1);
      diff[1] = (Z_hat_new - Z_hat)/(abs(Z_hat) + 1);
      diff[2] = (sigma_sq_hat_new - sigma_sq_hat)/(abs(sigma_sq_hat) + 1);
      diff[3] = (p_hat_new - p_hat)/(abs(p_hat) + 1);
    }
    delta = max(abs(diff));
    if((delta < tol) | (cnt > MaxIter)){
      flag = FALSE;
      if(cnt > MaxIter){
        Rcout << "The time period:  " << start_timestamp << " - " << end_timestamp << std::endl;
        Rcout << "Excess the MaxIter: " << cnt << std::endl;
      }
    }

    // updating the parameters
    beta_hat = beta_hat_new;
    Z_hat = Z_hat_new;
    sigma_sq_hat = sigma_sq_hat_new;
    p_hat = p_hat_new;
  }
  double Z_star = Z_hat * (2 * p_hat - 1);
  NumericVector out = NumericVector::create(beta_hat, Z_star);
  return out;
}
