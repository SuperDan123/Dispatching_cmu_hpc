// -*- mode: C++; c-indent-level: 4; c-basic-offset: 4; indent-tabs-mode: nil; -*-

// we only include RcppEigen.h which pulls Rcpp.h in for us
#include <RcppEigen.h>

// [[Rcpp::export]]
Eigen::VectorXd convert_parameters_from_structural_to_reduced_cpp(
    Eigen::VectorXd lambda,
    Eigen::VectorXd matching_params
) {
    // Structural params
    double lambda_W = lambda(0);
    double lambda_F = lambda(1);
    double alpha_W = matching_params(0);
    double alpha_F = matching_params(1);
    
    // Reduced form params
    Eigen::VectorXd beta = Eigen::VectorXd::Zero(4);

    // // beta_DW
    beta(0) = (1 + (alpha_F + 2 * (alpha_W - 1)) / 2 / (2 - alpha_W - alpha_F)) * lambda_W;
    // beta_SW
    beta(1) = alpha_W / 2 / (2 - alpha_W - alpha_F) * lambda_W;
    // beta_DF
    beta(2) = alpha_F / 2 / (2 - alpha_W - alpha_F) * lambda_F;
    // beta_SF
    beta(3) = (1 + (alpha_W + 2 * (alpha_F - 1)) / 2 / (2 - alpha_W - alpha_F)) * lambda_F;

    return beta;
}

// [[Rcpp::export]]
Eigen::VectorXd compute_actual_aggregator_cpp(
        Eigen::MatrixXd price_act,
        Eigen::VectorXd mu,
        Eigen::MatrixXd a_act,
        double w_0,
        Eigen::VectorXd lambda,
        Eigen::VectorXd matching_params,
        std::string spec = "linear"
) {
    Eigen::VectorXd beta = convert_parameters_from_structural_to_reduced_cpp(lambda, matching_params);
    double H_W = 0;
    double H_F = 0;
    
    double alpha_W = matching_params(0);
    double alpha_F = matching_params(1);
    Eigen::VectorXd mu_tilde = mu.array().pow(2 - (1 - alpha_W - alpha_F) / (2 - alpha_W - alpha_F));
    
    
    if (spec == "linear") {
        H_W = (mu_tilde.array() * (beta(0) * (a_act.col(0) + price_act.col(0)) + beta(2) * (a_act.col(1) - price_act.col(1))).array().exp()).sum();
        H_F = (mu_tilde.array() * (beta(1) * (a_act.col(0) + price_act.col(0)) + beta(3) * (a_act.col(1) - price_act.col(1))).array().exp()).sum();
        
        // Adding the contribution of outside option
        H_W = H_W + std::exp(beta(0) * w_0 + beta(2) * (- w_0));
        H_F = H_F + std::exp(beta(1) * w_0 + beta(3) * (- w_0));
        
    } else if (spec == "log") {
        H_W = (mu_tilde.array() * (beta(0) * (a_act.col(0) + price_act.col(0).array().log().matrix()) + beta(2) * (a_act.col(1) - price_act.col(1).array().log().matrix())).array().exp()).sum();
        H_F = (mu_tilde.array() * (beta(1) * (a_act.col(0) + price_act.col(0).array().log().matrix()) + beta(3) * (a_act.col(1) - price_act.col(1).array().log().matrix())).array().exp()).sum();
        
        // Adding the contribution of outside option
        H_W = H_W + std::exp(beta(0) * std::log(w_0) + beta(2) * (- std::log(w_0)));
        H_F = H_F + std::exp(beta(1) * std::log(w_0) + beta(3) * (- std::log(w_0)));
    } else if (spec == "log-linear") {
        H_W = (mu_tilde.array() * (beta(0) * (a_act.col(0) + price_act.col(0).array().log().matrix()) + beta(2) * (a_act.col(1) - price_act.col(1).array().matrix())).array().exp()).sum();
        H_F = (mu_tilde.array() * (beta(1) * (a_act.col(0) + price_act.col(0).array().log().matrix()) + beta(3) * (a_act.col(1) - price_act.col(1).array().matrix())).array().exp()).sum();
        
        // Adding the contribution of outside option
        H_W = H_W + std::exp(beta(0) * std::log(w_0) + beta(2) * (- std::log(w_0)));
        H_F = H_F + std::exp(beta(1) * std::log(w_0) + beta(3) * (- std::log(w_0)));
    }
    Eigen::VectorXd output = Eigen::VectorXd::Zero(2);
    output(0) = H_W;
    output(1) = H_F;
    return output;
}

// [[Rcpp::export]]
Eigen::VectorXd compute_match_cpp(
  Eigen::VectorXd mu,
  Eigen::MatrixXd s,
  Eigen::VectorXd S,
  Eigen::VectorXd matching_params
) {
  double alpha_W = matching_params(0);
  double alpha_F = matching_params(1);
  Eigen::VectorXd q = mu.array() * (s.col(0) * S(0)).array().pow(alpha_W) * (s.col(1) * S(1)).array().pow(alpha_F);
  return q;
}


// [[Rcpp::export]]
double compute_profit_cpp(
        Eigen::VectorXd price,
        Eigen::VectorXd s,
        double mu,
        Eigen::VectorXd mc,
        Eigen::VectorXd S,
        Eigen::VectorXd matching_params,
        double rp
){
    
    double alpha_W = matching_params(0);
    double alpha_F = matching_params(1);
    
    double q = mu * std::pow(s(0) * S(0), alpha_W) * std::pow(s(1) * S(1), alpha_F);
    double profit = q * (price(1) - price(0)) - s(0) * S(0) * mc(0) - s(1) * S(1) * mc(1);
    
    return profit;
}

// [[Rcpp::export]]
Eigen::VectorXd compute_profit_vec_cpp(
    Eigen::MatrixXd price,
    Eigen::MatrixXd s,
    Eigen::VectorXd mu,
    Eigen::MatrixXd mc,
    Eigen::VectorXd S,
    Eigen::VectorXd matching_params,
    double rp
){
  Eigen::VectorXd q =
    compute_match_cpp(
      mu,
      s,
      S,
      matching_params
    );
  Eigen::VectorXd profit = q.array() * (price.col(1) - price.col(0)).array() 
    - s.col(0).array() * S(0) * mc.col(0).array() - s.col(1).array() * S(1) * mc.col(1).array()
    - rp * price.col(0).array() * mc.col(0).array() + rp * price.col(1).array() * mc.col(1).array();
    
    return profit;
}



// [[Rcpp::export]]
Eigen::VectorXd compute_worker_indirect_utility_cpp(
        Eigen::VectorXd wage,
        Eigen::VectorXd a_W,
        Eigen::VectorXd n_W,
        Eigen::VectorXd n_F,
        Eigen::VectorXd shock,
        Eigen::VectorXd lambda,
        Eigen::VectorXd matching_params,
        std::string spec){
    
    int n = wage.size();
    Eigen::VectorXd u = Eigen::VectorXd::Zero(n);
    if (spec == "log"){
        for (int i; i < n; i++){
            u(i) = (a_W(i) + std::log(wage(i))) * lambda(0) - (1 - matching_params(0)) * std::log(n_W(i)) + matching_params(1) * std::log(n_F(i)) + shock(i);
        }
    } else if (spec == "linear") {
        for (int i; i < n; i++){
            u(i) = (a_W(i) + wage(i)) * lambda(0) - (1 - matching_params(0)) * std::log(n_W(i)) + matching_params(1) * std::log(n_F(i)) + shock(i);
        }
    } else if (spec == "log-linear") {
        for (int i; i < n; i++){
            u(i) = (a_W(i) + std::log(wage(i))) * lambda(0) - (1 - matching_params(0)) * std::log(n_W(i)) + matching_params(1) * std::log(n_F(i)) + shock(i);
        }
    }
    
    return u;
}

// [[Rcpp::export]]
Eigen::VectorXd compute_client_firm_indirect_utility_cpp(
        Eigen::VectorXd fee,
        Eigen::VectorXd a_F,
        Eigen::VectorXd n_W,
        Eigen::VectorXd n_F,
        Eigen::VectorXd shock,
        Eigen::VectorXd lambda,
        Eigen::VectorXd matching_params,
        std::string spec){
    
    int n = fee.size();
    Eigen::VectorXd u = Eigen::VectorXd::Zero(n);
    if (spec == "log"){  
        for (int i; i < n; i++){
            u(i) = (a_F(i) - std::log(fee(i))) * lambda(1) +  matching_params(0) * std::log(n_W(i))  - (1 - matching_params(1)) * std::log(n_F(i)) + shock(i);
        }
    } else if (spec == "linear") {
        for (int i; i < n; i++){
            u(i) = (a_F(i) - fee(i)) * lambda(1) +  matching_params(0) * std::log(n_W(i))  - (1 - matching_params(1)) * std::log(n_F(i)) + shock(i);
        }
    }else if (spec == "log-linear") {
        for (int i; i < n; i++){
            u(i) = (a_F(i) - fee(i)) * lambda(1) +  matching_params(0) * std::log(n_W(i))  - (1 - matching_params(1)) * std::log(n_F(i)) + shock(i);
        }
    }
    
    return u;
}

// [[Rcpp::export]]
Eigen::VectorXd compute_mu_tilde_cpp(
  Eigen::VectorXd mu,
  Eigen::VectorXd matching_params
) {
  double alpha_W = matching_params(0);
  double alpha_F = matching_params(1);
  Eigen::VectorXd mu_tilde = mu.array().pow(2 - (1 - alpha_W - alpha_F) / (2 - alpha_W - alpha_F));
  return mu_tilde;
}

// [[Rcpp::export]]
Eigen::VectorXd compute_aggregator_cpp(
    Eigen::MatrixXd price,
    Eigen::VectorXd mu,
    Eigen::MatrixXd a,
    double w_0,
    Eigen::VectorXd lambda,
    Eigen::VectorXd matching_params,
    std::string spec
) {
  Eigen::VectorXd beta = convert_parameters_from_structural_to_reduced_cpp(lambda, matching_params);
  double H_W = 0;
  double H_F = 0;
  
  Eigen::VectorXd mu_tilde = compute_mu_tilde_cpp(mu, matching_params);
  
  if (spec == "linear") {
    H_W = (mu_tilde.array() * (beta(0) * (a.col(0) + price.col(0)) + beta(2) * (a.col(1) - price.col(1))).array().exp()).sum();
    H_F = (mu_tilde.array() * (beta(1) * (a.col(0) + price.col(0)) + beta(3) * (a.col(1) - price.col(1))).array().exp()).sum();
    
    // Adding the contribution of outside option
    H_W = H_W + std::exp(beta(0) * w_0 + beta(2) * (- w_0));
    H_F = H_F + std::exp(beta(1) * w_0 + beta(3) * (- w_0));
    
  } else if (spec == "log") {
    H_W = (mu_tilde.array() * (beta(0) * (a.col(0) + price.col(0).array().log().matrix()) + beta(2) * (a.col(1) - price.col(1).array().log().matrix())).array().exp()).sum();
    H_F = (mu_tilde.array() * (beta(1) * (a.col(0) + price.col(0).array().log().matrix()) + beta(3) * (a.col(1) - price.col(1).array().log().matrix())).array().exp()).sum();
    
    // Adding the contribution of outside option
    H_W = H_W + std::exp(beta(0) * std::log(w_0) + beta(2) * (- std::log(w_0)));
    H_F = H_F + std::exp(beta(1) * std::log(w_0) + beta(3) * (- std::log(w_0)));
    
  } else if (spec == "log-linear") {
    H_W = (mu_tilde.array() * (beta(0) * (a.col(0) + price.col(0).array().log().matrix()) + beta(2) * (a.col(1) - price.col(1).array().matrix())).array().exp()).sum();
    H_F = (mu_tilde.array() * (beta(1) * (a.col(0) + price.col(0).array().log().matrix()) + beta(3) * (a.col(1) - price.col(1).array().matrix())).array().exp()).sum();
    
    // Adding the contribution of outside option
    H_W = H_W + std::exp(beta(0) * std::log(w_0) + beta(2) * (-w_0));
    H_F = H_F + std::exp(beta(1) * std::log(w_0) + beta(3) * (-w_0));
  }
  
  Eigen::VectorXd output = Eigen::VectorXd::Zero(2);
  output(0) = H_W;
  output(1) = H_F;
  return output;
}

// [[Rcpp::export]]
Eigen::VectorXd compute_inside_share_cpp(
    Eigen::VectorXd price,
    double mu,
    Eigen::VectorXd a,
    Eigen::VectorXd H,
    Eigen::VectorXd lambda,
    Eigen::VectorXd matching_params,
    std::string spec
) {
  Eigen::VectorXd beta = convert_parameters_from_structural_to_reduced_cpp(lambda, matching_params);
  Eigen::VectorXd mu_vec = Eigen::MatrixXd::Constant(1, 1, mu);
  Eigen::VectorXd mu_tilde = compute_mu_tilde_cpp(mu_vec, matching_params);
  
  Eigen::VectorXd s(2);
  if (spec == "linear") {
    s(0) = mu_tilde(0) * std::exp(beta(0) * (a(0) + price(0)) + beta(2) * (a(1) - price(1))) / H(0);
    s(1) = mu_tilde(0) * std::exp(beta(1) * (a(0) + price(0)) + beta(3) * (a(1) - price(1))) / H(1);
  } else if (spec == "log") {
    s(0) = mu_tilde(0) * std::exp(beta(0) * (a(0) + std::log(price(0))) + beta(2) * (a(1) - std::log(price(1)))) / H(0);
    s(1) = mu_tilde(0) * std::exp(beta(1) * (a(0) + std::log(price(0))) + beta(3) * (a(1) - std::log(price(1)))) / H(1);
  } else if (spec == "log-linear") {
    s(0) = mu_tilde(0) * std::exp(beta(0) * (a(0) + std::log(price(0))) + beta(2) * (a(1) - price(1))) / H(0);
    s(1) = mu_tilde(0) * std::exp(beta(1) * (a(0) + std::log(price(0))) + beta(3) * (a(1) - price(1))) / H(1);
  }
  
  return s;
}



// [[Rcpp::export]]
Eigen::MatrixXd compute_inside_share_vec_cpp(
        Eigen::MatrixXd price,
        Eigen::VectorXd mu,
        Eigen::MatrixXd a,
        Eigen::VectorXd H,
        Eigen::VectorXd lambda,
        Eigen::VectorXd matching_params,
        std::string spec
) {
    Eigen::VectorXd beta = convert_parameters_from_structural_to_reduced_cpp(lambda, matching_params);
    
    double alpha_W = matching_params(0);
    double alpha_F = matching_params(1);
    Eigen::VectorXd mu_tilde = mu.array().pow(2 - (1 - alpha_W - alpha_F) / (2 - alpha_W - alpha_F));
    
    int N = price.rows();
    Eigen::MatrixXd s(N, 2);
    if (spec == "linear") {
        s.col(0) = mu_tilde.array() * (beta(0) * (a.col(0) + price.col(0)) + beta(2) * (a.col(1) - price.col(1))).array().exp() / H(0);
        s.col(1) = mu_tilde.array() * (beta(1) * (a.col(0) + price.col(0)) + beta(3) * (a.col(1) - price.col(1))).array().exp() / H(1);
    } else if (spec == "log") {
        s.col(0) = mu_tilde.array() * (beta(0) * (a.col(0).array() + price.col(0).array().log()) + beta(2) * (a.col(1).array() - price.col(1).array().log())).exp() / H(0);
        s.col(1) = mu_tilde.array() * (beta(1) * (a.col(0).array() + price.col(0).array().log()) + beta(3) * (a.col(1).array() - price.col(1).array().log())).exp() / H(1);
    } else if (spec == "log-linear") {
        s.col(0) = mu_tilde.array() * (beta(0) * (a.col(0).array() + price.col(0).array().log()) + beta(2) * (a.col(1).array() - price.col(1).array())).exp() / H(0);
        s.col(1) = mu_tilde.array() * (beta(1) * (a.col(0).array() + price.col(0).array().log()) + beta(3) * (a.col(1).array() - price.col(1).array())).exp() / H(1);
    }
    
    return s;
}

// [[Rcpp::export]]
Eigen::MatrixXd compute_outside_share_cpp(
    double w_0,
    Eigen::VectorXd H,
    Eigen::VectorXd lambda,
    Eigen::VectorXd matching_params,
    std::string spec
) {
  Eigen::VectorXd beta = convert_parameters_from_structural_to_reduced_cpp(lambda, matching_params);
  Eigen::MatrixXd price = Eigen::MatrixXd::Constant(1, 2, w_0);
  Eigen::MatrixXd a = Eigen::MatrixXd::Zero(1, 2);
  Eigen::VectorXd mu = Eigen::VectorXd::Constant(1, 1);
  Eigen::MatrixXd s_0 = compute_inside_share_vec_cpp(price, mu, a, H, lambda, matching_params, spec);
  return s_0;
}

// [[Rcpp::export]]
Rcpp::List compute_derivatives_cpp(
    Eigen::VectorXd q,
    Eigen::MatrixXd price,
    Eigen::MatrixXd s,
    Eigen::VectorXd S,
    Eigen::VectorXd lambda,
    Eigen::VectorXd matching_params,
    std::string spec
) {
    int N = price.rows();
    double alpha_W = matching_params(0);
    double alpha_F = matching_params(1);
    Eigen::VectorXd beta = convert_parameters_from_structural_to_reduced_cpp(lambda, matching_params);
    double beta_DW = beta(0);
    double beta_SW = beta(1);
    double beta_DF = beta(2);
    double beta_SF = beta(3);

    
    Eigen::MatrixXd dq_dw(N, N);
    Eigen::MatrixXd dq_df(N, N);
    Eigen::MatrixXd dDw_dw(N, N);
    Eigen::MatrixXd dDw_df(N, N);
    Eigen::MatrixXd dDf_dw(N, N);
    Eigen::MatrixXd dDf_df(N, N);
    Eigen::MatrixXd ones = Eigen::MatrixXd::Ones(N, 1);
    
    
    // off diagonal
    dq_dw = -q * (alpha_W * beta_DW * s.col(0) + alpha_F * beta_SW * s.col(1)).transpose();
    dq_df = q * (alpha_W * beta_DF * s.col(0) + alpha_F * beta_SF * s.col(1)).transpose();
    dDw_dw = -S(0) * beta_DW * s.col(0) * s.col(0).transpose();
    dDf_dw = -S(1) * beta_SW * s.col(1) * s.col(1).transpose();
    dDw_df = S(0) * beta_DF * s.col(0) * s.col(0).transpose();
    dDf_df = S(1) * beta_SF * s.col(1) * s.col(1).transpose();
    
    // diagonal
    dq_dw.diagonal() =  q.array() * (alpha_W * beta_DW * (ones - s.col(0)) + alpha_F * beta_SW * (ones - s.col(1))).array();
    dq_df.diagonal() = -q.array() * (alpha_W * beta_DF * (ones - s.col(0)) + alpha_F * beta_SF * (ones - s.col(1))).array();
    dDw_dw.diagonal() = S(0) * beta_DW * (ones - s.col(0)).array() * s.col(0).array();
    dDf_dw.diagonal() = S(1) * beta_SW * (ones - s.col(1)).array() * s.col(1).array();
    dDw_df.diagonal() = -S(0) * beta_DF * (ones - s.col(0)).array() * s.col(0).array();
    dDf_df.diagonal() = -S(1) * beta_SF * (ones - s.col(1)).array() * s.col(1).array();
    
    
    if (spec == "log"){
      
      Eigen::MatrixXd p1 = price.col(0).transpose().replicate(N, 1);
      Eigen::MatrixXd p2 = price.col(1).transpose().replicate(N, 1);
      
      dq_dw = dq_dw.cwiseQuotient(p1);
      dq_df = dq_df.cwiseQuotient(p2);
      dDw_dw = dDw_dw.cwiseQuotient(p1);
      dDf_dw = dDf_dw.cwiseQuotient(p1);
      dDw_df = dDw_df.cwiseQuotient(p2);
      dDf_df = dDf_df.cwiseQuotient(p2);
    } else if (spec == "log-linear"){
      
      Eigen::MatrixXd p1 = price.col(0).transpose().replicate(N, 1);
      
      dq_dw = dq_dw.cwiseQuotient(p1);
      dDw_dw = dDw_dw.cwiseQuotient(p1);
      dDf_dw = dDf_dw.cwiseQuotient(p1);
      
    }
    
    Rcpp::List output = 
      Rcpp::List::create(
        Rcpp::Named("dq_dw") = dq_dw,
        Rcpp::Named("dq_df") = dq_df,
        Rcpp::Named("dDw_dw") = dDw_dw,
        Rcpp::Named("dDf_dw") = dDf_dw,
        Rcpp::Named("dDw_df") = dDw_df,
        Rcpp::Named("dDf_df") = dDf_df
      );
    return output;
}

// [[Rcpp::export]]
Eigen::VectorXd compute_foc_error_cpp(
        Eigen::VectorXd price,
        Eigen::VectorXd s,
        double mu,
        Eigen::VectorXd mc,
        Eigen::VectorXd H,
        Eigen::VectorXd S,
        Eigen::VectorXd lambda,
        Eigen::VectorXd matching_params,
        std::string spec,
        double rp
){
    
    price(0) = std::max(price(0), 1e-20);
    price(1) = std::max(price(1), 1e-20);
    
    Eigen::VectorXd beta = convert_parameters_from_structural_to_reduced_cpp(lambda, matching_params);

    double beta_DW = beta(0);
    double beta_SW = beta(1);
    double beta_DF = beta(2);
    double beta_SF = beta(3);

    double alpha_W = matching_params(0);
    double alpha_F = matching_params(1);

    double q = mu * std::pow(s(0) * S(0), alpha_W) * std::pow(s(1) * S(1), alpha_F);

    double dq_dw, dq_df, dDw_dw, dDw_df, dDf_dw, dDf_df;
    if (spec == "linear"){
        dq_dw = q * (alpha_W * beta_DW * (1 - s(0)) + alpha_F * beta_SW * (1 - s(1)));
        dq_df = -q * (alpha_W * beta_DF * (1 - s(0)) + alpha_F * beta_SF * (1 - s(1)));
        dDw_dw = S(0) * beta_DW * (1 - s(0)) * s(0);
        dDf_dw = S(1) * beta_SW * (1 - s(1)) * s(1);
        dDw_df = -S(0) * beta_DF * (1 - s(0)) * s(0);
        dDf_df = -S(1) * beta_SF * (1 - s(1)) * s(1);
    } else if (spec == "log"){
        dq_dw = q * (alpha_W * beta_DW * (1 - s(0)) + alpha_F * beta_SW * (1 - s(1))) / price(0);
        dq_df = -q * (alpha_W * beta_DF * (1 - s(0)) + alpha_F * beta_SF * (1 - s(1))) / price(1);
        dDw_dw = S(0) * beta_DW * (1 - s(0)) * s(0) / price(0);
        dDf_dw = S(1) * beta_SW * (1 - s(1)) * s(1) / price(0);
        dDw_df = -S(0) * beta_DF * (1 - s(0)) * s(0) / price(1);
        dDf_df = -S(1) * beta_SF * (1 - s(1)) * s(1) / price(1);
    } else if (spec == "log-linear"){
        dq_dw = q * (alpha_W * beta_DW * (1 - s(0)) + alpha_F * beta_SW * (1 - s(1))) / price(0);
        dq_df = -q * (alpha_W * beta_DF * (1 - s(0)) + alpha_F * beta_SF * (1 - s(1)));
        dDw_dw = S(0) * beta_DW * (1 - s(0)) * s(0) / price(0);
        dDf_dw = S(1) * beta_SW * (1 - s(1)) * s(1) / price(0);
        dDw_df = -S(0) * beta_DF * (1 - s(0)) * s(0);
        dDf_df = -S(1) * beta_SF * (1 - s(1)) * s(1);
    }

    Eigen::VectorXd foc_error = Eigen::VectorXd::Zero(2);
    foc_error(0) = - q + dq_dw * (price(1) - price(0)) - dDw_dw * mc(0) - dDf_dw * mc(1) - rp * mc(0);
    foc_error(1) = q + dq_df * (price(1) - price(0)) - dDw_df * mc(0) - dDf_df * mc(1) + rp * mc(1);

    return foc_error;
}

// 
// // [[Rcpp::export]]
// Eigen::MatrixXd compute_foc_error_jacobian_cpp(
//         Eigen::VectorXd price,
//         Eigen::VectorXd s,
//         double mu,
//         Eigen::VectorXd mc,
//         Eigen::VectorXd H,
//         Eigen::VectorXd S,
//         Eigen::VectorXd lambda,
//         Eigen::VectorXd matching_params,
//         std::string spec
// ){
//     
//     // avoid division by 0
//     price(0) = std::max(price(0), 1e-20);
//     price(1) = std::max(price(1), 1e-20);
// 
//     Eigen::VectorXd beta = convert_parameters_from_structural_to_reduced_cpp(lambda, matching_params);
//     
//     double beta_DW = beta(0);
//     double beta_SW = beta(1);
//     double beta_DF = beta(2);
//     double beta_SF = beta(3);
//     
//     double alpha_W = matching_params(0);
//     double alpha_F = matching_params(1);
//     
//     double q = mu * std::pow(s(0) * S(0), alpha_W) * std::pow(s(1) * S(1), alpha_F);
//     
//     double dq_dw, dq_df, dsw_dw, dsw_df, dsf_dw, dsf_df;
//     double dq_dwdw, dq_dfdf, dq_dwdf, dDw_dwdw, dDw_dwdf, dDw_dfdf, dDf_dfdf, dDf_dwdf, dDf_dwdw;
//     if (spec == "linear"){
//         dq_dw = q * (alpha_W * beta_DW * (1 - s(0)) + alpha_F * beta_SW * (1 - s(1)));
//         dq_df = -q * (alpha_W * beta_DF * (1 - s(0)) + alpha_F * beta_SF * (1 - s(1)));
//         dsw_dw = beta_DW * (1 - s(0)) * s(0);
//         dsf_dw = beta_SW * (1 - s(1)) * s(1);
//         dsw_df = -beta_DF * (1 - s(0)) * s(0);
//         dsf_df = -beta_SF * (1 - s(1)) * s(1);
//         
//         // second derivatives
//         dDw_dwdw = (S(0) * dsw_dw) * (-dsw_dw / (1 - s(0)) + dsw_dw / s(0));
//         dDw_dwdf = (S(0) * dsw_dw) * (-dsw_df / (1 - s(0)) + dsw_df / s(0));
//         dDw_dfdf = (S(0) * dsw_df) * (-dsw_df / (1 - s(0)) + dsw_df / s(0)); 
//         
//         dDf_dfdf = (S(1) * dsf_df) * (-dsf_df / (1 - s(1)) + dsf_df / s(1));
//         dDf_dwdf = (S(1) * dsf_df) * (-dsf_dw / (1 - s(1)) + dsf_dw / s(1));
//         dDf_dwdw = (S(1) * dsf_dw) * (-dsf_dw / (1 - s(1)) + dsf_dw / s(1));
//         
//         dq_dwdw = -(dq_dw) * (alpha_W * beta_DW * dsw_dw + alpha_F * beta_SW * dsf_dw) / (alpha_W * beta_DW * (1 - s(0)) + alpha_F * beta_SW * (1 - s(1)));
//         dq_dfdf = -(dq_df) * (alpha_W * beta_DF * dsw_df + alpha_F * beta_SF * dsf_df) / (alpha_W * beta_DF * (1 - s(0)) + alpha_F * beta_SF * (1 - s(1)));
//         dq_dwdf = -(dq_dw) * (alpha_W * beta_DW * dsw_df + alpha_F * beta_SW * dsf_df) / (alpha_W * beta_DW * (1 - s(0)) + alpha_F * beta_SW * (1 - s(1)));
//         
//     } else if (spec == "log"){
//         dq_dw = q * (alpha_W * beta_DW * (1 - s(0)) + alpha_F * beta_SW * (1 - s(1))) / price(0);
//         dq_df = -q * (alpha_W * beta_DF * (1 - s(0)) + alpha_F * beta_SF * (1 - s(1))) / price(1);
//         dsw_dw =  beta_DW * (1 - s(0)) * s(0) / price(0);
//         dsf_dw =  beta_SW * (1 - s(1)) * s(1) / price(0);
//         dsw_df = - beta_DF * (1 - s(0)) * s(0) / price(1);
//         dsf_df = - beta_SF * (1 - s(1)) * s(1) / price(1);
//         
//         // second derivatives
//         dDw_dwdw = (S(0) * dsw_dw) * (-dsw_dw / (1 - s(0)) + dsw_dw / s(0) - 1 / price(0));
//         dDw_dwdf = (S(0) * dsw_dw) * (-dsw_df / (1 - s(0)) + dsw_df / s(0));
//         dDw_dfdf = (S(0) * dsw_df) * (-dsw_df / (1 - s(0)) + dsw_df / s(0) - 1 / price(1));
// 
//         dDf_dfdf = (S(1) * dsf_df) * (-dsf_df / (1 - s(1)) + dsf_df / s(1) - 1 / price(1));
//         dDf_dwdf = (S(1) * dsf_df) * (-dsf_dw / (1 - s(1)) + dsf_dw / s(1));
//         dDf_dwdw = (S(1) * dsf_dw) * (-dsf_dw / (1 - s(1)) + dsf_dw / s(1) - 1 / price(0));
// 
//         dq_dwdw = (dq_dw) * (-(alpha_W * beta_DW * dsw_dw + alpha_F * beta_SW * dsf_dw) / (alpha_W * beta_DW * (1 - s(0)) + alpha_F * beta_SW * (1 - s(1))) - 1 / price(0));
//         dq_dfdf = (dq_df) * (-(alpha_W * beta_DF * dsw_df + alpha_F * beta_SF * dsf_df) / (alpha_W * beta_DF * (1 - s(0)) + alpha_F * beta_SF * (1 - s(1))) - 1 / price(1));
//         dq_dwdf = -(dq_dw) * (alpha_W * beta_DW * dsw_df + alpha_F * beta_SW * dsf_df) / (alpha_W * beta_DW * (1 - s(0)) + alpha_F * beta_SW * (1 - s(1)));
//             
//     }
//     
// 
//     Eigen::MatrixXd jacob(2, 2);
//     jacob(0, 0) = -2 * dq_dw + dq_dwdw * (price(1) - price(0)) - dDw_dwdw * mc(0) - dDf_dwdw * mc(1);
//     jacob(0, 1) = -dq_df + dq_dw + dq_dwdf * (price(1) - price(0)) - dDw_dwdf * mc(0) - dDf_dwdf * mc(1);
//     jacob(1, 0) = jacob(0, 1);
//     jacob(1, 1) = 2 * dq_df + dq_dfdf * (price(1) - price(0)) - dDw_dfdf * mc(0) - dDf_dfdf * mc(1);
//     
//     return jacob;
// }
// 
// 
// // [[Rcpp::export]]
// Eigen::MatrixXd compute_foc_error_jacobian_cross_cpp(
//         Eigen::VectorXd price_i,
//         Eigen::VectorXd price_j,
//         Eigen::VectorXd s_i,
//         Eigen::VectorXd s_j,
//         double mu_i,
//         Eigen::VectorXd mc_i,
//         Eigen::VectorXd H,
//         Eigen::VectorXd S,
//         Eigen::VectorXd lambda,
//         Eigen::VectorXd matching_params,
//         std::string spec
// ){
//     
// 
//     // avoid division by 0
//     price_i(0) = std::max(price_i(0), 1e-20);
//     price_i(1) = std::max(price_i(1), 1e-20);
//     price_j(0) = std::max(price_j(0), 1e-20);
//     price_j(1) = std::max(price_j(1), 1e-20);
// 
//     
//     Eigen::VectorXd beta = convert_parameters_from_structural_to_reduced_cpp(lambda, matching_params);
//     
//     double beta_DW = beta(0);
//     double beta_SW = beta(1);
//     double beta_DF = beta(2);
//     double beta_SF = beta(3);
//     
//     double alpha_W = matching_params(0);
//     double alpha_F = matching_params(1);
//     
//     double q_i = mu_i * std::pow(s_i(0) * S(0), alpha_W) * std::pow(s_i(1) * S(1), alpha_F);
//     
//     double dqi_dwi, dqi_dfi, dqi_dwj, dqi_dfj;
//     double dswi_dwi, dsfi_dwi, dswi_dfi, dsfi_dfi;
//     double dswi_dwj, dsfi_dwj, dswi_dfj, dsfi_dfj;
//     double dDwi_dwj_dwi, dDwi_dfj_dwi, dDwi_dwj_dfi, dDwi_dfj_dfi;
//     double dDfi_dwj_dwi, dDfi_dfj_dwi, dDfi_dwj_dfi, dDfi_dfj_dfi;
//     double dqi_dwj_dwi, dqi_dj_dwi, dqi_dfj_dwi, dqi_dwj_dfi, dqi_dfj_dfi;
//     if (spec == "linear"){
//         dqi_dwi = q_i * (alpha_W * beta_DW * (1 - s_i(0)) + alpha_F * beta_SW * (1 - s_i(1)));
//         dqi_dfi = -q_i * (alpha_W * beta_DF * (1 - s_i(0)) + alpha_F * beta_SF * (1 - s_i(1)));
//         dswi_dwi =  beta_DW * (1 - s_i(0)) * s_i(0);
//         dsfi_dwi =  beta_SW * (1 - s_i(1)) * s_i(1);
//         dswi_dfi = - beta_DF * (1 - s_i(0)) * s_i(0);
//         dsfi_dfi = - beta_SF * (1 - s_i(1)) * s_i(1);
//         
//         dqi_dwj = q_i * (alpha_W * beta_DW * (-s_j(0)) + alpha_F * beta_SW * (-s_j(1)));
//         dqi_dfj = -q_i * (alpha_W * beta_DF * (-s_j(0)) + alpha_F * beta_SF * (-s_j(1)));
//         dswi_dwj =  -beta_DW * s_i(0) * s_j(0);
//         dsfi_dwj =  -beta_SW * s_i(1) * s_j(1);
//         dswi_dfj = beta_DF * s_i(0) * s_j(0);
//         dsfi_dfj = beta_SF * s_i(1) * s_j(1);
//         
//         // second derivatives
//         dDwi_dwj_dwi = (S(0) * dswi_dwi) * (-dswi_dwj / (1 - s_i(0)) + dswi_dwj / s_i(0));
//         dDwi_dfj_dwi = (S(0) * dswi_dwi) * (-dswi_dfj / (1 - s_i(0)) + dswi_dfj / s_i(0));
//         dDwi_dwj_dfi = (S(0) * dswi_dfi) * (-dswi_dwj / (1 - s_i(0)) + dswi_dwj / s_i(0));
//         dDwi_dfj_dfi = (S(0) * dswi_dfi) * (-dswi_dfj / (1 - s_i(0)) + dswi_dfj / s_i(0));
//             
//         dDfi_dwj_dwi = (S(1) * dsfi_dwi) * (-dsfi_dwj / (1 - s_i(1)) + dsfi_dwj / s_i(1));
//         dDfi_dfj_dwi = (S(1) * dsfi_dwi) * (-dsfi_dfj / (1 - s_i(1)) + dsfi_dfj / s_i(1));
//         dDfi_dwj_dfi = (S(1) * dsfi_dfi) * (-dsfi_dwj / (1 - s_i(1)) + dsfi_dwj / s_i(1));
//         dDfi_dfj_dfi = (S(1) * dsfi_dfi) * (-dsfi_dfj / (1 - s_i(1)) + dsfi_dfj / s_i(1));
//             
//         dqi_dwj_dwi = -(dqi_dwi) * (alpha_W * beta_DW * dswi_dwj + alpha_F * beta_SW * dsfi_dwj) / (alpha_W * beta_DW * (1 - s_i(0)) + alpha_F * beta_SW * (1 - s_i(1)));
//         dqi_dfj_dwi = -(dqi_dwi) * (alpha_W * beta_DW * dswi_dfj + alpha_F * beta_SW * dsfi_dfj) / (alpha_W * beta_DW * (1 - s_i(0)) + alpha_F * beta_SW * (1 - s_i(1)));
//         dqi_dwj_dfi = -(dqi_dfi) * (alpha_W * beta_DF * dswi_dwj + alpha_F * beta_SF * dsfi_dwj) / (alpha_W * beta_DF * (1 - s_i(0)) + alpha_F * beta_SF * (1 - s_i(1)));
//         dqi_dfj_dfi = -(dqi_dfi) * (alpha_W * beta_DF * dswi_dfj + alpha_F * beta_SF * dsfi_dfj) / (alpha_W * beta_DF * (1 - s_i(0)) + alpha_F * beta_SF * (1 - s_i(1)));       
//     
//     } else if (spec == "log"){
//         dqi_dwi = q_i * (alpha_W * beta_DW * (1 - s_i(0)) + alpha_F * beta_SW * (1 - s_i(1))) / price_i(0);
//         dqi_dfi = -q_i * (alpha_W * beta_DF * (1 - s_i(0)) + alpha_F * beta_SF * (1 - s_i(1))) / price_i(1);
//         dswi_dwi =  beta_DW * (1 - s_i(0)) * s_i(0) / price_i(0);
//         dsfi_dwi =  beta_SW * (1 - s_i(1)) * s_i(1) / price_i(0);
//         dswi_dfi = - beta_DF * (1 - s_i(0)) * s_i(0) / price_i(1);
//         dsfi_dfi = - beta_SF * (1 - s_i(1)) * s_i(1) / price_i(1);
//         
//         dqi_dwj = q_i * (alpha_W * beta_DW * (-s_j(0)) + alpha_F * beta_SW * (-s_j(1))) / price_j(0);
//         dqi_dfj = -q_i * (alpha_W * beta_DF * (-s_j(0)) + alpha_F * beta_SF * (-s_j(1))) / price_j(1);
//         dswi_dwj =  -beta_DW * s_i(0) * s_j(0) / price_j(0);
//         dsfi_dwj =  -beta_SW * s_i(1) * s_j(1) / price_j(0);
//         dswi_dfj = beta_DF * s_i(0) * s_j(0) / price_j(1);
//         dsfi_dfj = beta_SF * s_i(1) * s_j(1) / price_j(1);
//         
//         // second derivatives
//         dDwi_dwj_dwi = (S(0) * dswi_dwi) * (-dswi_dwj / (1 - s_i(0)) + dswi_dwj / s_i(0));
//         dDwi_dfj_dwi = (S(0) * dswi_dwi) * (-dswi_dfj / (1 - s_i(0)) + dswi_dfj / s_i(0));
//         dDwi_dwj_dfi = (S(0) * dswi_dfi) * (-dswi_dwj / (1 - s_i(0)) + dswi_dwj / s_i(0));
//         dDwi_dfj_dfi = (S(0) * dswi_dfi) * (-dswi_dfj / (1 - s_i(0)) + dswi_dfj / s_i(0));
//         
//         dDfi_dwj_dwi = (S(1) * dsfi_dwi) * (-dsfi_dwj / (1 - s_i(1)) + dsfi_dwj / s_i(1));
//         dDfi_dfj_dwi = (S(1) * dsfi_dwi) * (-dsfi_dfj / (1 - s_i(1)) + dsfi_dfj / s_i(1));
//         dDfi_dwj_dfi = (S(1) * dsfi_dfi) * (-dsfi_dwj / (1 - s_i(1)) + dsfi_dwj / s_i(1));
//         dDfi_dfj_dfi = (S(1) * dsfi_dfi) * (-dsfi_dfj / (1 - s_i(1)) + dsfi_dfj / s_i(1));
//         
//         dqi_dwj_dwi = -(dqi_dwi) * (alpha_W * beta_DW * dswi_dwj + alpha_F * beta_SW * dsfi_dwj) / (alpha_W * beta_DW * (1 - s_i(0)) + alpha_F * beta_SW * (1 - s_i(1)));
//         dqi_dfj_dwi = -(dqi_dwi) * (alpha_W * beta_DW * dswi_dfj + alpha_F * beta_SW * dsfi_dfj) / (alpha_W * beta_DW * (1 - s_i(0)) + alpha_F * beta_SW * (1 - s_i(1)));
//         dqi_dwj_dfi = -(dqi_dfi) * (alpha_W * beta_DF * dswi_dwj + alpha_F * beta_SF * dsfi_dwj) / (alpha_W * beta_DF * (1 - s_i(0)) + alpha_F * beta_SF * (1 - s_i(1)));
//         dqi_dfj_dfi = -(dqi_dfi) * (alpha_W * beta_DF * dswi_dfj + alpha_F * beta_SF * dsfi_dfj) / (alpha_W * beta_DF * (1 - s_i(0)) + alpha_F * beta_SF * (1 - s_i(1)));       
//         
//     }
//     
//     
//     Eigen::MatrixXd jacob(2, 2);
//     jacob(0, 0) = -dqi_dwj + dqi_dwj_dwi * (price_i(1) - price_i(0)) - dDwi_dwj_dwi * mc_i(0) - dDfi_dwj_dwi * mc_i(1);
//     jacob(0, 1) = -dqi_dfj + dqi_dfj_dwi * (price_i(1) - price_i(0)) - dDwi_dfj_dwi * mc_i(0) - dDfi_dfj_dwi * mc_i(1);
//     jacob(1, 0) = dqi_dfj + dqi_dfj_dfi * (price_i(1) - price_i(0)) - dDwi_dfj_dfi * mc_i(0) - dDfi_dfj_dfi * mc_i(1);
//     jacob(1, 1) = dqi_dwj + dqi_dwj_dfi * (price_i(1) - price_i(0)) - dDwi_dwj_dfi * mc_i(0) - dDfi_dwj_dfi * mc_i(1);
//         
//     
//     return jacob;
// }
// 
// 


// [[Rcpp::export]]
Eigen::VectorXd compute_joint_equation_error_cpp(
        Eigen::VectorXd x,
        double w_0,
        Eigen::VectorXd mu,
        Eigen::MatrixXd a,
        Eigen::MatrixXd mc,
        Eigen::VectorXd S,
        Eigen::VectorXd lambda,
        Eigen::VectorXd matching_params,
        std::string spec,
        double rp
){
    
    int N = (x.size() - 2) / 2;
    Eigen::VectorXd H(2);
    Eigen::MatrixXd price(N, 2);
    
    H = x.head(2);
    price.col(0) = x.segment(2, N);
    price.col(1) = x.tail(N);
    
    Eigen::VectorXd joint_error(x.size());
    
    Eigen::MatrixXd s_0 = compute_outside_share_cpp(w_0, H, lambda, matching_params, spec);
    joint_error(0) = -1 + s_0(0, 0);
    joint_error(1) = -1 + s_0(0, 1);
    
    int idx = 2;
    for (int i = 0; i < N; i++){
        Eigen::VectorXd price_i = price.row(i);
        Eigen::VectorXd a_i = a.row(i);
        Eigen::VectorXd mc_i = mc.row(i);
        double mu_i = mu(i);
        
        Eigen::VectorXd share_i = compute_inside_share_cpp(price_i, mu_i, a_i, H, lambda, matching_params, spec);
        joint_error(0) += share_i(0);
        joint_error(1) += share_i(1);
        
        Eigen::VectorXd foc_error_i = compute_foc_error_cpp(price_i, share_i, mu_i, mc_i, H, S, lambda, matching_params, spec, rp);
        joint_error(idx) = foc_error_i(0);
        joint_error(idx + 1) = foc_error_i(1);
        
        idx += 2;
    }
    
    return joint_error;
}





// [[Rcpp::export]]
Eigen::VectorXd compute_foc_error_with_implied_aggregator_cpp(
        Eigen::VectorXd x,
        double w_0,
        Eigen::VectorXd mu,
        Eigen::MatrixXd a,
        Eigen::MatrixXd mc,
        Eigen::VectorXd S,
        Eigen::VectorXd lambda,
        Eigen::VectorXd matching_params,
        std::string spec,
        double rp
){
    
    int N = x.size() / 2;
    Eigen::MatrixXd price(N, 2);
    
    price.col(0) = x.head(N);
    price.col(1) = x.tail(N);
    
    Eigen::MatrixXd H = compute_aggregator_cpp(price, mu, a, w_0, lambda, matching_params, spec);
    Eigen::VectorXd foc_error(x.size());
    
    int idx = 0;
    for (int i = 0; i < N; i++){
        Eigen::VectorXd price_i = price.row(i);
        Eigen::VectorXd a_i = a.row(i);
        Eigen::VectorXd mc_i = mc.row(i);
        double mu_i = mu(i);
        
        Eigen::VectorXd share_i = compute_inside_share_cpp(price_i, mu_i, a_i, H, lambda, matching_params, spec);
        Eigen::VectorXd foc_error_i = compute_foc_error_cpp(price_i, share_i, mu_i, mc_i, H, S, lambda, matching_params, spec, rp);
        foc_error(idx) = foc_error_i(0);
        foc_error(idx + 1) = foc_error_i(1);
        
        idx += 2;
    }
    
    return foc_error;
}

// 
// // [[Rcpp::export]]
// Eigen::MatrixXd compute_foc_error_with_implied_aggregator_jacobian_cpp(
//         Eigen::VectorXd x,
//         double w_0,
//         Eigen::VectorXd mu,
//         Eigen::MatrixXd a,
//         Eigen::MatrixXd mc,
//         Eigen::VectorXd S,
//         Eigen::VectorXd lambda,
//         Eigen::VectorXd matching_params,
//         std::string spec,
//         double rp
// ){
//     
//     int N = x.size() / 2;
//     Eigen::MatrixXd price(N, 2);
//     
//     price.col(0) = x.head(N);
//     price.col(1) = x.tail(N);
//     
//     Eigen::MatrixXd H = compute_aggregator_cpp(price, a, w_0, lambda, matching_params, spec);
//     Eigen::MatrixXd jacob(x.size(), x.size());
//     Eigen::MatrixXd jacob_ij(2, 2);
//     
//     int idx_i = 0;
//     for (int i = 0; i < N; i++){
//         Eigen::VectorXd p_i = price.row(i);
//         Eigen::VectorXd a_i = a.row(i);
//         Eigen::VectorXd mc_i = mc.row(i);
//         double mu_i = mu(i);
//         Eigen::VectorXd s_i = compute_inside_share_cpp(p_i, a_i, H, lambda, matching_params, spec);
// 
//         int idx_j = 0;
//         for (int j = 0; j < N; j++){
//             Eigen::VectorXd p_j = price.row(j);
//             Eigen::VectorXd a_j = a.row(j);
//             Eigen::VectorXd mc_j = mc.row(j);
//             
//             Eigen::VectorXd s_j = compute_inside_share_cpp(p_j, a_j, H, lambda, matching_params, spec);            
//             
//             if (i == j){
//                 jacob_ij = compute_foc_error_jacobian_cpp(p_i, s_i, mu_i, mc_i, H, S, lambda, matching_params, spec);
//             } else {
//                 jacob_ij = compute_foc_error_jacobian_cross_cpp(p_i, p_j, s_i, s_j, mu_i, mc_i, H, S, lambda, matching_params, spec);     
//             }
//             
//             jacob(idx_i, idx_j) = jacob_ij(0, 0);
//             jacob(idx_i + 1, idx_j) = jacob_ij(1, 0);
//             jacob(idx_i, idx_j + N) = jacob_ij(0, 1);
//             jacob(idx_i + 1, idx_j + N) = jacob_ij(1, 1);
//             
//             idx_j += 1;
//         }
//         
//         idx_i += 2;
//     }
//     
//     return jacob;
// }
// 

// [[Rcpp::export]]
Eigen::VectorXd compute_foc_error_with_ownership_matrix_cpp(
    Eigen::VectorXd x,
    Eigen::MatrixXd ownership,
    double w_0,
    Eigen::VectorXd mu,
    Eigen::MatrixXd a,
    Eigen::MatrixXd mc,
    Eigen::VectorXd S,
    Eigen::VectorXd lambda,
    Eigen::VectorXd matching_params,
    std::string spec,
    double rp
) {
  int N = x.size() / 2;
  Eigen::MatrixXd price(N, 2);
  price.col(0) = x.head(N);
  price.col(1) = x.tail(N);
  Eigen::VectorXd H = compute_aggregator_cpp(price, mu, a, w_0, lambda, matching_params, spec);
  Eigen::MatrixXd s = compute_inside_share_vec_cpp(price, mu, a, H, lambda, matching_params, spec);
  Eigen::VectorXd q =
    compute_match_cpp(
      mu,
      s,
      S,
      matching_params
    );
  Rcpp::List derivatives = 
    compute_derivatives_cpp(
      q,
      price,
      s,
      S,
      lambda,
      matching_params,
      spec
    );
  Eigen::MatrixXd dq_dw(Rcpp::as<Eigen::MatrixXd>(derivatives["dq_dw"]));
  Eigen::MatrixXd dq_df(Rcpp::as<Eigen::MatrixXd>(derivatives["dq_df"]));
  Eigen::MatrixXd dDw_dw(Rcpp::as<Eigen::MatrixXd>(derivatives["dDw_dw"]));
  Eigen::MatrixXd dDw_df(Rcpp::as<Eigen::MatrixXd>(derivatives["dDw_df"]));
  Eigen::MatrixXd dDf_dw(Rcpp::as<Eigen::MatrixXd>(derivatives["dDf_dw"]));
  Eigen::MatrixXd dDf_df(Rcpp::as<Eigen::MatrixXd>(derivatives["dDf_df"]));
  
  int N_2 = 2 * N;
  Eigen::VectorXd foc_error(N_2);
  
  // FOCs wrt wages
  foc_error.head(N) = -q + ownership.cwiseProduct(dq_dw) * (price.col(1) - price.col(0))
    - ownership.cwiseProduct(dDw_dw) * mc.col(0) - ownership.cwiseProduct(dDf_dw) * mc.col(1)
    - rp * mc.col(0);
    
  // FOCs wrt fees
  foc_error.tail(N) =  q + ownership.cwiseProduct(dq_df) * (price.col(1) - price.col(0))
    - ownership.cwiseProduct(dDw_df) * mc.col(0) - ownership.cwiseProduct(dDf_df) * mc.col(1)
    + rp * mc.col(1);
    
  // Rescale to make FOC close to linear (Skrainka 2012)
  foc_error.head(N) = foc_error.head(N).array() / q.array();
  foc_error.tail(N) = foc_error.tail(N).array() / q.array();
  
  return foc_error;
}

// [[Rcpp::export]]
Eigen::MatrixXd compute_marginal_cost_for_each_market_cpp(
    Eigen::MatrixXd q,
    Eigen::MatrixXd ownership,
    Eigen::MatrixXd price,
    Eigen::MatrixXd s,
    Eigen::VectorXd S,
    Eigen::VectorXd lambda,
    Eigen::VectorXd matching_params,
    std::string spec,
    double rp
) { 
  int N = price.rows();
  Rcpp::List derivatives = 
    compute_derivatives_cpp(
      q,
      price,
      s,
      S,
      lambda,
      matching_params,
      spec
    );
  Eigen::MatrixXd dq_dw(Rcpp::as<Eigen::MatrixXd>(derivatives["dq_dw"]));
  Eigen::MatrixXd dq_df(Rcpp::as<Eigen::MatrixXd>(derivatives["dq_df"]));
  Eigen::MatrixXd dDw_dw(Rcpp::as<Eigen::MatrixXd>(derivatives["dDw_dw"]));
  Eigen::MatrixXd dDw_df(Rcpp::as<Eigen::MatrixXd>(derivatives["dDw_df"]));
  Eigen::MatrixXd dDf_dw(Rcpp::as<Eigen::MatrixXd>(derivatives["dDf_dw"]));
  Eigen::MatrixXd dDf_df(Rcpp::as<Eigen::MatrixXd>(derivatives["dDf_df"]));
  
  int N_2 = 2 * N;
  Eigen::MatrixXd A(N_2, N_2);
  Eigen::MatrixXd b(N_2, 1);
  
  A.topLeftCorner(N, N) = ownership.cwiseProduct(dDw_dw) + rp * Eigen::MatrixXd::Identity(N, N);
  A.topRightCorner(N, N) = ownership.cwiseProduct(dDf_dw);
  A.bottomLeftCorner(N, N) = ownership.cwiseProduct(dDw_df);
  A.bottomRightCorner(N, N) = ownership.cwiseProduct(dDf_df) - rp * Eigen::MatrixXd::Identity(N, N);
  
  b.topRows(N)    = -q + ownership.cwiseProduct(dq_dw) * (price.col(1) - price.col(0));
  b.bottomRows(N) =  q + ownership.cwiseProduct(dq_df) * (price.col(1) - price.col(0));
  
  Eigen::MatrixXd mc_long = A.lu().solve(b);
  Eigen::MatrixXd mc(N, 2);
  mc.col(0) = mc_long.topRows(N);
  mc.col(1) = mc_long.bottomRows(N);
  
  return mc;
}





// [[Rcpp::export]]
Eigen::MatrixXd compute_inside_share_single_market_cpp(
        Eigen::VectorXd H,
        Eigen::MatrixXd price,
        Eigen::VectorXd mu,
        double w_0,
        Eigen::MatrixXd a,
        Eigen::VectorXd S,
        Eigen::VectorXd lambda,
        Eigen::VectorXd matching_params,
        std::string spec
){
    
    int N = price.rows();
    Eigen::MatrixXd out(N, 2);
    for (int i = 0; i < N; i++){
        Eigen::VectorXd price_i = price.row(i);
        Eigen::VectorXd a_i = a.row(i);
        double mu_i = mu(i);
        
        Eigen::VectorXd share_i = compute_inside_share_cpp(price_i, mu_i, a_i, H, lambda, matching_params, spec);
        out.row(i) = share_i;
    }
    
    return out;
}




// [[Rcpp::export]]
double compute_union_foc_error_cpp(
        Eigen::VectorXd x,
        double w_0,
        Eigen::VectorXd mu,
        Eigen::MatrixXd a,
        Eigen::MatrixXd mc,
        Eigen::VectorXd S,
        Eigen::VectorXd lambda,
        Eigen::VectorXd matching_params,
        std::string spec,
        double rp
){  
    
    
    int N = x.size() / 3;
    
    Eigen::MatrixXd price(N, 2);
    price.col(0) = x.head(N);
    price.col(1) = x.segment(N, N);
    Eigen::VectorXd multiplier = x.tail(N);
    
    Eigen::VectorXd beta = convert_parameters_from_structural_to_reduced_cpp(lambda, matching_params);
    double beta_DW = beta(0);
    double beta_SW = beta(1);
    double beta_DF = beta(2);
    double beta_SF = beta(3);
    
    double alpha_W = matching_params(0);
    double alpha_F = matching_params(1);
    
    Eigen::VectorXd H = compute_aggregator_cpp(price, mu, a, w_0, lambda, matching_params, spec);
    Eigen::MatrixXd s = compute_inside_share_vec_cpp(price, mu, a, H, lambda, matching_params, spec);
    
    Eigen::VectorXd q = mu.array() * (s.col(0) * S(0)).array().pow(alpha_W) * (s.col(1) * S(1)).array().pow(alpha_F);
    
    Eigen::MatrixXd dWS(N, 2);
    Eigen::MatrixXd dq_dw(N, N);
    Eigen::MatrixXd dq_df(N, N);
    Eigen::MatrixXd dDw_dw(N, N);
    Eigen::MatrixXd dDw_df(N, N);
    Eigen::MatrixXd dDf_dw(N, N);
    Eigen::MatrixXd dDf_df(N, N);
    Eigen::MatrixXd ones = Eigen::MatrixXd::Ones(N, 1);
    
    // Worker surplus
    dWS.col(0) =  (2 - alpha_W) * beta_DW * s.col(0) - alpha_F * beta_SW * s.col(1);
    dWS.col(1) = -(2 - alpha_W) * beta_DF * s.col(0) + alpha_F * beta_SF * s.col(1);
        
    // off diagonal
    dq_dw = -q * (alpha_W * beta_DW * s.col(0) + alpha_F * beta_SW * s.col(1)).transpose();
    dq_df =  q * (alpha_W * beta_DF * s.col(0) + alpha_F * beta_SF * s.col(1)).transpose();
    dDw_dw = -S(0) * beta_DW * s.col(0) * s.col(0).transpose();
    dDf_dw = -S(1) * beta_SW * s.col(1) * s.col(1).transpose();
    dDw_df =  S(0) * beta_DF * s.col(0) * s.col(0).transpose();
    dDf_df =  S(1) * beta_SF * s.col(1) * s.col(1).transpose();
        
    // diagonal
    dq_dw.diagonal() =  q.array() * (alpha_W * beta_DW * (ones - s.col(0)) + alpha_F * beta_SW * (ones - s.col(1))).array();
    dq_df.diagonal() = -q.array() * (alpha_W * beta_DF * (ones - s.col(0)) + alpha_F * beta_SF * (ones - s.col(1))).array();
    dDw_dw.diagonal() =  S(0) * beta_DW * (ones - s.col(0)).array() * s.col(0).array();
    dDf_dw.diagonal() =  S(1) * beta_SW * (ones - s.col(1)).array() * s.col(1).array();
    dDw_df.diagonal() = -S(0) * beta_DF * (ones - s.col(0)).array() * s.col(0).array();
    dDf_df.diagonal() = -S(1) * beta_SF * (ones - s.col(1)).array() * s.col(1).array();
        
    if (spec == "log"){
        
        // Worker surplus
        Eigen::MatrixXd p1 = price.col(0).transpose().replicate(N, 1);
        Eigen::MatrixXd p2 = price.col(1).transpose().replicate(N, 1);
        
        dWS.col(0) = dWS.col(0).array() / price.col(0).array();
        dWS.col(1) = dWS.col(1).array() / price.col(1).array();
        dq_dw = dq_dw.cwiseQuotient(p1);
        dq_df = dq_df.cwiseQuotient(p2);
        dDw_dw = dDw_dw.cwiseQuotient(p1);
        dDf_dw = dDf_dw.cwiseQuotient(p1);
        dDw_df = dDw_df.cwiseQuotient(p2);
        dDf_df = dDf_df.cwiseQuotient(p2);
    } else if (spec == "log-linear"){
        
        // Worker surplus
        Eigen::MatrixXd p1 = price.col(0).transpose().replicate(N, 1);
        
        dWS.col(0) = dWS.col(0).array() / price.col(0).array();
        dq_dw = dq_dw.cwiseQuotient(p1);
        dDw_dw = dDw_dw.cwiseQuotient(p1);
        dDf_dw = dDf_dw.cwiseQuotient(p1);
    } 
    
    
    Eigen::VectorXd profit = compute_profit_vec_cpp(price, s, mu, mc, S, matching_params, rp);
    Eigen::VectorXd slack = (profit.array() > 0).cast< double >();
    multiplier = multiplier.array() * (1 - slack.array());
    
    Eigen::VectorXd dpi_w = -q + dq_dw * (price.col(1) - price.col(0)) - dDw_dw * mc.col(0) - dDf_dw * mc.col(1) - rp * mc.col(0);
    Eigen::VectorXd dpi_f =  q + dq_df * (price.col(1) - price.col(0)) - dDw_df * mc.col(0) - dDf_df * mc.col(1) + rp * mc.col(1);

    int N_2 = 2 * N;
    Eigen::VectorXd foc_error(N_2);
    
    foc_error.head(N) = dWS.col(0).array() + multiplier.array() * dpi_w.array();
    foc_error.tail(N) = dWS.col(1).array() + multiplier.array() * dpi_f.array();
    
    // Rescale to make FOC close to linear (Skrainka 2012)
    foc_error.head(N) = foc_error.head(N).array() / q.array();
    foc_error.tail(N) = foc_error.tail(N).array() / q.array();
            
    double out = foc_error.array().pow(2).mean();
            
    if (Rcpp::traits::is_nan<REALSXP>(out) | Rcpp::traits::is_infinite<REALSXP>(out)){
        out = 1e+12;
    }
            
    return out;
    
}




// 
// // [[Rcpp::export]]
// double compute_union_foc_error_for_single_market_cpp(
//         Eigen::VectorXd x,
//         double w_0,
//         Eigen::VectorXd mu,
//         Eigen::MatrixXd a,
//         Eigen::MatrixXd mc,
//         Eigen::VectorXd S,
//         Eigen::VectorXd lambda,
//         Eigen::VectorXd matching_params,
//         std::string spec,
//         double rp
// ){
//     
//     int N = x.size() / 3;
//     Eigen::MatrixXd price(N, 2);
//     
//     price.col(0) = x.head(N);
//     price.col(1) = x.segment(N, N); // N elements starting from idx = N (i.e. N+1 th element)
//     Eigen::VectorXd multiplier = x.tail(N);
//     
//     Eigen::VectorXd H = compute_aggregator_cpp(price, a, w_0, lambda, matching_params, spec);
//     double foc_error_sum = 0;
// 
//     for (int i = 0; i < N; i++){
//         Eigen::VectorXd price_i = price.row(i);
//         double multiplier_i = multiplier(i);
//         Eigen::VectorXd a_i = a.row(i);
//         Eigen::VectorXd mc_i = mc.row(i);
//         double mu_i = mu(i);
//         
//         Eigen::VectorXd share_i = compute_inside_share_cpp(price_i, a_i, H, lambda, matching_params, spec);
//         Eigen::VectorXd foc_error_i = compute_union_foc_error_cpp(price_i, multiplier_i, share_i, mu_i, mc_i, H, S, lambda, matching_params, spec, rp);
// 
//         foc_error_sum += std::abs(foc_error_i(0));
//         foc_error_sum += std::abs(foc_error_i(1));
//     }
//     
//     return foc_error_sum;
// }



// 
// 
// // [[Rcpp::export]]
// Eigen::VectorXd compute_foc_error_with_margin_constraint_cpp(
//         Eigen::VectorXd price,
//         double multiplier,
//         double margin_upper_bound,
//         Eigen::VectorXd s,
//         double mu,
//         Eigen::VectorXd mc,
//         Eigen::VectorXd H,
//         Eigen::VectorXd S,
//         Eigen::VectorXd lambda,
//         Eigen::VectorXd matching_params,
//         std::string spec,
//         double rp
// ){  
//     
//     double slack = std::log(price(1)) - std::log(price(0)) < margin_upper_bound;
//     multiplier = multiplier * (1 - slack);
//     double alpha_W = matching_params(0);
//     double alpha_F = matching_params(1);
//     
//     Eigen::VectorXd beta = convert_parameters_from_structural_to_reduced_cpp(lambda, matching_params);
//     
//     double beta_DW = beta(0);
//     double beta_SW = beta(1);
//     double beta_DF = beta(2);
//     double beta_SF = beta(3);
//     
//     double q = mu * std::pow(s(0) * S(0), alpha_W) * std::pow(s(1) * S(1), alpha_F);
//     double dq_dw, dq_df, dDw_dw, dDw_df, dDf_dw, dDf_df;
//     
//     if (spec == "linear"){
//         
//         dq_dw = q * (alpha_W * beta_DW * (1 - s(0)) + alpha_F * beta_SW * (1 - s(1)));
//         dq_df = -q * (alpha_W * beta_DF * (1 - s(0)) + alpha_F * beta_SF * (1 - s(1)));
//         dDw_dw = S(0) * beta_DW * (1 - s(0)) * s(0);
//         dDf_dw = S(1) * beta_SW * (1 - s(1)) * s(1);
//         dDw_df = -S(0) * beta_DF * (1 - s(0)) * s(0);
//         dDf_df = -S(1) * beta_SF * (1 - s(1)) * s(1);
//     } else if (spec == "log"){
//         
//         dq_dw = q * (alpha_W * beta_DW * (1 - s(0)) + alpha_F * beta_SW * (1 - s(1))) / price(0);
//         dq_df = -q * (alpha_W * beta_DF * (1 - s(0)) + alpha_F * beta_SF * (1 - s(1))) / price(1);
//         dDw_dw = S(0) * beta_DW * (1 - s(0)) * s(0) / price(0);
//         dDf_dw = S(1) * beta_SW * (1 - s(1)) * s(1) / price(0);
//         dDw_df = -S(0) * beta_DF * (1 - s(0)) * s(0) / price(1);
//         dDf_df = -S(1) * beta_SF * (1 - s(1)) * s(1) / price(1);
//     } 
//     
//     
//     
//     double dpi_w = - q + dq_dw * (price(1) - price(0)) - dDw_dw * mc(0) - dDf_dw * mc(1) - rp * mc(0);
//     double dpi_f = q + dq_df * (price(1) - price(0)) - dDw_df * mc(0) - dDf_df * mc(1) + rp * mc(1);
//     
//     Eigen::VectorXd foc_error(2);
//     foc_error(0) = dpi_w + multiplier / price(0);
//     foc_error(1) = dpi_f - multiplier / price(1);
//     
//     return foc_error;
//     
// }
// 
// 
// 
// 
// 
// // [[Rcpp::export]]
// double compute_foc_error_with_margin_constraint_for_single_market_cpp(
//         Eigen::VectorXd x,
//         double margin_upper_bound,
//         double w_0,
//         Eigen::VectorXd mu,
//         Eigen::MatrixXd a,
//         Eigen::MatrixXd mc,
//         Eigen::VectorXd S,
//         Eigen::VectorXd lambda,
//         Eigen::VectorXd matching_params,
//         std::string spec,
//         double rp
// ){
//     
//     int N = x.size() / 3;
//     Eigen::VectorXd margin = x.head(N);
//     Eigen::VectorXd wage = x.segment(N, N);
//     Eigen::VectorXd fee(N);
//     Eigen::VectorXd multiplier = x.tail(N);
//     
//     for (int i = 0; i < N; i++){
//         fee(i) = std::exp(margin(i) + std::log(wage(i)));
//     }    
//     
//     Eigen::MatrixXd price(N, 2);
//     price.col(0) = wage;
//     price.col(1) = fee;
//     
//     Eigen::VectorXd H = compute_aggregator_cpp(price, a, w_0, lambda, matching_params, spec);
//     double foc_error_sum = 0;
//     
//     for (int i = 0; i < N; i++){
//         Eigen::VectorXd price_i = price.row(i);
//         double multiplier_i = multiplier(i);
//         Eigen::VectorXd a_i = a.row(i);
//         Eigen::VectorXd mc_i = mc.row(i);
//         double mu_i = mu(i);
//         
//         Eigen::VectorXd share_i = compute_inside_share_cpp(price_i, a_i, H, lambda, matching_params, spec);
//         Eigen::VectorXd foc_error_i = compute_foc_error_with_margin_constraint_cpp(price_i, multiplier_i, margin_upper_bound, share_i, mu_i, mc_i, H, S, lambda, matching_params, spec, rp);
//         foc_error_sum += std::pow(foc_error_i(0), 2);
//         foc_error_sum += std::pow(foc_error_i(1), 2);
//     }
//     
//     if (Rcpp::traits::is_nan<REALSXP>(foc_error_sum) | Rcpp::traits::is_infinite<REALSXP>(foc_error_sum)){
//         foc_error_sum = 1e+10;
//     }
// 
//     return foc_error_sum;
// }


// [[Rcpp::export]]
Eigen::VectorXd compute_foc_error_with_margin_constraint_cpp(
        Eigen::VectorXd x,
        double margin_upper_bound,
        Eigen::MatrixXd ownership,
        double w_0,
        Eigen::VectorXd mu,
        Eigen::MatrixXd a,
        Eigen::MatrixXd mc,
        Eigen::VectorXd S,
        Eigen::VectorXd lambda,
        Eigen::VectorXd matching_params,
        std::string spec,
        double rp
){
    
    int N = x.size() / 3;
    
    Eigen::MatrixXd price(N, 2);
    Eigen::VectorXd margin = x.head(N);
    Eigen::VectorXd wage = x.segment(N, N);
    Eigen::VectorXd multiplier = x.tail(N);
    Eigen::VectorXd fee = (margin.array() + wage.array().log()).exp();
    price.col(0) = wage;
    price.col(1) = fee;
    
    Eigen::VectorXd beta = convert_parameters_from_structural_to_reduced_cpp(lambda, matching_params);
    double beta_DW = beta(0);
    double beta_SW = beta(1);
    double beta_DF = beta(2);
    double beta_SF = beta(3);
    
    double alpha_W = matching_params(0);
    double alpha_F = matching_params(1);
    
    Eigen::VectorXd H = compute_aggregator_cpp(price, mu, a, w_0, lambda, matching_params, spec);
    Eigen::MatrixXd s = compute_inside_share_vec_cpp(price, mu, a, H, lambda, matching_params, spec);
    
    Eigen::VectorXd q = mu.array() * (s.col(0) * S(0)).array().pow(alpha_W) * (s.col(1) * S(1)).array().pow(alpha_F);
    
    Eigen::MatrixXd dq_dw(N, N);
    Eigen::MatrixXd dq_df(N, N);
    Eigen::MatrixXd dDw_dw(N, N);
    Eigen::MatrixXd dDw_df(N, N);
    Eigen::MatrixXd dDf_dw(N, N);
    Eigen::MatrixXd dDf_df(N, N);
    Eigen::MatrixXd ones = Eigen::MatrixXd::Ones(N, 1);
    
    
    // off diagonal
    dq_dw = -q * (alpha_W * beta_DW * s.col(0) + alpha_F * beta_SW * s.col(1)).transpose();
    dq_df = q * (alpha_W * beta_DF * s.col(0) + alpha_F * beta_SF * s.col(1)).transpose();
    dDw_dw = -S(0) * beta_DW * s.col(0) * s.col(0).transpose();
    dDf_dw = -S(1) * beta_SW * s.col(1) * s.col(1).transpose();
    dDw_df = S(0) * beta_DF * s.col(0) * s.col(0).transpose();
    dDf_df = S(1) * beta_SF * s.col(1) * s.col(1).transpose();
    
    // diagonal
    dq_dw.diagonal() =  q.array() * (alpha_W * beta_DW * (ones - s.col(0)) + alpha_F * beta_SW * (ones - s.col(1))).array();
    dq_df.diagonal() = -q.array() * (alpha_W * beta_DF * (ones - s.col(0)) + alpha_F * beta_SF * (ones - s.col(1))).array();
    dDw_dw.diagonal() = S(0) * beta_DW * (ones - s.col(0)).array() * s.col(0).array();
    dDf_dw.diagonal() = S(1) * beta_SW * (ones - s.col(1)).array() * s.col(1).array();
    dDw_df.diagonal() = -S(0) * beta_DF * (ones - s.col(0)).array() * s.col(0).array();
    dDf_df.diagonal() = -S(1) * beta_SF * (ones - s.col(1)).array() * s.col(1).array();
    
    
    if (spec == "log"){
        
        Eigen::MatrixXd p1 = price.col(0).transpose().replicate(N, 1);
        Eigen::MatrixXd p2 = price.col(1).transpose().replicate(N, 1);
        
        dq_dw = dq_dw.cwiseQuotient(p1);
        dq_df = dq_df.cwiseQuotient(p2);
        dDw_dw = dDw_dw.cwiseQuotient(p1);
        dDf_dw = dDf_dw.cwiseQuotient(p1);
        dDw_df = dDw_df.cwiseQuotient(p2);
        dDf_df = dDf_df.cwiseQuotient(p2);
    } else if (spec == "log-linear"){
        
        Eigen::MatrixXd p1 = price.col(0).transpose().replicate(N, 1);
        
        dq_dw = dq_dw.cwiseQuotient(p1);
        dDw_dw = dDw_dw.cwiseQuotient(p1);
        dDf_dw = dDf_dw.cwiseQuotient(p1);
    }
    

    int N_2 = 2 * N;
    Eigen::VectorXd foc_error(N_2);
    
    // FOCs wrt wages
    foc_error.head(N) = -q + ownership.cwiseProduct(dq_dw) * (price.col(1) - price.col(0))
        - ownership.cwiseProduct(dDw_dw) * mc.col(0) - ownership.cwiseProduct(dDf_dw) * mc.col(1)
        - rp * mc.col(0) - multiplier.cwiseQuotient(price.col(0));

    // FOCs wrt fees
    foc_error.tail(N) =  q + ownership.cwiseProduct(dq_df) * (price.col(1) - price.col(0))
        - ownership.cwiseProduct(dDw_df) * mc.col(0) - ownership.cwiseProduct(dDf_df) * mc.col(1)
        + rp * mc.col(1) + multiplier.cwiseQuotient(price.col(1));



    // Rescale to make FOC close to linear (Skrainka 2012)
    foc_error.head(N) = foc_error.head(N).array() / q.array();
    foc_error.tail(N) = foc_error.tail(N).array() / q.array();
        
    return foc_error;
}


// [[Rcpp::export]]
double compute_equilibrium_objective_with_margin_constraint_cpp(
        Eigen::VectorXd x,
        double margin_upper_bound,
        Eigen::MatrixXd ownership,
        double w_0,
        Eigen::VectorXd mu,
        Eigen::MatrixXd a,
        Eigen::MatrixXd mc,
        Eigen::VectorXd S,
        Eigen::VectorXd lambda,
        Eigen::VectorXd matching_params,
        std::string spec,
        double rp
){
    
    int N = x.size() / 3;
    
    Eigen::MatrixXd price(N, 2);
    Eigen::VectorXd margin = x.head(N);
    Eigen::VectorXd wage = x.segment(N, N);
    Eigen::VectorXd multiplier = x.tail(N);
    Eigen::VectorXd fee = (margin.array() + wage.array().log()).exp();
    price.col(0) = wage;
    price.col(1) = fee;
    
    
    int N_2 = 2 * N;
    int N_3 = 3 * N;
    Eigen::VectorXd obj(N_3);
    
    obj.head(N_2) = compute_foc_error_with_margin_constraint_cpp(x, margin_upper_bound, ownership, w_0, mu, a, mc, S, lambda, matching_params, spec, rp);
    obj.tail(N) = multiplier.array() * ((price.col(1).array().log() - price.col(0).array().log()) - margin_upper_bound);
    
    double out = obj.array().abs().maxCoeff();
    
    return out;
}


// [[Rcpp::export]]
double compute_foc_error_with_minimum_wage_cpp(
        Eigen::VectorXd x,
        Eigen::MatrixXd ownership,
        double minimum_wage,
        double w_0,
        Eigen::VectorXd mu,
        Eigen::MatrixXd a,
        Eigen::MatrixXd mc,
        Eigen::VectorXd S,
        Eigen::VectorXd lambda,
        Eigen::VectorXd matching_params,
        std::string spec,
        double rp
){
    
    int N = x.size() / 3;
    
    Eigen::MatrixXd price(N, 2);
    price.col(0) = x.head(N);
    price.col(1) = x.segment(N, N);
    Eigen::VectorXd multiplier = x.tail(N);

    Eigen::VectorXd beta = convert_parameters_from_structural_to_reduced_cpp(lambda, matching_params);
    double beta_DW = beta(0);
    double beta_SW = beta(1);
    double beta_DF = beta(2);
    double beta_SF = beta(3);
    
    double alpha_W = matching_params(0);
    double alpha_F = matching_params(1);
    
    Eigen::VectorXd H = compute_aggregator_cpp(price, mu, a, w_0, lambda, matching_params, spec);
    Eigen::MatrixXd s = compute_inside_share_vec_cpp(price, mu, a, H, lambda, matching_params, spec);
    
    Eigen::VectorXd q = mu.array() * (s.col(0) * S(0)).array().pow(alpha_W) * (s.col(1) * S(1)).array().pow(alpha_F);
    
    Eigen::MatrixXd dq_dw(N, N);
    Eigen::MatrixXd dq_df(N, N);
    Eigen::MatrixXd dDw_dw(N, N);
    Eigen::MatrixXd dDw_df(N, N);
    Eigen::MatrixXd dDf_dw(N, N);
    Eigen::MatrixXd dDf_df(N, N);
    Eigen::MatrixXd ones = Eigen::MatrixXd::Ones(N, 1);
    
    
    // off diagonal
    dq_dw = -q * (alpha_W * beta_DW * s.col(0) + alpha_F * beta_SW * s.col(1)).transpose();
    dq_df = q * (alpha_W * beta_DF * s.col(0) + alpha_F * beta_SF * s.col(1)).transpose();
    dDw_dw = -S(0) * beta_DW * s.col(0) * s.col(0).transpose();
    dDf_dw = -S(1) * beta_SW * s.col(1) * s.col(1).transpose();
    dDw_df = S(0) * beta_DF * s.col(0) * s.col(0).transpose();
    dDf_df = S(1) * beta_SF * s.col(1) * s.col(1).transpose();
    
    // diagonal
    dq_dw.diagonal() =  q.array() * (alpha_W * beta_DW * (ones - s.col(0)) + alpha_F * beta_SW * (ones - s.col(1))).array();
    dq_df.diagonal() = -q.array() * (alpha_W * beta_DF * (ones - s.col(0)) + alpha_F * beta_SF * (ones - s.col(1))).array();
    dDw_dw.diagonal() = S(0) * beta_DW * (ones - s.col(0)).array() * s.col(0).array();
    dDf_dw.diagonal() = S(1) * beta_SW * (ones - s.col(1)).array() * s.col(1).array();
    dDw_df.diagonal() = -S(0) * beta_DF * (ones - s.col(0)).array() * s.col(0).array();
    dDf_df.diagonal() = -S(1) * beta_SF * (ones - s.col(1)).array() * s.col(1).array();
    
    
    if (spec == "log"){
        
        Eigen::MatrixXd p1 = price.col(0).transpose().replicate(N, 1);
        Eigen::MatrixXd p2 = price.col(1).transpose().replicate(N, 1);
        
        dq_dw = dq_dw.cwiseQuotient(p1);
        dq_df = dq_df.cwiseQuotient(p2);
        dDw_dw = dDw_dw.cwiseQuotient(p1);
        dDf_dw = dDf_dw.cwiseQuotient(p1);
        dDw_df = dDw_df.cwiseQuotient(p2);
        dDf_df = dDf_df.cwiseQuotient(p2);
    } else if (spec == "log-linear"){
        
        Eigen::MatrixXd p1 = price.col(0).transpose().replicate(N, 1);
        
        dq_dw = dq_dw.cwiseQuotient(p1);
        dDw_dw = dDw_dw.cwiseQuotient(p1);
        dDf_dw = dDf_dw.cwiseQuotient(p1);
    }
    

    int N_3 = 3 * N;
    Eigen::VectorXd foc_error(N_3);
    
    // FOCs wrt wages
    foc_error.head(N) = -q + ownership.cwiseProduct(dq_dw) * (price.col(1) - price.col(0))
        - ownership.cwiseProduct(dDw_dw) * mc.col(0) - ownership.cwiseProduct(dDf_dw) * mc.col(1)
        - rp * mc.col(0) - multiplier;

    // FOCs wrt fees
    foc_error.segment(N, N) =  q + ownership.cwiseProduct(dq_df) * (price.col(1) - price.col(0))
        - ownership.cwiseProduct(dDw_df) * mc.col(0) - ownership.cwiseProduct(dDf_df) * mc.col(1)
        + rp * mc.col(1);

    // complementary slackness
    foc_error.tail(N) = multiplier.array() * (minimum_wage - price.col(0).array());
   
    // Rescale to make FOC close to linear (Skrainka 2012)
    foc_error.head(N) = foc_error.head(N).array() / q.array();
    foc_error.segment(N, N) = foc_error.segment(N, N).array() / q.array();
    
    double out = foc_error.array().abs().maxCoeff();
    
    return out;
}


// Added by Kohei Kawaguchi

// [[Rcpp::export]]
Eigen::MatrixXd compute_payoff_vec_cpp(
    Eigen::MatrixXd price, 
    Eigen::VectorXd mu, 
    Eigen::MatrixXd a, 
    Eigen::MatrixXd mc,
    double w_0, 
    Eigen::VectorXd S,
    Eigen::VectorXd lambda, 
    Eigen::VectorXd matching_params, 
    double rp,
    std::string spec
) {
  Eigen::MatrixXd H =
    compute_aggregator_cpp(
      price, 
      mu, 
      a, 
      w_0, 
      lambda, 
      matching_params, 
      spec
    );
  Eigen::MatrixXd s =
    compute_inside_share_single_market_cpp(
      H, 
      price, 
      mu, 
      w_0, 
      a, 
      S, 
      lambda, 
      matching_params, 
      spec
    );
  Eigen::MatrixXd profit =
    compute_profit_vec_cpp(
      price, 
      s, 
      mu, 
      mc, 
      S, 
      matching_params, 
      rp
    );
  return profit;
}


// [[Rcpp::export]]
double compute_profit_owner_cpp(
    Eigen::VectorXd x,
    Eigen::ArrayXi index,
    Eigen::MatrixXd price,
    Eigen::VectorXd mu,
    Eigen::MatrixXd a,
    double w_0,
    Eigen::VectorXd lambda,
    Eigen::VectorXd matching_params,
    std::string spec,
    Eigen::VectorXd S,
    Eigen::MatrixXd mc,
    double rp
) {
  // adjust for 0-starting index
  index = index - 1;
  int num_est = x.size() / 2;
  // ensures fee >= wage
  x.tail(num_est) = x.tail(num_est) + x.head(num_est);
  // insert the relevant price
  for (int i = 0; i < index.size(); i++) {
    price(index(i), 0) = x(i);
    price(index(i), 1) = x(num_est + i);
  }
  
  // calculate the profit
  Eigen::VectorXd profit = 
    compute_payoff_vec_cpp(
      price, 
      mu, 
      a, 
      mc,
      w_0, 
      S,
      lambda, 
      matching_params, 
      rp,
      spec
    );
  // profit of the owner
  double profit_owner = 0;
  for (int i = 0; i < index.size(); i++) {
    profit_owner = profit_owner + profit(index(i));
  }
  return(profit_owner);
}
